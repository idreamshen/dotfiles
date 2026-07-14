;;; ttyd-drop.el --- Browser drag-and-drop bridge for ttyd -*- lexical-binding: t; -*-

;; Serves a wrapper page embedding the ttyd terminal (port 7681) and an
;; /upload endpoint on port 7682, running inside the Emacs daemon via the
;; `web-server' package.  Dropped files are staged under
;; ~/.local/share/ttyd-drop/ and dispatched by foreground buffer mode:
;; Dired buffers receive a copy of the files; agent-shell buffers get an
;; attachment reference inserted at the prompt; anything else keeps the
;; files staged for manual pickup.
;;
;; Trust model matches ttyd itself: no authentication, access control
;; relies entirely on VPN/firewall.

(require 'cl-lib)
(require 'dired)
(require 'eieio)                        ; with-slots
(require 'subr-x)                       ; string-join, string-empty-p

(defvar my/ttyd-drop-port 7682
  "Port the ttyd drop bridge listens on.")

(defvar my/ttyd-drop--server nil
  "Running `ws-server' instance, or nil.")

(defvar my/ttyd-drop-staging-directory
  (expand-file-name "ttyd-drop" "~/.local/share/")
  "Directory where dropped files are staged before dispatch.")

(defvar my/ttyd-drop-page
  (expand-file-name "ttyd-drop.html" user-emacs-directory)
  "Path of the static wrapper page served at /.")

(defun my/ttyd-drop--target-buffer ()
  "Return the buffer in the selected Emacs window.
This assumes a single-user devbox with normally one active ttyd Emacs
session.  With several simultaneous client frames, the daemon's
selected frame determines the destination."
  (window-buffer (selected-window)))

(defun my/ttyd-drop--stage-files (parts)
  "Write multipart PARTS into a fresh staging directory.
Each part is an alist with `filename' and `content' entries.  Returns
the list of absolute staged file paths."
  (make-directory my/ttyd-drop-staging-directory t)
  (set-file-modes my/ttyd-drop-staging-directory #o700)
  (let* ((names
          (mapcar
           (lambda (part)
             (let ((name (file-name-nondirectory
                          (or (cdr (assoc 'filename part)) ""))))
               (if (or (string-empty-p name) (member name '("." "..")))
                   "unnamed"
                 name)))
           parts))
         (unique-names (delete-dups (copy-sequence names))))
    (unless (= (length names) (length unique-names))
      (error "Duplicate file names in one upload are not supported"))
    (let* ((temporary-file-directory my/ttyd-drop-staging-directory)
           (dir (make-temp-file (format-time-string "%Y%m%d-%H%M%S-") t)))
      (cl-mapcar
       (lambda (part name)
         (let ((path (expand-file-name name dir))
               (coding-system-for-write 'binary))
           (write-region (cdr (assoc 'content part)) nil path nil 'silent)
           path))
       parts names))))

(defun my/ttyd-dispatch-dropped-files (files)
  "Dispatch staged FILES to the foreground buffer.  Returns a status string."
  (let ((buf (my/ttyd-drop--target-buffer)))
    (cond
     ((with-current-buffer buf (derived-mode-p 'dired-mode))
      (let ((dest (with-current-buffer buf (dired-current-directory))))
        (dolist (src files)
          (copy-file src
                     (expand-file-name (file-name-nondirectory src) dest)
                     nil))
        (with-current-buffer buf (revert-buffer))
        (format "dired: %d file(s) → %s" (length files) dest)))
     ((and (with-current-buffer buf (derived-mode-p 'agent-shell-mode))
           (fboundp 'agent-shell-insert)
           (fboundp 'agent-shell--get-files-context))
      ;; Files stay staged so the agent can read them later.
      (with-current-buffer buf
        (agent-shell-insert
         :text (agent-shell--get-files-context :files files)
         :shell-buffer buf
         :no-focus t))
      (format "agent-shell: %d attachment(s) → %s" (length files) (buffer-name buf)))
     (t
      (format "staged (foreground is %s): %s"
              (buffer-name buf) (string-join files " "))))))

(defun my/ttyd-drop--handle-index (request)
  (with-slots (process) request
    (if (file-readable-p my/ttyd-drop-page)
        (ws-send-file process my/ttyd-drop-page "text/html; charset=utf-8")
      (ws-response-header process 500 '("Content-type" . "text/plain"))
      (process-send-string process "ttyd-drop.html not found"))))

(defun my/ttyd-drop--handle-upload (request)
  (with-slots (process headers) request
    ;; Multipart parts appear in HEADERS as ("field" . ((content . ...)
    ;; (filename . ...) ...)) entries; real headers have keyword cars.
    (let ((result
           (condition-case err
               (let ((parts (delq nil
                                  (mapcar (lambda (h)
                                            (and (stringp (car h))
                                                 (assoc 'filename (cdr h))
                                                 (cdr h)))
                                          headers))))
                 (if (null parts)
                     "no files in request"
                   (my/ttyd-dispatch-dropped-files
                    (my/ttyd-drop--stage-files parts))))
             (error (format "error: %s" (error-message-string err))))))
      (ws-response-header process 200
                          '("Content-type" . "text/plain; charset=utf-8"))
      (process-send-string process (encode-coding-string result 'utf-8)))))

(defun my/ttyd-drop--handle-404 (request)
  (with-slots (process) request
    (ws-response-header process 404 '("Content-type" . "text/plain"))
    (process-send-string process "not found")))

(defun my/ttyd-drop-server-start ()
  "Start (or restart) the ttyd drop bridge on `my/ttyd-drop-port'."
  (interactive)
  (require 'web-server)
  (my/ttyd-drop-server-stop)
  (setq my/ttyd-drop--server
        (ws-start
         (list (cons '(:GET . "^/$") #'my/ttyd-drop--handle-index)
               (cons '(:POST . "^/upload$") #'my/ttyd-drop--handle-upload)
               (cons (lambda (_) t) #'my/ttyd-drop--handle-404))
         ;; No :host arg: make-network-process servers accept on all
         ;; interfaces by default, matching ttyd's 0.0.0.0 exposure.
         my/ttyd-drop-port))
  (message "ttyd-drop: listening on port %d" my/ttyd-drop-port))

(defun my/ttyd-drop-server-stop ()
  "Stop the ttyd drop bridge if it is running."
  (interactive)
  (when my/ttyd-drop--server
    (ignore-errors (ws-stop my/ttyd-drop--server))
    (setq my/ttyd-drop--server nil)))

;; Only relevant on the Linux devbox daemon; a no-op elsewhere (macOS
;; profiles don't ship the web-server package).
(when (and (daemonp)
           (eq system-type 'gnu/linux)
           (require 'web-server nil 'noerror))
  (condition-case err
      (my/ttyd-drop-server-start)
    (error (message "ttyd-drop: failed to start: %s"
                    (error-message-string err)))))

(provide 'ttyd-drop)
;;; ttyd-drop.el ends here
