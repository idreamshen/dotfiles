;;; dape-config.el --- Dape configuration -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'subr-x)

(defvar dape-configs)
(declare-function dape "dape")

(defconst my/dape-vscode--type-adapter-alist
  '(("go" . dlv)
    ("dart" . flutter))
  "Map VS Code debug adapter types to Dape configuration names.")

(defun my/dape-vscode--key-name (key)
  "Return KEY as a string."
  (if (symbolp key) (symbol-name key) key))

(defun my/dape-vscode--alist-get (key alist)
  "Return KEY from ALIST using string comparison."
  (cdr (cl-find key alist
                :key (lambda (item) (my/dape-vscode--key-name (car item)))
                :test #'string=)))

(defun my/dape-vscode--project-root ()
  "Return the current project root for launch.json discovery."
  (expand-file-name
   (or (when-let ((project (project-current nil)))
         (project-root project))
       (locate-dominating-file default-directory ".vscode/launch.json")
       default-directory)))

(defun my/dape-vscode--launch-json-file (&optional root)
  "Return the launch.json file path for ROOT."
  (expand-file-name ".vscode/launch.json" (or root (my/dape-vscode--project-root))))

(defun my/dape-vscode--read-launch-json (&optional root)
  "Read VS Code launch.json for ROOT."
  (let ((file (my/dape-vscode--launch-json-file root)))
    (when (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'alist
                               :array-type 'list
                               :null-object nil
                               :false-object nil))
        (error
         (message "Unable to read %s: %s" file (error-message-string err))
         nil)))))

(defun my/dape-vscode--current-file ()
  "Return the current buffer file name, if any."
  (when buffer-file-name
    (expand-file-name buffer-file-name)))

(defun my/dape-vscode--variable-value (name root)
  "Return VS Code variable NAME expanded for ROOT."
  (let* ((workspace-folder (directory-file-name (expand-file-name root)))
         (file (my/dape-vscode--current-file)))
    (cond
     ((member name '("workspaceFolder" "workspaceRoot")) workspace-folder)
     ((string-prefix-p "env:" name) (or (getenv (substring name 4)) ""))
     ((string= name "file") (or file ""))
     ((string= name "relativeFile")
      (if file (file-relative-name file workspace-folder) ""))
     ((string= name "fileBasename")
      (if file (file-name-nondirectory file) ""))
     ((string= name "fileDirname")
      (if file (file-name-directory file) ""))
     (t (format "${%s}" name)))))

(defun my/dape-vscode--expand-string (string root)
  "Expand VS Code variables in STRING for ROOT."
  (replace-regexp-in-string
   "\\${\\([^}]+\\)}"
   (lambda (match)
     (my/dape-vscode--variable-value
      (substring match 2 (1- (length match)))
      root))
   string t t))

(defun my/dape-vscode--expand-value (value root)
  "Expand VS Code variables in VALUE for ROOT."
  (cond
   ((stringp value) (my/dape-vscode--expand-string value root))
   ((listp value) (mapcar (lambda (item) (my/dape-vscode--expand-value item root)) value))
   (t value)))

(defun my/dape-vscode--array-to-vector (value root)
  "Return JSON array VALUE as a vector after variable expansion for ROOT."
  (vconcat (mapcar (lambda (item) (my/dape-vscode--expand-value item root)) value)))

(defun my/dape-vscode--env-to-plist (env root)
  "Return VS Code ENV alist as a Dape plist after expansion for ROOT."
  (cl-loop for (key . value) in env
           append (list (intern (concat ":" (my/dape-vscode--key-name key)))
                        (my/dape-vscode--expand-value value root))))

(defun my/dape-vscode--slug (name)
  "Return NAME as a symbol-safe slug."
  (let* ((slug (downcase name))
         (slug (replace-regexp-in-string "[^[:alnum:]]+" "-" slug))
         (slug (string-trim slug "-+" "-+")))
    (if (string-empty-p slug) "launch" slug)))

(defun my/dape-vscode--config-symbol (name)
  "Return Dape config symbol for VS Code configuration NAME."
  (intern (concat "vscode-" (my/dape-vscode--slug name))))

(defun my/dape-vscode--base-config (adapter)
  "Return a copy of the Dape base config named ADAPTER."
  (unless (boundp 'dape-configs)
    (require 'dape))
  (unless (boundp 'dape-configs)
    (user-error "Dape is not loaded"))
  (let ((base (alist-get adapter dape-configs)))
    (unless base
      (user-error "Unable to find Dape config `%s'" adapter))
    (copy-tree base)))

(defun my/dape-flutter--list-devices ()
  "Return alist of (LABEL . ID) for attached Flutter devices.
Runs `flutter devices --device-connection attached --machine' in the
current project root."
  (let* ((default-directory (my/dape-vscode--project-root))
         (output (with-temp-buffer
                   (unless (zerop (call-process
                                   "flutter" nil t nil
                                   "devices"
                                   "--device-connection" "attached"
                                   "--machine"))
                     (user-error "`flutter devices' failed: %s"
                                 (string-trim (buffer-string))))
                   (buffer-string)))
         ;; --machine can emit leading lines; start at the first '['.
         (json (substring output (or (string-search "[" output) 0)))
         (devices (json-parse-string json
                                     :object-type 'alist
                                     :array-type 'list
                                     :null-object nil
                                     :false-object nil)))
    (mapcar (lambda (device)
              (let ((id (my/dape-vscode--alist-get "id" device))
                    (name (my/dape-vscode--alist-get "name" device))
                    (platform (my/dape-vscode--alist-get "targetPlatform" device)))
                (cons (format "%s (%s)" name (or platform id)) id)))
            devices)))

(defun my/dape-flutter-read-device ()
  "Prompt for an attached Flutter device and return its id string."
  (let ((devices (my/dape-flutter--list-devices)))
    (unless devices
      (user-error "No attached Flutter devices found"))
    (let ((choice (completing-read "Flutter device: "
                                   (mapcar #'car devices) nil t)))
      (or (cdr (assoc choice devices)) choice))))

(defun my/dape-flutter--strip-device (args)
  "Return list ARGS with any -d/--device-id option (and its value) removed."
  (let (result)
    (while args
      (let ((arg (pop args)))
        (if (member arg '("-d" "--device-id"))
            (pop args)          ; also drop the device value
          (push arg result))))
    (nreverse result)))

(defun my/dape-flutter-set-device (config)
  "Prompt for a Flutter device, set `-d ID' in CONFIG :toolArgs.
Intended as a `fn' entry in a Dape configuration."
  (let* ((device (my/dape-flutter-read-device))
         (base (my/dape-flutter--strip-device
                (append (plist-get config :toolArgs) nil))))
    (plist-put config :toolArgs
               (vconcat (vector "-d" device) base))))

(defun my/dape-vscode--resolve-mode (mode)
  "Resolve a VS Code Go debug MODE for delve.
Delve's DAP server does not understand the VS Code-only \"auto\"
mode, so resolve it to \"debug\" as the VS Code Go extension would
for a non-test buffer."
  (if (and (stringp mode) (string= mode "auto"))
      "debug"
    mode))

(defun my/dape-vscode--put-string (config key value root)
  "Put expanded string VALUE at KEY in CONFIG for ROOT."
  (if value
      (plist-put config key (my/dape-vscode--expand-string value root))
    config))

(defun my/dape-vscode--configuration-to-dape (configuration root)
  "Convert VS Code CONFIGURATION to a Dape config for ROOT."
  (let* ((name (my/dape-vscode--alist-get "name" configuration))
         (type (my/dape-vscode--alist-get "type" configuration))
         (adapter (cdr (assoc type my/dape-vscode--type-adapter-alist))))
    (when adapter
      (let ((config (my/dape-vscode--base-config adapter))
            (cwd (my/dape-vscode--alist-get "cwd" configuration)))
        (setq config (plist-put config 'modes nil))
        (setq config (plist-put config 'my/dape-vscode t))
        (setq config (plist-put config 'my/dape-vscode-name name))
        (setq config (my/dape-vscode--put-string
                      config :request
                      (my/dape-vscode--alist-get "request" configuration)
                      root))
        (setq config (my/dape-vscode--put-string
                      config :program
                      (my/dape-vscode--alist-get "program" configuration)
                      root))
        (setq config (my/dape-vscode--put-string
                      config :mode
                      (my/dape-vscode--resolve-mode
                       (my/dape-vscode--alist-get "mode" configuration))
                      root))
        (setq config (my/dape-vscode--put-string
                      config :flutterMode
                      (my/dape-vscode--alist-get "flutterMode" configuration)
                      root))
        (when cwd
          (let ((expanded-cwd (my/dape-vscode--expand-string cwd root)))
            (setq config (plist-put config :cwd expanded-cwd))
            (setq config (plist-put config 'command-cwd expanded-cwd))))
        (when-let ((args (my/dape-vscode--alist-get "args" configuration)))
          (setq config (plist-put config :args
                                  (my/dape-vscode--array-to-vector args root))))
        (when-let ((tool-args (my/dape-vscode--alist-get "toolArgs" configuration)))
          (setq config (plist-put config :toolArgs
                                  (my/dape-vscode--array-to-vector tool-args root))))
        (when-let ((env (my/dape-vscode--alist-get "env" configuration)))
          (setq config (plist-put config :env
                                  (my/dape-vscode--env-to-plist env root))))
        (when (eq adapter 'flutter)
          (setq config (plist-put config 'fn #'my/dape-flutter-set-device)))
        (cons (my/dape-vscode--config-symbol name) config)))))

(defun my/dape-vscode--launch-configs (&optional root)
  "Return Dape configs generated from ROOT's launch.json."
  (let* ((root (or root (my/dape-vscode--project-root)))
         (launch-json (my/dape-vscode--read-launch-json root))
         (configurations (my/dape-vscode--alist-get "configurations" launch-json)))
    (cl-loop for configuration in configurations
             for dape-config = (my/dape-vscode--configuration-to-dape configuration root)
             when dape-config
             collect dape-config)))

(defun my/dape-vscode-register-launch-configs ()
  "Register VS Code launch.json configurations for `dape'."
  (let* ((base-configs (cl-remove-if
                        (lambda (entry) (plist-get (cdr entry) 'my/dape-vscode))
                        dape-configs))
         (launch-configs (my/dape-vscode--launch-configs)))
    (setq-local dape-configs (append launch-configs base-configs))))

(defun my/dape-vscode--configuration-names (compound)
  "Return configuration names from VS Code COMPOUND."
  (my/dape-vscode--alist-get "configurations" compound))

(defun my/dape-vscode-compound ()
  "Start a VS Code compound configuration from launch.json with Dape."
  (interactive)
  (let* ((root (my/dape-vscode--project-root))
         (launch-json (my/dape-vscode--read-launch-json root))
         (compounds (my/dape-vscode--alist-get "compounds" launch-json)))
    (unless compounds
      (user-error "No compounds found in %s" (my/dape-vscode--launch-json-file root)))
    (let* ((compound-name
            (completing-read
             "Compound: "
             (mapcar (lambda (compound)
                       (my/dape-vscode--alist-get "name" compound))
                     compounds)
             nil t))
           (compound
            (cl-find compound-name compounds
                     :key (lambda (item) (my/dape-vscode--alist-get "name" item))
                     :test #'string=))
           (launch-configs (my/dape-vscode--launch-configs root)))
      (dolist (configuration-name (my/dape-vscode--configuration-names compound))
        (let ((entry (cl-find configuration-name launch-configs
                              :key (lambda (item)
                                     (plist-get (cdr item) 'my/dape-vscode-name))
                              :test #'string=)))
          (unless entry
            (user-error "Unable to find supported launch configuration `%s'"
                        configuration-name))
          (dape (cdr entry))))
      (message "Started VS Code compound: %s" compound-name))))

(use-package dape
  :bind (("C-c d d" . dape)
         ("C-c d b" . dape-breakpoint-toggle)
         ("C-c d c" . dape-continue)
         ("C-c d n" . dape-next)
         ("C-c d i" . dape-step-in)
         ("C-c d o" . dape-step-out)
         ("C-c d r" . dape-restart)
         ("C-c d q" . dape-quit)
         ("C-c d v" . my/dape-vscode-compound))
  :config
  (add-hook 'dape-read-config-hook #'my/dape-vscode-register-launch-configs))

(provide 'dape-config)

;;; dape-config.el ends here
