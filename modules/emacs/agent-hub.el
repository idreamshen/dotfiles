;;; agent-hub.el --- Workspace + agent-shell dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A dashboard that lists project.el workspaces with their agent-shell sessions
;; nested under each, plus a fast path to record a new requirement as an Org
;; TODO.  This is the "front-chain" of an Org-TODO driven agent workflow:
;;
;;   1. Record a requirement     -> `agent-hub-new-requirement' (key `c')
;;   2. Render the dashboard      -> `agent-hub' / `agent-hub-refresh'
;;   3. Launch / open agent-shell -> `agent-hub-visit' (RET) / `agent-hub-start-session' (w)
;;
;; Standalone: it does not depend on `org-task-ai'; the few helpers it needs are
;; implemented locally.  agent-shell is required lazily inside commands so the
;; file can load before agent-shell is available.

;;; Code:

(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'org)
(require 'magit-section)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function org-fold-show-entry "org-fold")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-ident "magit-section")
(declare-function magit-get-section "magit-section")
(declare-function magit-section-toggle "magit-section")
(declare-function magit-insert-heading "magit-section")

(defgroup agent-hub nil
  "Workspace + agent-shell dashboard."
  :group 'tools)

;;;; Faces

(defface agent-hub-workspace '((t :inherit font-lock-keyword-face :weight bold))
  "Face for workspace and section heading lines."
  :group 'agent-hub)

(defface agent-hub-repo '((t :inherit font-lock-comment-face))
  "Face for the owner/repo annotation on a workspace line."
  :group 'agent-hub)

(defface agent-hub-badge '((t :inherit shadow))
  "Face for the `[live]' badge on a workspace line."
  :group 'agent-hub)

(defface agent-hub-session-title '((t :inherit default))
  "Face for a session's title."
  :group 'agent-hub)

(defface agent-hub-date '((t :inherit font-lock-comment-face))
  "Face for a session's date column."
  :group 'agent-hub)

(defface agent-hub-live '((t :inherit success))
  "Face marking a session that has a live agent-shell buffer."
  :group 'agent-hub)

(defface agent-hub-remote '((t :inherit font-lock-type-face))
  "Face marking a workspace whose root lives on a remote (TRAMP) host."
  :group 'agent-hub)

(defface agent-hub-key-help '((t :inherit shadow))
  "Face for the key hints shown in the dashboard header."
  :group 'agent-hub)

(defcustom agent-hub-workspace-exclude-regexps
  '("/\\.agent-shell/worktrees/" "/auto-interviewer/candidates/")
  "Regexps; project roots matching any of these are hidden from the list."
  :type '(repeat regexp)
  :group 'agent-hub)

(defcustom agent-hub-todo-file nil
  "Org file new requirements are recorded into.
When nil, `agent-hub-new-requirement' signals an error.  Typically set from
your init to a host-specific path."
  :type '(choice (const :tag "Unset" nil) file)
  :group 'agent-hub)

(defcustom agent-hub-todo-headline "FeedMe"
  "Level-1 Org headline under which new requirement TODOs are inserted.
Matched case-insensitively; created at the end of the file if absent."
  :type 'string
  :group 'agent-hub)

(defcustom agent-hub-start-command #'agent-shell-anthropic-start-claude-code
  "Command invoked interactively to start a new agent-shell session.
Called with `default-directory' bound to the workspace root."
  :type 'function
  :group 'agent-hub)

(defcustom agent-hub-buffer-name "*Agent Hub*"
  "Name of the dashboard buffer."
  :type 'string
  :group 'agent-hub)

(defcustom agent-hub-session-limit 12
  "Maximum number of Claude sessions listed under an expanded workspace.
Newest sessions are shown first; the rest are summarized as a \"… N more\"
line.  The expanded workspace's info line always reflects the full count."
  :type 'integer
  :group 'agent-hub)

;; Folding, per-section visibility, and point preservation across refresh are
;; all handled by `magit-section'; no buffer-local fold state is needed.

;;;; Workspace / session helpers (self-contained)

(defun agent-hub--workspaces ()
  "Return a filtered, normalized list of workspace roots (local and remote)."
  (let (roots)
    (dolist (root (and (fboundp 'project-known-project-roots)
                       (project-known-project-roots)))
      (when (and (stringp root)
                 (not (seq-some (lambda (re) (string-match-p re root))
                                agent-hub-workspace-exclude-regexps))
                 (file-directory-p root))
        (push (directory-file-name (expand-file-name root)) roots)))
    (seq-uniq (nreverse roots) #'string-equal)))

(defun agent-hub--agent-buffers ()
  "Return live agent-shell buffers."
  (when (fboundp 'agent-shell-buffers)
    (seq-filter
     (lambda (buf)
       (and (buffer-live-p buf)
            (with-current-buffer buf (derived-mode-p 'agent-shell-mode))))
     (agent-shell-buffers))))

(defun agent-hub--buffer-cwd (buffer)
  "Return BUFFER's working directory as a normalized path."
  (with-current-buffer buffer
    (directory-file-name
     (expand-file-name
      (or (and (fboundp 'agent-shell-cwd) (ignore-errors (agent-shell-cwd)))
          default-directory)))))

(defun agent-hub--buffer-agent-id (buffer)
  "Return the agent identifier (claude/opencode/...) for BUFFER, or nil."
  (when (fboundp 'agent-shell-get-config)
    (let ((config (ignore-errors (agent-shell-get-config buffer))))
      (or (alist-get :identifier config)
          (alist-get 'identifier config)))))

(defun agent-hub--path-prefix-p (root path)
  "Return non-nil when PATH is ROOT or sits inside ROOT."
  (when (and root path)
    (string-prefix-p (file-name-as-directory (expand-file-name root))
                     (file-name-as-directory (expand-file-name path)))))

(defun agent-hub--git-origin-repo (root)
  "Return owner/repo for ROOT's git origin remote, or nil."
  (when (and root (file-directory-p root))
    (let ((default-directory (file-name-as-directory root)))
      (with-temp-buffer
        (when (zerop (ignore-errors
                       (process-file "git" nil t nil "remote" "get-url" "origin")))
          (let ((url (string-trim (buffer-string))))
            (when (string-match
                   "github\\.com[:/]\\([^/[:space:]]+/[^/[:space:]]+?\\)\\(?:\\.git\\)?\\'"
                   url)
              (match-string 1 url))))))))

;;;; Claude session discovery (mirrors the `session/list' picker, off disk)

;; Claude Code persists each session as
;;   <home>/.claude/projects/<mangled-cwd>/<session-id>.jsonl
;; where <mangled-cwd> is the session's absolute cwd with every "/" and "." turned
;; into "-".  This is the same data the agent-shell start picker fetches via the
;; ACP `session/list' request, so we read it directly instead of spawning a
;; process per workspace.  For remote (TRAMP) roots the store lives on the remote
;; host, reached through the same connection prefix.

(defun agent-hub--claude-projects-dir (root)
  "Return ROOT's `~/.claude/projects/<mangled-cwd>/' directory (TRAMP-aware)."
  (let* ((abs (expand-file-name root))
         (remote (or (file-remote-p abs) ""))
         (localname (or (file-remote-p abs 'localname) abs))
         (mangled (replace-regexp-in-string "[/.]" "-" localname)))
    ;; The "~/" dir part resolves to the (remote) home via TRAMP.
    (expand-file-name (concat ".claude/projects/" mangled "/")
                      (concat remote "~/"))))

(defun agent-hub--session-files (root)
  "Return ROOT's Claude session files as (FILE . MTIME), newest first.
Cheap: one directory listing, no file contents are read.  Returns nil on any
error (e.g. an unreachable remote host) so the dashboard degrades gracefully."
  (condition-case nil
      (let ((dir (agent-hub--claude-projects-dir root)))
        (when (file-directory-p dir)
          (let ((entries (directory-files-and-attributes dir t "\\.jsonl\\'" t)))
            (seq-sort (lambda (a b) (time-less-p (cdr b) (cdr a)))
                      (mapcar (lambda (e)
                                (cons (car e)
                                      (file-attribute-modification-time (cdr e))))
                              entries)))))
    (error nil)))

(defconst agent-hub--session-title-window 1048576
  "Max bytes read from a session file when extracting its title.
Generous enough to clear an early user turn that embeds a screenshot
\(such lines can be a few hundred KB), while bounding remote reads.")

(defun agent-hub--session-title (file)
  "Return a display title for session FILE, or nil.
Reads a bounded head of FILE and returns the first real user message text
\(skipping tool/system lines that start with \"<\"), collapsed and truncated.
Only complete lines are parsed, so a truncated final line is ignored."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file nil 0 agent-hub--session-title-window)
        ;; When the window cut the last line short, don't parse that partial
        ;; line: scan only up to the start of the final unterminated line.
        (let ((limit (save-excursion
                       (goto-char (point-max))
                       (if (bolp) (point-max) (line-beginning-position))))
              title)
          (goto-char (point-min))
          (while (and (not title) (< (point) limit))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (when (> (length line) 0)
                (when-let* ((obj (ignore-errors
                                   (json-parse-string line :object-type 'alist
                                                      :array-type 'list)))
                            ((equal (alist-get 'type obj) "user"))
                            (msg (alist-get 'message obj))
                            (content (alist-get 'content msg))
                            (text (cond
                                   ((stringp content) content)
                                   ((listp content)
                                    (mapconcat
                                     (lambda (b)
                                       (if (and (listp b)
                                                (equal (alist-get 'type b) "text"))
                                           (or (alist-get 'text b) "") ""))
                                     content " "))
                                   (t ""))))
                  (setq text (string-trim text))
                  (when (and (> (length text) 0)
                             (not (string-prefix-p "<" text)))
                    (setq title text)))))
            (forward-line 1))
          (when title
            (setq title (replace-regexp-in-string "[ \t\n\r]+" " " title))
            (if (> (length title) 60) (concat (substring title 0 57) "...") title))))
    (error nil)))

(defun agent-hub--format-session-date (time)
  "Format TIME as \"Today, HH:MM\" / \"Yesterday, HH:MM\" / \"Mon DD\" / \"Mon DD, YYYY\"."
  (condition-case nil
      (let* ((now (current-time))
             (dn (decode-time now))
             (today (encode-time 0 0 0 (decoded-time-day dn)
                                 (decoded-time-month dn) (decoded-time-year dn)))
             (yesterday (time-subtract today (seconds-to-time 86400))))
        (cond
         ((not (time-less-p time today)) (format-time-string "Today, %H:%M" time))
         ((not (time-less-p time yesterday)) (format-time-string "Yesterday, %H:%M" time))
         ((= (decoded-time-year (decode-time time)) (decoded-time-year dn))
          (format-time-string "%b %d" time))
         (t (format-time-string "%b %d, %Y" time))))
    (error "")))

(defun agent-hub--buffer-session-id (buffer)
  "Return the ACP session id bound in agent-shell BUFFER, or nil."
  (with-current-buffer buffer
    (and (boundp 'agent-shell--state)
         (map-nested-elt agent-shell--state '(:session :id)))))

(defun agent-hub--live-session-map (buffers)
  "Return a hash mapping session id -> live agent-shell BUFFER."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (buf buffers map)
      (when-let* ((id (ignore-errors (agent-hub--buffer-session-id buf))))
        (puthash id buf map)))))

(defun agent-hub--sessions-by-workspace (workspaces)
  "Group agent-shell buffers under WORKSPACES.
Return a plist (:grouped ALIST :ungrouped LIST) where ALIST maps each root to a
list of buffers, and LIST holds buffers matching no workspace."
  (let ((grouped (mapcar (lambda (r) (cons r nil)) workspaces))
        ungrouped)
    (dolist (buf (agent-hub--agent-buffers))
      (let* ((cwd (agent-hub--buffer-cwd buf))
             (matches (seq-filter (lambda (r) (agent-hub--path-prefix-p r cwd))
                                  workspaces))
             ;; Most specific (longest) matching root wins, so worktree sessions
             ;; under <repo>/.agent-shell/worktrees/... attach to <repo>.
             (best (car (sort matches (lambda (a b) (> (length a) (length b)))))))
        (if best
            (push buf (cdr (assoc best grouped)))
          (push buf ungrouped))))
    (list :grouped (mapcar (lambda (cell) (cons (car cell) (nreverse (cdr cell))))
                           grouped)
          :ungrouped (nreverse ungrouped))))

;;;; Rendering

(defun agent-hub--insert-line (text &rest props)
  "Insert TEXT as a line carrying text PROPS."
  (let ((start (point)))
    (insert text "\n")
    (add-text-properties start (point) props)))

(defun agent-hub--insert-heading (text &rest props)
  "Insert TEXT as the current section's heading, carrying text PROPS.
Uses `magit-insert-heading' so the section can be folded; PROPS are added
over the heading line for the line accessors at point."
  (let ((start (point)))
    (magit-insert-heading text)
    (add-text-properties start (point) props)))

(defun agent-hub--insert-buffer-line (root buffer)
  "Insert a section for live agent-shell BUFFER nested under ROOT (may be nil)."
  (let* ((cwd (agent-hub--buffer-cwd buffer))
         (id (agent-hub--buffer-agent-id buffer))
         (suffix (and root
                      (not (string-equal (file-name-as-directory cwd)
                                         (file-name-as-directory root)))
                      (string-remove-prefix (file-name-as-directory root)
                                            (file-name-as-directory cwd)))))
    (magit-insert-section (agent-hub-session buffer)
      (agent-hub--insert-line
       (format "    %s%s%s"
               (buffer-name buffer)
               (if id (propertize (format "  %s" id) 'font-lock-face 'shadow) "")
               (if (and suffix (not (string-empty-p suffix)))
                   (propertize (format "  @%s" (directory-file-name suffix))
                               'font-lock-face 'shadow)
                 ""))
       'agent-hub-type 'session
       'agent-hub-root root
       'agent-hub-buffer buffer))))

(defun agent-hub--insert-session-line (root session)
  "Insert a Claude SESSION section nested under workspace ROOT.
SESSION is a plist (:id :file :time :title :buffer); :buffer is the live
agent-shell buffer when one is already attached to this session."
  (let* ((id (plist-get session :id))
         (title (or (plist-get session :title) "(untitled)"))
         (date (agent-hub--format-session-date (plist-get session :time)))
         (buffer (plist-get session :buffer)))
    (magit-insert-section (agent-hub-session (or id buffer))
      (agent-hub--insert-line
       (concat "    "
               (propertize title 'font-lock-face 'agent-hub-session-title)
               ;; Align the date into a fixed column regardless of title width
               ;; (CJK-safe, unlike `format' padding).
               (propertize " " 'display '(space :align-to 72))
               (propertize date 'font-lock-face 'agent-hub-date)
               (if (buffer-live-p buffer)
                   (propertize "  ●" 'font-lock-face 'agent-hub-live) ""))
       'agent-hub-type 'session
       'agent-hub-root root
       'agent-hub-session session
       'agent-hub-session-id id
       'agent-hub-buffer (and (buffer-live-p buffer) buffer)))))

(defun agent-hub--render ()
  "Rebuild the dashboard buffer in place, preserving point when possible.
Folding, per-workspace visibility, and point are preserved across refreshes by
`magit-section': workspace sections inherit their predecessor's collapsed state,
and point is restored by section identity."
  (let* ((ident (ignore-errors
                  (magit-section-ident (magit-current-section))))
         (workspaces (agent-hub--workspaces))
         (data (agent-hub--sessions-by-workspace workspaces))
         (grouped (plist-get data :grouped))
         (ungrouped (plist-get data :ungrouped)))
    (erase-buffer)
    (magit-insert-section (agent-hub-root)
      (insert (propertize "Agent Hub" 'font-lock-face 'bold) "  "
              (propertize
               "(RET open  TAB fold  n/p move  c new-req  w start  o dir  k kill  g refresh  q quit)"
               'font-lock-face 'agent-hub-key-help)
              "\n\n")
      (dolist (cell grouped)
        (let* ((root (car cell))
               (live-buffers (cdr cell))
               (live (length live-buffers))
               (remote (file-remote-p root)))
          ;; HIDE = t collapses the workspace on first insert; on later refreshes
          ;; magit-section inherits the user's toggle, so the dashboard opens
          ;; compact and expansions stick.  Because the section is hidden on first
          ;; insert, the `magit-insert-section-body' below is deferred into a washer
          ;; that only runs on expansion -- so no session files (local or remote)
          ;; are touched until the user opens a workspace.
          (magit-insert-section (agent-hub-workspace root t)
            (agent-hub--insert-heading
             (format "%s%s  %s"
                     (propertize (abbreviate-file-name root) 'font-lock-face 'agent-hub-workspace)
                     (if remote (propertize "  ⇄" 'font-lock-face 'agent-hub-remote) "")
                     ;; [<live sessions>]; total is shown lazily once expanded.
                     (propertize (format "[%d]" live)
                                 'font-lock-face (if (> live 0) 'agent-hub-live 'agent-hub-badge)))
             'agent-hub-type 'workspace
             'agent-hub-root root)
            (magit-insert-section-body
              (let* ((session-files (agent-hub--session-files root))
                     (total (length session-files))
                     (repo (agent-hub--git-origin-repo root)))
                (agent-hub--insert-line
                 (concat "    "
                         (if repo (propertize (format "(%s)  " repo)
                                              'font-lock-face 'agent-hub-repo)
                           "")
                         (propertize (format "%d session%s" total
                                             (if (= total 1) "" "s"))
                                     'font-lock-face 'agent-hub-badge))
                 'agent-hub-type 'info
                 'agent-hub-root root)
                (when (> total 0)
                  (let ((id->buffer (agent-hub--live-session-map live-buffers))
                        (shown (seq-take session-files agent-hub-session-limit)))
                    (dolist (entry shown)
                      (let ((id (file-name-base (car entry))))
                        (agent-hub--insert-session-line
                         root (list :id id
                                    :file (car entry)
                                    :time (cdr entry)
                                    :title (agent-hub--session-title (car entry))
                                    :buffer (gethash id id->buffer)))))
                    (when (> total (length shown))
                      (agent-hub--insert-line
                       (propertize (format "    … %d more" (- total (length shown)))
                                   'font-lock-face 'shadow)
                       'agent-hub-type 'info)))))))))
      (when ungrouped
        (insert "\n")
        (magit-insert-section (agent-hub-ungrouped 'ungrouped)
          (agent-hub--insert-heading
           (propertize "Ungrouped sessions" 'font-lock-face 'agent-hub-workspace)
           'agent-hub-type 'ungrouped-header)
          (dolist (buf ungrouped)
            (agent-hub--insert-buffer-line nil buf)))))
    (when ident
      (when-let* ((section (magit-get-section ident)))
        (goto-char (oref section start))))))

;;;; Line accessors

(defun agent-hub--type-at-point ()
  "Return the `agent-hub-type' of the line at point."
  (get-text-property (line-beginning-position) 'agent-hub-type))

(defun agent-hub--root-at-point ()
  "Return the workspace root associated with the line at point."
  (or (get-text-property (line-beginning-position) 'agent-hub-root)
      (user-error "No workspace on this line")))

(defun agent-hub--buffer-at-point ()
  "Return the agent-shell buffer on the line at point."
  (or (get-text-property (line-beginning-position) 'agent-hub-buffer)
      (user-error "No agent-shell session on this line")))

;;;; Commands

(defun agent-hub-refresh ()
  "Re-render the dashboard."
  (interactive)
  (let ((inhibit-read-only t))
    (agent-hub--render)))

(defun agent-hub--display-buffer (buffer)
  "Display agent-shell BUFFER using agent-shell's own logic when available."
  (require 'agent-shell nil t)
  (if (fboundp 'agent-shell--display-buffer)
      (agent-shell--display-buffer buffer)
    (pop-to-buffer buffer)))

(defun agent-hub--open-session ()
  "Resume the Claude session on the line at point.
Switch to its live buffer when one already exists; otherwise start a
claude-code shell that resumes the session id in the workspace directory."
  (let* ((bol (line-beginning-position))
         (session (get-text-property bol 'agent-hub-session))
         (buffer (get-text-property bol 'agent-hub-buffer))
         (root (get-text-property bol 'agent-hub-root))
         (id (or (plist-get session :id)
                 (get-text-property bol 'agent-hub-session-id))))
    (cond
     ((buffer-live-p buffer) (agent-hub--display-buffer buffer))
     (id
      (require 'agent-shell nil t)
      (require 'agent-shell-anthropic nil t)
      (unless (and (fboundp 'agent-shell-start)
                   (fboundp 'agent-shell-anthropic-make-claude-code-config))
        (user-error "agent-shell is not available"))
      (let ((default-directory (file-name-as-directory (or root default-directory))))
        (message "Resuming session %s…" id)
        (agent-shell-start
         :config (agent-shell-anthropic-make-claude-code-config)
         :session-id id)))
     (t (user-error "No session to resume on this line")))))

(defun agent-hub-visit ()
  "Open the session at point, or fold/unfold the workspace at point."
  (interactive)
  (pcase (agent-hub--type-at-point)
    ('session (agent-hub--open-session))
    ((or 'workspace 'ungrouped-header)
     (magit-section-toggle (magit-current-section)))
    (_ (user-error "Nothing to open here"))))

(defun agent-hub--insert-todo (file desc workspace)
  "Insert a TODO with DESC under the top headline in FILE.
Tag it with the WORKSPACE root and return a marker on the new heading."
  (let ((buffer (find-file-noselect file)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (let ((case-fold-search t)
             (regexp (format "^\\* +%s\\b" (regexp-quote agent-hub-todo-headline))))
         (unless (re-search-forward regexp nil t)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert (format "* %s\n" agent-hub-todo-headline)))
         (org-back-to-heading t)
         (goto-char (save-excursion (org-end-of-subtree t t) (point)))
         (unless (bolp) (insert "\n"))
         (let ((insert-pos (point)))
           (insert (format "** TODO %s\n:PROPERTIES:\n:WORKSPACE: %s\n:END:\n"
                           desc workspace))
           (save-buffer)
           (goto-char insert-pos)
           (org-back-to-heading t)
           (point-marker)))))))

(defun agent-hub-new-requirement ()
  "Record a new requirement TODO for the workspace at point and open it."
  (interactive)
  (unless (eq (agent-hub--type-at-point) 'workspace)
    (user-error "Move to a workspace line first"))
  (let* ((root (agent-hub--root-at-point))
         (desc (string-trim
                (read-string (format "Requirement for %s: "
                                     (abbreviate-file-name root))))))
    (when (string-empty-p desc)
      (user-error "Empty requirement"))
    (unless agent-hub-todo-file
      (user-error "Set `agent-hub-todo-file' to record requirements"))
    (let ((marker (agent-hub--insert-todo agent-hub-todo-file desc root)))
      (pop-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-back-to-heading t)
      (when (fboundp 'org-fold-show-entry) (ignore-errors (org-fold-show-entry)))
      (recenter)
      (message "Recorded requirement under %s" agent-hub-todo-headline))))

(defun agent-hub-start-session ()
  "Start a new agent-shell session in the workspace at point."
  (interactive)
  (let ((root (agent-hub--root-at-point)))
    (require 'agent-shell nil t)
    (let ((default-directory (file-name-as-directory root)))
      (call-interactively agent-hub-start-command))))

(defun agent-hub-open-workspace ()
  "Open the workspace at point with `project-switch-project'."
  (interactive)
  ;; `project-switch-project' records its argument verbatim via
  ;; `project--remember-dir', which dedupes by exact string match.  Our roots
  ;; are stored slashless (`directory-file-name'), so pass the trailing-slash
  ;; form to match project.el's canonical entries and avoid duplicates.
  (project-switch-project (file-name-as-directory (agent-hub--root-at-point))))

(defun agent-hub-kill-session ()
  "Kill the live agent-shell buffer for the session at point.
Only acts on sessions that currently have a live buffer; the on-disk session
record is never deleted."
  (interactive)
  (let ((buffer (get-text-property (line-beginning-position) 'agent-hub-buffer)))
    (unless (buffer-live-p buffer)
      (user-error "No live buffer for this session"))
    (when (yes-or-no-p (format "Kill session %s? " (buffer-name buffer)))
      (kill-buffer buffer)
      (agent-hub-refresh))))

;;;; Mode + entry point

(defvar agent-hub-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `n'/`p' (section navigation) and `TAB' (fold) come from the parent
    ;; `magit-section-mode-map'; only the dashboard's own actions are bound here.
    (define-key map (kbd "RET") #'agent-hub-visit)
    (define-key map (kbd "c") #'agent-hub-new-requirement)
    (define-key map (kbd "w") #'agent-hub-start-session)
    (define-key map (kbd "o") #'agent-hub-open-workspace)
    (define-key map (kbd "k") #'agent-hub-kill-session)
    (define-key map (kbd "g") #'agent-hub-refresh)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `agent-hub-mode'.")

(define-derived-mode agent-hub-mode magit-section-mode "Agent-Hub"
  "Major mode for the Agent Hub dashboard."
  (setq truncate-lines t))

;;;###autoload
(defun agent-hub ()
  "Open the Agent Hub dashboard."
  (interactive)
  (let ((buffer (get-buffer-create agent-hub-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'agent-hub-mode)
        (agent-hub-mode))
      (agent-hub-refresh))
    (pop-to-buffer-same-window buffer)))

(global-set-key (kbd "C-c d") #'agent-hub)

(provide 'agent-hub)

;;; agent-hub.el ends here
