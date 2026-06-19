;;; org-task-ai.el --- Org TODO driven agent-shell workflows -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Treat the current Org TODO subtree as the durable task spec for AI-assisted
;; clarification and plan/code sessions.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'project)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")

(defgroup org-task-ai nil
  "AI workflows driven by Org TODO subtrees."
  :group 'org)

(defcustom org-task-ai-pr-cache-ttl 60
  "Seconds to keep GitHub PR status results cached."
  :type 'integer
  :group 'org-task-ai)

(defcustom org-task-ai-context-properties '("REPOS" "WORKTREES" "BRANCHES" "PRS")
  "Org properties used as machine-readable task context."
  :type '(repeat string)
  :group 'org-task-ai)

(defcustom org-task-ai-session-properties
  '((clarify . "AGENT_CLARIFY_SESSION")
    (plan . "AGENT_PLAN_SESSION"))
  "Org properties used to persist agent-shell session IDs by phase."
  :type '(alist :key-type symbol :value-type string)
  :group 'org-task-ai)

(defcustom org-task-ai-session-id-persist-retries 20
  "Number of attempts to wait for an agent-shell session ID."
  :type 'integer
  :group 'org-task-ai)

(defcustom org-task-ai-session-id-persist-delay 0.5
  "Seconds between attempts to persist an agent-shell session ID."
  :type 'number
  :group 'org-task-ai)

(defcustom org-task-ai-managed-section-begin "# BEGIN_ORG_TASK_AI"
  "Marker that starts the managed AI task context section."
  :type 'string
  :group 'org-task-ai)

(defcustom org-task-ai-managed-section-end "# END_ORG_TASK_AI"
  "Marker that ends the managed AI task context section."
  :type 'string
  :group 'org-task-ai)

(defvar org-task-ai--context-overlay nil
  "Overlay used to show virtual context for the current Org task.")

(defvar org-task-ai--pr-cache (make-hash-table :test #'equal)
  "Cache for PR status lookups.")

(defvar org-task-ai--task-sessions nil
  "Alist of task session metadata.
Each value is a plist with keys :marker, :phase, :heading, and :buffer.")

(defvar-local org-task-ai-org-id nil
  "Org ID associated with the current agent-shell task session.")

(defvar-local org-task-ai-org-file nil
  "Org file associated with the current agent-shell task session.")

(defvar-local org-task-ai-heading nil
  "Org heading title associated with the current agent-shell task session.")

(defvar-local org-task-ai-phase nil
  "Task AI phase associated with the current agent-shell task session.")

(defun org-task-ai--ensure-org-heading ()
  "Move to the current Org heading or signal a user error."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (unless (org-before-first-heading-p)
    (org-back-to-heading t))
  (when (org-before-first-heading-p)
    (user-error "No Org heading at point")))

(defun org-task-ai--heading-title ()
  "Return current heading text without TODO keywords, tags, or priority."
  (org-get-heading t t t t))

(defun org-task-ai--subtree-string ()
  "Return current Org subtree as text."
  (save-excursion
    (org-task-ai--ensure-org-heading)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t t))))
      (buffer-substring-no-properties beg end))))

(defun org-task-ai--subtree-end ()
  "Return point at the end of the current subtree."
  (save-excursion
    (org-task-ai--ensure-org-heading)
    (org-end-of-subtree t t)
    (point)))

(defun org-task-ai--context-insertion-point ()
  "Return point where virtual task context should be displayed."
  (save-excursion
    (org-task-ai--ensure-org-heading)
    (org-end-of-meta-data t)
    (point)))

(defun org-task-ai--property (name)
  "Return Org property NAME at point or nil."
  (let ((value (org-entry-get nil name)))
    (and value (not (string-empty-p (string-trim value))) value)))

(defun org-task-ai--task-id ()
  "Return stable Org ID for the current task, creating one if needed."
  (unless buffer-file-name
    (user-error "Org task AI requires a file-backed Org buffer"))
  (org-id-get-create))

(defun org-task-ai--session-property (phase)
  "Return Org property name used for PHASE session id."
  (or (cdr (assq phase org-task-ai-session-properties))
      (user-error "Unknown Org task AI phase: %s" phase)))

(defun org-task-ai--none-value-p (value)
  "Return non-nil when VALUE explicitly means no entries."
  (and value
       (member (downcase (string-trim value))
               '("none" "nil" "null" "n/a" "na" "-"))))

(defun org-task-ai--split-list (value)
  "Split comma/newline/space separated VALUE into trimmed entries.
Whitespace inside key=value pairs and URLs is not supported intentionally; paths
with spaces should be avoided in task properties."
  (when (and value (not (org-task-ai--none-value-p value)))
    (seq-filter
     (lambda (entry) (not (string-empty-p entry)))
     (mapcar #'string-trim
             (split-string value "[,\n\t ]+" t)))))

(defun org-task-ai--parse-map-property (value repo-ids)
  "Parse VALUE entries shaped as KEY=VALUE into an alist.
When there is exactly one repo, a bare VALUE is associated with that repo."
  (seq-keep
   (lambda (entry)
     (cond
      ((string-match "\\`\\([^=]+\\)=\\(.+\\)\\'" entry)
       (cons (string-trim (match-string 1 entry))
             (string-trim (match-string 2 entry))))
      ((= (length repo-ids) 1)
       (cons (car repo-ids) entry))))
   (org-task-ai--split-list value)))

(defun org-task-ai--github-repo-from-url (text)
  "Return owner/repo from GitHub URL or remote TEXT."
  (cond
   ((string-match "\\`git@github\\.com:\\([^/[:space:]]+/[^[:space:]]+\\)\\'" text)
    (string-remove-suffix ".git" (match-string 1 text)))
   ((string-match "\\`ssh://git@github\\.com/\\([^/[:space:]]+/[^[:space:]]+\\)\\'" text)
    (string-remove-suffix ".git" (match-string 1 text)))
   ((string-match "\\`https://github\\.com/\\([^/[:space:]]+/[^/?#[:space:]]+\\)" text)
    (string-remove-suffix ".git" (match-string 1 text)))
   ((string-match "\\`\\([^/[:space:]]+\\)/\\([^/[:space:]]+\\)\\'" text)
    text)
   (t nil)))

(defun org-task-ai--git-root (path)
  "Return git root for PATH or nil."
  (let ((default-directory (file-name-as-directory (expand-file-name path)))
        (buffer (generate-new-buffer " *org-task-ai-git*")))
    (unwind-protect
        (when (and (file-directory-p default-directory)
                   (zerop (process-file "git" nil buffer nil "rev-parse" "--show-toplevel")))
          (with-current-buffer buffer
            (concat (file-remote-p default-directory)
                    (string-trim (buffer-string)))))
      (kill-buffer buffer))))

(defun org-task-ai--git-origin-repo (root)
  "Return GitHub owner/repo for git ROOT origin remote, or nil."
  (when (file-directory-p root)
    (let ((default-directory (file-name-as-directory root))
          (buffer (generate-new-buffer " *org-task-ai-git-origin*")))
      (unwind-protect
          (condition-case nil
              (when (zerop (process-file "git" nil buffer nil "remote" "get-url" "origin"))
                (org-task-ai--github-repo-from-url
                 (with-current-buffer buffer
                   (string-trim (buffer-string)))))
            (file-error nil))
        (kill-buffer buffer)))))

(defun org-task-ai--known-repo-path-by-id (id)
  "Return known local project root for GitHub repo ID, or nil."
  (seq-some
   (lambda (root)
     (when (equal id (org-task-ai--git-origin-repo root))
       root))
   (org-task-ai--known-repo-roots)))

(defun org-task-ai--normalize-repo (spec)
  "Normalize repo SPEC to a plist.
The returned plist has :spec, :id, and optional :path."
  (let* ((spec (string-trim spec))
         (expanded (expand-file-name spec))
         (root (and (file-exists-p expanded)
                    (org-task-ai--git-root expanded)))
         (id (or (and root (org-task-ai--git-origin-repo root))
                 (org-task-ai--github-repo-from-url spec)
                 spec))
         (resolved-root (or root (org-task-ai--known-repo-path-by-id id))))
    (list :spec spec :id id :path resolved-root)))

(defun org-task-ai--known-repo-roots ()
  "Return known local project git roots."
  (delete-dups
   (seq-keep
    (lambda (root)
      (when (file-directory-p root)
        (org-task-ai--git-root root)))
    (if (fboundp 'project-known-project-roots)
        (project-known-project-roots)
      nil))))

(defun org-task-ai--known-repo-lines ()
  "Return display lines for known local repos."
  (let ((roots (org-task-ai--known-repo-roots)))
    (if roots
        (mapconcat
         (lambda (root)
           (format "- %s%s"
                   root
                   (if-let ((repo (org-task-ai--git-origin-repo root)))
                       (format " (%s)" repo)
                     "")))
         roots
         "\n")
      "- No known local repos from project.el")))

(defun org-task-ai--task-context ()
  "Return parsed context for the current Org task."
  (save-excursion
    (org-task-ai--ensure-org-heading)
    (let* ((repos (mapcar #'org-task-ai--normalize-repo
                          (org-task-ai--split-list (org-task-ai--property "REPOS"))))
           (repo-ids (mapcar (lambda (repo) (plist-get repo :id)) repos))
           (id (org-task-ai--task-id)))
      (list :heading (org-task-ai--heading-title)
            :marker (copy-marker (point-marker))
            :id id
            :file (buffer-file-name)
            :sessions (seq-keep
                       (lambda (entry)
                         (when-let ((value (org-task-ai--property (cdr entry))))
                           (cons (car entry) value)))
                       org-task-ai-session-properties)
            :repos repos
            :repo-ids repo-ids
            :worktrees (org-task-ai--parse-map-property
                        (org-task-ai--property "WORKTREES")
                        repo-ids)
            :branches (org-task-ai--parse-map-property
                       (org-task-ai--property "BRANCHES")
                       repo-ids)
            :prs (org-task-ai--parse-prs (org-task-ai--property "PRS") repo-ids)
            :subtree (org-task-ai--subtree-string)))))

(defun org-task-ai--parse-pr (entry repo-ids)
  "Parse one PR ENTRY using REPO-IDS for bare numbers."
  (let ((entry (string-trim entry)))
    (cond
     ((string-match "\\`https://github\\.com/\\([^/]+/[^/]+\\)/pull/\\([0-9]+\\)" entry)
      (list :repo (match-string 1 entry)
            :number (string-to-number (match-string 2 entry))
            :spec entry))
     ((string-match "\\`\\([^#[:space:]]+/[^#[:space:]]+\\)#\\([0-9]+\\)\\'" entry)
      (list :repo (match-string 1 entry)
            :number (string-to-number (match-string 2 entry))
            :spec entry))
     ((and (string-match "\\`#?\\([0-9]+\\)\\'" entry)
           (= (length repo-ids) 1))
      (list :repo (car repo-ids)
            :number (string-to-number (match-string 1 entry))
            :spec entry))
     (t
      (list :repo nil :number nil :spec entry :error "Unrecognized PR spec")))))

(defun org-task-ai--parse-prs (value repo-ids)
  "Parse PR property VALUE using REPO-IDS for bare PR numbers."
  (mapcar (lambda (entry)
            (org-task-ai--parse-pr entry repo-ids))
          (org-task-ai--split-list value)))

(defun org-task-ai--alist-get-string (key alist)
  "Return KEY from JSON ALIST as string-ish value."
  (let ((value (cdr (assq key alist))))
    (cond
     ((eq value :json-false) nil)
     ((null value) nil)
     (t value))))

(defun org-task-ai--pr-cache-key (repo number)
  "Return cache key for REPO and PR NUMBER."
  (format "%s#%s" repo number))

(defun org-task-ai--gh-pr-view (repo number)
  "Return PR status plist for REPO NUMBER using gh."
  (let* ((key (org-task-ai--pr-cache-key repo number))
         (cached (gethash key org-task-ai--pr-cache))
         (now (float-time)))
    (if (and cached (< (- now (plist-get cached :time)) org-task-ai-pr-cache-ttl))
        (plist-get cached :value)
      (let ((buffer (generate-new-buffer " *org-task-ai-gh*")))
        (unwind-protect
            (let* ((default-directory temporary-file-directory)
                   (exit (process-file
                          "gh" nil buffer nil
                          "pr" "view" (number-to-string number)
                          "-R" repo
                          "--json" "number,title,state,isDraft,mergedAt,url,headRefName,updatedAt"))
                   (raw (with-current-buffer buffer
                          (string-trim (buffer-string))))
                   (value
                    (if (zerop exit)
                        (let ((json-object-type 'alist)
                              (json-array-type 'list)
                              (json-false :json-false))
                          (condition-case err
                              (let* ((data (json-read-from-string raw))
                                     (state (org-task-ai--alist-get-string 'state data))
                                     (is-draft (org-task-ai--alist-get-string 'isDraft data))
                                     (merged-at (org-task-ai--alist-get-string 'mergedAt data)))
                                (list :ok t
                                      :repo repo
                                      :number number
                                      :title (org-task-ai--alist-get-string 'title data)
                                      :state (cond
                                              (merged-at "MERGED")
                                              (is-draft "DRAFT")
                                              (state (upcase state))
                                              (t "UNKNOWN"))
                                      :url (org-task-ai--alist-get-string 'url data)
                                      :head (org-task-ai--alist-get-string 'headRefName data)
                                      :updated-at (org-task-ai--alist-get-string 'updatedAt data)))
                            (error
                             (list :ok nil :repo repo :number number
                                   :error (format "Failed to parse gh JSON: %s" err)))))
                      (list :ok nil :repo repo :number number
                            :error (if (string-empty-p raw)
                                       "gh pr view failed without output"
                                     raw)))))
              (puthash key (list :time now :value value) org-task-ai--pr-cache)
              value)
          (kill-buffer buffer))))))

(defun org-task-ai--pr-statuses (context)
  "Return PR statuses for CONTEXT."
  (mapcar
   (lambda (pr)
     (if-let ((error (plist-get pr :error)))
         (append pr (list :status-error error))
       (let ((repo (plist-get pr :repo))
             (number (plist-get pr :number)))
         (if (and repo number)
             (append pr (list :status (org-task-ai--gh-pr-view repo number)))
           (append pr (list :status-error "Missing repo or number"))))))
   (plist-get context :prs)))

(defun org-task-ai--safe-path-prefix-p (root path)
  "Return non-nil when PATH is under ROOT."
  (when (and root path (file-directory-p root))
    (let ((root-dir (file-name-as-directory (expand-file-name root)))
          (target (expand-file-name path)))
      (file-in-directory-p target root-dir))))

(defun org-task-ai--agent-buffers ()
  "Return live agent-shell buffers."
  (when (fboundp 'agent-shell-buffers)
    (seq-filter
     (lambda (buffer)
       (and (buffer-live-p buffer)
            (with-current-buffer buffer
              (derived-mode-p 'agent-shell-mode))))
     (agent-shell-buffers))))

(defun org-task-ai--agent-buffer-cwd (buffer)
  "Return working directory for agent-shell BUFFER."
  (with-current-buffer buffer
    (or (and (fboundp 'agent-shell-cwd)
             (ignore-errors (agent-shell-cwd)))
        default-directory)))

(defun org-task-ai--agent-buffer-id (buffer)
  "Return agent identifier for agent-shell BUFFER."
  (when (fboundp 'agent-shell-get-config)
    (let ((config (ignore-errors (agent-shell-get-config buffer))))
      (or (alist-get :identifier config)
          (alist-get 'identifier config)))))

(defun org-task-ai--sessions-for-repo (repo)
  "Return agent-shell session plists for REPO."
  (let ((path (plist-get repo :path)))
    (seq-keep
     (lambda (buffer)
       (let ((cwd (org-task-ai--agent-buffer-cwd buffer)))
         (when (and path (org-task-ai--safe-path-prefix-p path cwd))
           (list :buffer buffer
                 :name (buffer-name buffer)
                 :cwd cwd
                 :identifier (org-task-ai--agent-buffer-id buffer)))))
     (org-task-ai--agent-buffers))))

(defun org-task-ai--all-task-sessions (context)
  "Return sessions matched to repos in CONTEXT."
  (apply #'append
         (mapcar #'org-task-ai--sessions-for-repo
                 (plist-get context :repos))))

(defun org-task-ai--org-link (target label)
  "Return Org link to TARGET with LABEL."
  (format "[[%s][%s]]" target label))

(defun org-task-ai--file-link (path label)
  "Return Org file link to PATH with LABEL."
  (org-task-ai--org-link (concat "file:" (expand-file-name path)) label))

(defun org-task-ai--github-repo-url (repo-id)
  "Return GitHub URL for REPO-ID when it is owner/repo shaped."
  (when (org-task-ai--github-repo-from-url repo-id)
    (format "https://github.com/%s" repo-id)))

(defun org-task-ai--repo-link (repo)
  "Return display link for REPO."
  (let ((id (plist-get repo :id))
        (path (plist-get repo :path)))
    (cond
     ((and path (file-exists-p path))
      (org-task-ai--file-link path id))
     ((org-task-ai--github-repo-url id)
      (org-task-ai--org-link (org-task-ai--github-repo-url id) id))
     (t id))))

(defun org-task-ai--path-link (path label)
  "Return display link for PATH with LABEL when possible."
  (if (and path (not (string-empty-p path)))
      (org-task-ai--file-link path label)
    label))

(defun org-task-ai--branch-link (branch repo-path worktree-path)
  "Return display link for BRANCH using REPO-PATH or WORKTREE-PATH."
  (if-let ((path (or worktree-path repo-path)))
      (org-task-ai--file-link path branch)
    branch))

(defun org-task-ai--pr-link-label (repo number)
  "Return compact label for REPO PR NUMBER."
  (format "%s#%s" repo number))

(defun org-task-ai--format-context (context)
  "Return human-readable context text for CONTEXT."
  (let ((repos (plist-get context :repos))
        (worktrees (plist-get context :worktrees))
        (branches (plist-get context :branches))
        (recorded-sessions (plist-get context :sessions))
        (prs (org-task-ai--pr-statuses context)))
    (string-join
     (append
      (list "Task context")
      (if recorded-sessions
          (cons "- Recorded agent sessions:"
                (mapcar #'org-task-ai--format-recorded-session-line
                        recorded-sessions))
        (list "- Recorded agent sessions: none"))
      (if repos
          (apply
           #'append
           (mapcar
            (lambda (repo)
              (let* ((id (plist-get repo :id))
                     (repo-path (plist-get repo :path))
                     (worktree (cdr (assoc-string id worktrees t)))
                     (repo-prs (seq-filter
                                (lambda (pr) (equal (plist-get pr :repo) id))
                                prs))
                     (sessions (org-task-ai--sessions-for-repo repo)))
                (append
                 (list (format "- Repo: %s" (org-task-ai--repo-link repo)))
                 (when repo-path
                   (list (format "  path: %s"
                                 (org-task-ai--path-link repo-path repo-path))))
                 (when-let ((branch (cdr (assoc-string id branches t))))
                   (list (format "  branch: %s"
                                 (org-task-ai--branch-link branch repo-path worktree))))
                 (when worktree
                   (list (format "  worktree: %s"
                                 (org-task-ai--path-link worktree
                                                         (format "%s worktree" id)))))
                 (if repo-prs
                     (mapcar #'org-task-ai--format-pr-line repo-prs)
                   (list "  prs: none"))
                 (if sessions
                     (mapcar #'org-task-ai--format-session-line sessions)
                   (list "  agent-shell sessions: none")))))
            repos))
        (list "- Repos: none"))
      (let ((unknown-prs (seq-filter
                          (lambda (pr) (not (member (plist-get pr :repo)
                                                     (plist-get context :repo-ids))))
                          prs)))
        (when unknown-prs
          (cons "- PRs outside linked repos:"
                (mapcar #'org-task-ai--format-pr-line unknown-prs)))))
     "\n")))

(defun org-task-ai--format-recorded-session-line (session)
  "Format recorded SESSION property line."
  (format "  %s: %s" (car session) (cdr session)))

(defun org-task-ai--format-pr-line (pr)
  "Format PR status line for PR."
  (cond
   ((plist-get pr :status-error)
    (format "  PR %s: %s" (plist-get pr :spec) (plist-get pr :status-error)))
   ((plist-get pr :error)
    (format "  PR %s: %s" (plist-get pr :spec) (plist-get pr :error)))
   (t
    (let ((status (plist-get pr :status)))
      (if (plist-get status :ok)
          (format "  PR %s [%s] %s"
                  (org-task-ai--org-link
                   (or (plist-get status :url)
                       (format "https://github.com/%s/pull/%s"
                               (plist-get pr :repo)
                               (plist-get pr :number)))
                   (org-task-ai--pr-link-label
                    (plist-get pr :repo)
                    (plist-get pr :number)))
                  (plist-get status :state)
                  (or (plist-get status :title) ""))
        (format "  PR %s: %s"
                (org-task-ai--org-link
                 (format "https://github.com/%s/pull/%s"
                         (plist-get pr :repo)
                         (plist-get pr :number))
                 (org-task-ai--pr-link-label
                  (plist-get pr :repo)
                  (plist-get pr :number)))
                (plist-get status :error)))))))

(defun org-task-ai--format-session-line (session)
  "Format agent-shell SESSION line."
  (format "  agent-shell: %s%s"
          (plist-get session :name)
          (if-let ((identifier (plist-get session :identifier)))
              (format " (%s)" identifier)
            "")))

(defun org-task-ai--delete-context-overlay ()
  "Delete the task context overlay when present."
  (when (overlayp org-task-ai--context-overlay)
    (delete-overlay org-task-ai--context-overlay)
    (setq org-task-ai--context-overlay nil)))

(defun org-task-ai--fontify-org-text (text)
  "Return TEXT fontified as Org markup."
  (with-temp-buffer
    (org-mode)
    (insert text)
    (font-lock-ensure)
    (buffer-string)))

(defun org-task-ai-refresh-context ()
  "Refresh virtual context display for the current Org task."
  (interactive)
  (org-task-ai--delete-context-overlay)
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-task-ai--ensure-org-heading)
      (let* ((context (org-task-ai--task-context))
             (text (concat "\n"
                           (org-task-ai--fontify-org-text
                            (org-task-ai--format-context context))
                           "\n")))
        (setq org-task-ai--context-overlay
              (make-overlay (org-task-ai--context-insertion-point)
                            (org-task-ai--context-insertion-point)
                            (current-buffer)
                            nil t))
        (overlay-put org-task-ai--context-overlay 'after-string text)))))

(defun org-task-ai--agenda-refresh-context ()
  "Refresh task context after jumping from agenda."
  (when (derived-mode-p 'org-mode)
    (org-task-ai-refresh-context)))

(add-hook 'org-agenda-after-show-hook #'org-task-ai--agenda-refresh-context)

(defun org-task-ai--slug (text)
  "Return safe slug for TEXT."
  (let ((slug (downcase (replace-regexp-in-string "[^[:alnum:]]+" "-" text))))
    (string-trim slug "-+" "-+")))

(defun org-task-ai--task-key (phase heading)
  "Return task session key for PHASE and HEADING."
  (format "%s:%s" phase (org-task-ai--slug heading)))

(defun org-task-ai--make-claude-code-config (buffer-name)
  "Return a Claude Code agent config named BUFFER-NAME."
  (require 'agent-shell-anthropic nil t)
  (unless (fboundp 'agent-shell-anthropic-make-claude-code-config)
    (user-error "agent-shell-anthropic is unavailable"))
  (let ((config (copy-tree (agent-shell-anthropic-make-claude-code-config))))
    (setf (alist-get :buffer-name config) buffer-name)
    config))

(defun org-task-ai--existing-task-session (key)
  "Return live session buffer for KEY or nil."
  (when-let* ((entry (assoc key org-task-ai--task-sessions))
              (buffer (plist-get (cdr entry) :buffer)))
    (when (buffer-live-p buffer)
      buffer)))

(defun org-task-ai--set-session-metadata (buffer phase context)
  "Store task CONTEXT metadata on agent-shell BUFFER for PHASE."
  (with-current-buffer buffer
    (setq-local org-task-ai-org-id (plist-get context :id))
    (setq-local org-task-ai-org-file (plist-get context :file))
    (setq-local org-task-ai-heading (plist-get context :heading))
    (setq-local org-task-ai-phase phase)))

(defun org-task-ai--agent-session-id (buffer)
  "Return current agent-shell ACP session id for BUFFER, or nil."
  (when (fboundp 'agent-shell--state)
    (with-current-buffer buffer
      (condition-case nil
          (map-nested-elt (agent-shell--state) '(:session :id))
        (error nil)))))

(defun org-task-ai--persist-session-id (buffer session-id)
  "Persist SESSION-ID from BUFFER onto its linked Org task."
  (let* ((metadata (org-task-ai--target-metadata buffer))
         (phase (plist-get metadata :phase))
         (property (org-task-ai--session-property phase)))
    (save-window-excursion
      (save-excursion
        (unless (org-task-ai--goto-linked-task metadata)
          (user-error "Cannot find linked Org task for %s" (buffer-name buffer)))
        (org-entry-put nil property session-id)
        (save-buffer)))))

(defun org-task-ai--persist-session-id-when-ready (buffer &optional attempts)
  "Persist agent-shell session id for BUFFER once available.
ATTEMPTS is the number of attempts already made."
  (let ((attempts (or attempts 0)))
    (when (buffer-live-p buffer)
      (if-let ((session-id (org-task-ai--agent-session-id buffer)))
          (org-task-ai--persist-session-id buffer session-id)
        (when (< attempts org-task-ai-session-id-persist-retries)
          (run-at-time org-task-ai-session-id-persist-delay
                       nil
                       #'org-task-ai--persist-session-id-when-ready
                       buffer
                       (1+ attempts)))))))

(defun org-task-ai--repo-default-directory (context)
  "Return default directory for CONTEXT agent session."
  (or (seq-some (lambda (repo) (plist-get repo :path))
                (plist-get context :repos))
      default-directory))

(defun org-task-ai--recorded-session-id (phase context)
  "Return recorded session id for PHASE from CONTEXT."
  (cdr (assq phase (plist-get context :sessions))))

(defun org-task-ai--start-or-reuse-session (phase context)
  "Start or reuse agent-shell session for PHASE and CONTEXT."
  (require 'agent-shell nil t)
  (unless (and (fboundp 'agent-shell-start)
               (fboundp 'agent-shell-insert))
    (user-error "agent-shell is unavailable"))
  (let* ((heading (plist-get context :heading))
         (key (org-task-ai--task-key phase heading))
         (buffer-name (format "Org %s @ %s"
                              (if (eq phase 'clarify) "Clarify" "Plan")
                              heading))
         (session-id (org-task-ai--recorded-session-id phase context))
         (buffer (or (org-task-ai--existing-task-session key)
                     (let ((default-directory
                            (file-name-as-directory
                             (org-task-ai--repo-default-directory context))))
                       (agent-shell-start
                        :config (org-task-ai--make-claude-code-config buffer-name)
                        :session-id session-id)))))
    (setf (alist-get key org-task-ai--task-sessions nil nil #'equal)
          (list :marker (plist-get context :marker)
                :id (plist-get context :id)
                :file (plist-get context :file)
                :phase phase
                :heading heading
                :buffer buffer))
    (org-task-ai--set-session-metadata buffer phase context)
    (org-task-ai--persist-session-id-when-ready buffer)
    buffer))

(defun org-task-ai--prompt-header (context purpose)
  "Return common prompt header for CONTEXT and PURPOSE."
  (format "You are working from an Org TODO that is the durable task spec.\nPurpose: %s\n\nCurrent task heading: %s\n\n"
          purpose
          (plist-get context :heading)))

(defun org-task-ai--context-report (context)
  "Return context report for prompt insertion."
  (format "Parsed context:\n%s\n\nKnown local repos from project.el:\n%s\n"
          (org-task-ai--format-context context)
          (org-task-ai--known-repo-lines)))

(defun org-task-ai--clarification-prompt (context)
  "Return clarification prompt for CONTEXT."
  (concat
   (org-task-ai--prompt-header context "clarify repo/worktree/branch/PR context and final task requirements")
   (org-task-ai--context-report context)
   "\nCurrent complete Org subtree:\n```org\n"
   (plist-get context :subtree)
   "\n```\n\n"
   "Workflow:\n"
   "1. Ask me clarifying questions before changing the task spec.\n"
   "2. Help confirm linked repos, desired worktree paths, branches, PRs, final requirements, decisions, and acceptance criteria.\n"
   "3. When ready, output exactly one fenced org block containing a patched complete version of the task subtree.\n"
   "4. Keep machine-readable context in REPOS, WORKTREES, BRANCHES, and PRS properties.\n"
   "5. Do not put Org links inside REPOS, WORKTREES, BRANCHES, or PRS property values; keep those values plain and parseable.\n"
   "6. In the readable task body, prefer Org links for repos, local worktree paths, and PRs.\n"
   "7. Preserve the user's original description when possible; add refined context, decisions, and acceptance criteria after it.\n"
   "8. Do not edit the Org file directly; Emacs will apply the patch after I confirm.\n"))

(defun org-task-ai--plan-code-prompt (context)
  "Return plan/code prompt for CONTEXT."
  (concat
   (org-task-ai--prompt-header context "plan and implement from the latest Org task spec")
   (org-task-ai--context-report context)
   "\nUse this complete Org subtree as the source of truth:\n```org\n"
   (plist-get context :subtree)
   "\n```\n\n"
   "Instructions:\n"
   "1. Use the repos, worktrees, branches, PRs, requirements, decisions, and acceptance criteria from the Org task.\n"
   "2. Create or switch git worktrees/branches with normal git commands when needed.\n"
   "3. Start with a concrete implementation plan, then code and validate.\n"
   "4. If the task spec is missing critical context, ask before coding.\n"))

(defun org-task-ai--send-prompt (phase prompt context)
  "Send PROMPT for PHASE using CONTEXT."
  (let ((buffer (org-task-ai--start-or-reuse-session phase context)))
    (agent-shell-insert :text prompt :submit t :shell-buffer buffer)
    (when (fboundp 'agent-shell--display-buffer)
      (agent-shell--display-buffer buffer))
    buffer))

(defun org-task-ai-clarify ()
  "Start or reuse an agent-shell clarification session for the current TODO."
  (interactive)
  (let* ((context (org-task-ai--task-context))
         (prompt (org-task-ai--clarification-prompt context)))
    (org-task-ai--send-prompt 'clarify prompt context)))

(defun org-task-ai-plan-code ()
  "Start or reuse an agent-shell plan/code session for the current TODO."
  (interactive)
  (let* ((context (org-task-ai--task-context))
         (prompt (org-task-ai--plan-code-prompt context)))
    (org-task-ai--send-prompt 'plan prompt context)))

(defun org-task-ai--buffer-org-blocks (buffer)
  "Return fenced org blocks from BUFFER, latest first.
Also handles rendered agent-shell buffers by falling back to visible Org
subtrees when raw Markdown fences are not present."
  (with-current-buffer buffer
    (save-excursion
      (goto-char (point-min))
      (let (blocks)
        (while (re-search-forward "^[[:space:]]*```org[[:space:]]*$" nil t)
          (let ((beg (match-end 0)))
            (when (re-search-forward "^[[:space:]]*```[[:space:]]*$" nil t)
              (push (string-trim
                     (buffer-substring-no-properties beg (match-beginning 0)))
                    blocks))))
        (unless blocks
          (setq blocks (org-task-ai--visible-org-subtrees)))
        blocks))))

(defun org-task-ai--visible-org-subtrees ()
  "Return visible Org-looking subtrees in the current buffer, latest first."
  (let (blocks)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) \\(?:TODO\\|WAIT\\|DONE\\|CANCELED\\|CANCELLED\\)\\_>" nil t)
        (let ((beg (line-beginning-position))
              (level (length (match-string 1)))
              end)
          (setq end
                (save-excursion
                  (forward-line 1)
                  (if (re-search-forward
                       (format "^\\*\\{1,%d\\} " level)
                       nil t)
                      (line-beginning-position)
                    (point-max))))
          (push (string-trim
                 (buffer-substring-no-properties beg end))
                blocks))))
    blocks))

(defun org-task-ai--candidate-session-buffers ()
  "Return known task session buffers."
  (seq-keep
   (lambda (entry)
     (let ((buffer (plist-get (cdr entry) :buffer)))
       (when (buffer-live-p buffer)
         buffer)))
   org-task-ai--task-sessions))

(defun org-task-ai--read-session-buffer ()
  "Read a task agent-shell buffer."
  (let* ((buffers (delete-dups
                   (append
                    (when (derived-mode-p 'agent-shell-mode)
                      (list (current-buffer)))
                    (org-task-ai--candidate-session-buffers)
                    (org-task-ai--agent-buffers))))
         (choices (mapcar (lambda (buffer)
                            (cons (buffer-name buffer) buffer))
                          buffers)))
    (unless choices
      (user-error "No agent-shell buffers available"))
    (cdr (assoc (completing-read "Agent shell buffer: " choices nil t)
                choices))))

(defun org-task-ai--latest-org-block (buffer)
  "Return latest Org task block from BUFFER."
  (or (car (org-task-ai--buffer-org-blocks buffer))
      (user-error "No Org task block found in %s" (buffer-name buffer))))

(defun org-task-ai--session-plist-for-buffer (buffer)
  "Return in-memory task session plist for BUFFER."
  (seq-some
   (lambda (entry)
     (let ((plist (cdr entry)))
       (when (eq (plist-get plist :buffer) buffer)
         plist)))
   org-task-ai--task-sessions))

(defun org-task-ai--buffer-metadata (buffer)
  "Return persisted buffer-local task metadata for BUFFER."
  (with-current-buffer buffer
    (list :id org-task-ai-org-id
          :file org-task-ai-org-file
          :heading org-task-ai-heading
          :phase org-task-ai-phase)))

(defun org-task-ai--target-metadata (buffer)
  "Return combined task target metadata for BUFFER."
  (let ((session (org-task-ai--session-plist-for-buffer buffer))
        (metadata (org-task-ai--buffer-metadata buffer)))
    (list :marker (plist-get session :marker)
          :id (or (plist-get metadata :id) (plist-get session :id))
          :file (or (plist-get metadata :file) (plist-get session :file))
          :heading (or (plist-get metadata :heading) (plist-get session :heading))
          :phase (or (plist-get metadata :phase) (plist-get session :phase)))))

(defun org-task-ai--valid-marker-p (marker)
  "Return non-nil if MARKER points to a live Org buffer."
  (and (markerp marker)
       (marker-buffer marker)
       (buffer-live-p (marker-buffer marker))))

(defun org-task-ai--goto-id (id)
  "Go to Org heading with ID and return non-nil on success."
  (when (and id (not (string-empty-p id)))
    (condition-case nil
        (progn
          (org-id-goto id)
          (org-task-ai--ensure-org-heading)
          t)
      (error nil))))

(defun org-task-ai--goto-file-heading (file heading)
  "Go to HEADING in FILE after confirmation and return non-nil."
  (when (and file heading (file-exists-p file))
    (let ((buffer (find-file-noselect file))
          found)
      (with-current-buffer buffer
        (org-with-wide-buffer
         (goto-char (point-min))
         (while (and (not found)
                     (re-search-forward org-heading-regexp nil t))
           (when (equal (org-get-heading t t t t) heading)
             (setq found (point-marker))))))
      (when (and found
                 (yes-or-no-p
                  (format "Patch Org heading \"%s\" in %s? "
                          heading
                          (abbreviate-file-name file))))
        (switch-to-buffer (marker-buffer found))
        (goto-char found)
        (org-task-ai--ensure-org-heading)
        t))))

(defun org-task-ai--goto-linked-task (metadata)
  "Go to task described by METADATA without interactive fallbacks."
  (let ((marker (plist-get metadata :marker))
        (id (plist-get metadata :id))
        (file (plist-get metadata :file))
        (heading (plist-get metadata :heading)))
    (cond
     ((org-task-ai--valid-marker-p marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-task-ai--ensure-org-heading)
      t)
     ((org-task-ai--goto-id id)
      t)
     ((and file heading (file-exists-p file))
      (let ((buffer (find-file-noselect file))
            found)
        (with-current-buffer buffer
          (org-with-wide-buffer
           (goto-char (point-min))
           (while (and (not found)
                       (re-search-forward org-heading-regexp nil t))
             (when (equal (org-get-heading t t t t) heading)
               (setq found (point-marker))))))
        (when found
          (switch-to-buffer (marker-buffer found))
          (goto-char found)
          (org-task-ai--ensure-org-heading)
          t))))))

(defun org-task-ai--goto-patch-target (buffer)
  "Go to the Org task associated with agent-shell BUFFER."
  (let* ((metadata (org-task-ai--target-metadata buffer))
         (marker (plist-get metadata :marker))
         (id (plist-get metadata :id))
         (file (plist-get metadata :file))
         (heading (plist-get metadata :heading)))
    (cond
     ((org-task-ai--valid-marker-p marker)
      (switch-to-buffer (marker-buffer marker))
      (goto-char marker)
      (org-task-ai--ensure-org-heading)
      t)
     ((org-task-ai--goto-id id)
      t)
     ((org-task-ai--goto-file-heading file heading)
      t)
     ((derived-mode-p 'org-mode)
      (org-task-ai--ensure-org-heading)
      (unless (yes-or-no-p
               (format "Agent buffer is not linked. Patch current Org heading \"%s\"? "
                       (org-task-ai--heading-title)))
        (user-error "Patch canceled"))
      t)
     ((not (or id file heading))
      (user-error "This agent-shell buffer is not linked to an Org task; start from an Org TODO with C-c t q or C-c t p"))
     (t
      (user-error "Cannot find linked Org task for %s; run C-c t x from the target Org heading or start a new linked session"
                  (buffer-name buffer))))))

(defun org-task-ai--parse-subtree-patch (text)
  "Parse Org subtree TEXT as a patch plist."
  (with-temp-buffer
    (org-mode)
    (insert (string-trim text))
    (goto-char (point-min))
    (org-task-ai--ensure-org-heading)
    (let* ((heading (org-task-ai--heading-title))
           (props (seq-keep
                   (lambda (name)
                     (when-let ((value (org-task-ai--property name)))
                       (cons name value)))
                   org-task-ai-context-properties))
           (body (org-task-ai--subtree-body-string)))
      (list :heading heading :properties props :body body))))

(defun org-task-ai--subtree-body-string ()
  "Return current subtree body without heading or property/planning metadata."
  (save-excursion
    (org-task-ai--ensure-org-heading)
    (let ((beg (save-excursion
                 (org-end-of-meta-data t)
                 (point)))
          (end (org-task-ai--subtree-end)))
      (string-trim (buffer-substring-no-properties beg end)))))

(defun org-task-ai--managed-section-regexp ()
  "Return regexp matching the managed AI section."
  (concat "^" (regexp-quote org-task-ai-managed-section-begin) "$"
          "\\(?:.\\|\n\\)*?"
          "^" (regexp-quote org-task-ai-managed-section-end) "$"))

(defun org-task-ai--strip-managed-section (body)
  "Remove managed AI section from BODY."
  (string-trim
   (replace-regexp-in-string
    (org-task-ai--managed-section-regexp)
    ""
    body)))

(defun org-task-ai--patch-body-text (body)
  "Return managed section text for patch BODY."
  (let ((body (org-task-ai--strip-managed-section body)))
    (concat org-task-ai-managed-section-begin "\n"
            (if (string-empty-p body) "" (concat body "\n"))
            org-task-ai-managed-section-end "\n")))

(defun org-task-ai--replace-managed-section (patch-body)
  "Replace or append managed section with PATCH-BODY in current subtree."
  (let* ((section (org-task-ai--patch-body-text patch-body))
         (beg (save-excursion
                (org-task-ai--ensure-org-heading)
                (org-end-of-meta-data t)
                (point)))
         (end (org-task-ai--subtree-end))
         (regexp (org-task-ai--managed-section-regexp)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (if (re-search-forward regexp nil t)
          (replace-match section t t)
        (goto-char (point-max))
        (unless (or (bobp) (bolp))
          (insert "\n"))
        (unless (or (bobp)
                    (save-excursion
                      (forward-line -1)
                      (looking-at-p "[[:space:]]*$")))
          (insert "\n"))
        (insert section)))))

(defun org-task-ai--apply-subtree-patch (patch)
  "Apply parsed PATCH to current Org subtree without replacing original body."
  (org-task-ai--ensure-org-heading)
  (dolist (prop (plist-get patch :properties))
    (org-entry-put nil (car prop) (cdr prop)))
  (org-task-ai--replace-managed-section (plist-get patch :body)))

(defun org-task-ai-apply-replacement-subtree (buffer)
  "Patch current Org task from latest Org subtree in task agent-shell BUFFER."
  (interactive (list (org-task-ai--read-session-buffer)))
  (let* ((patch-text (org-task-ai--latest-org-block buffer))
         (patch (org-task-ai--parse-subtree-patch patch-text)))
    (when (yes-or-no-p
           (format "Patch Org task from latest org block in %s? "
                   (buffer-name buffer)))
      (org-task-ai--goto-patch-target buffer)
      (org-task-ai--apply-subtree-patch patch)
      (save-buffer)
      (message "Patched Org task from %s" (buffer-name buffer)))))

(defun org-task-ai-switch-session ()
  "Switch to an agent-shell session associated with the current task."
  (interactive)
  (let* ((context (org-task-ai--task-context))
         (sessions (delete-dups (org-task-ai--all-task-sessions context)))
         (choices (mapcar
                   (lambda (session)
                     (cons (format "%s | %s"
                                   (plist-get session :name)
                                   (or (plist-get session :cwd) ""))
                           (plist-get session :buffer)))
                   sessions)))
    (unless choices
      (user-error "No matching agent-shell sessions for current task"))
    (let ((buffer (cdr (assoc (completing-read "Task session: " choices nil t)
                              choices))))
      (if (fboundp 'agent-shell--display-buffer)
          (agent-shell--display-buffer buffer)
        (switch-to-buffer buffer)))))

(defvar org-task-ai-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'org-task-ai-clarify)
    (define-key map (kbd "x") #'org-task-ai-apply-replacement-subtree)
    (define-key map (kbd "p") #'org-task-ai-plan-code)
    (define-key map (kbd "c") #'org-task-ai-refresh-context)
    (define-key map (kbd "s") #'org-task-ai-switch-session)
    map)
  "Prefix keymap for Org task AI commands.")

(global-set-key (kbd "C-c t") org-task-ai-prefix-map)

(provide 'org-task-ai)

;;; org-task-ai.el ends here
