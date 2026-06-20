;;; org-task-ai.el --- Org TODO driven agent-shell workflows -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Treat the current Org TODO subtree as the durable task spec for AI-assisted
;; clarification sessions.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'org-agenda)
(require 'org-id)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'thingatpt)

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
  '((clarify . "AGENT_CLARIFY_SESSION"))
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

(defcustom org-task-ai-managed-heading-title "AI Context"
  "Title of the foldable child heading holding managed AI task context."
  :type 'string
  :group 'org-task-ai)

(defcustom org-task-ai-managed-property "ORG_TASK_AI"
  "Org property that marks the managed AI context heading.
The managed heading is located by this property, not by its title, so the
title can be renamed without breaking re-apply."
  :type 'string
  :group 'org-task-ai)

(defcustom org-task-ai-managed-property-value "t"
  "Value stored in `org-task-ai-managed-property' on the managed heading."
  :type 'string
  :group 'org-task-ai)

(defcustom org-task-ai-worktree-base-ref "origin/main"
  "Git ref new worktree branches are created from.
When non-nil and non-empty, `org-task-ai-start-agent-at-path' creates new
branches starting at this ref (e.g. the latest upstream main) instead of the
repository's current HEAD.  Set to nil to base new branches on HEAD.  Existing
branches are checked out as-is and never reset to this ref."
  :type '(choice (const :tag "Repository HEAD" nil) string)
  :group 'org-task-ai)

(defcustom org-task-ai-worktree-fetch-before-create t
  "When non-nil, fetch the remote for `org-task-ai-worktree-base-ref' first.
The fetch runs before creating a worktree so new branches start from the latest
upstream commit.  Has no effect when `org-task-ai-worktree-base-ref' is nil or
does not name a remote tracking branch."
  :type 'boolean
  :group 'org-task-ai)

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
         (buffer-name (format "Org Clarify @ %s" heading))
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
   (org-task-ai--prompt-header context "clarify the real need, success criteria, and repo/worktree/branch context")
   (org-task-ai--context-report context)
   "\nCurrent complete Org subtree:\n```org\n"
   (plist-get context :subtree)
   "\n```\n\n"
   "Clarify workflow (ask me one focused question at a time before changing the spec):\n"
   "1. Understand the real need: restate the underlying goal in your own words, not just the surface request, and ask until it is unambiguous.\n"
   "2. Define success criteria: establish concrete, measurable acceptance criteria and how the final goal will be verified.\n"
   "3. Confirm repos: confirm the relevant repo(s) using the parsed context above and the known local repos from project.el.\n"
   "4. Ask about worktree: explicitly ask whether this task should use a git worktree.\n"
   "5. Derive worktree + branch naming: when a worktree is wanted, derive a branch name from the need and propose a worktree path using the convention <repo>/.agent-shell/worktrees/<branch-slug>, where <repo> is the repo's full path from the parsed context verbatim, including any remote/TRAMP prefix (e.g. /ssh:host:), and the branch's \"/\" becomes \"--\" (e.g. feature/x -> feature--x). For a remote repo the worktree path must begin with the same /ssh:host: prefix as the repo path; never reduce it to a local-only path. Let me adjust the names.\n"
   "6. Record paths: write the chosen branch into BRANCHES and the worktree path into WORKTREES, one entry per repo, using repo-id=value form when there is more than one repo.\n"
   "\nOutput contract:\n"
   "1. When ready, output exactly one fenced org block containing a patched complete version of the task subtree.\n"
   "2. Keep machine-readable context in REPOS, WORKTREES, BRANCHES, and PRS properties.\n"
   "3. Do not put Org links inside REPOS, WORKTREES, BRANCHES, or PRS property values; keep those values plain and parseable.\n"
   "4. In the readable task body, prefer Org links for repos, worktree paths, and PRs. For a repo with a known local path (see the parsed context above), link it to that file: path, not its GitHub URL; for a remote repo keep the same /ssh:host: prefix in both the repo link and the worktree link so they match the WORKTREES value.\n"
   "5. Preserve the user's original description when possible; add refined context, decisions, and acceptance criteria after it.\n"
   "6. Write the entire patched subtree in English, even if I describe the task in another language.\n"
   "7. Do not edit the Org file directly; Emacs will apply the patch after I confirm.\n"))

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

(defun org-task-ai--buffer-org-blocks (buffer)
  "Return Org task blocks from BUFFER, latest (by buffer position) first.
Collect both raw ```org fenced blocks and rendered visible Org subtrees, then
order them by position.  This matters because the clarify prompt echoes the
original subtree inside a ```org fence: scanning only for fences would return
that echoed original (which still carries the raw fence) instead of the
agent's later, rendered reply.  Ordering by position lets the agent's most
recent block win regardless of which form it takes."
  (with-current-buffer buffer
    (save-excursion
      (let (entries)
        (goto-char (point-min))
        (while (re-search-forward "^[[:space:]]*```org[[:space:]]*$" nil t)
          (let ((start (match-beginning 0))
                (beg (match-end 0)))
            (when (re-search-forward "^[[:space:]]*```[[:space:]]*$" nil t)
              (push (cons start
                          (string-trim
                           (buffer-substring-no-properties
                            beg (match-beginning 0))))
                    entries))))
        (setq entries (append entries (org-task-ai--visible-org-subtrees)))
        (mapcar #'cdr
                (sort entries (lambda (a b) (> (car a) (car b)))))))))

(defun org-task-ai--visible-org-subtrees ()
  "Return visible Org-looking subtrees as (POSITION . TEXT) entries."
  (let (entries)
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
          (push (cons beg
                      (string-trim
                       (buffer-substring-no-properties beg end)))
                entries))))
    entries))

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

(defun org-task-ai--min-heading-level (body)
  "Return the shallowest heading star-count in BODY, or nil when none."
  (with-temp-buffer
    (insert body)
    (goto-char (point-min))
    (let (min)
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (let ((level (length (match-string 1))))
          (when (or (null min) (< level min))
            (setq min level))))
      min)))

(defun org-task-ai--shift-heading-levels (body delta)
  "Return BODY with every heading's star-count shifted by DELTA (min 1)."
  (if (zerop delta)
      body
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward "^\\(\\*+\\) " nil t)
        (let* ((stars (length (match-string 1)))
               (new (max 1 (+ stars delta))))
          (replace-match (concat (make-string new ?*) " ") t t)))
      (buffer-string))))

(defun org-task-ai--demote-body (body level)
  "Return BODY with its headings demoted to sit under a heading at LEVEL.
The shallowest heading in BODY is normalized to LEVEL+1; BODY with no headings
is returned unchanged."
  (if-let ((min (org-task-ai--min-heading-level body)))
      (org-task-ai--shift-heading-levels body (- (+ level 2) min))
    body))

(defun org-task-ai--build-managed-heading (body level)
  "Return managed child heading text for BODY under a task at LEVEL."
  (let ((body (org-task-ai--demote-body
               (org-task-ai--strip-managed-heading body)
               level)))
    (concat (make-string (1+ level) ?*) " "
            org-task-ai-managed-heading-title "\n"
            ":PROPERTIES:\n"
            ":" org-task-ai-managed-property ": "
            org-task-ai-managed-property-value "\n"
            ":END:\n"
            (if (string-empty-p body) "" (concat body "\n")))))

(defun org-task-ai--managed-heading-p ()
  "Return non-nil when point is on the managed AI context heading."
  (equal (org-entry-get nil org-task-ai-managed-property)
         org-task-ai-managed-property-value))

(defun org-task-ai--strip-managed-heading (body)
  "Remove an echoed managed AI context heading subtree from BODY."
  (with-temp-buffer
    (delay-mode-hooks (org-mode))
    (insert body)
    (goto-char (point-min))
    (let (found)
      (while (and (not found)
                  (re-search-forward org-heading-regexp nil t))
        (when (org-task-ai--managed-heading-p)
          (setq found t)
          (let ((beg (line-beginning-position))
                (end (save-excursion (org-end-of-subtree t t) (point))))
            (delete-region beg end))))
      (string-trim (buffer-string)))))

(defun org-task-ai--replace-managed-heading (patch-body level)
  "Replace or append the managed AI context heading in the current subtree.
PATCH-BODY becomes the managed heading body; LEVEL is the task heading level."
  (let ((section (org-task-ai--build-managed-heading patch-body level))
        (beg (save-excursion
               (org-task-ai--ensure-org-heading)
               (org-end-of-meta-data t)
               (point)))
        (end (org-task-ai--subtree-end)))
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let (replaced)
        (while (and (not replaced)
                    (re-search-forward org-heading-regexp nil t))
          (when (org-task-ai--managed-heading-p)
            (setq replaced t)
            (let ((hbeg (line-beginning-position))
                  (hend (save-excursion (org-end-of-subtree t t) (point))))
              (delete-region hbeg hend)
              (goto-char hbeg)
              (insert section))))
        (unless replaced
          (goto-char (point-max))
          (unless (or (bobp) (bolp))
            (insert "\n"))
          (unless (or (bobp)
                      (save-excursion
                        (forward-line -1)
                        (looking-at-p "[[:space:]]*$")))
            (insert "\n"))
          (insert section))))))

(defun org-task-ai--apply-subtree-patch (patch)
  "Apply parsed PATCH to current Org subtree without replacing original body."
  (org-task-ai--ensure-org-heading)
  (when-let ((heading (plist-get patch :heading)))
    (unless (or (string-empty-p heading)
                (equal heading (org-task-ai--heading-title)))
      (org-edit-headline heading)))
  (dolist (prop (plist-get patch :properties))
    (org-entry-put nil (car prop) (cdr prop)))
  (org-task-ai--replace-managed-heading (plist-get patch :body)
                                        (org-current-level)))

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

(defun org-task-ai--path-at-point ()
  "Return absolute filesystem path at point, or nil.
Recognizes an Org `file:' link, =verbatim=/~code~ markup, or a plain path."
  (let* ((elem (and (derived-mode-p 'org-mode) (org-element-context)))
         (type (and elem (org-element-type elem)))
         (raw (cond
               ((and (eq type 'link)
                     (equal (org-element-property :type elem) "file"))
                (org-element-property :path elem))
               ((memq type '(verbatim code))
                (org-element-property :value elem))
               (t (thing-at-point 'filename t)))))
    (when (and raw (not (string-empty-p (string-trim raw))))
      (expand-file-name (string-trim raw)))))

(defun org-task-ai--inherited-property (name)
  "Return inherited Org property NAME at point, or nil when empty."
  (when (derived-mode-p 'org-mode)
    (let ((value (org-entry-get nil name t)))
      (and value (not (string-empty-p (string-trim value))) value))))

(defun org-task-ai--branch-for-path (dir)
  "Return recorded branch for worktree DIR from the Org task context, or nil."
  (when (derived-mode-p 'org-mode)
    (let* ((repos (mapcar #'org-task-ai--normalize-repo
                          (org-task-ai--split-list
                           (org-task-ai--inherited-property "REPOS"))))
           (repo-ids (mapcar (lambda (repo) (plist-get repo :id)) repos))
           (worktrees (org-task-ai--parse-map-property
                       (org-task-ai--inherited-property "WORKTREES") repo-ids))
           (branches (org-task-ai--parse-map-property
                      (org-task-ai--inherited-property "BRANCHES") repo-ids))
           (target (directory-file-name dir))
           (repo-id (car (seq-find
                          (lambda (entry)
                            (equal (directory-file-name
                                    (expand-file-name (cdr entry)))
                                   target))
                          worktrees))))
      (cond
       (repo-id (cdr (assoc-string repo-id branches t)))
       ((= (length branches) 1) (cdar branches))))))

(defun org-task-ai--worktree-repo-root (dir)
  "Return source git repo root for worktree DIR, or nil.
Prefers splitting the agent-shell worktrees path; falls back to DIR's parent."
  (let ((dir (directory-file-name dir)))
    (or (and (string-match "\\`\\(.+\\)/\\.agent-shell/worktrees/[^/]+\\'" dir)
             (match-string 1 dir))
        (org-task-ai--git-root (file-name-directory dir)))))

(defun org-task-ai--git-remotes (repo-root)
  "Return the list of configured git remotes in REPO-ROOT."
  (let ((default-directory (file-name-as-directory repo-root))
        (buffer (generate-new-buffer " *org-task-ai-remotes*")))
    (unwind-protect
        (when (zerop (process-file "git" nil buffer nil "remote"))
          (split-string (with-current-buffer buffer (buffer-string)) "\n" t))
      (kill-buffer buffer))))

(defun org-task-ai--git-ref-exists-p (repo-root ref)
  "Return non-nil when REF resolves to a commit in REPO-ROOT."
  (let ((default-directory (file-name-as-directory repo-root)))
    (zerop (process-file "git" nil nil nil
                         "rev-parse" "--verify" "--quiet"
                         (concat ref "^{commit}")))))

(defun org-task-ai--git-fetch-base-ref (repo-root base-ref)
  "Fetch the remote tracking branch named by BASE-REF in REPO-ROOT.
BASE-REF shaped as REMOTE/BRANCH triggers `git fetch REMOTE BRANCH' when REMOTE
is a configured remote.  Other refs (local branches, SHAs) are left untouched."
  (when (string-match "\\`\\([^/]+\\)/\\(.+\\)\\'" base-ref)
    (let ((remote (match-string 1 base-ref))
          (remote-branch (match-string 2 base-ref)))
      (when (member remote (org-task-ai--git-remotes repo-root))
        (let* ((default-directory (file-name-as-directory repo-root))
               (buffer (generate-new-buffer " *org-task-ai-fetch*"))
               (exit (process-file "git" nil buffer nil "fetch" remote remote-branch))
               (output (with-current-buffer buffer (string-trim (buffer-string)))))
          (kill-buffer buffer)
          (unless (zerop exit)
            (user-error "git fetch %s %s failed: %s" remote remote-branch output)))))))

(defun org-task-ai--create-worktree (dir repo-root branch)
  "Create a git worktree at DIR from REPO-ROOT, checking out BRANCH.
When BRANCH is nil, git derives the branch from DIR's basename.  New branches
are based on `org-task-ai-worktree-base-ref' (fetched first when
`org-task-ai-worktree-fetch-before-create' is non-nil) so they start from the
latest upstream commit; existing branches are checked out unchanged.  Prompts
for confirmation with the exact command before running it."
  (let* ((default-directory (file-name-as-directory repo-root))
         (base-ref (and org-task-ai-worktree-base-ref
                        (let ((ref (string-trim org-task-ai-worktree-base-ref)))
                          (and (not (string-empty-p ref)) ref))))
         (branch-exists (and branch
                             (zerop (process-file "git" nil nil nil
                                                  "rev-parse" "--verify" "--quiet"
                                                  branch))))
         (use-base-ref (and base-ref (not branch-exists))))
    (when use-base-ref
      (when org-task-ai-worktree-fetch-before-create
        (org-task-ai--git-fetch-base-ref repo-root base-ref))
      (unless (org-task-ai--git-ref-exists-p repo-root base-ref)
        (user-error "Base ref %s does not resolve in %s" base-ref repo-root)))
    (let* ((local-dir (file-local-name dir))
           (start-point (and use-base-ref base-ref))
           (new-branch (or branch
                           (and start-point
                                (file-name-nondirectory
                                 (directory-file-name local-dir)))))
           (args (cond
                  (branch-exists (list "worktree" "add" local-dir branch))
                  (start-point (list "worktree" "add" "-b" new-branch local-dir start-point))
                  (branch (list "worktree" "add" "-b" branch local-dir))
                  (t (list "worktree" "add" local-dir))))
           (cmd (mapconcat #'shell-quote-argument (cons "git" args) " ")))
      (unless (yes-or-no-p (format "Run in %s: %s ? " repo-root cmd))
        (user-error "Worktree creation canceled"))
      (make-directory (file-name-directory (directory-file-name dir)) t)
      (let* ((buffer (generate-new-buffer " *org-task-ai-worktree*"))
             (exit (apply #'process-file "git" nil buffer nil args))
             (output (with-current-buffer buffer (string-trim (buffer-string)))))
        (kill-buffer buffer)
        (unless (and (zerop exit) (file-directory-p dir))
          (user-error "Failed to create worktree: %s" output))))))

(defun org-task-ai--open-agent-shell-in (dir)
  "Reuse or start a Claude Code agent-shell with cwd DIR, then display it."
  (require 'agent-shell nil t)
  (unless (fboundp 'agent-shell-start)
    (user-error "agent-shell is unavailable"))
  (let* ((dir (directory-file-name (expand-file-name dir)))
         (existing (seq-find
                    (lambda (buffer)
                      (equal (directory-file-name
                              (expand-file-name
                               (org-task-ai--agent-buffer-cwd buffer)))
                             dir))
                    (org-task-ai--agent-buffers)))
         (buffer (or existing
                     (let ((default-directory (file-name-as-directory dir)))
                       (agent-shell-start
                        :config (org-task-ai--make-claude-code-config
                                 (format "Claude @ %s"
                                         (file-name-nondirectory dir))))))))
    (if (fboundp 'agent-shell--display-buffer)
        (agent-shell--display-buffer buffer)
      (switch-to-buffer buffer))
    buffer))

(defun org-task-ai-start-agent-at-path ()
  "Open an agent-shell (Claude Code) in the worktree path at point.
Reuse an existing session for that directory; otherwise create the git worktree
first when it does not yet exist, then start the shell."
  (interactive)
  (let ((dir (directory-file-name
              (or (org-task-ai--path-at-point)
                  (user-error "No path at point")))))
    (unless (file-directory-p dir)
      (let ((repo-root (or (org-task-ai--worktree-repo-root dir)
                           (user-error "Cannot determine source repo for %s" dir)))
            (branch (org-task-ai--branch-for-path dir)))
        (org-task-ai--create-worktree dir repo-root branch)))
    (org-task-ai--open-agent-shell-in dir)))

(defvar org-task-ai-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'org-task-ai-clarify)
    (define-key map (kbd "x") #'org-task-ai-apply-replacement-subtree)
    (define-key map (kbd "s") #'org-task-ai-switch-session)
    (define-key map (kbd "w") #'org-task-ai-start-agent-at-path)
    map)
  "Prefix keymap for Org task AI commands.")

(global-set-key (kbd "C-c t") org-task-ai-prefix-map)

(provide 'org-task-ai)

;;; org-task-ai.el ends here
