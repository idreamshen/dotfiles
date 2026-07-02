;;; agent-hub.el --- Workspace + agent-shell dashboard -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; A dashboard that lists project.el workspaces with their agent-shell sessions
;; nested under each, plus a fast path to record a new requirement as an Org
;; TODO.  This is the "front-chain" of an Org-TODO driven agent workflow:
;;
;;   1. Record a requirement     -> `agent-hub-new-requirement' (key `c')
;;   2. Render the dashboard      -> `agent-hub' / `agent-hub-refresh'
;;   3. Launch / open agent-shell -> `agent-hub-visit' (RET) / `agent-hub-start-session' (N)
;;
;; Standalone: it does not depend on `org-task-ai'; the few helpers it needs are
;; implemented locally.  agent-shell is required lazily inside commands so the
;; file can load before agent-shell is available.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'project)
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'org)
(require 'magit-section)

(declare-function agent-shell-buffers "agent-shell")
(declare-function agent-shell-status "agent-shell")
(declare-function agent-shell-cwd "agent-shell")
(declare-function agent-shell-get-config "agent-shell")
(declare-function agent-shell--display-buffer "agent-shell")
(declare-function agent-shell--dot-subdir "agent-shell")
(declare-function agent-shell-anthropic-start-claude-code "agent-shell-anthropic")
(declare-function agent-shell-start "agent-shell")
(declare-function agent-shell-anthropic-make-claude-code-config "agent-shell-anthropic")
(declare-function org-fold-show-entry "org-fold")
(declare-function magit-current-section "magit-section")
(declare-function magit-section-ident "magit-section")
(declare-function magit-get-section "magit-section")
(declare-function magit-section-toggle "magit-section")
(declare-function magit-insert-heading "magit-section")
(declare-function llm-chat "llm")
(declare-function llm-make-chat-prompt "llm")
(defvar llm-github-provider)

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

(defface agent-hub-worktree '((t :inherit font-lock-string-face))
  "Face for the worktree path suffix on a session line."
  :group 'agent-hub)

(defface agent-hub-branch '((t :inherit font-lock-variable-name-face))
  "Face for the branch annotation on a session line."
  :group 'agent-hub)

(defface agent-hub-diff-add '((t :inherit success))
  "Face for added-line counts in git diff annotations."
  :group 'agent-hub)

(defface agent-hub-diff-del '((t :inherit warning))
  "Face for deleted-line counts in git diff annotations."
  :group 'agent-hub)

(defface agent-hub-pr-open '((t :inherit success))
  "Face for open PR badges."
  :group 'agent-hub)

(defface agent-hub-pr-draft '((t :inherit warning))
  "Face for draft PR badges."
  :group 'agent-hub)

(defface agent-hub-pr-merged '((t :inherit font-lock-keyword-face))
  "Face for merged PR badges."
  :group 'agent-hub)

(defface agent-hub-pr-closed '((t :inherit error))
  "Face for closed PR badges."
  :group 'agent-hub)

(defface agent-hub-pr-other '((t :inherit shadow))
  "Face for PR badges whose state is unknown."
  :group 'agent-hub)

(defface agent-hub-live '((t :inherit success))
  "Face marking a session that has a live agent-shell buffer."
  :group 'agent-hub)

(defface agent-hub-status-ready '((t :inherit success))
  "Face for the status icon of an idle, ready agent."
  :group 'agent-hub)

(defface agent-hub-status-working '((t :inherit warning))
  "Face for the status icon of an actively working agent."
  :group 'agent-hub)

(defface agent-hub-status-waiting '((t :foreground "red" :weight bold))
  "Face for the status icon of an agent waiting for input (e.g. a permission)."
  :group 'agent-hub)

(defface agent-hub-status-init '((t :inherit font-lock-comment-face))
  "Face for the status icon of an initializing agent."
  :group 'agent-hub)

(defface agent-hub-status-killed '((t :inherit error))
  "Face for the status icon of an agent whose process has died."
  :group 'agent-hub)

(defface agent-hub-mark '((t :inherit warning))
  "Face for marked sessions."
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

(defcustom agent-hub-git-info-cache-ttl 60
  "Seconds to cache git and GitHub metadata used by the dashboard."
  :type 'integer
  :group 'agent-hub)

;; Folding, per-section visibility, and point preservation across refresh are
;; all handled by `magit-section'; no buffer-local fold state is needed.

(defvar agent-hub--git-info-cache (make-hash-table :test #'equal)
  "Cache for git and GitHub metadata.
Values are plists shaped as (:time FLOAT :value VALUE).")

(defcustom agent-hub-title-cache-file
  (locate-user-emacs-file "agent-hub-titles.eld")
  "File where extracted session titles are persisted across Emacs sessions.
A session's title comes from its first user message and never changes, so the
title is cached on disk keyed by session file.  On this repo's hardware the
dominant cold-start cost is opening each session file (endpoint-security
scanning adds ~100-250ms per open), so persisting titles means a cold start
only opens newly-added session files."
  :type 'file
  :group 'agent-hub)

(defvar agent-hub--title-cache nil
  "Hash table mapping session FILE -> extracted TITLE, or nil until loaded.
Only non-nil titles are stored, so a still-initializing session (no user turn
yet) is retried on a later render.  Backed by `agent-hub-title-cache-file'.")

(defvar agent-hub--title-cache-dirty nil
  "Non-nil when `agent-hub--title-cache' has entries not yet written to disk.")

(defvar-local agent-hub--marked-session-files nil
  "Persisted Claude session files marked for batch deletion.")

;;;; Workspace / session helpers (self-contained)

(defun agent-hub--workspace-name (root)
  "Return ROOT's worktree name for sorting."
  (file-name-nondirectory
   (directory-file-name (or (file-remote-p root 'localname) root))))

(defun agent-hub--workspace< (a b)
  "Return non-nil when workspace A should sort before workspace B."
  (let ((remote-a (file-remote-p a))
        (remote-b (file-remote-p b)))
    (cond
     ((and (not remote-a) remote-b) t)
     ((and remote-a (not remote-b)) nil)
     (t (let ((name-a (downcase (agent-hub--workspace-name a)))
              (name-b (downcase (agent-hub--workspace-name b))))
          (if (string-equal name-a name-b)
              (string-lessp (downcase a) (downcase b))
            (string-lessp name-a name-b)))))))

(defun agent-hub--workspaces ()
  "Return a filtered, normalized list of workspace roots (local first, then remote)."
  (let (roots)
    (dolist (root (and (fboundp 'project-known-project-roots)
                       (project-known-project-roots)))
      (when (and (stringp root)
                 (not (seq-some (lambda (re) (string-match-p re root))
                                agent-hub-workspace-exclude-regexps))
                 (file-directory-p root))
        (push (directory-file-name (expand-file-name root)) roots)))
    (seq-sort #'agent-hub--workspace<
              (seq-uniq (nreverse roots) #'string-equal))))

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

(defun agent-hub--same-directory-p (a b)
  "Return non-nil when A and B name the same directory."
  (and a b
       (string-equal (file-name-as-directory (expand-file-name a))
                     (file-name-as-directory (expand-file-name b)))))

(defconst agent-hub--cache-miss (make-symbol "agent-hub-cache-miss")
  "Sentinel returned when a cached value is absent or expired.")

(defun agent-hub--cache-get (key)
  "Return cached value for KEY, or `agent-hub--cache-miss'."
  (let ((cached (gethash key agent-hub--git-info-cache))
        (now (float-time)))
    (if (and cached
             (< (- now (plist-get cached :time)) agent-hub-git-info-cache-ttl))
        (plist-get cached :value)
      agent-hub--cache-miss)))

(defun agent-hub--cache-put (key value)
  "Cache VALUE under KEY and return VALUE."
  (puthash key (list :time (float-time) :value value) agent-hub--git-info-cache)
  value)

(defun agent-hub--process-output (directory program &rest args)
  "Run PROGRAM with ARGS in DIRECTORY and return trimmed stdout on success."
  (condition-case nil
      (let ((default-directory (file-name-as-directory
                                (or directory temporary-file-directory))))
        (with-temp-buffer
          (when (zerop (apply #'process-file program nil t nil args))
            (string-trim (buffer-string)))))
    (error nil)))

(defun agent-hub--github-repo-from-url (url)
  "Return owner/repo parsed from GitHub remote URL."
  (when (and url
             (string-match
              "github\\.com[:/]\\([^/[:space:]]+/[^/[:space:]]+?\\)\\(?:\\.git\\)?\\'"
              url))
    (match-string 1 url)))

(defun agent-hub--git-origin-repo (root)
  "Return owner/repo for ROOT's git origin remote, or nil."
  (let* ((key (format "origin:%s" root))
         (cached (agent-hub--cache-get key)))
    (if (not (eq cached agent-hub--cache-miss))
        cached
      (agent-hub--cache-put
       key
       (when (and root (file-directory-p root))
         (agent-hub--github-repo-from-url
          (agent-hub--process-output root "git" "remote" "get-url" "origin")))))))

(defun agent-hub--git-default-branch (root)
  "Return ROOT's origin default branch, falling back to main."
  (let ((ref (agent-hub--process-output
              root "git" "symbolic-ref" "--quiet" "--short"
              "refs/remotes/origin/HEAD")))
    (cond
     ((and ref (string-match "\\`origin/\\(.+\\)\\'" ref))
      (match-string 1 ref))
     ((and ref (not (string-empty-p ref))) ref)
     (t "main"))))

(defun agent-hub--git-current-branch (root)
  "Return ROOT's current branch, or nil for detached HEAD."
  (when-let ((branch (agent-hub--process-output root "git" "branch" "--show-current")))
    (unless (string-empty-p branch) branch)))

(defun agent-hub--normalize-git-path (root path)
  "Normalize git PATH output for ROOT, preserving ROOT's TRAMP prefix."
  (when path
    (let* ((remote (file-remote-p root))
           (normalized (directory-file-name (expand-file-name path))))
      (if (and remote (not (file-remote-p normalized)))
          (concat remote normalized)
        normalized))))

(defun agent-hub--parse-worktree-branches (root text)
  "Parse `git worktree list --porcelain' TEXT for ROOT.
Return an alist of (WORKTREE-PATH . BRANCH)."
  (let (path branch result)
    (cl-labels ((flush ()
                  (when (and path branch)
                    (push (cons (agent-hub--normalize-git-path root path) branch)
                          result))))
      (dolist (line (split-string (or text "") "\n"))
        (cond
         ((string-empty-p line)
          (flush)
          (setq path nil branch nil))
         ((string-match "\\`worktree \\(.+\\)\\'" line)
          (flush)
          (setq path (match-string 1 line)
                branch nil))
         ((string-match "\\`branch refs/heads/\\(.+\\)\\'" line)
          (setq branch (match-string 1 line)))))
      (flush))
    (nreverse result)))

(defun agent-hub--git-workspace-info (root)
  "Return cached git metadata for workspace ROOT."
  (let* ((key (format "git:%s" root))
         (cached (agent-hub--cache-get key)))
    (if (not (eq cached agent-hub--cache-miss))
        cached
      (agent-hub--cache-put
       key
       (condition-case nil
           (when (and root (file-directory-p root))
             (let* ((default-branch (agent-hub--git-default-branch root))
                    (root-path (agent-hub--normalize-git-path root root))
                    (root-branch (agent-hub--git-current-branch root))
                    (worktrees (agent-hub--parse-worktree-branches
                                root
                                (agent-hub--process-output
                                 root "git" "worktree" "list" "--porcelain"))))
               (when (and root-branch (not (assoc root-path worktrees)))
                 (push (cons root-path root-branch) worktrees))
               (list :default-branch default-branch
                     :cwd-branches worktrees)))
         (error nil))))))

(defun agent-hub--numstat-sum (directory &rest args)
  "Run `git diff --numstat' with ARGS in DIRECTORY and sum additions/deletions."
  (when-let ((raw (apply #'agent-hub--process-output
                         directory "git" "diff" "--numstat" args)))
    (let ((add 0)
          (del 0))
      (dolist (line (split-string raw "\n" t))
        (when (string-match "\\`\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
          (setq add (+ add (string-to-number (match-string 1 line))))
          (setq del (+ del (string-to-number (match-string 2 line))))))
      (list :add add :del del))))

(defun agent-hub--git-branch-diff (root branch default-branch)
  "Return cached line diff for BRANCH against DEFAULT-BRANCH under ROOT."
  (let* ((key (format "branch-diff:%s:%s:%s" root default-branch branch))
         (cached (agent-hub--cache-get key)))
    (if (not (eq cached agent-hub--cache-miss))
        cached
      (agent-hub--cache-put
       key
       (when (and branch default-branch
                  (not (string-equal branch default-branch)))
         (or (agent-hub--numstat-sum
              root (format "origin/%s...%s" default-branch branch))
             (agent-hub--numstat-sum
              root (format "%s...%s" default-branch branch))))))))

(defun agent-hub--git-dirty-diff (cwd)
  "Return cached tracked dirty line diff for CWD."
  (let* ((key (format "dirty:%s" cwd))
         (cached (agent-hub--cache-get key)))
    (if (not (eq cached agent-hub--cache-miss))
        cached
      (agent-hub--cache-put
       key
       (when (and cwd (file-directory-p cwd))
         (agent-hub--numstat-sum cwd "HEAD" "--"))))))

(defun agent-hub--json-alist-value (key alist)
  "Return KEY's value from JSON ALIST, treating JSON false as nil."
  (let ((value (cdr (assq key alist))))
    (cond
     ((eq value :json-false) nil)
     ((null value) nil)
     (t value))))

(defun agent-hub--pr-plist (data)
  "Convert gh PR JSON DATA alist into a plist."
  (when data
    (let* ((draft (agent-hub--json-alist-value 'isDraft data))
           (state (agent-hub--json-alist-value 'state data))
           (state (if draft "DRAFT" (if (stringp state) (upcase state) "UNKNOWN"))))
      (list :number (agent-hub--json-alist-value 'number data)
            :title (agent-hub--json-alist-value 'title data)
            :state state
            :draft draft
            :url (agent-hub--json-alist-value 'url data)
            :head (agent-hub--json-alist-value 'headRefName data)
            :updated-at (agent-hub--json-alist-value 'updatedAt data)
            :additions (agent-hub--json-alist-value 'additions data)
            :deletions (agent-hub--json-alist-value 'deletions data)))))

(defun agent-hub--parse-pr-view (raw)
  "Parse RAW JSON from `gh pr view'."
  (when (and raw (not (string-empty-p raw)))
    (agent-hub--pr-plist
     (json-parse-string raw :object-type 'alist :array-type 'list
                        :false-object :json-false))))

(defun agent-hub--parse-pr-list (raw)
  "Parse RAW JSON from branch-filtered `gh pr list'.
When more than one PR matches a branch, choose the most recently updated one."
  (when (and raw (not (string-empty-p raw)))
    (let* ((items (json-parse-string raw :object-type 'alist
                                     :array-type 'list
                                     :false-object :json-false))
           (sorted (sort items
                         (lambda (a b)
                           (string> (or (agent-hub--json-alist-value 'updatedAt a) "")
                                    (or (agent-hub--json-alist-value 'updatedAt b) ""))))))
      (when-let ((first (car sorted)))
        (agent-hub--pr-plist first)))))

(defun agent-hub--gh-pr-for-branch (repo branch)
  "Return cached GitHub PR metadata for REPO's exact BRANCH."
  (let* ((key (format "pr:%s:%s" repo branch))
         (cached (agent-hub--cache-get key)))
    (if (not (eq cached agent-hub--cache-miss))
        cached
      (agent-hub--cache-put
       key
       (condition-case nil
           (when (and repo branch (executable-find "gh"))
             (or (agent-hub--parse-pr-view
                  (agent-hub--process-output
                   nil "gh" "pr" "view" branch "-R" repo
                   "--json" "number,title,state,isDraft,url,headRefName,updatedAt,additions,deletions"))
                 (agent-hub--parse-pr-list
                  (agent-hub--process-output
                   nil "gh" "pr" "list" "-R" repo "--head" branch
                   "--state" "all" "--limit" "10"
                   "--json" "number,title,state,isDraft,url,headRefName,updatedAt,additions,deletions"))))
         (error nil))))))

(defun agent-hub--branch-for-cwd (cwd git-info)
  "Return the checked-out branch for CWD according to GIT-INFO."
  (when-let ((branches (and cwd (plist-get git-info :cwd-branches))))
    (cdr (assoc (directory-file-name (expand-file-name cwd)) branches))))

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

(defun agent-hub--session-cwds (root)
  "Return ROOT plus any agent-shell worktree roots nested under it."
  (let* ((worktrees-dir (expand-file-name ".agent-shell/worktrees/"
                                          (file-name-as-directory root)))
         (worktrees (condition-case nil
                        (when (file-directory-p worktrees-dir)
                          (seq-filter
                           #'file-directory-p
                           (mapcar #'directory-file-name
                                   (directory-files worktrees-dir t
                                                    directory-files-no-dot-files-regexp
                                                    t))))
                      (error nil))))
    (seq-uniq (cons root worktrees) #'string-equal)))

(defun agent-hub--session-files-for-cwd (cwd)
  "Return CWD's Claude session plists, newest first."
  (condition-case nil
      (let ((dir (agent-hub--claude-projects-dir cwd)))
        (when (file-directory-p dir)
          (mapcar (lambda (e)
                    (list :file (car e)
                          :time (file-attribute-modification-time (cdr e))
                          :cwd cwd))
                  (directory-files-and-attributes dir t "\\.jsonl\\'" t))))
    (error nil)))

(defun agent-hub--session-files (root)
  "Return ROOT's Claude session plists, newest first.
Includes sessions whose cwd is an agent-shell worktree nested under ROOT.
Cheap: one directory listing per cwd, no file contents are read.  Returns nil
on any error (e.g. an unreachable remote host) so the dashboard degrades
gracefully."
  (seq-sort (lambda (a b) (time-less-p (plist-get b :time)
                                       (plist-get a :time)))
            (mapcan #'agent-hub--session-files-for-cwd
                    (agent-hub--session-cwds root))))

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

;; Cache entries are FILE -> (MTIME . TITLE), where MTIME is `float-time' of the
;; file's modification time and TITLE may be nil.  A non-nil TITLE is immutable
;; (a session's first user message never changes) so it is reused regardless of
;; MTIME and without a `stat'.  A nil TITLE (screenshot-only / still-initializing
;; session) is a negative result trusted only while MTIME is unchanged, so such a
;; file is re-opened just once, after it grows -- not on every cold start.

(defun agent-hub--title-cache-ensure ()
  "Return the title cache, loading it from disk on first use.
A corrupt or missing cache file yields a fresh empty table."
  (or agent-hub--title-cache
      (setq agent-hub--title-cache
            (let ((table (make-hash-table :test #'equal)))
              (ignore-errors
                (when (file-readable-p agent-hub-title-cache-file)
                  (with-temp-buffer
                    (insert-file-contents agent-hub-title-cache-file)
                    (dolist (cell (read (current-buffer)))
                      ;; (FILE MTIME . TITLE) with TITLE optional/nil.
                      (when (and (consp cell) (stringp (car cell))
                                 (consp (cdr cell)) (numberp (cadr cell))
                                 (or (null (cddr cell)) (stringp (cddr cell))))
                        (puthash (car cell) (cdr cell) table))))))
              table))))

(defun agent-hub--title-cache-save ()
  "Write the title cache to `agent-hub-title-cache-file' when it has new entries."
  (when (and agent-hub--title-cache-dirty agent-hub--title-cache)
    (ignore-errors
      (let (alist)
        (maphash (lambda (k v) (push (cons k v) alist)) agent-hub--title-cache)
        (with-temp-file agent-hub-title-cache-file
          (let ((print-length nil) (print-level nil))
            (prin1 alist (current-buffer)))))
      (setq agent-hub--title-cache-dirty nil))))

(defun agent-hub--session-title-cached (file &optional mtime)
  "Return a display title for session FILE, cached persistently.
MTIME is FILE's modification time when the caller already knows it (from the
directory listing), avoiding a `stat'; otherwise it is read here.  Opening a
session file is the dominant cold-start cost, so a cached non-nil title is
reused with no I/O, and a cached \"no title\" result skips the re-open until
FILE's MTIME changes."
  (let* ((cache (agent-hub--title-cache-ensure))
         (cached (gethash file cache)))
    (if (and cached (cdr cached))
        (cdr cached)                    ; immutable title: reuse, no stat/open
      (let* ((mtime (or mtime (file-attribute-modification-time
                               (file-attributes file))))
             (key (and mtime (float-time mtime))))
        (if (and cached key (equal (car cached) key))
            nil                         ; known title-less at this MTIME
          (let ((title (agent-hub--session-title file)))
            (when key
              (puthash file (cons key title) cache)
              (setq agent-hub--title-cache-dirty t))
            title))))))

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

;;;; Live agent status

;; Status is derived from agent-shell's own supported API (`agent-shell-status'
;; returns busy/blocked/ready) plus a couple of states read off
;; `agent-shell--state' directly: a dead comint/ACP process is `killed', and a
;; session that has no id yet is `initializing'.  Only live buffers carry a
;; status, so persisted (on-disk) sessions never get an icon.

(defconst agent-hub--status-icons
  '((working . "◐") (waiting . "◉") (ready . "●")
    (initializing . "○") (killed . "✕"))
  "Alist mapping a live-agent status symbol to its display glyph.")

(defconst agent-hub--status-faces
  '((working . agent-hub-status-working) (waiting . agent-hub-status-waiting)
    (ready . agent-hub-status-ready) (initializing . agent-hub-status-init)
    (killed . agent-hub-status-killed))
  "Alist mapping a live-agent status symbol to its display face.")

(defun agent-hub--buffer-status (buffer)
  "Return a status symbol for live agent-shell BUFFER.
One of `working', `waiting', `ready', `initializing', or `killed'.  Reads
agent-shell state defensively so an unexpected shape degrades gracefully."
  (if (not (buffer-live-p buffer))
      'killed
    (with-current-buffer buffer
      (let* ((state (and (boundp 'agent-shell--state) agent-shell--state))
             (acp-process (and state (map-nested-elt state '(:client :process)))))
        (cond
         ;; comint process gone, or the ACP client exists but its process died.
         ((or (not (process-live-p (get-buffer-process buffer)))
              (and (map-elt state :client)
                   acp-process (not (process-live-p acp-process))))
          'killed)
         ((not (map-nested-elt state '(:session :id))) 'initializing)
         (t (pcase (and (fboundp 'agent-shell-status)
                        (ignore-errors (agent-shell-status :shell-buffer buffer)))
              ('blocked 'waiting)
              ('busy 'working)
              (_ 'ready))))))))

(defun agent-hub--format-status-badge (buffer)
  "Return a propertized status icon for live agent-shell BUFFER.
Returns an empty string when BUFFER is not live, so persisted sessions
display no icon."
  (if (not (buffer-live-p buffer))
      ""
    (let ((status (agent-hub--buffer-status buffer)))
      (concat "  " (propertize (or (alist-get status agent-hub--status-icons) "?")
                               'font-lock-face
                               (or (alist-get status agent-hub--status-faces)
                                   'default))))))

(defun agent-hub--live-session-map (buffers)
  "Return a hash mapping session id -> live agent-shell BUFFER."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (buf buffers map)
      (when-let* ((id (ignore-errors (agent-hub--buffer-session-id buf))))
        (puthash id buf map)))))

(defun agent-hub--live-buffer-session (buffer)
  "Return a session plist for live agent-shell BUFFER.
Shaped like the persisted-session plists built in `agent-hub--render'
\(:id :file :time :title :cwd :buffer), so the shared session line accessors
and commands operate on it unchanged.  :file/:time are populated only when the
on-disk session record exists; :title falls back to the buffer name."
  (let* ((id (ignore-errors (agent-hub--buffer-session-id buffer)))
         (cwd (agent-hub--buffer-cwd buffer))
         (file (and id (expand-file-name
                        (concat id ".jsonl")
                        (agent-hub--claude-projects-dir cwd))))
         (attrs (and file (ignore-errors (file-attributes file))))
         (file (and attrs file))
         (time (and attrs (file-attribute-modification-time attrs)))
         (title (or (and file (ignore-errors
                                (agent-hub--session-title-cached file time)))
                    (buffer-name buffer))))
    (list :id id :file file :time time :title title :cwd cwd :buffer buffer)))

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

(defun agent-hub--positive-number-p (value)
  "Return non-nil when VALUE is a positive number."
  (and (numberp value) (> value 0)))

(defun agent-hub--diff-nonzero-p (add del)
  "Return non-nil when ADD or DEL is positive."
  (or (agent-hub--positive-number-p add)
      (agent-hub--positive-number-p del)))

(defun agent-hub--format-diff-count (add del)
  "Format ADD/DEL as a propertized +N/-M string."
  (string-join
   (delq nil
         (list (when (agent-hub--positive-number-p add)
                 (propertize (format "+%d" add)
                             'font-lock-face 'agent-hub-diff-add))
               (when (agent-hub--positive-number-p del)
                 (propertize (format "-%d" del)
                             'font-lock-face 'agent-hub-diff-del))))
   "/"))

(defun agent-hub--format-git-badge (metadata &optional show-default-branch)
  "Return a compact branch/diff badge for METADATA.
When SHOW-DEFAULT-BRANCH is non-nil, include the branch name even when it is the
workspace default branch."
  (let* ((branch (plist-get metadata :branch))
         (default-branch (plist-get metadata :default-branch))
         (base-add (plist-get metadata :base-add))
         (base-del (plist-get metadata :base-del))
         (dirty-add (plist-get metadata :dirty-add))
         (dirty-del (plist-get metadata :dirty-del))
         (has-diff (or (agent-hub--diff-nonzero-p base-add base-del)
                       (agent-hub--diff-nonzero-p dirty-add dirty-del)))
         (show-branch (and branch
                           (or (not (string-equal branch default-branch))
                               (and show-default-branch has-diff))))
         (parts nil))
    (when show-branch
      (push (propertize branch 'font-lock-face 'agent-hub-branch) parts)
      (when (agent-hub--diff-nonzero-p base-add base-del)
        (push (agent-hub--format-diff-count base-add base-del) parts)))
    (when (agent-hub--diff-nonzero-p dirty-add dirty-del)
      (push (concat (propertize "dirty" 'font-lock-face 'agent-hub-badge)
                    " "
                    (agent-hub--format-diff-count dirty-add dirty-del))
            parts))
    (if parts
        (concat "  [" (string-join (nreverse parts) " ") "]")
      "")))

(defun agent-hub--format-pr-badge (pr)
  "Return a compact PR badge for PR metadata."
  (if-let ((number (plist-get pr :number)))
      (let* ((state (or (plist-get pr :state) "UNKNOWN"))
             (face (pcase state
                     ("OPEN" 'agent-hub-pr-open)
                     ("DRAFT" 'agent-hub-pr-draft)
                     ("MERGED" 'agent-hub-pr-merged)
                     ("CLOSED" 'agent-hub-pr-closed)
                     (_ 'agent-hub-pr-other))))
        (propertize (format "  #%s %s" number state)
                    'font-lock-face face
                    'agent-hub-pr-url (plist-get pr :url)))
    ""))

(defun agent-hub--visible-branch-info (root repo git-info cwds)
  "Return branch metadata for distinct branches represented by CWDS."
  (let* ((default-branch (plist-get git-info :default-branch))
         (branches (seq-uniq
                    (seq-keep (lambda (cwd)
                                (agent-hub--branch-for-cwd cwd git-info))
                              cwds)
                    #'string-equal)))
    (mapcar
     (lambda (branch)
       (let* ((diff (and (not (string-equal branch default-branch))
                         (agent-hub--git-branch-diff root branch default-branch)))
              (pr (and repo
                       (not (string-equal branch default-branch))
                       (agent-hub--gh-pr-for-branch repo branch))))
         (cons branch
               (append (when diff
                         (list :base-add (plist-get diff :add)
                               :base-del (plist-get diff :del)))
                       (when pr (list :pr pr))))))
     branches)))

(defun agent-hub--visible-dirty-info (cwds)
  "Return dirty diff metadata for distinct session CWDS."
  (mapcar
   (lambda (cwd)
     (let ((diff (agent-hub--git-dirty-diff cwd)))
       (cons (directory-file-name (expand-file-name cwd))
             (when diff
               (list :dirty-add (plist-get diff :add)
                     :dirty-del (plist-get diff :del))))))
   (seq-uniq cwds #'string-equal)))

(defun agent-hub--metadata-for-cwd (cwd git-info branch-info dirty-info)
  "Return display metadata for CWD from precomputed git metadata maps."
  (let* ((branch (agent-hub--branch-for-cwd cwd git-info))
         (default-branch (plist-get git-info :default-branch))
         (branch-data (cdr (assoc branch branch-info)))
         (dirty-data (and cwd
                          (cdr (assoc (directory-file-name (expand-file-name cwd))
                                      dirty-info)))))
    (append (list :branch branch :default-branch default-branch)
            branch-data
            dirty-data)))

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
       (concat
        (format "    %s%s%s"
                (buffer-name buffer)
                (if id (propertize (format "  %s" id) 'font-lock-face 'shadow) "")
                (if (and suffix (not (string-empty-p suffix)))
                    (propertize (format "  @%s" (directory-file-name suffix))
                                'font-lock-face 'shadow)
                  ""))
        (agent-hub--format-status-badge buffer))
       'agent-hub-type 'session
       'agent-hub-root root
       'agent-hub-buffer buffer))))

(defun agent-hub--insert-session-line (root session metadata)
  "Insert a Claude SESSION section nested under workspace ROOT.
SESSION is a plist (:id :file :time :title :cwd :buffer); :buffer is the live
agent-shell buffer when one is already attached to this session.  METADATA is a
plist with precomputed branch, diff, dirty, and PR data."
  (let* ((id (plist-get session :id))
         (title (or (plist-get session :title) "(untitled)"))
         (date (agent-hub--format-session-date (plist-get session :time)))
         (cwd (plist-get session :cwd))
         (suffix (and root cwd
                      (not (string-equal (file-name-as-directory cwd)
                                         (file-name-as-directory root)))
                      (string-remove-prefix (file-name-as-directory root)
                                            (file-name-as-directory cwd))))
         (buffer (plist-get session :buffer))
         (pr (plist-get metadata :pr))
         (pr-url (plist-get pr :url))
         (mark (if (agent-hub--session-marked-p session)
                   (concat "  " (propertize "*" 'font-lock-face 'agent-hub-mark) " ")
                 "    ")))
    (magit-insert-section (agent-hub-session (or id buffer))
      (agent-hub--insert-line
       (concat mark
               (propertize title 'font-lock-face 'agent-hub-session-title)
               (if (and suffix (not (string-empty-p suffix)))
                   (propertize (format "  @%s" (directory-file-name suffix))
                               'font-lock-face 'agent-hub-worktree)
                 "")
               (agent-hub--format-git-badge metadata)
               (agent-hub--format-pr-badge pr)
               ;; Align the date into a fixed column regardless of title width
               ;; (CJK-safe, unlike `format' padding).  If metadata runs past the
               ;; target column, keep a small real gap before DATE.
               (propertize " " 'display '(space :align-to 72))
               "  "
               (propertize date 'font-lock-face 'agent-hub-date)
               (agent-hub--format-status-badge buffer))
       'agent-hub-type 'session
       'agent-hub-root root
       'agent-hub-session session
       'agent-hub-session-id id
       'agent-hub-buffer (and (buffer-live-p buffer) buffer)
       'agent-hub-pr-url pr-url))))

(defun agent-hub--insert-active-session-line (root session metadata)
  "Insert an aggregated active SESSION line tagged with workspace ROOT.
Like `agent-hub--insert-session-line' but prefixes the workspace name so the
session's workspace is visible in the top-level Active Sessions list.  ROOT may
be nil for an ungrouped session.  SESSION and METADATA share the shapes used by
`agent-hub--insert-session-line'."
  (let* ((id (plist-get session :id))
         (title (or (plist-get session :title) "(untitled)"))
         (date (agent-hub--format-session-date (plist-get session :time)))
         (cwd (plist-get session :cwd))
         (suffix (and root cwd
                      (not (string-equal (file-name-as-directory cwd)
                                         (file-name-as-directory root)))
                      (string-remove-prefix (file-name-as-directory root)
                                            (file-name-as-directory cwd))))
         (buffer (plist-get session :buffer))
         (pr (plist-get metadata :pr))
         (pr-url (plist-get pr :url))
         (workspace (if root
                        (propertize (agent-hub--workspace-name root)
                                    'font-lock-face 'agent-hub-workspace)
                      (propertize "(ungrouped)" 'font-lock-face 'agent-hub-badge)))
         (mark (if (agent-hub--session-marked-p session)
                   (concat "  " (propertize "*" 'font-lock-face 'agent-hub-mark) " ")
                 "    ")))
    (magit-insert-section (agent-hub-session (or id buffer))
      (agent-hub--insert-line
       (concat (agent-hub--format-status-badge buffer)
               mark
               workspace "  "
               (propertize title 'font-lock-face 'agent-hub-session-title)
               (if (and suffix (not (string-empty-p suffix)))
                   (propertize (format "  @%s" (directory-file-name suffix))
                               'font-lock-face 'agent-hub-worktree)
                 "")
               (agent-hub--format-git-badge metadata)
               (agent-hub--format-pr-badge pr)
               (propertize " " 'display '(space :align-to 72))
               "  "
               (propertize date 'font-lock-face 'agent-hub-date))
       'agent-hub-type 'session
       'agent-hub-root root
       'agent-hub-session session
       'agent-hub-session-id id
       'agent-hub-buffer (and (buffer-live-p buffer) buffer)
       'agent-hub-pr-url pr-url))))

(defun agent-hub--insert-active-section (data)
  "Insert the top-level aggregated Active Sessions section from DATA.
DATA is the result of `agent-hub--sessions-by-workspace'.  Lists every live
agent-shell session across workspaces, each tagged with its workspace, with
branch/diff/PR badges resolved from the same cached helpers the by-workspace
view uses (so it warms that cache too).  Ungrouped sessions get no badges."
  (let* ((grouped (seq-filter #'cdr (plist-get data :grouped)))
         (ungrouped (plist-get data :ungrouped))
         ;; (ROOT . BUFFERS) groups in workspace-sorted order, ungrouped last.
         ;; ROOT nil carries the buffers that matched no workspace.
         (groups (append grouped (when ungrouped (list (cons nil ungrouped)))))
         (count (apply #'+ (mapcar (lambda (g) (length (cdr g))) groups))))
    (magit-insert-section (agent-hub-active 'active)
      (agent-hub--insert-heading
       (propertize (format "Active Sessions (%d)" count)
                   'font-lock-face 'agent-hub-workspace)
       'agent-hub-type 'active-header)
      (if (zerop count)
          (agent-hub--insert-line
           (propertize "    (no active sessions)" 'font-lock-face 'shadow)
           'agent-hub-type 'info)
        (dolist (group groups)
          (let* ((root (car group))
                 (buffers (cdr group))
                 (sessions (mapcar #'agent-hub--live-buffer-session buffers))
                 (cwds (and root
                            (seq-uniq (mapcar (lambda (s) (plist-get s :cwd))
                                              sessions)
                                      #'string-equal)))
                 (repo (and root (agent-hub--git-origin-repo root)))
                 (git-info (and root (agent-hub--git-workspace-info root)))
                 (branch-info (and git-info
                                   (agent-hub--visible-branch-info
                                    root repo git-info cwds)))
                 (dirty-info (and git-info
                                  (agent-hub--visible-dirty-info cwds))))
            (dolist (session sessions)
              (agent-hub--insert-active-session-line
               root session
               (and git-info
                    (agent-hub--metadata-for-cwd
                     (plist-get session :cwd) git-info branch-info dirty-info))))))))))

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
    (agent-hub--prune-marks)
    (erase-buffer)
    (magit-insert-section (agent-hub-root)
      (insert (propertize "Agent Hub" 'font-lock-face 'bold) "  "
              (propertize
               "(RET open  TAB fold  n/p move  m mark  u unmark  U clear  c new-req  N start  C-u N story worktree  o dir  b PR  k kill  D delete  g refresh  C-u g force  q quit)"
               'font-lock-face 'agent-hub-key-help)
              "\n\n")
      (agent-hub--insert-active-section data)
      (insert "\n")
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
                     (shown (seq-take session-files agent-hub-session-limit))
                     (visible-cwds (seq-uniq
                                    (mapcar (lambda (entry) (plist-get entry :cwd))
                                            shown)
                                    #'string-equal))
                     (has-root-session
                      (seq-some (lambda (entry)
                                  (agent-hub--same-directory-p
                                   root (plist-get entry :cwd)))
                                session-files))
                     (metadata-cwds (if has-root-session
                                        (seq-uniq (cons root visible-cwds)
                                                  #'string-equal)
                                      visible-cwds))
                     (repo (agent-hub--git-origin-repo root))
                     (git-info (and (> total 0) (agent-hub--git-workspace-info root)))
                     (branch-info (and git-info
                                       (agent-hub--visible-branch-info
                                        root repo git-info metadata-cwds)))
                     (dirty-info (and git-info
                                      (agent-hub--visible-dirty-info metadata-cwds)))
                     (root-metadata (and has-root-session
                                         (agent-hub--metadata-for-cwd
                                          root git-info branch-info dirty-info))))
                (agent-hub--insert-line
                 (concat "    "
                         (if repo (propertize (format "(%s)  " repo)
                                              'font-lock-face 'agent-hub-repo)
                           "")
                         (propertize (format "%d session%s" total
                                             (if (= total 1) "" "s"))
                                     'font-lock-face 'agent-hub-badge)
                         (agent-hub--format-git-badge root-metadata t)
                         (agent-hub--format-pr-badge
                          (plist-get root-metadata :pr)))
                 'agent-hub-type 'info
                 'agent-hub-root root
                 'agent-hub-pr-url (plist-get (plist-get root-metadata :pr) :url))
                (when (> total 0)
                  (let ((id->buffer (agent-hub--live-session-map live-buffers)))
                    (dolist (entry shown)
                      (let* ((file (plist-get entry :file))
                             (id (file-name-base file))
                             (cwd (plist-get entry :cwd)))
                        (agent-hub--insert-session-line
                         root (list :id id
                                    :file file
                                    :time (plist-get entry :time)
                                    :title (agent-hub--session-title-cached
                                            file (plist-get entry :time))
                                    :cwd cwd
                                    :buffer (gethash id id->buffer))
                         (unless (agent-hub--same-directory-p root cwd)
                           (agent-hub--metadata-for-cwd
                            cwd git-info branch-info dirty-info)))))
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
      ;; Climb the section identity outward until one resolves, so deleting the
      ;; section under point lands on its surviving parent (workspace / Active
      ;; Sessions heading) instead of jumping to the end of the buffer.
      (let ((tail ident))
        (while (and tail (not (magit-get-section tail)))
          (setq tail (cdr tail)))
        (when-let* ((section (and tail (magit-get-section tail))))
          (goto-char (oref section start)))))
    ;; Persist any titles extracted during this render so the next cold start
    ;; (even after an Emacs restart) skips re-opening those session files.
    (agent-hub--title-cache-save)))

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

(defun agent-hub--session-at-point ()
  "Return the persisted Claude session plist at point."
  (or (get-text-property (line-beginning-position) 'agent-hub-session)
      (user-error "No persisted session on this line")))

(defun agent-hub--session-file (session)
  "Return SESSION's on-disk file path."
  (or (plist-get session :file)
      (user-error "No on-disk session file for this line")))

(defun agent-hub--session-marked-p (session)
  "Return non-nil when SESSION is marked for batch deletion."
  (when-let* ((file (plist-get session :file)))
    (member file agent-hub--marked-session-files)))

(defun agent-hub--mark-session-file (file)
  "Mark session FILE for batch deletion."
  (cl-pushnew file agent-hub--marked-session-files :test #'string-equal))

(defun agent-hub--unmark-session-file (file)
  "Remove session FILE from the batch deletion marks."
  (setq agent-hub--marked-session-files
        (cl-remove file agent-hub--marked-session-files :test #'string-equal)))

(defun agent-hub--prune-marks ()
  "Drop marked session files that no longer exist."
  (setq agent-hub--marked-session-files
        (seq-filter (lambda (file) (ignore-errors (file-exists-p file)))
                    agent-hub--marked-session-files)))

;;;; Commands

(defun agent-hub-refresh (&optional force)
  "Re-render the dashboard.
With prefix FORCE, clear cached git and GitHub metadata first."
  (interactive "P")
  ;; Only git/GitHub metadata is time-sensitive.  Session titles come from a
  ;; session's first user message and never change, so the persistent title
  ;; cache is left intact -- clearing it would just re-pay the slow file opens.
  (when force
    (clrhash agent-hub--git-info-cache))
  (let ((inhibit-read-only t))
    (agent-hub--render)))

(defun agent-hub-browse-pr ()
  "Open the PR URL for the session at point."
  (interactive)
  (if-let ((url (get-text-property (line-beginning-position) 'agent-hub-pr-url)))
      (browse-url url)
    (user-error "No PR on this line")))

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
      (let ((default-directory (file-name-as-directory
                                (or (plist-get session :cwd)
                                    root
                                    default-directory))))
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
    ((or 'workspace 'ungrouped-header 'active-header)
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

(defun agent-hub--slug (text)
  "Return a safe ASCII slug for TEXT.
Non-ASCII characters (e.g. CJK) are dropped, so the result is always
lowercase English alphanumerics joined by hyphens."
  (let ((slug (downcase (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" text))))
    (string-trim slug "-+" "-+")))

(defun agent-hub--read-worktree-story ()
  "Read a user story describing the work for a new worktree."
  (let ((story (string-trim (read-string "User story for new worktree: "))))
    (when (string-empty-p story)
      (user-error "Empty user story"))
    story))

(defun agent-hub--llm-provider ()
  "Return the configured LLM provider for name inference, or nil."
  (and (require 'llm nil t)
       (boundp 'llm-github-provider)
       llm-github-provider))

(defun agent-hub--worktree-name-prompt (root story)
  "Build the LLM prompt asking for worktree and branch names.
ROOT names the repository (basename only); STORY is the user story."
  (let ((repo (file-name-nondirectory (directory-file-name root))))
    (concat
     "You name git worktrees and branches from a short user story.\n"
     "Return ONLY a JSON object, no prose, no markdown fences:\n"
     "{\"worktree_name\":\"...\",\"branch_name\":\"...\"}\n\n"
     "Rules:\n"
     "- Both names MUST be in English using only ASCII letters, digits, "
     "and hyphens, even when the story is written in another language. "
     "Translate or summarize the story into concise English first.\n"
     "- worktree_name: lowercase kebab-case, a single path segment "
     "(no slashes), short but descriptive.\n"
     "- branch_name: a valid git branch name; prefer a prefix like "
     "feature/, fix/, chore/, docs/, or refactor/ followed by a slug.\n\n"
     "Repository: " repo "\n"
     "Story: " story)))

(defun agent-hub--infer-worktree-names (root story)
  "Ask the LLM to infer worktree and branch names for ROOT from STORY.
Return the raw response string, or nil when no provider is available or
the call fails."
  (when-let ((provider (agent-hub--llm-provider)))
    (condition-case err
        (llm-chat provider
                  (llm-make-chat-prompt
                   (agent-hub--worktree-name-prompt root story)
                   :temperature 0 :max-tokens 200))
      (error
       (message "agent-hub: LLM name inference failed: %s"
                (error-message-string err))
       nil))))

(defun agent-hub--parse-worktree-name-json (raw)
  "Parse RAW LLM output into a JSON alist, or nil.
Strips markdown fences and tolerates surrounding prose."
  (when (and raw (not (string-empty-p raw)))
    (let* ((text (string-trim (replace-regexp-in-string "```[a-zA-Z]*" "" raw))))
      (or (ignore-errors
            (json-parse-string text :object-type 'alist :array-type 'list
                               :false-object :json-false))
          ;; Tolerate surrounding prose: parse the first {...} block.
          (let ((start (string-match "{" text))
                (end (and (string-match "}[^}]*\\'" text)
                          (1+ (string-match "}[^}]*\\'" text)))))
            (when (and start end (< start end))
              (ignore-errors
                (json-parse-string (substring text start end)
                                   :object-type 'alist :array-type 'list
                                   :false-object :json-false))))))))

(defun agent-hub--fallback-worktree-names (story)
  "Return deterministic worktree and branch names derived from STORY."
  (let ((slug (agent-hub--slug story)))
    (when (string-empty-p slug)
      ;; STORY has no ASCII-usable characters (e.g. it is pure CJK). Derive a
      ;; stable, non-empty slug from a hash so a deterministic name is always
      ;; available even without an LLM provider (e.g. in a remote workspace).
      (setq slug (concat "story-" (substring (md5 story) 0 8))))
    (when (> (length slug) 50)
      (setq slug (string-trim (substring slug 0 50) "-+" "-+")))
    (list :worktree-name slug
          :branch-name (concat "feature/" slug))))

(defun agent-hub--sanitize-worktree-name (name fallback)
  "Return a safe worktree basename from NAME, or FALLBACK."
  (let ((slug (and name (agent-hub--slug name))))
    (if (and slug (not (string-empty-p slug))
             (not (member slug '("." ".."))))
        slug
      fallback)))

(defun agent-hub--git-branch-name-valid-p (root branch)
  "Return non-nil when BRANCH is a valid git branch name in ROOT."
  (and branch (not (string-empty-p branch))
       (let ((default-directory (file-name-as-directory root)))
         (eq 0 (process-file "git" nil nil nil
                             "check-ref-format" "--branch" branch)))))

(defun agent-hub--sanitize-branch-name (root name fallback-worktree-name)
  "Return a valid git branch name for ROOT from NAME.
Fall back to feature/FALLBACK-WORKTREE-NAME when NAME is unusable."
  (let* ((candidate
          (and name
               (let ((s (downcase (string-trim name))))
                 ;; Keep only ASCII branch-safe characters; non-ASCII (e.g. CJK)
                 ;; is dropped so the branch name is always English.
                 (setq s (replace-regexp-in-string "[^a-z0-9/_-]+" "-" s))
                 (setq s (replace-regexp-in-string "-+" "-" s))
                 ;; Collapse slashes and strip hyphens hugging a slash so a
                 ;; dropped non-ASCII segment can't leave "feature/-foo".
                 (setq s (replace-regexp-in-string "-*/+-*" "/" s))
                 (setq s (string-trim s "[-/]+" "[-/]+"))
                 s)))
         (fallback (concat "feature/" fallback-worktree-name)))
    (cond
     ((and candidate (not (string-empty-p candidate))
           (agent-hub--git-branch-name-valid-p root candidate))
      candidate)
     ((agent-hub--git-branch-name-valid-p root fallback)
      fallback)
     (t (user-error "Could not derive a valid branch name from: %s" name)))))

(defun agent-hub--worktree-names-from-story (root story)
  "Infer, sanitize, and confirm worktree and branch names for ROOT from STORY.
Return a plist (:worktree-name NAME :branch-name BRANCH)."
  (let* ((fallback (agent-hub--fallback-worktree-names story))
         (parsed (agent-hub--parse-worktree-name-json
                  (agent-hub--infer-worktree-names root story)))
         (raw-worktree (or (agent-hub--json-alist-value 'worktree_name parsed)
                           (plist-get fallback :worktree-name)))
         (raw-branch (or (agent-hub--json-alist-value 'branch_name parsed)
                         (plist-get fallback :branch-name)))
         (worktree-name (agent-hub--sanitize-worktree-name
                         raw-worktree (plist-get fallback :worktree-name)))
         (branch-name (agent-hub--sanitize-branch-name
                       root raw-branch worktree-name))
         ;; Let the user review and edit the inferred names before creating.
         (worktree-name (agent-hub--sanitize-worktree-name
                         (read-string "Worktree name: " worktree-name)
                         (plist-get fallback :worktree-name)))
         (branch-name (agent-hub--sanitize-branch-name
                       root (read-string "Branch name: " branch-name)
                       worktree-name)))
    (list :worktree-name worktree-name :branch-name branch-name)))

(defun agent-hub--git-branch-exists-p (root branch)
  "Return non-nil when BRANCH already exists in ROOT."
  (let ((default-directory (file-name-as-directory root)))
    (eq 0 (process-file "git" nil nil nil
                       "rev-parse" "--verify" "--quiet"
                       (concat "refs/heads/" branch)))))

(defun agent-hub--worktrees-dir (root)
  "Return the agent-shell worktrees directory for ROOT."
  (require 'agent-shell nil t)
  (let ((default-directory (file-name-as-directory root)))
    (if (fboundp 'agent-shell--dot-subdir)
        (agent-shell--dot-subdir "worktrees")
      (expand-file-name ".agent-shell/worktrees/" root))))

(defun agent-hub--git (root &rest args)
  "Run git with ARGS in ROOT and return trimmed stdout on success."
  (let ((default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (let ((status (apply #'process-file "git" nil t nil args)))
        (unless (and (integerp status) (zerop status))
          (user-error "git %s failed: %s"
                      (string-join args " ")
                      (string-trim (buffer-string))))
        (string-trim (buffer-string))))))

(defun agent-hub--git-path-argument (path)
  "Return PATH as a pathname suitable for git subprocess arguments."
  (if (file-remote-p path)
      (file-local-name path)
    path))

(defun agent-hub--create-worktree (root)
  "Create a git worktree under ROOT from a user story and return its path.
Prompts for a user story, infers worktree and branch names via the LLM
\(editable before creation), then creates the worktree from the origin's
default branch."
  (let* ((story (agent-hub--read-worktree-story))
         (names (agent-hub--worktree-names-from-story root story))
         (worktree-name (plist-get names :worktree-name))
         (branch-name (plist-get names :branch-name))
         (default-branch (agent-hub--git-default-branch root))
         (worktrees-dir (agent-hub--worktrees-dir root))
         (worktree-path (directory-file-name
                         (expand-file-name worktree-name
                                           (file-name-as-directory worktrees-dir)))))
    (when (file-exists-p worktree-path)
      (user-error "Directory already exists: %s" worktree-path))
    (agent-hub--git root "fetch" "origin" default-branch)
    (make-directory (file-name-directory worktree-path) t)
    (let ((branch-exists (agent-hub--git-branch-exists-p root branch-name))
          (path-arg (agent-hub--git-path-argument worktree-path)))
      (if (and branch-exists
               (yes-or-no-p (format "Branch %s exists.  Reuse it? " branch-name)))
          (agent-hub--git root "worktree" "add" path-arg branch-name)
        (when branch-exists
          (user-error "Branch already exists: %s" branch-name))
        (agent-hub--git root "worktree" "add" "-b" branch-name
                        path-arg (concat "origin/" default-branch))))
    (unless (file-directory-p worktree-path)
      (user-error "Failed to create worktree: %s" worktree-path))
    worktree-path))

(defun agent-hub-start-session (&optional worktree)
  "Start a new agent-shell session in the workspace at point.
With prefix WORKTREE, prompt for a user story, infer worktree and branch
names from it via the LLM (editable before creation), create a git
worktree under `.agent-shell/worktrees/', and start the session there."
  (interactive "P")
  (let* ((root (agent-hub--root-at-point))
         (session-root (if worktree (agent-hub--create-worktree root) root)))
    (require 'agent-shell nil t)
    (let ((default-directory (file-name-as-directory session-root)))
      (call-interactively agent-hub-start-command))))

(defun agent-hub-open-workspace ()
  "Open the workspace at point with `project-switch-project'."
  (interactive)
  ;; `project-switch-project' records its argument verbatim via
  ;; `project--remember-dir', which dedupes by exact string match.  Our roots
  ;; are stored slashless (`directory-file-name'), so pass the trailing-slash
  ;; form to match project.el's canonical entries and avoid duplicates.
  (project-switch-project (file-name-as-directory (agent-hub--root-at-point))))

(defun agent-hub-mark-session ()
  "Mark the persisted Claude session at point for batch deletion."
  (interactive)
  (let* ((session (agent-hub--session-at-point))
         (file (agent-hub--session-file session)))
    (unless (file-exists-p file)
      (user-error "No on-disk session file for this line"))
    (agent-hub--mark-session-file file)
    (agent-hub-refresh)
    (message "Marked session %s" (or (plist-get session :title)
                                     (plist-get session :id)
                                     (file-name-base file)))))

(defun agent-hub-unmark-session ()
  "Unmark the persisted Claude session at point."
  (interactive)
  (let* ((session (agent-hub--session-at-point))
         (file (agent-hub--session-file session)))
    (agent-hub--unmark-session-file file)
    (agent-hub-refresh)
    (message "Unmarked session %s" (or (plist-get session :title)
                                       (plist-get session :id)
                                       (file-name-base file)))))

(defun agent-hub-unmark-all-sessions ()
  "Clear all batch deletion marks."
  (interactive)
  (let ((count (length agent-hub--marked-session-files)))
    (setq agent-hub--marked-session-files nil)
    (agent-hub-refresh)
    (message "Cleared %d marked session%s" count (if (= count 1) "" "s"))))

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

(defun agent-hub--delete-session-file (file buffer)
  "Delete persisted session FILE, killing BUFFER first when live."
  (unless (file-exists-p file)
    (user-error "No on-disk session file for this line"))
  (when (buffer-live-p buffer)
    (unless (kill-buffer buffer)
      (user-error "Could not kill live buffer %s" (buffer-name buffer))))
  (delete-file file))

(defun agent-hub--delete-current-session ()
  "Delete the persisted Claude session at point."
  (let* ((session (agent-hub--session-at-point))
         (file (agent-hub--session-file session))
         (id (or (plist-get session :id) (file-name-base file)))
         (title (or (plist-get session :title) id "(untitled)"))
         (buffer (plist-get session :buffer)))
    (when (yes-or-no-p
           (if (buffer-live-p buffer)
               (format "Delete session %s and kill live buffer %s? "
                       title (buffer-name buffer))
             (format "Delete session %s? " title)))
      (agent-hub--delete-session-file file buffer)
      (agent-hub--unmark-session-file file)
      (agent-hub-refresh)
      (message "Deleted session %s" title))))

(defun agent-hub--delete-marked-sessions ()
  "Delete all marked persisted Claude sessions."
  (let* ((files (agent-hub--prune-marks))
         (count (length files))
         (id->buffer (agent-hub--live-session-map (agent-hub--agent-buffers)))
         (live-count (length (seq-filter
                              #'buffer-live-p
                              (mapcar (lambda (file)
                                        (gethash (file-name-base file) id->buffer))
                                      files)))))
    (unless files
      (user-error "No marked sessions"))
    (when (yes-or-no-p
           (if (> live-count 0)
               (format "Delete %d marked sessions and kill %d live buffer%s? "
                       count live-count (if (= live-count 1) "" "s"))
             (format "Delete %d marked sessions? " count)))
      (dolist (file files)
        (agent-hub--delete-session-file
         file (gethash (file-name-base file) id->buffer))
        (agent-hub--unmark-session-file file))
      (agent-hub-refresh)
      (message "Deleted %d marked session%s" count (if (= count 1) "" "s")))))

(defun agent-hub-delete-session ()
  "Delete marked sessions, or the persisted Claude session at point.
If a deleted session has a live agent-shell buffer, kill it first.  Use
`agent-hub-kill-session' to only close a live buffer."
  (interactive)
  (if (agent-hub--prune-marks)
      (agent-hub--delete-marked-sessions)
    (agent-hub--delete-current-session)))

;;;; Mode + entry point

(defvar agent-hub-mode-map
  (let ((map (make-sparse-keymap)))
    ;; `n'/`p' (section navigation) and `TAB' (fold) come from the parent
    ;; `magit-section-mode-map'; only the dashboard's own actions are bound here.
    (define-key map (kbd "RET") #'agent-hub-visit)
    (define-key map (kbd "c") #'agent-hub-new-requirement)
    (define-key map (kbd "N") #'agent-hub-start-session)
    (define-key map (kbd "o") #'agent-hub-open-workspace)
    (define-key map (kbd "b") #'agent-hub-browse-pr)
    (define-key map (kbd "m") #'agent-hub-mark-session)
    (define-key map (kbd "u") #'agent-hub-unmark-session)
    (define-key map (kbd "U") #'agent-hub-unmark-all-sessions)
    (define-key map (kbd "k") #'agent-hub-kill-session)
    (define-key map (kbd "D") #'agent-hub-delete-session)
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

;; Flush titles extracted by lazy workspace expansions (which run outside
;; `agent-hub--render' and so miss its save) before Emacs exits.
(add-hook 'kill-emacs-hook #'agent-hub--title-cache-save)

(provide 'agent-hub)

;;; agent-hub.el ends here
