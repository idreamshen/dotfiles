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
(declare-function agent-shell-openai-make-codex-config "agent-shell-openai")
(declare-function agent-shell-pi-make-agent-config "agent-shell-pi")
(declare-function agent-shell--ensure-transcript-file "agent-shell")
(defvar agent-shell--state)
(defvar agent-shell--transcript-file)
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
  "Maximum number of agent-shell sessions listed under an expanded workspace.
Newest sessions are shown first; the rest are summarized as a \"… N more\"
line.  The expanded workspace's info line always reflects the full count."
  :type 'integer
  :group 'agent-hub)

(defcustom agent-hub-recent-session-limit 12
  "Target number of sessions shown in the top-level Recent Sessions section.
All live sessions are shown even when they exceed this limit; otherwise the
newest persisted sessions fill the remaining slots."
  :type 'integer
  :group 'agent-hub)

(defcustom agent-hub-recent-session-max-age-days 30
  "Maximum age in days for non-live sessions in Recent Sessions.
Live sessions are always shown.  Set this to nil to disable the age cutoff."
  :type '(choice (const :tag "No age cutoff" nil) number)
  :group 'agent-hub)

(defcustom agent-hub-git-info-cache-ttl 60
  "Seconds to cache git and GitHub metadata used by the dashboard."
  :type 'integer
  :group 'agent-hub)

(defcustom agent-hub-rerender-debounce 0.15
  "Seconds to coalesce async cache updates before re-rendering the dashboard.
Many background fetches completing in a burst collapse into a single repaint."
  :type 'number
  :group 'agent-hub)

;; Folding, per-section visibility, and point preservation across refresh are
;; all handled by `magit-section'; no buffer-local fold state is needed.

(defvar agent-hub--git-info-cache (make-hash-table :test #'equal)
  "Cache for git and GitHub metadata.
Values are plists shaped as (:time FLOAT :value VALUE).  Rendering reads it
stale-while-revalidate: a stale entry is displayed immediately while a
background fetch refreshes it.")

(defvar agent-hub--inflight (make-hash-table :test #'equal)
  "Set of fetch keys with an async subprocess currently running.
Dedupes concurrent revalidations of the same key across every cache.")

(defvar agent-hub--rerender-timer nil
  "Pending debounce timer for an async-triggered re-render, or nil.")

(defvar agent-hub--projects-dir-cache (make-hash-table :test #'equal)
  "Deprecated compatibility cache retained for isolated test bindings.")

(defcustom agent-hub-title-cache-file
  (locate-user-emacs-file "agent-hub-transcripts.eld")
  "File where parsed agent-shell transcript metadata is persisted.
The cache is keyed by transcript path and validated against modification time,
size, and `agent-hub--transcript-parser-version'."
  :type 'file
  :group 'agent-hub)

(defvar agent-hub--title-cache nil
  "Hash table mapping transcript FILE to cached metadata entries.
The historical variable name is retained for compatibility with existing
configuration and tests.  Each value is `(:signature SIG :metadata PLIST)'.")

(defvar agent-hub--title-cache-dirty nil
  "Non-nil when transcript metadata cache entries need to be persisted.")

(defconst agent-hub--transcript-parser-version 1
  "Version included in transcript metadata cache signatures.")

(defvar-local agent-hub--marked-session-files nil
  "Persisted agent-shell transcript files marked for batch deletion.")

(defvar-local agent-hub--marked-session-data nil
  "Hash table mapping marked session files to their session plists.")

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
  "Return a filtered, normalized list of workspace roots.
Local roots sort first, then remote.  A remote root is NOT probed with
`file-directory-p' (that is a TRAMP round-trip, or a blocking reconnect on a
dead host); a stale remote entry simply renders with zero sessions."
  (let (roots)
    (dolist (root (and (fboundp 'project-known-project-roots)
                       (project-known-project-roots)))
      (when (and (stringp root)
                 (not (seq-some (lambda (re) (string-match-p re root))
                                agent-hub-workspace-exclude-regexps))
                 (or (file-remote-p root) (file-directory-p root)))
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
  "Return the agent identifier for BUFFER, or nil."
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

(defun agent-hub--workspace-for-cwd (cwd workspaces)
  "Return the most specific workspace containing CWD, or nil.
When WORKSPACES overlap, the longest matching root wins."
  (car (seq-sort-by #'length #'>
                    (seq-filter (lambda (root)
                                  (agent-hub--path-prefix-p root cwd))
                                workspaces))))

(defun agent-hub--cache-put (key value)
  "Cache VALUE under KEY and return VALUE."
  (puthash key (list :time (float-time) :value value) agent-hub--git-info-cache)
  value)

;;;; Stale-while-revalidate: async fetch + debounced repaint

;; Rendering never blocks on I/O.  A getter returns whatever is cached (a stale
;; value, or nil rendered as a placeholder) and, when that value is stale or
;; missing, enqueues an async subprocess to refresh it.  When the process
;; finishes and its value differs, a debounced re-render repaints.  Dependency
;; chains (workspace-info -> branch names -> branch-diff/PR; worktree list ->
;; per-cwd sessions) fill in progressively: each repaint enqueues the next
;; now-computable wave until everything is fresh and nothing more is spawned.

(defun agent-hub--cache-entry (key)
  "Return the raw cache entry plist for KEY, or nil."
  (gethash key agent-hub--git-info-cache))

(defun agent-hub--cache-fresh-p (entry)
  "Return non-nil when cache ENTRY is within `agent-hub-git-info-cache-ttl'."
  (and entry
       (< (- (float-time) (plist-get entry :time))
          agent-hub-git-info-cache-ttl)))

(defun agent-hub--cached (key)
  "Return KEY's cached value regardless of freshness (nil if never fetched).
This is the stale-while-revalidate read used while rendering: it never blocks
and never triggers I/O."
  (plist-get (agent-hub--cache-entry key) :value))

(defun agent-hub--inflight-p ()
  "Return non-nil while any async fetch is still running."
  (> (hash-table-count agent-hub--inflight) 0))

(defun agent-hub--needs-fetch-p (key)
  "Return non-nil when KEY should be (re)fetched now.
True when KEY is stale or absent and no fetch for it is already in flight."
  (and (not (gethash key agent-hub--inflight))
       (not (agent-hub--cache-fresh-p (agent-hub--cache-entry key)))))

(defun agent-hub--invalidate-all ()
  "Mark every git/GitHub cache entry stale without discarding its value.
Stale values keep rendering while async revalidation refreshes them, so a
forced refresh never flashes the dashboard back to placeholders."
  (maphash (lambda (_key entry) (setf (plist-get entry :time) 0))
           agent-hub--git-info-cache))

(defun agent-hub--default-async-runner (_key directory program args callback)
  "Run PROGRAM ARGS asynchronously in DIRECTORY; deliver output to CALLBACK.
CALLBACK is called with (SUCCESS-P OUTPUT-STRING).  A remote (TRAMP) DIRECTORY
runs PROGRAM on the remote host; a local one runs it locally.  A sentinel
delivers the result later, so process exit never blocks Emacs -- except that
the first call after a dropped TRAMP connection may block briefly while TRAMP
re-establishes it."
  (let* ((default-directory (or directory temporary-file-directory))
         (buffer (generate-new-buffer " *agent-hub-fetch*"))
         ;; Use a pipe, not a PTY: with a PTY git/gh think stdout is a terminal
         ;; and launch a pager that blocks forever on a "Press RETURN" prompt.
         (process-connection-type nil)
         (process (apply #'start-file-process "agent-hub-fetch" buffer program args)))
    (set-process-query-on-exit-flag process nil)
    (set-process-sentinel
     process
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (let ((ok (and (eq (process-status proc) 'exit)
                        (eq (process-exit-status proc) 0)))
               (out (with-current-buffer (process-buffer proc) (buffer-string))))
           (kill-buffer (process-buffer proc))
           (funcall callback ok out)))))))

(defvar agent-hub--async-runner #'agent-hub--default-async-runner
  "Function used to run an async fetch subprocess.
Called with (KEY DIRECTORY PROGRAM ARGS CALLBACK), where CALLBACK takes
(SUCCESS-P OUTPUT-STRING).  Rebindable in tests to run synchronously.")

(defun agent-hub--fetch-complete (key value)
  "Store VALUE under KEY, clear its in-flight mark, and repaint if it changed."
  (remhash key agent-hub--inflight)
  (let ((changed (not (equal value (agent-hub--cached key)))))
    (agent-hub--cache-put key value)
    (when changed
      (agent-hub--schedule-rerender))))

(defun agent-hub--spawn (key directory program program-args parse-fn)
  "Asynchronously fetch KEY by running PROGRAM PROGRAM-ARGS in DIRECTORY.
PARSE-FN converts the trimmed stdout string into the value cached under KEY.
A failed, errored, or unreachable process caches nil -- a negative result
trusted for the normal TTL, so a broken host is not retried in a tight loop."
  (unless (gethash key agent-hub--inflight)
    (puthash key t agent-hub--inflight)
    (condition-case _err
        (funcall agent-hub--async-runner key directory program program-args
                 (lambda (ok out)
                   (agent-hub--fetch-complete
                    key
                    (and ok
                         (condition-case nil
                             (funcall parse-fn (string-trim (or out "")))
                           (error nil))))))
      (error (remhash key agent-hub--inflight)))))

(defun agent-hub--swr (key directory parse-fn program &rest args)
  "Return KEY's cached value now; revalidate asynchronously when stale.
Spawns PROGRAM ARGS in DIRECTORY (stdout parsed by PARSE-FN) only when KEY is
stale or absent and not already in flight.  Never blocks."
  (when (agent-hub--needs-fetch-p key)
    (agent-hub--spawn key directory program args parse-fn))
  (agent-hub--cached key))

(defun agent-hub--schedule-rerender ()
  "Coalesce async cache updates into one debounced re-render."
  (unless agent-hub--rerender-timer
    (setq agent-hub--rerender-timer
          (run-with-timer agent-hub-rerender-debounce nil
                          #'agent-hub--rerender-now))))

(defun agent-hub--rerender-now ()
  "Re-render the dashboard from cache when its buffer is live."
  (setq agent-hub--rerender-timer nil)
  (let ((buffer (get-buffer agent-hub-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when (derived-mode-p 'agent-hub-mode)
          (let ((inhibit-read-only t))
            (agent-hub--render)))))))

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
  "Return owner/repo for ROOT's git origin remote, or nil.
Cached stale-while-revalidate; the fetch runs asynchronously."
  (agent-hub--swr (format "origin:%s" root) root
                  #'agent-hub--github-repo-from-url
                  "git" "remote" "get-url" "origin"))

(defun agent-hub--parse-default-branch (text)
  "Return the default branch from `git symbolic-ref' output TEXT, or \"main\"."
  (cond
   ((and text (string-match "\\`origin/\\(.+\\)\\'" text)) (match-string 1 text))
   ((and text (not (string-empty-p text))) text)
   (t "main")))

(defun agent-hub--parse-current-branch (text)
  "Return the current branch from `git branch --show-current' TEXT, or nil."
  (and text (not (string-empty-p text)) text))

(defun agent-hub--git-default-branch (root)
  "Return ROOT's origin default branch, falling back to main.
Synchronous; used by the interactive worktree-creation command."
  (agent-hub--parse-default-branch
   (agent-hub--process-output
    root "git" "symbolic-ref" "--quiet" "--short" "refs/remotes/origin/HEAD")))

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

(defconst agent-hub--workspace-info-separator "===AGENTHUB==="
  "Marker line separating the sub-outputs of the combined workspace-info fetch.")

(defun agent-hub--parse-workspace-info (root text)
  "Build workspace git metadata for ROOT from combined git output TEXT.
TEXT is three sections -- symbolic-ref, current-branch, and worktree porcelain
-- joined by `agent-hub--workspace-info-separator' lines.  Return a plist
\(:default-branch BRANCH :cwd-branches ALIST)."
  (let* ((parts (split-string (or text "") agent-hub--workspace-info-separator))
         (default-branch (agent-hub--parse-default-branch
                          (string-trim (or (nth 0 parts) ""))))
         (root-path (agent-hub--normalize-git-path root root))
         (root-branch (agent-hub--parse-current-branch
                       (string-trim (or (nth 1 parts) ""))))
         (worktrees (agent-hub--parse-worktree-branches root (or (nth 2 parts) ""))))
    (when (and root-branch (not (assoc root-path worktrees)))
      (push (cons root-path root-branch) worktrees))
    (list :default-branch default-branch
          :cwd-branches worktrees)))

(defun agent-hub--git-workspace-info (root)
  "Return cached git metadata for workspace ROOT.
Fetched with one combined git invocation (three sub-commands joined by marker
lines) so a remote ROOT costs a single round-trip instead of three; cached
stale-while-revalidate and revalidated asynchronously."
  (agent-hub--swr
   (format "git:%s" root) root
   (lambda (text) (agent-hub--parse-workspace-info root text))
   "sh" "-c"
   (concat
    "git symbolic-ref --quiet --short refs/remotes/origin/HEAD; "
    "echo " agent-hub--workspace-info-separator "; "
    "git branch --show-current; "
    "echo " agent-hub--workspace-info-separator "; "
    "git worktree list --porcelain")))

(defun agent-hub--parse-numstat (text)
  "Sum additions/deletions from `git diff --numstat' output TEXT.
Return a plist (:add N :del M); binary rows (\"-\t-\") contribute nothing."
  (let ((add 0)
        (del 0))
    (dolist (line (split-string (or text "") "\n" t))
      (when (string-match "\\`\\([0-9]+\\)\t\\([0-9]+\\)\t" line)
        (setq add (+ add (string-to-number (match-string 1 line))))
        (setq del (+ del (string-to-number (match-string 2 line))))))
    (list :add add :del del)))

(defun agent-hub--git-branch-diff (root branch default-branch)
  "Return cached line diff for BRANCH against DEFAULT-BRANCH under ROOT.
Prefers `origin/DEFAULT...BRANCH', falling back to `DEFAULT...BRANCH' when the
origin ref is absent.  Cached stale-while-revalidate; the fetch is async."
  (when (and branch default-branch (not (string-equal branch default-branch)))
    (agent-hub--swr
     (format "branch-diff:%s:%s:%s" root default-branch branch) root
     #'agent-hub--parse-numstat
     "sh" "-c"
     (format "git diff --numstat %s 2>/dev/null || git diff --numstat %s"
             (shell-quote-argument (format "origin/%s...%s" default-branch branch))
             (shell-quote-argument (format "%s...%s" default-branch branch))))))

(defun agent-hub--git-dirty-diff (cwd)
  "Return cached tracked dirty line diff for CWD.
Cached stale-while-revalidate; the fetch is async."
  (when cwd
    (agent-hub--swr (format "dirty:%s" cwd) cwd
                    #'agent-hub--parse-numstat
                    "git" "diff" "--numstat" "HEAD" "--")))

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

(defun agent-hub--parse-pr (text)
  "Parse gh PR JSON TEXT, dispatching on shape.
A `gh pr view' object (\"{...}\") goes to `agent-hub--parse-pr-view'; a
`gh pr list' array (\"[...]\") goes to `agent-hub--parse-pr-list'.  Malformed
or empty output returns nil so a failed GitHub fetch degrades quietly."
  (condition-case nil
      (when (and text (not (string-empty-p text)))
        (if (string-prefix-p "[" (string-trim-left text))
            (agent-hub--parse-pr-list text)
          (agent-hub--parse-pr-view text)))
    (error nil)))

(defun agent-hub--gh-pr-for-branch (repo branch)
  "Return cached GitHub PR metadata for REPO's exact BRANCH.
Runs `gh pr view' with a `gh pr list' fallback in one local shell command.
Cached stale-while-revalidate; the (network) fetch runs asynchronously."
  (when (and repo branch (executable-find "gh"))
    (let ((fields (concat "number,title,state,isDraft,url,headRefName,"
                          "updatedAt,additions,deletions"))
          (r (shell-quote-argument repo))
          (b (shell-quote-argument branch)))
      (agent-hub--swr
       (format "pr:%s:%s" repo branch) nil
       #'agent-hub--parse-pr
       "sh" "-c"
       (format (concat "gh pr view %s -R %s --json %s 2>/dev/null || "
                       "gh pr list -R %s --head %s --state all --limit 10 --json %s")
               b r fields r b fields)))))

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

(defun agent-hub--claude-projects-localname (root)
  "Return ROOT's `.claude/projects/<mangled>' path relative to home.
Pure string transform of ROOT's localname -- no I/O and no `~' resolution, so
it is safe to build a remote shell command (\"$HOME/<this>\") around it."
  (let* ((abs (expand-file-name root))
         (localname (or (file-remote-p abs 'localname) abs))
         (mangled (replace-regexp-in-string "[/.]" "-" localname)))
    (concat ".claude/projects/" mangled)))

(defun agent-hub--claude-projects-dir (root)
  "Return ROOT's `~/.claude/projects/<mangled-cwd>/' directory (TRAMP-aware).
Memoized in `agent-hub--projects-dir-cache' so the remote `~' expansion costs
one TRAMP round-trip per connection lifetime rather than one per render."
  (or (gethash root agent-hub--projects-dir-cache)
      (puthash
       root
       (let ((remote (or (file-remote-p (expand-file-name root)) "")))
         ;; The "~/" dir part resolves to the (remote) home via TRAMP.
         (expand-file-name
          (file-name-as-directory (agent-hub--claude-projects-localname root))
          (concat remote "~/")))
       agent-hub--projects-dir-cache)))

(defun agent-hub--session-cwds (root)
  "Return ROOT plus any agent-shell worktree roots nested under it.
A local ROOT is listed synchronously.  A remote ROOT is listed asynchronously
via a remote `find'; until it arrives only ROOT is returned and the nested
worktrees fill in on a later repaint."
  (let ((worktrees
         (if (file-remote-p root)
             (agent-hub--swr
              (format "worktree-cwds:%s" root) root
              (lambda (text)
                (agent-hub--parse-find-dirs text (or (file-remote-p root) "")))
              "sh" "-c"
              (format "find %s -mindepth 1 -maxdepth 1 -type d 2>/dev/null"
                      (shell-quote-argument
                       (concat (file-local-name (directory-file-name root))
                               "/.agent-shell/worktrees"))))
           (condition-case nil
               (let ((worktrees-dir (expand-file-name
                                     ".agent-shell/worktrees/"
                                     (file-name-as-directory root))))
                 (when (file-directory-p worktrees-dir)
                   (seq-filter
                    #'file-directory-p
                    (mapcar #'directory-file-name
                            (directory-files worktrees-dir t
                                             directory-files-no-dot-files-regexp
                                             t)))))
             (error nil)))))
    (seq-uniq (cons root worktrees) #'string-equal)))

(defun agent-hub--legacy-session-files-for-cwd (cwd)
  "Return CWD's legacy Claude session plists, newest first.
A local CWD is listed synchronously.  A remote CWD is fetched asynchronously
via a remote `find'; until it arrives the cached (possibly nil) value is
returned.  The result is always a freshly-consed list, safe for the
destructive `mapcan'/`sort' in `agent-hub--session-files'."
  (if (file-remote-p cwd)
      ;; `$HOME' is expanded shell-side so Emacs never resolves the remote `~'.
      (copy-sequence
       (agent-hub--swr
        (format "session-files:%s" cwd) cwd
        (lambda (text)
          (agent-hub--parse-find-jsonl text cwd (or (file-remote-p cwd) "")))
        "sh" "-c"
        (concat "d=\"$HOME/" (agent-hub--claude-projects-localname cwd) "\"; "
                "[ -d \"$d\" ] && "
                "find \"$d\" -maxdepth 1 -name '*.jsonl' -printf '%T@ %p\\n'")))
    (condition-case nil
        (let ((dir (agent-hub--claude-projects-dir cwd)))
          (when (file-directory-p dir)
            (mapcar (lambda (e)
                      (list :file (car e)
                            :time (file-attribute-modification-time (cdr e))
                            :cwd cwd))
                    (directory-files-and-attributes dir t "\\.jsonl\\'" t))))
      (error nil))))

(defun agent-hub--legacy-session-files (root)
  "Return ROOT's legacy Claude session plists, newest first.
Includes sessions whose cwd is an agent-shell worktree nested under ROOT.
Cheap: one directory listing per cwd, no file contents are read.  Returns nil
on any error (e.g. an unreachable remote host) so the dashboard degrades
gracefully."
  (seq-sort (lambda (a b) (time-less-p (plist-get b :time)
                                       (plist-get a :time)))
            (mapcan #'agent-hub--session-files-for-cwd
                    (agent-hub--session-cwds root))))

(defun agent-hub--parse-find-jsonl (text cwd remote-prefix)
  "Parse remote `find -printf \"%T@ %p\\n\"' TEXT into session plists for CWD.
Each line is `EPOCH PATH'; REMOTE-PREFIX (e.g. \"/ssh:host:\") is prepended to
PATH so the result carries a TRAMP filename.  Returns plists (:file :time :cwd)."
  (delq nil
        (mapcar
         (lambda (line)
           (let ((line (string-trim-right line "\r+")))
             (when (string-match "\\`\\([0-9.]+\\) \\(.+\\)\\'" line)
               (list :file (concat remote-prefix (match-string 2 line))
                     :time (seconds-to-time
                            (string-to-number (match-string 1 line)))
                     :cwd cwd))))
         (split-string (or text "") "\n" t))))

(defun agent-hub--parse-find-dirs (text remote-prefix)
  "Parse remote `find -type d' TEXT into directory paths.
REMOTE-PREFIX is prepended to each path and trailing slashes are removed."
  (mapcar (lambda (line)
            (concat remote-prefix
                    (directory-file-name (string-trim-right line "\r+"))))
          (split-string (or text "") "\n" t)))

(defconst agent-hub--session-title-window 1048576
  "Max bytes read from a session file when extracting its title.
Generous enough to clear an early user turn that embeds a screenshot
\(such lines can be a few hundred KB), while bounding remote reads.")

(defun agent-hub--parse-session-title (text)
  "Extract a display title from session-file head TEXT, or nil.
TEXT is the leading bytes of a .jsonl session; return the first real user
message (skipping tool/system lines that start with \"<\"), collapsed and
truncated to 60 chars.  A truncated final line (no trailing newline) is
ignored, so the same logic serves a local byte-window read and a remote
`head -c' fetch."
  (with-temp-buffer
    (insert (or text ""))
    ;; When the window cut the last line short, don't parse that partial line:
    ;; scan only up to the start of the final unterminated line.
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
                        (msg-text (cond
                                   ((stringp content) content)
                                   ((listp content)
                                    (mapconcat
                                     (lambda (b)
                                       (if (and (listp b)
                                                (equal (alist-get 'type b) "text"))
                                           (or (alist-get 'text b) "") ""))
                                     content " "))
                                   (t ""))))
              (setq msg-text (string-trim msg-text))
              (when (and (> (length msg-text) 0)
                         (not (string-prefix-p "<" msg-text)))
                (setq title msg-text)))))
        (forward-line 1))
      (when title
        (setq title (replace-regexp-in-string "[ \t\n\r]+" " " title))
        (if (> (length title) 60) (concat (substring title 0 57) "...") title)))))

(defun agent-hub--session-title (file)
  "Return a display title for local session FILE, or nil.
Reads a bounded head of FILE (see `agent-hub--session-title-window') and
delegates parsing to `agent-hub--parse-session-title'."
  (condition-case nil
      (agent-hub--parse-session-title
       (with-temp-buffer
         (insert-file-contents file nil 0 agent-hub--session-title-window)
         (buffer-string)))
    (error nil)))

;; Cache entries are FILE -> (MTIME . TITLE), where MTIME is `float-time' of the
;; file's modification time and TITLE may be nil.  A non-nil TITLE is immutable
;; (a session's first user message never changes) so it is reused regardless of
;; MTIME and without a `stat'.  A nil TITLE (screenshot-only / still-initializing
;; session) is a negative result trusted only while MTIME is unchanged, so such a
;; file is re-opened just once, after it grows -- not on every cold start.

(defun agent-hub--legacy-title-cache-ensure ()
  "Return the legacy title cache, loading it from disk on first use.
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

(defun agent-hub--legacy-title-cache-save ()
  "Write the legacy title cache when it has new entries."
  (when (and agent-hub--title-cache-dirty agent-hub--title-cache)
    (ignore-errors
      (let (alist)
        (maphash (lambda (k v) (push (cons k v) alist)) agent-hub--title-cache)
        (with-temp-file agent-hub-title-cache-file
          (let ((print-length nil) (print-level nil))
            (prin1 alist (current-buffer)))))
      (setq agent-hub--title-cache-dirty nil))))

(defun agent-hub--fetch-remote-title (file mtime)
  "Asynchronously extract and cache the title of remote session FILE.
Reads a bounded head of FILE on the remote host via `head -c', parses it with
`agent-hub--parse-session-title', and stores the result in the persistent
title cache keyed by FILE.  MTIME lets a nil result be retried only after the
file grows.  Schedules a repaint when a title is found."
  (let ((key (format "title:%s" file)))
    (unless (gethash key agent-hub--inflight)
      (puthash key t agent-hub--inflight)
      (condition-case _err
          (funcall
           agent-hub--async-runner key (file-name-directory file)
           "head" (list "-c" (number-to-string agent-hub--session-title-window)
                        (file-local-name file))
           (lambda (ok out)
             (remhash key agent-hub--inflight)
             (let ((title (and ok
                               (condition-case nil
                                   (agent-hub--parse-session-title (or out ""))
                                 (error nil))))
                   (cache (agent-hub--title-cache-ensure)))
               (puthash file (cons (and mtime (float-time mtime)) title) cache)
               (setq agent-hub--title-cache-dirty t)
               (when title (agent-hub--schedule-rerender)))))
        (error (remhash key agent-hub--inflight))))))

(defun agent-hub--legacy-session-title-cached (file &optional mtime)
  "Return a display title for legacy session FILE, cached persistently.
MTIME is FILE's modification time when the caller already knows it (from the
directory listing), avoiding a `stat'.  A cached non-nil title is reused with
no I/O.  A local FILE is read synchronously on a miss (backed by the persistent
cache); a remote FILE is fetched asynchronously and returns nil until it
arrives, so rendering never blocks on the network."
  (let* ((cache (agent-hub--title-cache-ensure))
         (cached (gethash file cache)))
    (cond
     ((and cached (cdr cached)) (cdr cached)) ; immutable title: reuse, no I/O
     ((file-remote-p file)
      ;; Never read a remote file inline.  Enqueue an async fetch unless a
      ;; negative result is already cached at this MTIME (so a title-less file
      ;; is not re-fetched on every repaint).  With no MTIME (e.g. a live
      ;; session) just read the cache; the session-list path fetches it.
      (let ((key (and mtime (float-time mtime))))
        (when (and mtime (not (and cached (equal (car cached) key))))
          (agent-hub--fetch-remote-title file mtime))
        nil))
     (t
      (let* ((mtime (or mtime (file-attribute-modification-time
                               (file-attributes file))))
             (key (and mtime (float-time mtime))))
        (if (and cached key (equal (car cached) key))
            nil                         ; known title-less at this MTIME
          (let ((title (agent-hub--session-title file)))
            (when key
              (puthash file (cons key title) cache)
              (setq agent-hub--title-cache-dirty t))
            title)))))))

;;;; Provider-neutral agent-shell transcript metadata

(defconst agent-hub--transcript-read-window 1048576
  "Bytes read from each end of a large transcript metadata file.")

(defun agent-hub--transcript-dir-for-cwd (cwd)
  "Return CWD's agent-shell transcript directory without creating it."
  (expand-file-name ".agent-shell/transcripts/" (file-name-as-directory cwd)))

(defun agent-hub--normalize-agent (agent)
  "Normalize transcript display name AGENT to an agent-shell identifier."
  (pcase (downcase (string-trim (or agent "")))
    ((or "claude" "claude code" "claude-code") 'claude-code)
    ((or "codex" "openai codex") 'codex)
    ((or "pi" "pi coding agent") 'pi)
    ;; Unknown labels remain strings: transcript content is untrusted and
    ;; interning arbitrary values would leak symbols into the global obarray.
    (_ (and agent (not (string-empty-p (string-trim agent)))
            (downcase (string-trim agent))))))

(defun agent-hub--parse-transcript-time (text)
  "Parse agent-shell timestamp TEXT, returning nil on malformed input."
  (ignore-errors (date-to-time text)))

(defun agent-hub--collapse-title (text)
  "Collapse transcript body TEXT into a safe, short display title."
  (when text
    (let ((title (string-trim
                  (replace-regexp-in-string "[ \t\n\r]+" " "
                                            (substring-no-properties text)))))
      (unless (string-empty-p title)
        (if (> (length title) 60)
            (concat (substring title 0 57) "...")
          title)))))

(defun agent-hub--parse-transcript-metadata (text &optional file fallback-cwd)
  "Parse agent-shell Markdown transcript TEXT into a session plist.
FILE is retained as identity.  FALLBACK-CWD is used when the header omits a
working directory.  Only top-level User and Agent sections count as messages."
  (with-temp-buffer
    ;; Normalize CRLF first so generated section regexps are identical across
    ;; local, remote, and Windows-authored transcript files.
    (insert (replace-regexp-in-string "\r\n" "\n" (or text "")))
    (goto-char (point-min))
    (let ((header-end (or (save-excursion
                            (when (re-search-forward "^---[ \t]*$" nil t)
                              (line-beginning-position)))
                          (point-max)))
          raw-agent started cwd id first-user last-message)
      (cl-labels ((header (name)
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward
                             (format "^\\*\\*%s:\\*\\*[ \t]*\\(.+?\\)[ \t]*$"
                                     (regexp-quote name))
                             header-end t)
                        (string-trim (match-string-no-properties 1))))))
        (setq raw-agent (header "Agent")
              started (agent-hub--parse-transcript-time (header "Started"))
              cwd (or (header "Working Directory") fallback-cwd)
              id (header "Session ID")))
      (goto-char (min (point-max) (1+ header-end)))
      (let (section-start section-kind)
        (while (re-search-forward
                "^\\(## \\(User\\|Agent\\|Agent's Thoughts\\) (\\([^\n)]+\\))\\|### Tool Call \\[[^]\n]+\\]\\)[ \t]*$"
                nil t)
          (when (and (eq section-kind 'user) (not first-user) section-start)
            (setq first-user
                  (buffer-substring-no-properties section-start
                                                  (match-beginning 0))))
          (let ((kind (match-string 2)))
            (setq section-kind
                  (cond ((equal kind "User") 'user)
                        ((equal kind "Agent") 'agent)
                        (t 'other))
                  section-start (line-beginning-position 2))
            (when (member kind '("User" "Agent"))
              (let ((parsed (agent-hub--parse-transcript-time (match-string 3))))
                (when parsed (setq last-message parsed))))))
        (when (and (eq section-kind 'user) (not first-user) section-start)
          (setq first-user (buffer-substring-no-properties section-start (point-max)))))
      (let ((effective (or last-message started)))
        (list :id (and id (not (string-empty-p id)) id)
              :agent (agent-hub--normalize-agent raw-agent)
              :agent-label raw-agent
              :file file
              :cwd (and cwd
                        (let* ((fallback-prefix (and fallback-cwd
                                                     (file-remote-p fallback-cwd)))
                               (path (if (and fallback-prefix
                                              (not (file-remote-p cwd)))
                                         (concat fallback-prefix cwd)
                                       cwd)))
                          (directory-file-name (expand-file-name path))))
              :started-at started
              :last-message-at last-message
              :time effective
              :title (agent-hub--collapse-title first-user))))))

(defun agent-hub--parse-find-transcripts (text cwd remote-prefix)
  "Parse remote transcript listing TEXT for CWD and REMOTE-PREFIX.
Lines have the form EPOCH SIZE PATH."
  (delq nil
        (mapcar
         (lambda (line)
           (let ((line (string-trim-right line "\r+")))
             (when (string-match "\\`\\([0-9.]+\\) \\([0-9]+\\) \\(.+\\)\\'" line)
               (list :file (concat remote-prefix (match-string 3 line))
                     :mtime (seconds-to-time (string-to-number (match-string 1 line)))
                     :size (string-to-number (match-string 2 line))
                     :cwd cwd))))
         (split-string (or text "") "\n" t))))

(defun agent-hub--transcript-signature (mtime size)
  "Return a cache signature for transcript MTIME and SIZE."
  (list agent-hub--transcript-parser-version
        (and mtime (float-time mtime)) size))

(defun agent-hub--title-cache-ensure ()
  "Return the transcript metadata cache, loading safe plist entries once."
  (or agent-hub--title-cache
      (setq agent-hub--title-cache
            (let ((table (make-hash-table :test #'equal)))
              (ignore-errors
                (when (file-readable-p agent-hub-title-cache-file)
                  (with-temp-buffer
                    (insert-file-contents agent-hub-title-cache-file)
                    (dolist (cell (read (current-buffer)))
                      (when (and (consp cell) (stringp (car cell))
                                 (listp (cdr cell))
                                 (plist-member (cdr cell) :signature)
                                 (plist-member (cdr cell) :metadata))
                        (puthash (car cell) (cdr cell) table))))))
              table))))

(defun agent-hub--title-cache-save ()
  "Persist changed transcript metadata cache entries."
  (when (and agent-hub--title-cache-dirty agent-hub--title-cache)
    (ignore-errors
      (let (alist)
        (maphash (lambda (file entry) (push (cons file entry) alist))
                 agent-hub--title-cache)
        (make-directory (file-name-directory agent-hub-title-cache-file) t)
        (with-temp-file agent-hub-title-cache-file
          (let ((print-length nil) (print-level nil))
            (prin1 alist (current-buffer)))))
      (setq agent-hub--title-cache-dirty nil))))

(defun agent-hub--read-transcript-window (file size)
  "Read bounded metadata content from local transcript FILE of SIZE bytes."
  (with-temp-buffer
    (if (<= size (* 2 agent-hub--transcript-read-window))
        (insert-file-contents file)
      (insert-file-contents file nil 0 agent-hub--transcript-read-window)
      (insert "\n")
      (insert-file-contents file nil (- size agent-hub--transcript-read-window) size))
    (buffer-string)))

(defun agent-hub--cache-transcript-metadata (file signature metadata)
  "Store METADATA for FILE and SIGNATURE, returning METADATA."
  (puthash file (list :signature signature :metadata metadata)
           (agent-hub--title-cache-ensure))
  (setq agent-hub--title-cache-dirty t)
  metadata)

(defun agent-hub--fetch-transcript-metadata (entry signature)
  "Asynchronously parse local or remote transcript ENTRY for SIGNATURE."
  (let* ((file (plist-get entry :file))
         (key (format "transcript:%s" file))
         (local (file-local-name file))
         (window (number-to-string agent-hub--transcript-read-window)))
    (unless (gethash key agent-hub--inflight)
      (puthash key t agent-hub--inflight)
      (condition-case nil
          (funcall
           agent-hub--async-runner key (file-name-directory file) "sh"
           (list "-c"
                 (format "s=$(wc -c < %s); if [ $s -le %d ]; then cat %s; else head -c %s %s; printf '\\n'; tail -c %s %s; fi"
                         (shell-quote-argument local)
                         (* 2 agent-hub--transcript-read-window)
                         (shell-quote-argument local) window
                         (shell-quote-argument local) window
                         (shell-quote-argument local)))
           (lambda (ok out)
             (remhash key agent-hub--inflight)
             (if ok
                 (let ((metadata (agent-hub--parse-transcript-metadata
                                  out file (plist-get entry :cwd))))
                   (agent-hub--cache-transcript-metadata file signature metadata)
                   (agent-hub--schedule-rerender))
               ;; Negative-cache this exact signature.  Permission or transport
               ;; failures are retried only after the file changes.
               (agent-hub--cache-transcript-metadata
                file signature
                (list :file file :cwd (plist-get entry :cwd)
                      :time (or (plist-get entry :mtime)
                                (plist-get entry :time)))))))
        (error (remhash key agent-hub--inflight))))))

(defun agent-hub--transcript-metadata-cached (entry)
  "Return parsed metadata for transcript listing ENTRY.
Remote misses are fetched asynchronously and return a minimal placeholder."
  (let* ((file (plist-get entry :file))
         (mtime (or (plist-get entry :mtime) (plist-get entry :time)))
         (size (plist-get entry :size))
         (signature (agent-hub--transcript-signature mtime size))
         (cached (gethash file (agent-hub--title-cache-ensure))))
    (cond
     ((equal signature (plist-get cached :signature))
      (copy-sequence (plist-get cached :metadata)))
     (t
      ;; A cold cache miss never opens transcript content on the render path,
      ;; even for local workspaces.  The async runner reads a bounded head/tail;
      ;; synchronous test runners may populate the cache before this returns.
      (agent-hub--fetch-transcript-metadata entry signature)
      (or (let ((fresh (gethash file (agent-hub--title-cache-ensure))))
            (and (equal signature (plist-get fresh :signature))
                 (copy-sequence (plist-get fresh :metadata))))
          (list :file file :cwd (plist-get entry :cwd) :time mtime))))))

(defun agent-hub--session-files-for-cwd (cwd)
  "Return transcript listing entries for CWD without blocking remote renders."
  (if (file-remote-p cwd)
      (copy-sequence
       (agent-hub--swr
        (format "transcript-files:%s" cwd) cwd
        (lambda (text)
          (agent-hub--parse-find-transcripts text cwd (or (file-remote-p cwd) "")))
        "sh" "-c"
        (format "d=%s; [ -d \"$d\" ] && find \"$d\" -maxdepth 1 -type f -name '*.md' -printf '%%T@ %%s %%p\\n'"
                (shell-quote-argument
                 (file-local-name (directory-file-name
                                   (agent-hub--transcript-dir-for-cwd cwd)))))))
    (condition-case nil
        (let ((dir (agent-hub--transcript-dir-for-cwd cwd)))
          (when (file-directory-p dir)
            (mapcar (lambda (pair)
                      (let ((attrs (cdr pair)))
                        (list :file (car pair)
                              :mtime (file-attribute-modification-time attrs)
                              :time (file-attribute-modification-time attrs)
                              :size (file-attribute-size attrs)
                              :cwd cwd)))
                    (directory-files-and-attributes dir t "\\.md\\'" t))))
      (error nil))))

(defun agent-hub--session-files (root)
  "Return normalized transcript sessions for ROOT and its managed worktrees."
  (let ((seen (make-hash-table :test #'equal)) sessions)
    (dolist (cwd (agent-hub--session-cwds root))
      (dolist (entry (agent-hub--session-files-for-cwd cwd))
        (let ((file (plist-get entry :file)))
          (puthash file t seen)
          (let ((session (agent-hub--transcript-metadata-cached entry)))
            (setq session (plist-put session :root root))
            (unless (plist-get session :cwd)
              (setq session (plist-put session :cwd cwd)))
            (push session sessions)))))
    (setq sessions (agent-hub--aggregate-sessions sessions))
    (let ((cache (agent-hub--title-cache-ensure)) stale)
      (maphash (lambda (file _entry)
                 (when (and (agent-hub--path-prefix-p root file)
                            (not (gethash file seen)))
                   (push file stale)))
               cache)
      (dolist (file stale)
        (remhash file cache)
        (setq agent-hub--title-cache-dirty t)))
    (seq-sort #'agent-hub--recent-session< sessions)))

(defun agent-hub--session-title-cached (file &optional _mtime)
  "Return cached transcript title for FILE, for compatibility."
  (plist-get (plist-get (gethash file (agent-hub--title-cache-ensure)) :metadata)
             :title))

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

(defun agent-hub--session-key (session)
  "Return provider-aware identity key for SESSION.
An exact transcript path is strongest; otherwise agent and id form the key.
Rows without ids remain distinct."
  (or (and (plist-get session :file)
           (concat "file:" (plist-get session :file)))
      (and (plist-get session :id)
           (format "id:%s:%s" (or (plist-get session :agent) 'unknown)
                   (plist-get session :id)))
      (format "row:%s:%s:%s"
              (or (plist-get session :agent) 'unknown)
              (or (plist-get session :cwd) "")
              (or (plist-get session :title) ""))))

(defun agent-hub--live-session-map (buffers)
  "Return a hash mapping provider-aware session keys to live buffers."
  (let ((map (make-hash-table :test 'equal)))
    (dolist (buf buffers map)
      (let* ((session (agent-hub--live-buffer-session buf))
             (id (plist-get session :id)))
        (when id
          (puthash (format "id:%s:%s"
                           (or (plist-get session :agent) 'unknown) id)
                   buf map))
        (when-let ((file (plist-get session :file)))
          (puthash (concat "file:" file) buf map))))))

(defun agent-hub--live-buffer-session (buffer)
  "Return provider-neutral session metadata for live agent-shell BUFFER.
No remote file I/O is performed.  The transcript path is used only as exact
identity and cached metadata may enrich the row."
  (let* ((id (ignore-errors (agent-hub--buffer-session-id buffer)))
         (cwd (agent-hub--buffer-cwd buffer))
         (agent (agent-hub--normalize-agent
                 (format "%s" (or (agent-hub--buffer-agent-id buffer) ""))))
         (file (with-current-buffer buffer
                 (and (boundp 'agent-shell--transcript-file)
                      agent-shell--transcript-file)))
         (cached (and file
                      (plist-get (gethash file (agent-hub--title-cache-ensure))
                                 :metadata))))
    (append (list :id id :agent agent :file file :cwd cwd :buffer buffer
                  :title (or (plist-get cached :title) (buffer-name buffer))
                  :time (plist-get cached :time)
                  :started-at (plist-get cached :started-at)
                  :last-message-at (plist-get cached :last-message-at))
            nil)))

(defun agent-hub--persisted-session (root entry &optional buffer)
  "Return normalized transcript metadata ENTRY for ROOT with optional BUFFER."
  (let ((session (copy-sequence entry)))
    (setq session (plist-put session :root root))
    (when buffer (setq session (plist-put session :buffer buffer)))
    session))

(defun agent-hub--session-sort-key (session)
  "Return a stable fallback sort key for SESSION."
  (format "%s\0%s\0%s\0%s"
          (or (plist-get session :root) "")
          (or (plist-get session :agent) 'unknown)
          (or (plist-get session :id) "")
          (or (plist-get session :file) "")))

(defun agent-hub--recent-session< (a b)
  "Return non-nil when session A should appear before B.
Sessions without timestamps are live initializing sessions and sort first;
otherwise effective last-message/start time sorts newest first."
  (let ((ta (or (plist-get a :last-message-at)
                (plist-get a :started-at) (plist-get a :time)))
        (tb (or (plist-get b :last-message-at)
                (plist-get b :started-at) (plist-get b :time))))
    (cond
     ((and (null ta) tb) t)
     ((and ta (null tb)) nil)
     ((and ta tb (not (equal ta tb))) (time-less-p tb ta))
     (t (string-lessp (agent-hub--session-sort-key a)
                      (agent-hub--session-sort-key b))))))

(defun agent-hub--session-files-list (session)
  "Return SESSION's transcript files as a list.
Uses the aggregated :files when present, else the single :file."
  (or (plist-get session :files)
      (and (plist-get session :file) (list (plist-get session :file)))))

(defun agent-hub--session-with-files (session)
  "Return SESSION guaranteeing a :files list derived from :file."
  (if (plist-member session :files)
      session
    (plist-put (copy-sequence session) :files (agent-hub--session-files-list session))))

(defun agent-hub--aggregate-key (session)
  "Return the identity used to collapse transcripts of the same session.
Rows sharing a provider and non-empty session id aggregate; rows without an
id stay distinct so unrelated transcripts never merge."
  (if-let ((id (plist-get session :id)))
      (format "agg:%s:%s" (or (plist-get session :agent) 'unknown) id)
    (concat "file:" (or (plist-get session :file) (format "%S" session)))))

(defun agent-hub--merge-session-pair (a b)
  "Merge sessions A and B that share an identity into one aggregated row.
Sort time and the representative :file follow the newest transcript; :title and
:started-at follow the earliest transcript; :files collects every transcript."
  (let* ((a (agent-hub--session-with-files a))
         (b (agent-hub--session-with-files b))
         ;; `agent-hub--recent-session<' sorts newest first, so A precedes B
         ;; when A is the newer transcript.
         (newest (if (agent-hub--recent-session< a b) a b))
         (a-start (or (plist-get a :started-at) (plist-get a :time)))
         (b-start (or (plist-get b :started-at) (plist-get b :time)))
         (earliest (cond ((null a-start) b)
                         ((null b-start) a)
                         ((time-less-p a-start b-start) a)
                         (t b)))
         (files (delete-dups (append (plist-get a :files) (plist-get b :files))))
         (result (copy-sequence newest)))
    (setq result (plist-put result :files files))
    (setq result (plist-put result :title (plist-get earliest :title)))
    (setq result (plist-put result :started-at
                            (or (plist-get earliest :started-at)
                                (plist-get earliest :time))))
    (unless (plist-get result :buffer)
      (setq result (plist-put result :buffer
                              (or (plist-get a :buffer) (plist-get b :buffer)))))
    result))

(defun agent-hub--aggregate-sessions (sessions)
  "Collapse SESSIONS sharing (provider, session-id) into one row each.
Resuming a session mints a fresh transcript, so a single provider session can
own several transcript files; this presents them as one dashboard row."
  (let ((by-key (make-hash-table :test #'equal))
        order)
    (dolist (session sessions)
      (let* ((key (agent-hub--aggregate-key session))
             (existing (gethash key by-key)))
        (if existing
            (puthash key (agent-hub--merge-session-pair existing session) by-key)
          (puthash key (agent-hub--session-with-files session) by-key)
          (push key order))))
    (nreverse (mapcar (lambda (k) (gethash k by-key)) order))))

(defun agent-hub--merge-recent-sessions (persisted live &optional now)
  "Merge PERSISTED and LIVE session plists for the recent section.
Session ids are deduplicated, with transcript metadata retained and the live
buffer attached.  Live sessions bypass age and count limits.  NOW defaults to
`current-time' and is injectable for deterministic tests."
  (let ((by-key (make-hash-table :test #'equal))
        merged)
    (dolist (session persisted)
      (let ((key (agent-hub--session-key session)))
        (if-let ((existing (gethash key by-key)))
            (when (agent-hub--recent-session< session existing)
              (setq merged (delq existing merged))
              (push session merged)
              (puthash key session by-key))
          (push session merged)
          (puthash key session by-key)
          ;; A live buffer may not yet expose its transcript path.  Retain a
          ;; provider-aware id alias so it can still attach to this row.
          (when (plist-get session :id)
            (puthash (format "id:%s:%s"
                             (or (plist-get session :agent) 'unknown)
                             (plist-get session :id))
                     session by-key)))))
    (dolist (session live)
      (let* ((file-key (and (plist-get session :file)
                            (concat "file:" (plist-get session :file))))
             (id-key (and (plist-get session :id)
                          (format "id:%s:%s"
                                  (or (plist-get session :agent) 'unknown)
                                  (plist-get session :id))))
             (existing (or (and file-key (gethash file-key by-key))
                           (and id-key (gethash id-key by-key)))))
        (if existing
            (let ((updated (plist-put existing :buffer (plist-get session :buffer))))
              (unless (plist-get updated :title)
                (setq updated (plist-put updated :title (plist-get session :title))))
              (setq merged (cons updated (delq existing merged)))
              (puthash (agent-hub--session-key updated) updated by-key))
          (push session merged)
          (puthash (agent-hub--session-key session) session by-key))))
    (let* ((now (or now (current-time)))
           (max-age (and agent-hub-recent-session-max-age-days
                         (* agent-hub-recent-session-max-age-days 86400)))
           (live-sessions (seq-filter (lambda (s) (plist-get s :buffer)) merged))
           (persisted-sessions
            (seq-filter
             (lambda (s)
               (and (not (plist-get s :buffer))
                    (or (not max-age)
                        (not (plist-get s :time))
                        (<= (float-time (time-subtract now (plist-get s :time)))
                            max-age))))
             merged))
           (slots (max 0 (- agent-hub-recent-session-limit
                            (length live-sessions))))
           (selected (append live-sessions
                             (seq-take (seq-sort #'agent-hub--recent-session<
                                                 persisted-sessions)
                                       slots))))
      (seq-sort #'agent-hub--recent-session< selected))))

(defun agent-hub--recent-sessions (workspaces data)
  "Return recent persisted and live sessions across WORKSPACES.
DATA is the live-buffer grouping from `agent-hub--sessions-by-workspace'."
  (let ((persisted
         (mapcan (lambda (root)
                   (mapcar (lambda (entry)
                             (agent-hub--persisted-session root entry))
                           (agent-hub--session-files root)))
                 workspaces))
        live)
    (dolist (cell (plist-get data :grouped))
      (dolist (buffer (cdr cell))
        (let ((session (agent-hub--live-buffer-session buffer)))
          (setq session (plist-put session :root (car cell)))
          (push session live))))
    (dolist (buffer (plist-get data :ungrouped))
      (let ((session (agent-hub--live-buffer-session buffer)))
        (setq session (plist-put session :root nil))
        (push session live)))
    (agent-hub--merge-recent-sessions persisted (nreverse live))))

(defun agent-hub--sessions-by-workspace (workspaces)
  "Group agent-shell buffers under WORKSPACES.
Return a plist (:grouped ALIST :ungrouped LIST) where ALIST maps each root to a
list of buffers, and LIST holds buffers matching no workspace."
  (let ((grouped (mapcar (lambda (r) (cons r nil)) workspaces))
        ungrouped)
    (dolist (buf (agent-hub--agent-buffers))
      (let* ((cwd (agent-hub--buffer-cwd buf))
             ;; Worktree sessions under <repo>/.agent-shell/worktrees/... attach
             ;; to the most specific registered workspace.
             (best (agent-hub--workspace-for-cwd cwd workspaces)))
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
  "Insert an agent-shell SESSION section nested under workspace ROOT.
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

(defun agent-hub--insert-recent-session-line (root session metadata)
  "Insert an aggregated recent SESSION line tagged with workspace ROOT.
Like `agent-hub--insert-session-line' but prefixes the workspace name so the
session's workspace is visible in the top-level Recent Sessions list.  ROOT may
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
         ;; Fixed-width status column: persisted rows get blank padding so the
         ;; workspace/title columns line up with live rows.
         (status (agent-hub--format-status-badge buffer))
         (status (if (string-empty-p status) "   " status))
         (mark (if (agent-hub--session-marked-p session)
                   (concat "  " (propertize "*" 'font-lock-face 'agent-hub-mark) " ")
                 "    ")))
    (magit-insert-section (agent-hub-session (or id buffer))
      (agent-hub--insert-line
       (concat status
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

(defun agent-hub--insert-recent-section (sessions)
  "Insert the top-level aggregated Recent Sessions section from SESSIONS.
Each normalized session carries its workspace in :root.  Branch/diff/PR badges
reuse the same cached helpers as the expanded workspace view; ungrouped live
sessions get no badges."
  (magit-insert-section (agent-hub-recent 'recent)
    (agent-hub--insert-heading
     (propertize (format "Recent Sessions (%d)" (length sessions))
                 'font-lock-face 'agent-hub-workspace)
     'agent-hub-type 'recent-header)
    (if (null sessions)
        (agent-hub--insert-line
         (propertize "    (no recent sessions)" 'font-lock-face 'shadow)
         'agent-hub-type 'info)
      (let ((groups (seq-group-by (lambda (session) (plist-get session :root))
                                  sessions))
            metadata-by-session)
        (dolist (group groups)
          (let* ((root (car group))
                 (group-sessions (cdr group))
                 (cwds (and root
                            (seq-uniq (mapcar (lambda (s) (plist-get s :cwd))
                                              group-sessions)
                                      #'string-equal)))
                 (repo (and root (agent-hub--git-origin-repo root)))
                 (git-info (and root (agent-hub--git-workspace-info root)))
                 (branch-info (and git-info
                                   (agent-hub--visible-branch-info
                                    root repo git-info cwds)))
                 (dirty-info (and git-info
                                  (agent-hub--visible-dirty-info cwds))))
            (dolist (session group-sessions)
              (push (cons session
                          (and git-info
                               (agent-hub--metadata-for-cwd
                                (plist-get session :cwd)
                                git-info branch-info dirty-info)))
                    metadata-by-session))))
        (dolist (session sessions)
          (agent-hub--insert-recent-session-line
           (plist-get session :root) session
           (cdr (assq session metadata-by-session))))))))

(defun agent-hub--render ()
  "Rebuild the dashboard buffer in place, preserving point when possible.
Folding, per-workspace visibility, and point are preserved across refreshes by
`magit-section': workspace sections inherit their predecessor's collapsed state,
and point is restored by section identity."
  (let* ((ident (ignore-errors
                  (magit-section-ident (magit-current-section))))
         (workspaces (agent-hub--workspaces))
         (data (agent-hub--sessions-by-workspace workspaces))
         (recent (agent-hub--recent-sessions workspaces data))
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
      (agent-hub--insert-recent-section recent)
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
                  (let ((key->buffer (agent-hub--live-session-map live-buffers)))
                    (dolist (entry shown)
                      (let* ((cwd (plist-get entry :cwd))
                             (buffer (or (gethash (agent-hub--session-key entry)
                                                  key->buffer)
                                         (and (plist-get entry :id)
                                              (gethash
                                               (format "id:%s:%s"
                                                       (or (plist-get entry :agent)
                                                           'unknown)
                                                       (plist-get entry :id))
                                               key->buffer)))))
                        (agent-hub--insert-session-line
                         root (agent-hub--persisted-session root entry buffer)
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
      ;; section under point lands on its surviving parent (workspace / Recent
      ;; Sessions heading) instead of jumping to the end of the buffer.
      (let ((tail ident))
        (while (and tail (not (magit-get-section tail)))
          (setq tail (cdr tail)))
        (when-let* ((section (and tail (magit-get-section tail))))
          (goto-char (oref section start)))))
    ;; Persist parsed transcript metadata so unchanged files do not need to be
    ;; reopened on the next render or Emacs restart.
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
  "Return the persisted agent-shell transcript session plist at point."
  (or (get-text-property (line-beginning-position) 'agent-hub-session)
      (user-error "No persisted session on this line")))

(defun agent-hub--session-file (session)
  "Return SESSION's representative on-disk file path."
  (or (plist-get session :file)
      (car (agent-hub--session-files-list session))
      (user-error "No on-disk session file for this line")))

(defun agent-hub--session-marked-p (session)
  "Return non-nil when every transcript of SESSION is marked for deletion."
  (when-let* ((files (agent-hub--session-files-list session)))
    (seq-every-p (lambda (file) (member file agent-hub--marked-session-files))
                 files)))

(defun agent-hub--marked-session-data-table ()
  "Return the marked-session metadata table, creating it if needed."
  (or agent-hub--marked-session-data
      (setq agent-hub--marked-session-data (make-hash-table :test #'equal))))

(defun agent-hub--mark-session-file (file &optional session)
  "Mark session FILE for batch deletion.
When SESSION is non-nil, remember it so batch deletion can clean up its
worktree safely."
  (cl-pushnew file agent-hub--marked-session-files :test #'string-equal)
  (when session
    (puthash file session (agent-hub--marked-session-data-table))))

(defun agent-hub--unmark-session-file (file)
  "Remove session FILE from the batch deletion marks."
  (setq agent-hub--marked-session-files
        (cl-remove file agent-hub--marked-session-files :test #'string-equal))
  (when agent-hub--marked-session-data
    (remhash file agent-hub--marked-session-data)))

(defun agent-hub--prune-marks ()
  "Drop marked session files that no longer exist."
  (setq agent-hub--marked-session-files
        (seq-filter (lambda (file) (ignore-errors (file-exists-p file)))
                    agent-hub--marked-session-files))
  (when agent-hub--marked-session-data
    (let (stale)
      (maphash (lambda (file _session)
                 (unless (member file agent-hub--marked-session-files)
                   (push file stale)))
               agent-hub--marked-session-data)
      (dolist (file stale)
        (remhash file agent-hub--marked-session-data))))
  agent-hub--marked-session-files)

;;;; Commands

(defun agent-hub-refresh (&optional force)
  "Re-render the dashboard.
With prefix FORCE, mark cached git and GitHub metadata stale so it is
revalidated asynchronously.  The stale values keep rendering meanwhile, so a
forced refresh never blanks the dashboard back to placeholders."
  (interactive "P")
  ;; Only git/GitHub metadata is time-sensitive.  Session titles come from a
  ;; session's first user message and never change, so the persistent title
  ;; cache is left intact -- clearing it would just re-pay the slow file opens.
  (when force
    (agent-hub--invalidate-all)
    (setq agent-hub--title-cache (make-hash-table :test #'equal)
          agent-hub--title-cache-dirty t))
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

(defun agent-hub--config-for-agent (agent)
  "Return an agent-shell config for normalized AGENT, or signal `user-error'."
  (pcase agent
    ('claude-code
     (unless (and (require 'agent-shell-anthropic nil t)
                  (fboundp 'agent-shell-anthropic-make-claude-code-config))
       (user-error "Claude agent-shell support is unavailable"))
     (agent-shell-anthropic-make-claude-code-config))
    ('codex
     (unless (and (require 'agent-shell-openai nil t)
                  (fboundp 'agent-shell-openai-make-codex-config))
       (user-error "Codex agent-shell support is unavailable"))
     (agent-shell-openai-make-codex-config))
    ('pi
     (unless (and (require 'agent-shell-pi nil t)
                  (fboundp 'agent-shell-pi-make-agent-config))
       (user-error "Pi agent-shell support is unavailable"))
     (agent-shell-pi-make-agent-config))
    (_ (user-error "Cannot resume unsupported transcript agent: %s"
                   (or agent "missing")))))

(defun agent-hub--open-session ()
  "Open or resume the provider recorded by the transcript at point."
  (let* ((bol (line-beginning-position))
         (session (get-text-property bol 'agent-hub-session))
         (buffer (get-text-property bol 'agent-hub-buffer))
         (root (get-text-property bol 'agent-hub-root))
         (id (or (plist-get session :id)
                 (get-text-property bol 'agent-hub-session-id)))
         (agent (plist-get session :agent))
         (cwd (or (plist-get session :cwd) root)))
    (cond
     ((buffer-live-p buffer) (agent-hub--display-buffer buffer))
     ((not id) (user-error "Transcript has no session ID to resume"))
     ((not cwd) (user-error "Transcript has no working directory to resume"))
     (t
      (unless (require 'agent-shell nil t)
        (user-error "agent-shell is unavailable"))
      (unless (fboundp 'agent-shell-start)
        (user-error "agent-shell resume API is unavailable"))
      (let ((default-directory (file-name-as-directory cwd))
            (config (agent-hub--config-for-agent agent)))
        (message "Resuming %s session %s…" agent id)
        (agent-shell-start :config config :session-id id))))))

(defun agent-hub-visit ()
  "Open the session at point, or fold/unfold the workspace at point."
  (interactive)
  (pcase (agent-hub--type-at-point)
    ('session (agent-hub--open-session))
    ((or 'workspace 'ungrouped-header 'recent-header)
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
     "feat/, fix/, chore/, docs/, test/, or refactor/ followed by a slug.\n\n"
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

(defun agent-hub--protected-branch-p (branch default-branch)
  "Return non-nil when BRANCH should never be deleted."
  (or (not (and branch (not (string-empty-p branch))))
      (string-equal branch "main")
      (and default-branch (string-equal branch default-branch))))

(defun agent-hub--agent-worktree-cwd-p (root cwd)
  "Return non-nil when CWD is an agent-hub worktree under ROOT."
  (condition-case nil
      (let* ((worktrees-dir (directory-file-name
                             (expand-file-name (agent-hub--worktrees-dir root))))
             (cwd-dir (directory-file-name (expand-file-name cwd))))
        (and (not (agent-hub--same-directory-p root cwd-dir))
             (string-equal (file-name-as-directory worktrees-dir)
                           (file-name-directory cwd-dir))))
    (error nil)))

(defun agent-hub--worktree-branch-for-cwd (root cwd)
  "Return the checked-out branch for CWD from ROOT's git worktree list."
  (condition-case nil
      (let ((target (file-truename (directory-file-name (expand-file-name cwd)))))
        (cdr (seq-find
              (lambda (cell)
                (string-equal (file-truename (car cell)) target))
              (agent-hub--parse-worktree-branches
               root (agent-hub--git root "worktree" "list" "--porcelain")))))
    (error nil)))

(defun agent-hub--delete-plan (sessions)
  "Return a transcript-only deletion plan for persisted SESSIONS.
Each session may own several transcript files (a provider session gains a fresh
transcript on every resume); all of them are deleted.  Provider-native state and
worktrees are deliberately outside the plan."
  (list :sessions sessions
        :files (delete-dups
                (delq nil (mapcan #'agent-hub--session-files-list
                                  (copy-sequence sessions))))
        :buffers nil
        :cleanups nil
        :skips nil))

(defun agent-hub--counted (count singular &optional plural)
  "Return COUNT with SINGULAR or PLURAL noun."
  (format "%d %s" count (if (= count 1) singular (or plural (concat singular "s")))))

(defun agent-hub--delete-plan-prompt (plan &optional title)
  "Return transcript-only confirmation prompt for PLAN and optional TITLE."
  (let ((sessions (length (plist-get plan :sessions)))
        (files (length (plist-get plan :files))))
    (if (= sessions 1)
        (if (> files 1)
            (format "Delete %s for session %s? "
                    (agent-hub--counted files "transcript") (or title "(untitled)"))
          (format "Delete transcript for session %s? " (or title "(untitled)")))
      (format "Delete %s for %s? "
              (agent-hub--counted files "transcript")
              (agent-hub--counted sessions "marked session")))))

(defun agent-hub--apply-delete-plan (plan)
  "Delete only transcript files in PLAN and return a result plist.
The :deleted count reflects transcript files removed, so an aggregated row can
remove more than one file."
  (let ((deleted 0)
        (cache (agent-hub--title-cache-ensure)))
    (dolist (file (plist-get plan :files))
      (agent-hub--delete-session-file file nil)
      (remhash file cache)
      (setq agent-hub--title-cache-dirty t)
      (agent-hub--unmark-session-file file)
      (setq deleted (1+ deleted)))
    (list :deleted deleted :worktrees-removed 0 :branches-deleted 0
          :branches-kept 0 :cleanup-skipped 0 :cleanup-failures nil)))

(defun agent-hub--delete-result-message (result)
  "Return a summary message for deletion RESULT."
  (string-join
   (delq nil
         (list (format "Deleted %s"
                       (agent-hub--counted (plist-get result :deleted) "session"))
               (when (> (plist-get result :worktrees-removed) 0)
                 (format "removed %s"
                         (agent-hub--counted (plist-get result :worktrees-removed)
                                             "worktree")))
               (when (> (plist-get result :branches-deleted) 0)
                 (format "deleted %s"
                         (agent-hub--counted (plist-get result :branches-deleted)
                                             "branch")))
               (when (> (plist-get result :branches-kept) 0)
                 (format "kept %s"
                         (agent-hub--counted (plist-get result :branches-kept)
                                             "branch")))
               (when (> (plist-get result :cleanup-skipped) 0)
                 (format "skipped %s"
                         (agent-hub--counted (plist-get result :cleanup-skipped)
                                             "worktree cleanup"
                                             "worktree cleanups")))
               (when (plist-get result :cleanup-failures)
                 (format "failed to clean %s"
                         (agent-hub--counted
                          (length (plist-get result :cleanup-failures))
                          "worktree"))))
         )
   "; "))

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

(defun agent-hub--forward-session ()
  "Move point to the next line whose section is a session.
Stay put when there is no following session line (e.g. at the end)."
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (eq (agent-hub--type-at-point) 'session)))
      (forward-line 1))
    (unless (eq (agent-hub--type-at-point) 'session)
      (goto-char start))))

(defun agent-hub-mark-session ()
  "Mark every transcript of the session at point for batch deletion.
Advance point to the next session, dired-style."
  (interactive)
  (let* ((session (agent-hub--session-at-point))
         (files (seq-filter #'file-exists-p
                            (agent-hub--session-files-list session))))
    (unless files
      (user-error "No on-disk session file for this line"))
    (dolist (file files)
      (agent-hub--mark-session-file file session))
    (agent-hub-refresh)
    (agent-hub--forward-session)
    (message "Marked session %s" (or (plist-get session :title)
                                     (plist-get session :id)
                                     (file-name-base (car files))))))

(defun agent-hub-unmark-session ()
  "Unmark every transcript of the session at point.
Advance point to the next session, dired-style."
  (interactive)
  (let* ((session (agent-hub--session-at-point))
         (files (agent-hub--session-files-list session)))
    (dolist (file files)
      (agent-hub--unmark-session-file file))
    (agent-hub-refresh)
    (agent-hub--forward-session)
    (message "Unmarked session %s" (or (plist-get session :title)
                                       (plist-get session :id)
                                       (and files (file-name-base (car files)))))))

(defun agent-hub-unmark-all-sessions ()
  "Clear all batch deletion marks."
  (interactive)
  (let ((count (length agent-hub--marked-session-files)))
    (setq agent-hub--marked-session-files nil
          agent-hub--marked-session-data nil)
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

(defun agent-hub--delete-session-file (file _buffer)
  "Delete persisted transcript FILE only.
The optional historical buffer argument is ignored deliberately: transcript
deletion must not kill a live session or mutate provider-native state."
  (unless (file-exists-p file)
    (user-error "No on-disk transcript file for this line"))
  (delete-file file))

(defun agent-hub--delete-current-session ()
  "Delete every persisted transcript of the session at point."
  (let* ((session (agent-hub--session-at-point))
         (file (agent-hub--session-file session))
         (id (or (plist-get session :id) (file-name-base file)))
         (title (or (plist-get session :title) id "(untitled)")))
    (unless (seq-some #'file-exists-p (agent-hub--session-files-list session))
      (user-error "No on-disk session file for this line"))
    (let ((plan (agent-hub--delete-plan (list session))))
      (when (yes-or-no-p (agent-hub--delete-plan-prompt plan title))
        (let ((result (agent-hub--apply-delete-plan plan)))
          (agent-hub--invalidate-all)
          (agent-hub-refresh)
          (message "%s" (agent-hub--delete-result-message result)))))))

(defun agent-hub--marked-session-for-file (file id->buffer)
  "Return a deletion session plist for marked FILE using ID->BUFFER."
  (let* ((id (file-name-base file))
         (buffer (gethash id id->buffer))
         (session (and agent-hub--marked-session-data
                       (gethash file agent-hub--marked-session-data))))
    (setq session (or session (list :id id :file file)))
    (plist-put session :buffer buffer)))

(defun agent-hub--delete-marked-sessions ()
  "Delete all marked persisted transcripts."
  (let* ((files (agent-hub--prune-marks))
         (id->buffer (agent-hub--live-session-map (agent-hub--agent-buffers)))
         (sessions (mapcar (lambda (file)
                             (agent-hub--marked-session-for-file file id->buffer))
                           files))
         (plan (agent-hub--delete-plan sessions)))
    (unless files
      (user-error "No marked sessions"))
    (when (yes-or-no-p (agent-hub--delete-plan-prompt plan))
      (let ((result (agent-hub--apply-delete-plan plan)))
        (agent-hub--invalidate-all)
        (agent-hub-refresh)
        (message "%s" (agent-hub--delete-result-message result))))))

(defun agent-hub-delete-session ()
  "Delete marked transcripts, or the persisted transcript at point.
This never deletes provider-native state, kills a live buffer, or removes an
associated worktree.  Use `agent-hub-kill-session' to close a live buffer."
  (interactive)
  (if (agent-hub--prune-marks)
      (agent-hub--delete-marked-sessions)
    (agent-hub--delete-current-session)))

;;;; Transcript session-id backfill

;; agent-shell writes a transcript's header lazily, on the first append.  For a
;; resumed session the header can be written before `session/load' completes, so
;; `(:session :id)' is still nil and the header omits `**Session ID:**' -- which
;; leaves the transcript un-aggregatable.  agent-shell already knows the id we
;; asked to resume (top-level `:resume-session-id' in its state), so bridge it
;; into `(:session :id)' just while the header is created.  This is a temporary
;; workaround for an upstream ordering issue; it never invents an id (a brand-new
;; session that dies before `session/new' returns genuinely has none).

(defun agent-hub--ensure-transcript-file-advice (orig &rest args)
  "Backfill a resumed session id into `(:session :id)' around ORIG.
ORIG is `agent-shell--ensure-transcript-file'.  Only acts when the id is missing
but a resume id is known, and only for the header-writing call; the temporary
state change is reverted immediately afterward."
  (if (and (boundp 'agent-shell--state)
           (listp agent-shell--state)
           (boundp 'agent-shell--transcript-file)
           agent-shell--transcript-file
           (not (file-exists-p agent-shell--transcript-file))
           (not (map-nested-elt agent-shell--state '(:session :id)))
           (map-elt agent-shell--state :resume-session-id))
      (let ((session (map-elt agent-shell--state :session))
            (resume-id (map-elt agent-shell--state :resume-session-id)))
        ;; Set the id only for the header write, then clear it back to nil so
        ;; live status keeps deriving `initializing' until the real session id
        ;; arrives.  A nil id reads the same as an absent one for that check.
        (unless session
          (setq session (list (cons :id nil)))
          (map-put! agent-shell--state :session session))
        (map-put! session :id resume-id)
        (unwind-protect
            (apply orig args)
          (map-put! (map-elt agent-shell--state :session) :id nil)))
    (apply orig args)))

(defun agent-hub--install-transcript-advice ()
  "Install the resumed-session-id transcript backfill advice, once."
  (when (fboundp 'agent-shell--ensure-transcript-file)
    (advice-add 'agent-shell--ensure-transcript-file :around
                #'agent-hub--ensure-transcript-file-advice)))

(with-eval-after-load 'agent-shell
  (agent-hub--install-transcript-advice))

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
