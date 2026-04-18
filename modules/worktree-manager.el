;;; worktree-manager.el --- Manage git worktrees for agent-shell -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;;; Commentary:
;;
;; Manage create/list/archive lifecycle for project worktrees located under
;; .agent-shell/worktrees, and start Claude Code via agent-shell.
;;

;;; Code:

(require 'agent-shell)
(require 'agent-shell-anthropic)
(require 'cl-lib)
(require 'project)
(require 'seq)
(require 'subr-x)

(defgroup worktree-manager nil
  "Manage git worktrees for agent-shell workflows."
  :group 'agent-shell)

(defcustom worktree-manager-worktree-subdir ".agent-shell/worktrees"
  "Relative subdirectory under a repository root for managed worktrees."
  :type 'string
  :group 'worktree-manager)

(defcustom worktree-manager-base-remote "origin"
  "Remote name used when creating new branches."
  :type 'string
  :group 'worktree-manager)

(defcustom worktree-manager-base-branch "main"
  "Remote branch name used when creating new branches."
  :type 'string
  :group 'worktree-manager)

(defcustom worktree-manager-archive-log-file ".agent-shell/worktree-archive.log"
  "Relative path to archive log file under repository root."
  :type 'string
  :group 'worktree-manager)

(declare-function project-known-project-roots "project")
(declare-function project-remember-project "project")

(defun worktree-manager--git-run (repo &rest args)
  "Run git ARGS in REPO and return trimmed output.
Signal `user-error' if command fails."
  (let ((default-directory (file-name-as-directory repo))
        (buffer (generate-new-buffer " *worktree-manager-git*"))
        output)
    (unwind-protect
        (let ((exit-code (apply #'process-file "git" nil buffer nil args)))
          (setq output
                (with-current-buffer buffer
                  (string-trim (buffer-string))))
          (unless (zerop exit-code)
            (user-error "git %s failed in %s: %s"
                        (mapconcat #'identity args " ")
                        repo
                        (if (string-empty-p output)
                            "no output"
                          output))))
      (kill-buffer buffer))
    output))

(defun worktree-manager--git-lines (repo &rest args)
  "Run git ARGS in REPO and return non-empty output lines."
  (let ((output (apply #'worktree-manager--git-run repo args)))
    (if (string-empty-p output)
        nil
      (split-string output "\n" t "[[:space:]]+"))))

(defun worktree-manager--git-raw-lines (repo &rest args)
  "Run git ARGS in REPO and return raw output lines, preserving empty lines."
  (let ((default-directory (file-name-as-directory repo))
        (buffer (generate-new-buffer " *worktree-manager-git-raw*"))
        output)
    (unwind-protect
        (let ((exit-code (apply #'process-file "git" nil buffer nil args)))
          (setq output
                (with-current-buffer buffer
                  (buffer-string)))
          (unless (zerop exit-code)
            (user-error "git %s failed in %s: %s"
                        (mapconcat #'identity args " ")
                        repo
                        (if (string-empty-p (string-trim output))
                            "no output"
                          (string-trim output)))))
      (kill-buffer buffer))
    (split-string output "\n" nil)))

(defun worktree-manager--git-repo-root (directory)
  "Resolve DIRECTORY to its git repository root."
  (condition-case nil
      (string-trim (worktree-manager--git-run directory "rev-parse" "--show-toplevel"))
    (error nil)))

(defun worktree-manager--known-git-projects ()
  "Return known project roots that are valid git repositories."
  (let ((roots (if (fboundp 'project-known-project-roots)
                   (project-known-project-roots)
                 (and (boundp 'project--list) project--list)))
        result)
    (dolist (root roots)
      (when-let ((repo-root (worktree-manager--git-repo-root root)))
        (push repo-root result)))
    (delete-dups (nreverse result))))

(defun worktree-manager--all-git-projects ()
  "Return all candidate git projects for manager views."
  (let ((projects (worktree-manager--known-git-projects)))
    (if-let ((current-repo (worktree-manager--git-repo-root default-directory)))
        (delete-dups (cons current-repo projects))
      projects)))

(defun worktree-manager--remember-project (repo-root)
  "Remember REPO-ROOT in the global project list when possible."
  (when (fboundp 'project-remember-project)
    (when-let ((project (project-current nil repo-root)))
      (project-remember-project project)))
  repo-root)

(defun worktree-manager--select-git-project ()
  "Prompt for a git project root."
  (let* ((projects (worktree-manager--known-git-projects))
         (other-option "[选择其他目录...]")
         (choice (completing-read
                  "选择项目: "
                  (append projects (list other-option))
                  nil
                  t
                  nil
                  nil
                  (car projects))))
    (if (string= choice other-option)
        (let* ((directory (read-directory-name "项目目录: "))
               (repo-root (worktree-manager--git-repo-root directory)))
          (unless repo-root
            (user-error "不是 git 仓库: %s" directory))
          (worktree-manager--remember-project repo-root))
      choice)))

(defun worktree-manager--assert-valid-branch-name (repo branch)
  "Validate BRANCH name using git rules in REPO."
  (worktree-manager--git-run repo "check-ref-format" "--branch" branch)
  branch)

(defun worktree-manager--read-branch (repo)
  "Prompt for branch in REPO and classify how it should be handled.
Return plist with keys :kind and :branch.  Remote branches also include :start-point."
  (let* ((local-branches
          (worktree-manager--git-lines
           repo
           "for-each-ref"
           "--format=%(refname:short)"
           "refs/heads"))
         (remote-branches
          (seq-remove
           (lambda (branch)
             (string= branch (format "%s/HEAD" worktree-manager-base-remote)))
           (worktree-manager--git-lines
            repo
            "for-each-ref"
            "--format=%(refname:short)"
            (format "refs/remotes/%s" worktree-manager-base-remote))))
         (candidates (append local-branches remote-branches))
         (input (string-trim
                 (completing-read "分支（可新建）: " candidates nil nil))))
    (when (string-empty-p input)
      (user-error "分支名不能为空"))
    (cond
     ((member input local-branches)
      (list :kind 'local :branch input))
     ((member input remote-branches)
      (let ((local-name (string-remove-prefix
                         (format "%s/" worktree-manager-base-remote)
                         input)))
        (if (member local-name local-branches)
            (list :kind 'local :branch local-name)
          (list :kind 'remote
                :branch local-name
                :start-point input))))
     (t
      (worktree-manager--assert-valid-branch-name repo input)
      (list :kind 'new :branch input)))))

(defun worktree-manager--branch-slug (branch)
  "Convert BRANCH to a safe worktree directory slug."
  (replace-regexp-in-string "/" "--" branch))

(defun worktree-manager--mainline-branch-p (branch)
  "Return non-nil when BRANCH is a mainline branch."
  (member branch '("main" "master")))

(defun worktree-manager--prepare-mainline-workspace (repo branch-info)
  "Ensure REPO is ready for mainline BRANCH-INFO without creating worktree."
  (let ((branch (plist-get branch-info :branch))
        (branch-kind (plist-get branch-info :kind)))
    (pcase branch-kind
      ('local
       (worktree-manager--git-run repo "switch" branch))
      ('remote
       (worktree-manager--git-run
        repo
        "switch"
        "-c"
        branch
        "--track"
        (plist-get branch-info :start-point)))
      ('new
       (worktree-manager--git-run
        repo
        "fetch"
        worktree-manager-base-remote
        worktree-manager-base-branch)
       (worktree-manager--git-run
        repo
        "switch"
        "-c"
        branch
        (format "%s/%s" worktree-manager-base-remote worktree-manager-base-branch)))
      (_
       (user-error "未知分支类型: %s" branch-kind)))))

(defun worktree-manager--managed-worktree-root (repo)
  "Return absolute managed worktree root directory for REPO."
  (file-name-as-directory
   (expand-file-name worktree-manager-worktree-subdir repo)))

(defun worktree-manager--normalize-path (path)
  "Return canonical PATH for reliable comparisons."
  (file-name-as-directory
   (condition-case nil
       (file-truename path)
     (error (expand-file-name path)))))

(defun worktree-manager--create-worktree (repo branch-info)
  "Create worktree in REPO according to BRANCH-INFO.
Return created worktree absolute path."
  (let* ((branch (plist-get branch-info :branch))
         (branch-kind (plist-get branch-info :kind))
         (worktree-path (expand-file-name
                         (file-name-concat
                          repo
                          worktree-manager-worktree-subdir
                          (worktree-manager--branch-slug branch))))
         (worktree-relative-path
          (file-relative-name worktree-path repo)))
    (if (file-exists-p worktree-path)
        (progn
          (unless (file-directory-p worktree-path)
            (user-error "Worktree 路径已存在但不是目录: %s" worktree-path))
          worktree-path)
      (make-directory (file-name-directory worktree-path) t)
      (pcase branch-kind
        ('local
         (worktree-manager--git-run repo "worktree" "add" worktree-relative-path branch))
        ('remote
         (worktree-manager--git-run
          repo
          "worktree"
          "add"
          "-b"
          branch
          worktree-relative-path
          (plist-get branch-info :start-point)))
        ('new
         (worktree-manager--git-run
          repo
          "fetch"
          worktree-manager-base-remote
          worktree-manager-base-branch)
         (worktree-manager--git-run
          repo
          "worktree"
          "add"
          "-b"
          branch
          worktree-relative-path
          (format "%s/%s" worktree-manager-base-remote worktree-manager-base-branch)))
        (_
         (user-error "未知分支类型: %s" branch-kind)))
      worktree-path)))

(defun worktree-manager--make-claude-config (&optional buffer-name)
  "Build Claude Code config.
When BUFFER-NAME is non-nil, use it as the config's :buffer-name."
  (require 'agent-shell-anthropic nil t)
  (unless (fboundp 'agent-shell-anthropic-make-claude-code-config)
    (user-error "agent-shell Anthropic 配置不可用"))
  (unless (executable-find "claude-agent-acp")
    (user-error "缺少 claude-agent-acp 可执行文件"))
  (let ((config (copy-tree (agent-shell-anthropic-make-claude-code-config))))
    (when (and buffer-name (not (string-empty-p buffer-name)))
      (setf (alist-get :buffer-name config) buffer-name))
    config))

(defun worktree-manager--make-session-buffer-name (target-path)
  "Build a readable buffer name using project and branch from TARGET-PATH."
  (let* ((common-git-dir
          (condition-case nil
              (let ((raw (worktree-manager--git-run target-path "rev-parse" "--git-common-dir")))
                (unless (string-empty-p raw)
                  (directory-file-name
                   (expand-file-name raw (file-name-as-directory target-path)))))
            (error nil)))
         (repo
          (if (and common-git-dir
                   (string= (file-name-nondirectory common-git-dir) ".git"))
              (directory-file-name (file-name-directory common-git-dir))
            (or (worktree-manager--git-repo-root target-path)
                (expand-file-name target-path))))
         (project (file-name-nondirectory (directory-file-name (expand-file-name repo))))
         (branch (condition-case nil
                     (string-trim
                      (worktree-manager--git-run target-path "rev-parse" "--abbrev-ref" "HEAD"))
                   (error "unknown"))))
    (format "Agent @ %s/%s" project branch)))

(defun worktree-manager--find-existing-claude-session (worktree-path)
  "Return an existing Claude Code session buffer for WORKTREE-PATH, or nil."
  (when (fboundp 'agent-shell-project-buffers)
    (let ((default-directory (file-name-as-directory worktree-path)))
      (condition-case nil
          (seq-find
           (lambda (buffer)
             (and (buffer-live-p buffer)
                  (with-current-buffer buffer
                    (derived-mode-p 'agent-shell-mode))
                  (let ((config (ignore-errors (agent-shell-get-config buffer))))
                    (eq (alist-get :identifier config) 'claude-code))))
           (agent-shell-project-buffers))
        (error nil)))))

(defun worktree-manager--display-agent-shell-buffer (buffer)
  "Display existing agent-shell BUFFER and return it."
  (if (fboundp 'agent-shell--display-buffer)
      (agent-shell--display-buffer buffer)
    (switch-to-buffer buffer))
  buffer)

(defun worktree-manager--start-claude-in-worktree (worktree-path)
  "Start Claude Code shell in WORKTREE-PATH."
  (require 'agent-shell nil t)
  (unless (fboundp 'agent-shell-start)
    (user-error "agent-shell 未加载或版本不支持 agent-shell-start"))
  (let* ((default-directory (file-name-as-directory worktree-path))
         (existing-buffer (worktree-manager--find-existing-claude-session worktree-path)))
    (if existing-buffer
        (progn
          (worktree-manager--display-agent-shell-buffer existing-buffer)
          (message "已复用 Claude session: %s" (buffer-name existing-buffer))
          existing-buffer)
      (let* ((buffer-name (worktree-manager--make-session-buffer-name worktree-path))
             (agent-shell-buffer-name-format
              (lambda (agent-name _project-name)
                agent-name)))
        (agent-shell-start :config (worktree-manager--make-claude-config buffer-name))))))

(defun worktree-manager--parse-worktree-list (repo)
  "Parse `git worktree list --porcelain' output for REPO."
  (let ((lines (worktree-manager--git-raw-lines repo "worktree" "list" "--porcelain"))
        entries
        entry)
    (dolist (line (append lines (list "")))
      (if (string-empty-p line)
          (when entry
            (push entry entries)
            (setq entry nil))
        (pcase-let* ((parts (split-string line " " t))
                     (key (car parts))
                     (value (mapconcat #'identity (cdr parts) " ")))
          (pcase key
            ("worktree" (setf (alist-get :path entry nil nil #'eq) value))
            ("HEAD" (setf (alist-get :head entry nil nil #'eq) value))
            ("branch"
             (setf (alist-get :branch-ref entry nil nil #'eq) value)
             (when (string-prefix-p "refs/heads/" value)
               (setf (alist-get :branch entry nil nil #'eq)
                     (string-remove-prefix "refs/heads/" value))))
            ("detached" (setf (alist-get :detached entry nil nil #'eq) t))
            ("bare" (setf (alist-get :bare entry nil nil #'eq) t))
            (_ nil)))))
    (nreverse entries)))

(defun worktree-manager--managed-worktrees (repo)
  "Return managed worktrees under REPO."
  (let ((managed-root
         (worktree-manager--normalize-path
          (worktree-manager--managed-worktree-root repo))))
    (seq-filter
     (lambda (entry)
       (let ((path
              (worktree-manager--normalize-path
               (or (alist-get :path entry) ""))))
         (string-prefix-p managed-root path)))
     (worktree-manager--parse-worktree-list repo))))

(defun worktree-manager--worktree-dirty-p (path)
  "Return non-nil if git worktree at PATH has local changes."
  (not (null (worktree-manager--git-lines path "status" "--porcelain"))))

(defun worktree-manager--worktree-branch-display (entry)
  "Return branch display text for worktree ENTRY."
  (or (alist-get :branch entry)
      (if-let ((head (alist-get :head entry)))
          (format "(detached:%s)" (substring head 0 (min 8 (length head))))
        "(detached)")))

(defun worktree-manager--decorate-worktree-entry (repo entry)
  "Return ENTRY with derived fields for REPO."
  (let* ((repo-name (file-name-nondirectory
                     (directory-file-name
                      (expand-file-name repo))))
         (path (expand-file-name (alist-get :path entry)))
         (relative (file-relative-name path repo))
         (branch (worktree-manager--worktree-branch-display entry))
         (dirty (if (worktree-manager--worktree-dirty-p path) "dirty" "clean"))
         (label (format "%s | %s | %s | %s" repo-name branch relative dirty)))
    (append entry
            (list (cons :repo repo)
                  (cons :repo-name repo-name))
            ;; Keep all original fields from ENTRY first, then derived fields.
            (list (cons :path path)
                  (cons :path-relative relative)
                  (cons :branch-display branch)
                  (cons :dirty dirty)
                  (cons :label label)))))

(defun worktree-manager--managed-worktree-entries (repo)
  "Return decorated managed worktree entries for REPO."
  (mapcar (lambda (entry)
            (worktree-manager--decorate-worktree-entry repo entry))
          (worktree-manager--managed-worktrees repo)))

(defun worktree-manager--all-managed-worktree-entries ()
  "Return decorated managed worktree entries across all known projects."
  (apply #'append
         (mapcar #'worktree-manager--managed-worktree-entries
                 (worktree-manager--all-git-projects))))

(defun worktree-manager--select-managed-worktree (prompt)
  "Select one managed worktree with PROMPT across all projects.
Return decorated entry plist."
  (let* ((entries (worktree-manager--all-managed-worktree-entries)))
    (unless entries
      (user-error "没有可用的活跃 worktree"))
    (let* ((choices (mapcar (lambda (entry)
                              (cons (alist-get :label entry) entry))
                            entries))
           (selected (completing-read prompt choices nil t)))
      (alist-get selected choices nil nil #'string=))))

(defun worktree-manager--path-prefix-p (root path)
  "Return non-nil if PATH is under ROOT."
  (let ((root-dir (worktree-manager--normalize-path root))
        (target (worktree-manager--normalize-path path)))
    (string-prefix-p root-dir target)))

(defun worktree-manager--buffers-under-path (path)
  "Return buffers whose file/default-directory is under PATH."
  (seq-filter
   (lambda (buffer)
     (with-current-buffer buffer
       (let ((buffer-path (or (and (boundp 'default-directory) default-directory)
                              buffer-file-name)))
         (and buffer-path
              (worktree-manager--path-prefix-p path buffer-path)))))
   (buffer-list)))

(defun worktree-manager--kill-buffers-under-path (path)
  "Kill all buffers under PATH and return count."
  (let ((buffers (worktree-manager--buffers-under-path path))
        (count 0))
    (dolist (buffer buffers)
      (when (buffer-live-p buffer)
        (kill-buffer buffer)
        (setq count (1+ count))))
    count))

(defun worktree-manager--append-archive-log (repo entry)
  "Append archive ENTRY record for REPO."
  (let* ((log-path (expand-file-name worktree-manager-archive-log-file repo))
         (line (mapconcat
                #'identity
                (list (or (alist-get :timestamp entry) "")
                      (or (alist-get :repo entry) "")
                      (or (alist-get :branch entry) "")
                      (or (alist-get :worktree-path entry) "")
                      (if (alist-get :worktree-force entry) "true" "false")
                      (if (alist-get :branch-force entry) "true" "false")
                      (or (alist-get :head entry) ""))
                "\t")))
    (make-directory (file-name-directory log-path) t)
    (with-temp-buffer
      (insert line)
      (insert "\n")
      (append-to-file (point-min) (point-max) log-path))))

;;;###autoload
(defun worktree-manager-create ()
  "Create or reuse a managed worktree and start Claude Code in it."
  (interactive)
  (let* ((repo (worktree-manager--select-git-project))
         (branch-info (worktree-manager--read-branch repo))
         (branch (plist-get branch-info :branch)))
    (if (worktree-manager--mainline-branch-p branch)
        (progn
          (worktree-manager--prepare-mainline-workspace repo branch-info)
          (worktree-manager--start-claude-in-worktree repo)
          (message "已进入主工作区: %s (branch: %s)" repo branch))
      (let ((worktree-path (worktree-manager--create-worktree repo branch-info)))
        (worktree-manager--start-claude-in-worktree worktree-path)
        (message "已进入 worktree: %s (branch: %s)" worktree-path branch)))))

;;;###autoload
(defun worktree-manager-list-active-and-enter ()
  "Select active managed worktree and start Claude Code in it."
  (interactive)
  (let* ((entry (worktree-manager--select-managed-worktree "活跃 worktree: "))
         (worktree-path (alist-get :path entry)))
    (unless (file-directory-p worktree-path)
      (user-error "Worktree 目录不存在: %s" worktree-path))
    (worktree-manager--start-claude-in-worktree worktree-path)
    (message "已进入 worktree: %s (%s/%s)"
             worktree-path
             (alist-get :repo-name entry)
             (alist-get :branch-display entry))))

;;;###autoload
(defun worktree-manager-archive ()
  "Archive one managed worktree by removing worktree and deleting local branch."
  (interactive)
  (let* ((entry (worktree-manager--select-managed-worktree "归档 worktree: "))
         (repo (alist-get :repo entry))
         (worktree-path (alist-get :path entry))
         (branch (alist-get :branch entry))
         (head (alist-get :head entry))
         (killed-count (worktree-manager--kill-buffers-under-path worktree-path))
         (worktree-force nil)
         (branch-force nil))
    (when (> killed-count 0)
      (message "已关闭 %d 个相关 buffer" killed-count))
    (condition-case err
        (worktree-manager--git-run repo "worktree" "remove" worktree-path)
      (user-error
       (if (y-or-n-p (format "删除 worktree 失败，是否强制删除？\n%s\n"
                             (error-message-string err)))
           (progn
             (setq worktree-force t)
             (worktree-manager--git-run
              repo
              "worktree"
              "remove"
              "--force"
              worktree-path))
         (signal (car err) (cdr err)))))
    (when (and branch (not (string-empty-p branch)))
      (condition-case err
          (worktree-manager--git-run repo "branch" "-d" branch)
        (user-error
         (if (y-or-n-p (format "删除本地分支失败，是否强制删除分支 %s？\n%s\n"
                               branch
                               (error-message-string err)))
             (progn
               (setq branch-force t)
               (worktree-manager--git-run repo "branch" "-D" branch))
           (signal (car err) (cdr err))))))
    (worktree-manager--append-archive-log
     repo
     (list (cons :timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time) t))
           (cons :repo repo)
           (cons :branch (or branch ""))
           (cons :worktree-path worktree-path)
           (cons :worktree-force worktree-force)
           (cons :branch-force branch-force)
           (cons :head (or head ""))))
    (message "归档完成: %s" worktree-path)))

;;;###autoload
(defvar worktree-manager-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'worktree-manager-create)
    (define-key map (kbd "l") #'worktree-manager-list-active-and-enter)
    (define-key map (kbd "a") #'worktree-manager-archive)
    map)
  "Prefix keymap for worktree-manager commands.")

(provide 'worktree-manager)

;;; worktree-manager.el ends here
