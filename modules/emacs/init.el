(require 'package)
(package-initialize)

(require 'use-package)

(defvar my/org-directory "~/emacs-files/"
  "Directory containing Org files.")

(defvar my/org-agenda-directories nil
  "Directories scanned for org-agenda; defaults to (list my/org-directory).")

(defvar my/org-fm-directory nil
  "Directory containing fm-emacs-files Org files; defaults to my/org-directory.")

(load (expand-file-name "local.el" user-emacs-directory) t)

(defun my/org-file (file)
  (expand-file-name file my/org-directory))

(defun my/org-fm-file (file)
  (expand-file-name file (or my/org-fm-directory my/org-directory)))

(load (expand-file-name "lisp/dape-config.el" user-emacs-directory))
(load (expand-file-name "lisp/my-agent-shell-config.el" user-emacs-directory))
(load (expand-file-name "lisp/org-task-ai.el" user-emacs-directory))
(load (expand-file-name "lisp/agent-hub.el" user-emacs-directory))
(with-eval-after-load 'agent-hub
  (setq agent-hub-todo-file (my/org-fm-file "feedme.org")))

(use-package emacs
  :ensure nil
  :init
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (set-time-zone-rule "UTC-8")
  :config
  (defun my/save-buffers-kill-terminal-with-double-confirm ()
    (interactive)
    (when (and (yes-or-no-p "Really quit Emacs? "))
      (save-buffers-kill-terminal)))
  (global-set-key (kbd "C-x C-c") #'my/save-buffers-kill-terminal-with-double-confirm)
  ;; GUI frames opened via `emacsclient -c' from a background launcher don't
  ;; always come forward; pull each new server frame into focus.
  (add-hook 'server-after-make-frame-hook
            (lambda () (select-frame-set-input-focus (selected-frame))))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))

(defconst my/remote-exec-path
  '("/home/idreamshen/.nix-profile/bin"
    "/home/idreamshen/.local/state/nix/profiles/profile/bin"
    "/run/current-system/sw/bin"
    "/nix/var/nix/profiles/default/bin"
    "/usr/local/sbin"
    "/usr/local/bin"
    "/usr/sbin"
    "/usr/bin"
    "/sbin"
    "/bin"))

(use-package tramp
  :ensure nil
  :config
  (setq tramp-remote-path
        (append my/remote-exec-path
                '(tramp-own-remote-path tramp-default-remote-path)))
  (add-to-list 'tramp-remote-process-environment
               (concat "PATH=" (mapconcat #'identity my/remote-exec-path ":")))
  (connection-local-set-profile-variables
   'my/remote-exec-path-profile
   `((exec-path . ,(append my/remote-exec-path (list exec-directory)))
     (eshell-path-env-list . ,my/remote-exec-path)
     ;; Spawn a dedicated ssh per `make-process' (direct async) instead of
     ;; multiplexing through TRAMP's shared interactive shell.  The shared
     ;; shell's prompt markers + PTY echo corrupt agent-shell's
     ;; newline-delimited JSON-RPC stdio and hang the ACP client at
     ;; "Initializing..."; a direct async ssh gives the agent a clean pipe.
     (tramp-direct-async-process . t)))
  (dolist (protocol '("ssh" "scp" "sshx" "rsync"))
    (connection-local-set-profiles
     `(:protocol ,protocol)
     'my/remote-exec-path-profile)))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package hl-line
  :ensure nil
  :config
  (global-hl-line-mode 1))

(use-package magit)

(use-package dart-mode
  :mode "\\.dart\\'")

(use-package ediff
  :ensure nil
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package whitespace
  :ensure nil
  :hook ((before-save . whitespace-cleanup)
         (prog-mode . whitespace-mode))
  :custom
  (whitespace-style '(face tabs empty trailing))
  :config
  (add-hook 'go-mode-hook
            (lambda ()
              (setq-local whitespace-style '(face empty trailing)))))

(use-package llm
  :init
  (require 'llm-github)
  :config
  (setopt llm-github-provider
          (make-llm-github :key (auth-info-password
                                 (car (auth-source-search
                                       :host "githubmodel"
                                       :user "apikey")))
                           :chat-model "gpt-4o-mini"))
  :custom
  (llm-warn-on-nonfree nil))

(use-package magit-gptcommit
  :demand t
  :after magit
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider llm-github-provider)
  :config
  (magit-gptcommit-mode 1)
  (magit-gptcommit-status-buffer-setup))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package project
  :ensure nil
  :bind (("C-c p" . project-switch-project)
         ("C-c e" . project-eshell)
         ("C-c m" . project-compile)
         ("C-c f" . project-find-file))

  :custom
  (project-vc-extra-root-markers '(".project"))
  (project-switch-commands
   '((project-find-file "Find file")
     (project-eshell "Eshell")
     (project-dired "Dired"))))

(use-package eshell
  :ensure nil
  :bind (:map eshell-mode-map
              ("C-r" . consult-history))
  :custom
  (eshell-history-size 8192)
  (eshell-hist-ignoredups t)
  (eshell-buffer-maximum-lines 4096)
  :config
  (require 'em-hist)
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)
  (run-at-time t 60 #'eshell-save-some-history)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local company-backends '(company-capf))
              (setq-local company-idle-delay 0.2)
              (setq-local completion-styles '(basic partial-completion)))))

(use-package eglot
  :ensure nil
  :bind (("M-i" . my/eglot-find-implementation))
  :hook
  ((go-mode         . eglot-ensure)
    (typescript-mode . eglot-ensure)
    (yaml-mode       . eglot-ensure)
    (dart-mode       . eglot-ensure)
    (c-mode          . eglot-ensure)
    (c++-mode        . eglot-ensure))
  :config
  (defun my/eglot-find-implementation ()
    "Find implementation, jumping directly when there is only one result."
    (interactive)
    (let ((xref-show-xrefs-function
           (lambda (fetcher alist)
             (let ((xrefs (funcall fetcher)))
               (cond
                ((null xrefs)
                 (message "No implementations found"))
                ((null (cdr xrefs))
                 (xref-pop-to-location
                  (car xrefs)
                  (alist-get 'display-action alist)))
                (t
                 (xref--show-xref-buffer
                  fetcher
                  (cons (cons 'fetched-xrefs xrefs) alist))))))))
      (eglot-find-implementation))))

(use-package window
  :ensure nil
  :bind (("M-o" . other-window)))

(use-package which-key
  :ensure nil
  :config
  (which-key-mode))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package consult
  :custom
  (consult-async-min-input 2)
  (consult-git-grep-args "git --no-pager grep --recurse-submodules --null --color=never --ignore-case   --extended-regexp --line-number -I")
  :bind (("C-s" . consult-line)
         ("C-c j" . consult-git-grep)
         ("C-c i" . consult-imenu)
         ("C-x b" . consult-buffer)))

(use-package envrc
  :hook (after-init . envrc-global-mode))

(use-package typescript-mode
  :mode "\\.ts\\'")

(use-package go-mode
  :custom
  (gofmt-command "goimports")
  :hook
  (go-mode . (lambda ()
               (add-hook 'before-save-hook #'gofmt-before-save nil t))))

(use-package gptel
  :init
  (setq my/gptel-copilot-backend
        (gptel-make-gh-copilot
         "Copilot"
         :host "api.githubcopilot.com"))
  :config
  (setq gptel-backend my/gptel-copilot-backend
        gptel-model 'gpt-5-mini)
  (setq gptel-log-level 'debug)
  (global-set-key (kbd "C-c g") #'gptel)
  (global-set-key (kbd "C-c G") #'gptel-send))


(use-package emacs
  :ensure nil
  :config
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1)))

(use-package clipetty
  :ensure t
  :config
  (global-clipetty-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.1)
  (setq company-tooltip-limit 10)
  (setq company-frontends '(company-pseudo-tooltip-frontend
                            company-echo-metadata-frontend))
  (define-key company-active-map (kbd "M-n") 'company-select-next)
  (define-key company-active-map (kbd "M-p") 'company-select-previous)
)

(use-package undo-tree
  :init
  (let ((undo-tree-history-dir (expand-file-name "undo-tree-history/" user-emacs-directory)))
    (make-directory undo-tree-history-dir t)
    (setq undo-tree-history-directory-alist `(("." . ,undo-tree-history-dir))))
  (global-undo-tree-mode))

(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(closure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2)))

(use-package org
  :ensure nil
  :demand t
  :hook (org-mode . org-indent-mode)
  :init
  (setq org-directory my/org-directory)
  :custom
  (org-scheduled-past-days 100)
  (org-log-done 'time)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
  (org-edit-src-content-indentation 0)
  (org-tags-column 80)
  (org-priority-lowest ?D)
  (org-priority-default ?A)
  :config
  (when (version<= "9.2" (org-version))
    (require 'org-tempo))

  (add-hook 'org-mode-hook
            (lambda ()
              (when (fboundp 'org-table-header-line-mode)
                (org-table-header-line-mode 1))))

  (with-eval-after-load 'org-table
    (set-face-attribute 'org-table-header nil
                        :background "#373844"
                        :foreground "#f8f8f2"
                        :box nil))

  (global-set-key (kbd "C-c c") 'org-capture)

  (setq org-capture-templates nil)
  (add-to-list 'org-capture-templates
               `("l" "Life" entry
                 (file+headline ,(my/org-file "life.org") "Life")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n"))
  (add-to-list 'org-capture-templates
	       `("f" "Feedme" entry
                 (file+headline ,(my/org-fm-file "feedme.org") "Feedme")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n"))
  (add-to-list 'org-capture-templates
	       `("k" "Kincony" entry
                 (file+headline ,(my/org-file "kincony.org") "Kincony")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n:PROPERTIES:\n:ISSUE:\n:PR:\n:WORK_HOURS:\n:SETTLE_DATE:\n:END:\n"))

  (defvar my/org-save-all-org-buffers-timer nil
    "Timer used to debounce saving Org buffers.")

  (defun my/org-save-all-org-buffers-debounced (&rest _)
    (when (timerp my/org-save-all-org-buffers-timer)
      (cancel-timer my/org-save-all-org-buffers-timer))
    (setq my/org-save-all-org-buffers-timer
          (run-with-timer 1.5 nil #'org-save-all-org-buffers)))

;; 1. 针对 Agenda 视图下的 't' (修改状态) 操作
  (advice-add 'org-agenda-todo :after #'my/org-save-all-org-buffers-debounced)

;; 2. 针对 Agenda 视图下的 Refile (归档/移动) 操作
  (advice-add 'org-agenda-refile :after #'my/org-save-all-org-buffers-debounced)

;; 3. 针对普通 Org 文件中修改 TODO 状态 (C-c C-t)
  (advice-add 'org-todo :after #'my/org-save-all-org-buffers-debounced))

(use-package org-habit
  :ensure nil
  :after org
  :custom
  (org-habit-graph-column 40)
  :config
  (add-to-list 'org-modules 'org-habit))

(use-package org-agenda
  :ensure nil
  :after org
  :demand t
  :bind (("C-c a" . org-agenda))
  :init
  (setq org-agenda-files (or my/org-agenda-directories (list my/org-directory)))
  :custom
  (org-agenda-span 'day)
  (org-agenda-tags-column 80)
  (org-agenda-custom-commands
   '(("n" "My Agenda"
      ((tags-todo "+PRIORITY=\"A\"")
       (tags-todo "+PRIORITY=\"B\""))
      ((org-agenda-compact-blocks t))))))

(use-package org-ql
  :config
  (defun my/archive-done-tasks ()
    "使用 org-ql 批量归档"
    (interactive)
    (org-ql-select (org-agenda-files)
      '(and (todo "DONE" "CANCELLED")
            (not (habit))           ; 排除 Habit
            (not (property "STYLE" "habit")))
      :action #'org-archive-subtree)))

(defvar-local my/kincony-task-table--markers nil
  "Markers for tasks shown in the current KinCony task table buffer.")

(defun my/kincony-settle-visible-tasks (settle-date)
  "Set SETTLE_DATE for all tasks currently shown in the KinCony task table."
  (interactive
   (list (read-string "Settle date: " nil nil (format-time-string "%Y-%m-%d"))))
  (unless my/kincony-task-table--markers
    (user-error "No visible unsettled KinCony tasks"))
  (when (yes-or-no-p
         (format "Settle %d visible tasks with date %s? "
                 (length my/kincony-task-table--markers) settle-date))
    (let ((buffers-to-save nil))
      (dolist (marker my/kincony-task-table--markers)
        (let ((buf (marker-buffer marker)))
          (unless (buffer-live-p buf)
            (user-error "Source buffer for a KinCony task is no longer live"))
          (with-current-buffer buf
            (save-excursion
              (goto-char marker)
              (org-entry-put nil "SETTLE_DATE" settle-date))
            (add-to-list 'buffers-to-save buf))))
      (dolist (buf buffers-to-save)
        (with-current-buffer buf
          (save-buffer))))
    (message "Settled %d KinCony tasks" (length my/kincony-task-table--markers))
    (my/kincony-task-table)))

(defun my/kincony-task-table ()
  "从 kincony.org 生成只读任务表格到临时 buffer."
  (interactive)
  (let ((entries '())
        (kincony-file (my/org-file "kincony.org")))
    ;; 解析 kincony.org 中已完成且未结算的 level-2 heading
    (with-current-buffer (find-file-noselect kincony-file)
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (when (= (org-current-level) 2)
            (let* ((heading (org-get-heading t t t t))
                   (state (org-get-todo-state))
                   (scheduled (org-entry-get nil "SCHEDULED"))
                   (closed (org-entry-get nil "CLOSED"))
                   (issue (or (org-entry-get nil "ISSUE") ""))
                   (pr (or (org-entry-get nil "PR") ""))
                   (work-hours (or (org-entry-get nil "WORK_HOURS") ""))
                   (settle-date (or (org-entry-get nil "SETTLE_DATE") ""))
                   (status (cond
                            ((equal state "DONE") "已完成")
                            ((equal state "CANCELED") "已取消")
                            (t "未完成")))
                   (settled (if (and settle-date (not (string-empty-p settle-date)))
                                "是" "否")))
              (when (and (equal state "DONE")
                         (string-empty-p settle-date))
                (push (list heading issue pr
                            (or scheduled "")
                            (or closed "")
                            status work-hours settled settle-date
                            (copy-marker (point-marker)))
                      entries)))))
         nil nil)))
    (setq entries (nreverse entries))
    ;; 创建临时 buffer
    (let ((buf (get-buffer-create "*KinCony Tasks*"))
          (total-work-hours 0))
      (dolist (entry entries)
        (setq total-work-hours (+ total-work-hours (string-to-number (nth 6 entry)))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (setq-local my/kincony-task-table--markers
                      (mapcar (lambda (entry) (nth 9 entry)) entries))
          (insert "| 需求 | Issue | PR | 记录时间 | 完成时间 | 状态 | 工时 | 结算 | 结算日期 |\n")
          (insert "|-\n")
          (dolist (entry entries)
            (insert (format "| %s | %s | %s | %s | %s | %s | %s | %s | %s |\n"
                            (nth 0 entry)   ; 需求
                            (nth 1 entry)   ; Issue
                            (nth 2 entry)   ; PR
                            (nth 3 entry)   ; 记录时间
                            (nth 4 entry)   ; 完成时间
                            (nth 5 entry)   ; 状态
                            (nth 6 entry)   ; 工时
                            (nth 7 entry)   ; 结算
                            (nth 8 entry)   ; 结算日期
                            )))
          (insert "|-\n")
          (insert (format "| 合计 |  |  |  |  |  | %s |  |  |\n"
                          (number-to-string total-work-hours)))
          (org-mode)
          (local-set-key (kbd "s") #'my/kincony-settle-visible-tasks)
          (goto-char (point-min))
          (org-table-align)
          (goto-char (point-min)))
        (read-only-mode 1))
      (switch-to-buffer buf))))

(global-set-key (kbd "C-c k t") #'my/kincony-task-table)

(use-package cal-china
  :ensure nil
  :config
  (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
    (if year
        (let* ((d-date (diary-make-date lunar-month lunar-day year))
               (a-date (calendar-absolute-from-gregorian d-date))
               (c-date (calendar-chinese-from-absolute a-date))
               (cycle (car c-date))
               (yy (cadr c-date))
               (y (+ (* 100 cycle) yy)))
          (diary-chinese-anniversary lunar-month lunar-day y mark))
      (diary-chinese-anniversary lunar-month lunar-day year mark))))

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(use-package git-auto-commit-mode
  :init
  (add-to-list 'safe-local-variable-values '(eval git-auto-commit-mode 1))
  (add-to-list 'safe-local-variable-values '(gac-automatically-push-p . t))
  (add-to-list 'safe-local-variable-values '(gac-debounce-interval . 60)))

(use-package dired
  :ensure nil
  :preface
  (defun my/dired-split-dropped-paths (string)
    "Parse STRING of file paths pasted by iTerm2 on drag-and-drop.
iTerm2 inserts shell-escaped paths (backslash-escaped spaces and
special chars) separated by unescaped whitespace.  Single- and
double-quoted segments are also supported.  Returns a list of paths."
    (let ((chars (string-to-list (string-trim string)))
          (paths '())
          (cur nil)
          (quote nil))
      (while chars
        (let ((c (car chars)))
          (setq chars (cdr chars))
          (cond
           ;; backslash escape: take next char literally
           ((and (eq c ?\\) (null quote))
            (when chars
              (push (car chars) cur)
              (setq chars (cdr chars))))
           ;; toggle quoting
           ((and (memq c '(?\" ?\')) (or (null quote) (eq quote c)))
            (setq quote (if quote nil c)))
           ;; unescaped whitespace separates paths
           ((and (memq c '(?\s ?\t ?\n)) (null quote))
            (when cur
              (push (apply #'string (nreverse cur)) paths)
              (setq cur nil)))
           (t (push c cur)))))
      (when cur
        (push (apply #'string (nreverse cur)) paths))
      (nreverse paths)))

  (defun my/dired-drop-import ()
    "Copy file(s) dragged from Finder into the current dired directory.
Prompts in the minibuffer; drag the file(s) from Finder onto iTerm2 so
their paths are inserted, then press RET.  Each existing source file is
copied into the directory of the current dired buffer."
    (interactive)
    (unless (derived-mode-p 'dired-mode)
      (user-error "Not in a dired buffer"))
    (let* ((dest (dired-current-directory))
           (input (read-string (format "Drop file(s) to copy into %s: " dest)))
           (paths (my/dired-split-dropped-paths input))
           (copied 0))
      (when (null paths)
        (user-error "No file paths given"))
      (dolist (src paths)
        (let ((src (expand-file-name src)))
          (unless (file-exists-p src)
            (user-error "No such file: %s" src))
          (copy-file src
                     (expand-file-name (file-name-nondirectory
                                        (directory-file-name src))
                                       dest)
                     1)
          (setq copied (1+ copied))))
      (revert-buffer)
      (message "Copied %d file%s into %s"
               copied (if (= copied 1) "" "s") dest)))
  :bind (:map dired-mode-map
              ("C-c C-v" . my/dired-drop-import))
  :custom
  (dired-dwim-target t)
  :config
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil
        dired-use-ls-dired nil))

(use-package rime
  :preface
  (defun my/rime-predicate-space-only-line-p ()
    "Return non-nil when point is after indentation only."
    (save-excursion
      (skip-chars-backward " \t")
      (bolp)))

  (defun my/rime-predicate-english-context-p ()
    "Return non-nil when text before point should default to English."
    (let ((prev-char (char-before)))
      (cond
       ((memq prev-char '(?\s ?\t))
        t)
       ((or (null prev-char)
            (= prev-char ?\n))
        nil)
       (t
        (let ((string (buffer-substring-no-properties
                       (max (line-beginning-position) (- (point) 80))
                       (point))))
          (not (string-match-p
                "\\(?:\\cc\\|[，。！？；：（）《》【】、“”‘’「」『』]\\)$"
                string)))))))

  (defun my/rime--ascii-word-bounds ()
    "Return bounds of the ascii word immediately before point."
    (save-excursion
      (let ((end (point)))
        (skip-chars-backward "A-Za-z")
        (when (< (point) end)
          (cons (point) end)))))

  (defun my/rime--activate ()
    "Ensure Rime is the active input method."
    (unless (string= current-input-method default-input-method)
      (activate-input-method default-input-method)))

  (defun my/rime--replay-as-composition (string)
    "Replay STRING into Rime so it becomes an active composition."
    (my/rime--activate)
    (rime-force-enable)
    (when (fboundp 'rime-lib-clear-composition)
      (rime-lib-clear-composition))
    (dolist (char (string-to-list string))
      (let ((result (rime-input-method char)))
        (when result
          (insert (apply #'string result)))))
    (rime--redisplay))

  (defun my/rime-smart-input ()
    "Convert the previous ascii word with Rime, or enable Chinese once."
    (interactive)
    (if (bound-and-true-p rime-active-mode)
        (rime-inline-ascii)
      (if-let ((bounds (my/rime--ascii-word-bounds)))
          (let ((text (buffer-substring-no-properties
                       (car bounds)
                       (cdr bounds))))
            (delete-region (car bounds) (cdr bounds))
            (my/rime--replay-as-composition text))
        (my/rime--activate)
        (rime-force-enable))))

  (defun my/rime-activate-buffer-input-method ()
    "Activate Rime in the current buffer when appropriate."
    (unless (minibufferp)
      (my/rime--activate)))

  (defun my/rime-activate-existing-buffers ()
    "Activate Rime for existing buffers after startup."
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (my/rime-activate-buffer-input-method))))
  :hook
  (after-change-major-mode . my/rime-activate-buffer-input-method)
  (emacs-startup . my/rime-activate-existing-buffers)
  :custom
  (default-input-method "rime")
  (rime-disable-predicates
   '(my/rime-predicate-space-only-line-p
     my/rime-predicate-english-context-p
     rime-predicate-current-uppercase-letter-p))
  :bind
  (:map rime-mode-map
        ("M-j" . my/rime-smart-input)
        :map rime-active-mode-map
        ("M-j" . my/rime-smart-input))
  :config
  (require 'rime-predicates)
  )

(use-package json-mode
  :mode ("\\.json\\'" "\\.jsonl\\'"))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))
