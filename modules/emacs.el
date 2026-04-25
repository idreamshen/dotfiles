(require 'package)
(package-initialize)

(require 'use-package)

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
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

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
     (project-dired "Dired")
     (agent-shell-anthropic-start-claude-code "Claude Code" ?c)
     (agent-shell-openai-start-codex "Codex" ?x))))

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
  :hook
  ((go-mode         . eglot-ensure)
    (typescript-mode . eglot-ensure)
    (yaml-mode       . eglot-ensure)
    (dart-mode       . eglot-ensure)
    (c-mode          . eglot-ensure)
    (c++-mode        . eglot-ensure)))

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
    (scroll-bar-mode -1))
  (unless (display-graphic-p)
    (require 'term/xterm)
    (xterm--init-activate-set-selection)))

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
  :custom
  (org-scheduled-past-days 100)
  (org-log-done 'time)
  (org-directory "~/emacs-files/")
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
               '("l" "Life" entry
                 (file+headline "~/emacs-files/life.org" "Life")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n"))
  (add-to-list 'org-capture-templates
	       '("f" "Feedme" entry
                 (file+headline "~/emacs-files/feedme.org" "Feedme")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n"))
  (add-to-list 'org-capture-templates
	       '("k" "Kincony" entry
                 (file+headline "~/emacs-files/kincony.org" "Kincony")
                 "* TODO %?\nSCHEDULED: %^T DEADLINE: %^T\n"))

;; 1. 针对 Agenda 视图下的 't' (修改状态) 操作
  (advice-add 'org-agenda-todo :after
              (lambda (&rest _)
		(org-save-all-org-buffers)))

;; 2. 针对 Agenda 视图下的 Refile (归档/移动) 操作
  (advice-add 'org-agenda-refile :after
              (lambda (&rest _)
		(org-save-all-org-buffers)))

;; 3. 针对普通 Org 文件中修改 TODO 状态 (C-c C-t)
  (advice-add 'org-todo :after
              (lambda (&rest _)
		(org-save-all-org-buffers))))

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
  :custom
  (org-agenda-span 'day)
  (org-agenda-files '("~/emacs-files/"))
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

(use-package agent-shell
  :bind (("C-c s s" . agent-shell-opencode-start-agent)
         ("C-c s c" . agent-shell-anthropic-start-claude-code)
         ("C-c s g" . agent-shell-google-start-gemini)
         ("C-c s x" . agent-shell-openai-start-codex)
         ("C-c s o" . agent-shell-github-start-copilot))
  :init
  (setq agent-shell-openai-default-model-id "gpt-5.5/high"
        agent-shell-openai-default-session-mode-id "full-access"
        agent-shell-anthropic-default-model-id "claude-opus-4-6"
        agent-shell-anthropic-default-session-mode-id "bypassPermissions"
        agent-shell-github-acp-command '("copilot" "--acp" "--allow-all"))
  :custom
  (agent-shell-file-completion-enabled t))

(use-package agent-shell-attention
  :after agent-shell
  :config
  (agent-shell-attention-mode))

(use-package worktree-manager
  :ensure nil
  :load-path (lambda ()
               (list (expand-file-name "lisp" user-emacs-directory)))
  :bind-keymap ("C-c w" . worktree-manager-prefix-map))

(use-package dired
  :ensure nil
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
