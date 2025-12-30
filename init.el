(require 'package)
(package-initialize)

(require 'use-package)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package magit)

(use-package ivy
  :demand t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :bind
  (("C-c C-r" . ivy-resume)))

(use-package swiper
  :bind
  (("C-s" . swiper)))

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c j" . counsel-git-grep)))

(use-package vterm)

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
  (magit-gptcommit-mode 1)
  (magit-gptcommit-status-buffer-setup)
  :custom
  (llm-warn-on-nonfree nil))

(use-package magit-gptcommit
  :demand t
  :after llm
  :bind (:map git-commit-mode-map
              ("C-c C-g" . magit-gptcommit-commit-accept))
  :custom
  (magit-gptcommit-llm-provider llm-github-provider))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package project
  :ensure nil  ;; project 是内置包，通常设为 nil。如果你想从 ELPA 获取最新版，可以设为 t
  :bind (("C-c p" . project-switch-project)
         ("C-c e" . project-eshell)
         ("C-c v" . project-vterm)
         ("C-c m" . project-compile))
  :config
  ;; 自定义 project-vterm 函数
  (defun project-vterm ()
    "Open vterm in the current project root."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           ;; (可选) 给 buffer 起个特殊名字，例如 *vterm*<项目名>，防止多个项目混淆
           (vterm-buffer-name (format "*vterm*<%s>" (file-name-nondirectory (directory-file-name default-directory)))))
      (vterm)))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-eshell "Eshell")
     (project-vterm "Vterm" ?v)
     (project-dired "Dired")))

  )
