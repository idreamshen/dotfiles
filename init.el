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
  :ensure nil
  :bind (("C-c p" . project-switch-project)
         ("C-c e" . project-eshell)
         ("C-c v" . project-vterm)
         ("C-c m" . project-compile))
  :config
  (defun project-vterm ()
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (vterm-buffer-name (format "*vterm*<%s>" (file-name-nondirectory (directory-file-name default-directory)))))
      (vterm)))
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (project-eshell "Eshell")
     (project-vterm "Vterm" ?v)
     (project-dired "Dired"))))

(use-package eshell
  :ensure nil
  :bind (:map eshell-mode-map
              ("C-r" . counsel-esh-history))
  :custom
  (eshell-history-size 8192)
  (eshell-hist-ignoredups t)
  (eshell-buffer-maximum-lines 4096)
  :config
  (require 'em-hist)
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)
  (run-at-time t 60 #'eshell-save-some-history))
