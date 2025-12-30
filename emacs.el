(require 'package)
(package-initialize)

(require 'use-package)

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package magit)

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
              ("C-r" . consult-history))
  :custom
  (eshell-history-size 8192)
  (eshell-hist-ignoredups t)
  (eshell-buffer-maximum-lines 4096)
  :config
  (require 'em-hist)
  (add-hook 'eshell-output-filter-functions #'eshell-truncate-buffer)
  (run-at-time t 60 #'eshell-save-some-history))

(use-package eglot
  :ensure nil
  :hook
  ((go-mode         . eglot-ensure)
   (typescript-mode . eglot-ensure)
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
  :bind (("C-s" . consult-line)
         ("C-c j" . consult-ripgrep)
         ("C-x b" . consult-buffer)))
