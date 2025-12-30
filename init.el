(require 'package)
(package-initialize)

(require 'use-package)

(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package magit)

(use-package ivy
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
