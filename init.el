(require 'package)
(package-initialize)

(require 'use-package)

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

(use-package magit)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c f" . counsel-git)
         ("C-c j" . counsel-git-grep)))
