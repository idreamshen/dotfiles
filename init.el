(require 'package)
(package-initialize)

(require 'use-package)

(use-package magit)

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))
