(require 'package)
(package-initialize)

(require 'use-package)

(use-package dracula-theme
  :demand t
  :config
  (load-theme 'dracula t))

(use-package magit)

