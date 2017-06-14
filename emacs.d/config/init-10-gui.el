(require 'use-package)


(use-package base16-theme
             :ensure t
             :config (load-theme 'base16-ocean t))

(set-frame-font "Fira Code 15" nil t)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(if window-system (scroll-bar-mode -1))
; (tool-bar-mode -1)
(menu-bar-mode -1)

(setq line-number-mode 1)

(setq make-backup-files nil)
