(require 'use-package)

(use-package evil
             :ensure t
             :diminish undo-tree-mode
             :init
             (evil-mode 1)
             (setq sentence-end-double-space nil))
