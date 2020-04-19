;;; init-evil.el --- Bring vim back -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

(use-package evil
  :ensure t
  :hook (after-init . evil-mode)
  :init
  (setq evil-want-C-u-scroll t)
  :config
  ;; Switch to the new window after splitting
  (setq evil-split-window-below t
        evil-vsplit-window-right t
        evil-ex-complete-emacs-commands nil
        evil-ex-interactive-search-highlight 'selected-window
        evil-disable-insert-state-bindings t
        evil-want-fine-undo t
        evil-want-Y-yank-to-eol t
        evil-want-abbrev-expand-on-insert-exit nil
        evil-symbol-word-search t))

(provide 'init-evil)

;;; init-evil.el ends herel
