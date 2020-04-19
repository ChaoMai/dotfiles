;;; init-tools.el --- We all like productive tools -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:

;; Tips for next keystroke
(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-idle-delay 0.5))

;; The blazing grep tool
(use-package rg
  :ensure t
  :defer t)

;; Jump to arbitrary positions
(use-package avy
  :ensure t
  :config
  (setq avy-timeout-seconds 0.2
        avy-all-windows nil
        avy-background t
        avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?q ?w ?e ?r ?u ?i ?o ?p)))

;; ivy core
(use-package ivy
  :ensure t
  :hook (after-init . ivy-mode)
  :preface
  :config
  (setq ivy-display-style 'fancy
        ivy-count-format "%d/%d "
        ;; show recent files
        ivy-use-virtual-buffers t
        ;; dont quit minibuffer when del-error
        ivy-on-del-error-function 'ignore)

  (with-eval-after-load 'evil
    (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)))

;; ;; Fuzzy matcher
;; (use-package counsel
;;   :ensure t
;;   :hook (ivy-mode . counsel-mode)
;;   :bind (([remap evil-ex-registers]  . counsel-evil-registers)
;;          ([remap evil-show-mark]     . counsel-mark-ring)
;;          ([remap recentf-open-files] . counsel-recentf)
;;          ([remap swiper]             . counsel-grep-or-swiper)
;;          ("M-y"                      . counsel-yank-pop))
;;   :preface
;;   (defun my/rename-file (file)
;;     "Rename `FILE'. If the `FILE' is opened, kill the buffer too."
;;     (interactive)
;;     (let* ((new-name (read-string "NewName: "))
;;            (old-dir (file-name-directory file))
;;            (new-file (concat old-dir new-name)))
;;       (rename-file file new-file)
;;       (when-let* ((buf (find-buffer-visiting file)))
;;         (kill-buffer buf)
;;         (find-file new-file))))
;;   (defun my/delete-file (file)
;;     "Delete `FILE'. If the `FILE' is opened, kill the buffer too."
;;     (interactive)
;;     (when (y-or-n-p (format "Really delete '%s'? " file))
;;       (delete-file file)
;;       (when-let* ((buf (find-buffer-visiting file)))
;;         (kill-buffer buf))))
;;   :config
;;   (ivy-set-actions
;;    'counsel-find-file
;;    '(("d" my/delete-file "delete")
;;      ("r" my/rename-file "rename")
;;      ("x" counsel-find-file-as-root "open as root")))
;; 
;;   (setq counsel-preselect-current-file t
;;         counsel-yank-pop-preselect-last t
;;         counsel-yank-pop-separator "\n-----------\n"
;;         counsel-find-file-at-point t
;;         counsel-find-file-ignore-regexp "\\(?:\\`\\(?:\\.\\|__\\)\\|elc\\|pyc$\\)"))

(provide 'init-tools)

;;; init-tools.el ends here
