;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "chaomai"
      user-mail-address "loneymai@gmail.com")

(defun wsl2-copy (beg end)
  (interactive "r")
  (call-process-region beg end "clip.exe"))

(defun wsl2-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "powershell.exe" nil t nil "-Command" "Get-Clipboard"))

(defun osx-copy (beg end)
  (interactive "r")
  (call-process-region beg end "pbcopy"))

(defun osx-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "pbpaste" nil t nil))

(defun linux-copy (beg end)
  (interactive "r")
  (call-process-region beg end "xclip" nil nil nil "-selection" "c"))

(defun linux-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "xsel" nil t nil "-b"))

(cond
 ((string-equal system-type "darwin")
  (define-key global-map (kbd "C-x C-y") 'osx-copy)
  (define-key global-map (kbd "C-x C-p") 'osx-paste))

 ((string-equal system-type "gnu/linux")
  (define-key global-map (kbd "C-x C-y") 'linux-copy)
  (define-key global-map (kbd "C-x C-p") 'linux-paste))

 ((string-match "microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string)))
  (define-key global-map (kbd "C-x C-y") 'wsl2-copy)
  (define-key global-map (kbd "C-x C-p") 'wsl2-paste)))

(setq-default history-length 1000)

(setq projectile-require-project-root t)
(setq projectile-project-root-files '(".ccls-root" ".idea" "go.mod" ".bzr" "_darcs"
                                      "build.xml" ".project" ".root" ".svn" ".git"
                                      "index.org"))

(setq projectile-project-root-files-functions '(projectile-root-top-down
                                                projectile-root-top-down-recurring
                                                projectile-root-bottom-up
                                                projectile-root-local))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'regular))

;; (setq doom-theme 'doom-one)

(setq doom-theme 'spacemacs-dark
      spacemacs-theme-comment-bg nil
      spacemacs-theme-comment-italic t)

(setq-default line-spacing 5)

;; (setq display-line-numbers-type nil)

;; (use-package! awesome-tab
;;   :config
;;   (awesome-tab-mode t)
;;   (setq awesome-tab-show-tab-index t
;;         awesome-tab-height 120)
;; 
;;   (defun my-select-window ()
;;     (interactive)
;;     (let* ((event last-input-event)
;;            (key (make-vector 1 event))
;;            (key-desc (key-description key)))
;;       (my-select-window-by-number
;;        (string-to-number (car (nreverse (split-string key-desc "-"))))))))
;; 
;; (global-set-key (kbd "M-1") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-2") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-3") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-4") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-5") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-6") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-7") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-8") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-9") 'awesome-tab-select-visible-tab)
;; (global-set-key (kbd "M-0") 'awesome-tab-select-visible-tab)

(use-package! company
  :config
  (setq company-idle-delay 0.1))

(use-package ivy
  :config
  (setq ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-on-del-error-function 'ignore
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

(use-package ivy
  :config
  (setq org-directory "~/org/"
        org-ellipsis " ➤ "
        org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")
        ;; gdt task status
        org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "WAITTING(w)" "SOMEDAY(s)" "|" "DONE(d@/!)" "ABORT(a@/!)")
                            (sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "|" "FIXED(f)"))
        ;; 配置归档文件的名称和 Headline 格式。
        org-archive-location "%s_archive::date-tree"))

;; (defun nolinum ()
;;   (setq display-line-numbers-type nil))
;; (add-hook 'org-mode-hook 'nolinum)

;; (defun org-agenda-time-grid-spacing ()
;;   "Set different line spacing w.r.t. time duration."
;;   (save-excursion
;;     (let* ((background (alist-get 'background-mode (frame-parameters)))
;;            (background-dark-p (string= background "dark"))
;;            (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
;;            pos
;;            duration)
;;       (nconc colors colors)
;;       (goto-char (point-min))
;;       (while (setq pos (next-single-property-change (point) 'duration))
;;         (goto-char pos)
;;         (when (and (not (equal pos (point-at-eol)))
;;                    (setq duration (org-get-at-bol 'duration)))
;;           (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
;;                 (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
;;             (overlay-put ov 'face `(:background ,(car colors)
;;                                                 :foreground
;;                                                 ,(if background-dark-p "black" "white")))
;;             (setq colors (cdr colors))
;;             (overlay-put ov 'line-height line-height)
;;             (overlay-put ov 'line-spacing (1- line-height))))))))

;; (add-hook 'org-agenda-finalize-hook #'org-agenda-time-grid-spacing)

(use-package! org-download
  :after org
  :hook ('dired-mode-hook 'org-download-enable)
  :config
  (defun my-org-download-method (link)
    (let ((filename
           (file-name-nondirectory
            (car (url-path-and-query
                  (url-generic-parse-url link)))))
          (dirname (concat (file-name-sans-extension (buffer-name)) "_media")))
      ;; if directory not exist, create it
      (unless (file-exists-p dirname)
        (make-directory dirname))
      ;; return the path to save the download files
      (expand-file-name filename dirname)))

  (setq org-download-method 'my-org-download-method))

;; (after! format
;;   (set-formatter! 'clang-format
;;     '("clang-format"
;;       "-style={BasedOnStyle: Google, SortIncludes: false}"
;;       ("-assume-filename=%S" (or buffer-file-name mode-result "")))
;;     ))

;; :modes
;; '((c-mode ".c")
;;   (c++-mode ".cpp")
;;   (java-mode ".java")
;;   (objc-mode ".m")
;;   (protobuf-mode ".proto"))))

;; (after! format
;;  (set-formatter!
;;    'black "black -q -" :modes '(python-mode)))

;; (setq lsp-ui-sideline-enable nil
;;       lsp-enable-symbol-highlighting nil)

(use-package! ccls
  :config
  (setq ccls-sem-highlight-method 'font-lock)
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
  (ccls-use-default-rainbow-sem-highlight)

  (setq ccls-executable "~/Documents/workspace/github/ccls/Release/ccls"
        ccls-args '("--log-file=/tmp/ccls-emacs.log")
        ccls-initialization-options `(:capabilities (:foldingRangeProvider :json-false)
                                                    :cache (:directory ".ccls-cache")
                                                    :completion (:caseSensitivity 0)
                                                    :compilationDatabaseDirectory "cmake-build"
                                                    ;; :codeLens (:localVariables :json-false)
                                                    :client (:snippetSupport t)
                                                    :diagnostics (:onChang 100
                                                                           :onOpen 100
                                                                           :onSave 100)
                                                    :highlight (:lsRanges t)
                                                    :index (:threads 5)))
  (evil-set-initial-state 'ccls-tree-mode 'emacs))

(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package! pinyin-search)

(use-package! pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t))

(use-package! evil-nerd-commenter
  :config
  (evilnc-default-hotkeys))
