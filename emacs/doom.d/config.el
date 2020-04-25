;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "chaomai"
      user-mail-address "loneymai@gmail.com")

(defconst MACOS "macos")
(defconst WSL "wsl")
(defconst LINUX "linux")

(cond
 ((string-equal system-type "darwin")
  (defvar platform MACOS))

 ((string-match "microsoft"
                (with-temp-buffer (shell-command "uname -r" t)
                                  (goto-char (point-max))
                                  (delete-char -1)
                                  (buffer-string)))
  (defvar platform WSL))

 ((string-equal system-type "gnu/linux")
  (defvar platform LINUX)))

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
 ((string-equal platform MACOS)
  (define-key global-map (kbd "C-x C-y") 'osx-copy)
  (define-key global-map (kbd "C-x C-p") 'osx-paste))

 ((string-equal platform LINUX)
  (message "no implemented"))

 ((string-equal platform WSL)
  (define-key global-map (kbd "C-x C-y") 'wsl2-copy)
  (define-key global-map (kbd "C-x C-p") 'wsl2-paste)))

(setq-default history-length 1000)

(use-package! projectile
  :config
  (setq projectile-require-project-root t
        projectile-completion-system 'ivy
        projectile-indexing-method 'hybrid
        projectile-project-root-files '(".ccls-root" ".idea" "go.mod" ".bzr" "_darcs"
                                        "build.xml" ".project" ".root" ".svn" ".git"
                                        "index.org" ".projectile")
        projectile-project-root-files-functions '(projectile-root-top-down
                                                  projectile-root-top-down-recurring
                                                  projectile-root-bottom-up
                                                  projectile-root-local)))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(cond
 ((string-equal platform MACOS)
  (setq doom-font (font-spec :family "Fira Code" :size 13 :weight 'regular))
  (setq doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 13 :weight 'regular)))

 ((string-equal platform LINUX)
  (message "no implemented"))

 ((string-equal platform WSL)
  (setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'regular)))
  (setq doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 18 :weight 'regular)))

(setq fancy-splash-image (concat doom-private-dir "doom.jpg"))

(setq doom-theme 'doom-one)

;; (setq doom-theme 'spacemacs-dark
;;       spacemacs-theme-comment-bg nil
;;       spacemacs-theme-comment-italic t)

(setq-default line-spacing 5)

;; (setq display-line-numbers-type nil)

(use-package! ivy
  :defer t
  :config
  (setq ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-on-del-error-function 'ignore
        ivy-re-builders-alist '((swiper . ivy--regex-plus)
                                (t . ivy--regex-fuzzy))))

(use-package! counsel
  :defer t
  :hook (ivy-mode . counsel-mode)
  :bind (([remap evil-ex-registers]  . counsel-evil-registers)
         ([remap evil-show-mark]     . counsel-mark-ring)
         ([remap recentf-open-files] . counsel-recentf)
         ([remap swiper]             . counsel-grep-or-swiper)
         ("M-p" . counsel-projectile-find-file)
         ("M-n" . counsel-projectile-rg)))

(use-package! swiper
  :defer t
  :config
  (setq swiper-action-recenter t))

(cond
 ((string-equal platform MACOS)
  (defvar org_dir "~/Documents/onedrive/Documents/workspace/chaomai.org/"))

 ((string-equal platform LINUX)
  (message "no implemented"))

 ((string-equal platform WSL)
  (defvar org_dir "/mnt/d/maichao/Documents/onedrive/Documents/workspace/chaomai.org/")))

(use-package! org
  :defer t
  :init
  (setq org-directory org_dir)
  :config
  (setq org-agenda-files (list (concat org_dir "work/project.org")
                               (concat org_dir "home/project.org"))
        org-tags-column 0
        org-pretty-entities t
        org-startup-indented t
        org-image-actual-width nil
        org-hide-leading-stars t
        org-hide-emphasis-markers t
        org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-fontify-quote-and-verse-blocks t
        org-catch-invisible-edits 'smart
        org-insert-heading-respect-content t
        ;; block switching the parent to done state
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        ;; org-ellipsis " -> "
        ;; gdt task status
        org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i!)" "WAITTING(w!)" "SOMEDAY(s!)" "|" "DONE(d@/!)" "CANCELLED(a@/!)")
                            (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f!)"))
        ;; log
        org-log-done 'time
        org-log-repeat 'time
        org-log-redeadline 'note
        org-log-reschedule 'note
        org-log-into-drawer t
        org-log-state-notes-insert-after-drawers nil
        ;; refile
        org-refile-use-cache t
        org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm
        ;; 配置归档文件的名称和 Headline 格式。
        org-archive-location "%s_archive::date-tree"))

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
;; 
;; (add-hook 'org-agenda-finalize-hook #'org-agenda-time-grid-spacing)

;; Write codes in org-mode
(use-package! org-src
  :after org
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-preserve-indentation t
        org-src-window-setup 'current-window
        org-confirm-babel-evaluate t
        org-edit-src-content-indentation 0
        org-babel-load-languages '((shell . t)
                                   (python . t)
                                   (ocaml . t)
                                   (emacs-lisp . t))))

(use-package org-clock
  :after org
  :config
  (setq org-clock-in-resume t
        org-clock-idle-time 10
        org-clock-into-drawer t
        org-clock-out-when-done t
        org-clock-persist 'history
        org-clock-history-length 10
        org-clock-out-remove-zero-time-clocks t
        org-clock-report-include-clocking-task t)
  (org-clock-persistence-insinuate))

(use-package! org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")))

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

(use-package! ox-confluence
  :after org)

;; (use-package! format
;;   (set-formatter! 'clang-format
;;     '("clang-format"
;;       "-style={BasedOnStyle: Google, SortIncludes: false}"
;;       ("-assume-filename=%S" (or buffer-file-name mode-result "")))))

;; :modes
;; '((c-mode ".c")
;;   (c++-mode ".cpp")
;;   (java-mode ".java")
;;   (objc-mode ".m")
;;   (protobuf-mode ".proto"))))

;; (after! format
;;  (set-formatter!
;;    'black "black -q -" :modes '(python-mode)))

(use-package! company
  :defer t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-require-match nil
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        ;; complete `abbrev' only in current buffer
        company-dabbrev-other-buffers nil
        ;; make dabbrev case-sensitive
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-backends '(company-capf
                           company-files
                           (company-dabbrev-code company-keywords)
                           company-dabbrev)))

(use-package! lsp-mode
  :defer t
  :config
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-keymap-prefix "C-c l"
        lsp-idle-delay 0.5                 ;; lazy refresh
        lsp-log-io nil                     ;; enable log only for debug
        lsp-enable-folding nil             ;; use `evil-matchit' instead
        lsp-diagnostic-package :flycheck   ;; prefer flycheck
        lsp-lens-auto-enable nil           ;; disable lens
        lsp-flycheck-live-reporting nil    ;; obey `flycheck-check-syntax-automatically'
        lsp-prefer-capf t                  ;; using `company-capf' by default
        lsp-enable-snippet nil             ;; no snippet
        lsp-enable-file-watchers nil       ;; turn off for better performance
        lsp-enable-text-document-color nil ;; as above
        lsp-enable-symbol-highlighting nil ;; as above
        lsp-enable-indentation nil         ;; indent by ourself
        lsp-enable-on-type-formatting nil  ;; disable formatting on the fly
        lsp-auto-guess-root t              ;; auto guess root
        lsp-keep-workspace-alive nil       ;; auto kill lsp server
        lsp-enable-xref t
        lsp-eldoc-enable-hover nil         ;; disable eldoc hover displays in minibuffer, lsp-ui shows it
        lsp-signature-auto-activate t      ;; show function signature
        lsp-signature-doc-lines 2))        ;; but dont take up more lines

(use-package! ccls
  :after lsp-mode
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
  :after ccls
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package! lsp-ui
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-border (face-foreground 'default)

        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t

        lsp-ui-imenu-enable t
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face))))

(use-package! pinyin-search
  :defer t)

(use-package! pangu-spacing
  :defer t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t))

(use-package! evil
  :bind (:map evil-normal-state-map
          ("<backspace>" . evil-ex-nohighlight)
          ("/" . swiper)))

(use-package! evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys))

(use-package! awesome-tab
  :bind (("S-1" . awesome-tab-select-visible-tab)
         ("S-2" . awesome-tab-select-visible-tab)
         ("S-3" . awesome-tab-select-visible-tab)
         ("S-4" . awesome-tab-select-visible-tab)
         ("S-5" . awesome-tab-select-visible-tab)
         ("S-6" . awesome-tab-select-visible-tab)
         ("S-7" . awesome-tab-select-visible-tab)
         ("S-8" . awesome-tab-select-visible-tab)
         ("S-9" . awesome-tab-select-visible-tab)
         ("S-0" . awesome-tab-select-visible-tab))
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-height 125))
