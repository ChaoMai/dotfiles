;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "chaomai"
      user-mail-address "loneymai@gmail.com")

(use-package! benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(if (version< emacs-version "28")
    ()
  (setq comp-deferred-compilation t))

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

;; (defun wsl2-copy (beg end)
;;   (interactive "r")
;;   (call-process-region beg end "clip.exe"))

;; (defun wsl2-paste ()
;;   (interactive)
;;   (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
;;   (call-process "powershell.exe" nil t nil "-Command" "Get-Clipboard"))

(defun wsl2-copy (beg end)
  (interactive "r")
  (call-process-region beg end "xclip" "-in" "-selection" "clipboard"))

(defun wsl2-paste ()
  (interactive)
  (if (region-active-p) (delete-region (region-beginning) (region-end)) nil)
  (call-process "xclip" nil t nil "-out" "-selection" "clipboard"))

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
  :defer t
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

(cond
 ((string-equal platform WSL)
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic)))))

(use-package! undohist
  :demand t
  :config
  (setq undohist-directory (concat doom-cache-dir "undohist"))
  (undohist-initialize))

(use-package! saveplace
  :demand t
  :config
  (setq save-place t)
  (save-place-mode 1))

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(cond
 ((string-equal platform MACOS)
  (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'regular)))
  ;; (setq doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 14 :weight 'regular)))

 ((string-equal platform LINUX)
  (message "no implemented"))

 ((string-equal platform WSL)
  (setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'regular))))
  ;; (setq doom-variable-pitch-font (font-spec :family "Sarasa Mono SC" :size 18 :weight 'regular))))

(setq fancy-splash-image (concat doom-private-dir "doom.jpg"))

(use-package! doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(setq-default line-spacing 9)

(use-package! display-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
        (org-mode . display-line-numbers-mode)
  :init
  (setq display-line-numbers-width-start 5))

;; (use-package! highlight-indent-guides
;;   :config
;;   (setq highlight-indent-guides-character ?│))

(use-package! ivy
  :defer t
  :config
  (setq ivy-display-style 'fancy
        ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t
        ivy-on-del-error-function 'ignore)

  (defun eh-ivy-cregexp (str)
    (let ((x (ivy--regex-plus str))
          (case-fold-search nil))
      (if (listp x)
          (mapcar (lambda (y)
                    (if (cdr y)
                        (list (if (equal (car y) "")
                                  ""
                                (pyim-cregexp-build (car y)))
                              (cdr y))
                      (list (pyim-cregexp-build (car y)))))
                  x)
        (pyim-cregexp-build x))))

  (setq ivy-re-builders-alist '((t . eh-ivy-cregexp))))

(use-package! counsel
  :defer t
  :hook (ivy-mode . counsel-mode)
  :bind (("M-p" . counsel-projectile-find-file)
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
  (defvar org_dir "/mnt/d/maichao/OneDrive/Documents/workspace/chaomai.org/")))

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
                            (sequence "REPORT(r!)" "BUG(b!)" "KNOWNCAUSE(k!)" "|" "FIXED(f!)")
                            (sequence "PINNED(p)" "|" "DONE(f@/!)"))
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
        ;; 配置归档文件的名称和 Headline 格式
        org-archive-location "%s_archive::date-tree"))

  ;; (with-eval-after-load 'org
  ;;   (defun org-buffer-face-mode-variable ()
  ;;     (interactive)
  ;;     (make-face 'width-font-face)
  ;;     (set-face-attribute 'width-font-face nil :font "等距更纱黑体 SC 15")
  ;;     (setq buffer-face-mode-face 'width-font-face)

  ;;     (setq doom-font (font-spec :family "Fira Code" :size 18 :weight 'regular))
  ;;     (setq doom-variable-pitch-font (font-spec :family "Noto Sans CJK" :size 18 :weight 'regular))
  ;;     (buffer-face-mode))

  ;;   (add-hook 'org-mode-hook 'org-buffer-face-mode-variable)))

(defun org-agenda-time-grid-spacing ()
  "Set different line spacing w.r.t. time duration."
  (save-excursion
    (let* ((background (alist-get 'background-mode (frame-parameters)))
           (background-dark-p (string= background "dark"))
           (colors (list "#1ABC9C" "#2ECC71" "#3498DB" "#9966ff"))
           pos
           duration)
      (nconc colors colors)
      (goto-char (point-min))
      (while (setq pos (next-single-property-change (point) 'duration))
        (goto-char pos)
        (when (and (not (equal pos (point-at-eol)))
                   (setq duration (org-get-at-bol 'duration)))
          (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
            (overlay-put ov 'face `(:background ,(car colors)
                                    :foreground
                                    ,(if background-dark-p "black" "white")))
            (setq colors (cdr colors))
            (overlay-put ov 'line-height line-height)
            (overlay-put ov 'line-spacing (1- line-height))))))))

(add-hook 'org-agenda-finalize-hook #'org-agenda-time-grid-spacing)

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

(use-package! valign
  :after org)
(add-hook 'org-mode-hook #'valign-mode)

(use-package! evil
  :defer t
  :bind (:map evil-normal-state-map
         ("<backspace>" . evil-ex-nohighlight)
         ("/" . swiper))
  :config
  (setq evil-want-fine-undo t
        evil-split-window-below t
        evil-vsplit-window-right t))

(use-package! evil-nerd-commenter
  :after evil
  :config
  (evilnc-default-hotkeys))

(use-package! evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

;; (cond
;;  ((string-equal platform MACOS)
;;   (defvar conda_home "/usr/local/Caskroom/miniconda/base/")
;;   (defvar conda_env_home "/usr/local/Caskroom/miniconda/base/"))

;;  ((string-equal platform LINUX)
;;   (message "no implemented"))

;;  ((string-equal platform WSL)
;;   (defvar conda_home "/home/chaomai/Programs/opt/miniconda3/")
;;   (defvar conda_env_home "/home/chaomai/Programs/opt/miniconda3/")))

;; (use-package! conda
;;   :defer t
;;   :config
;;   (setq conda-anaconda-home conda_home)
;;   (setq conda-env-home-directory conda_env_home))
;;   ;; (setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format)))

;; (cond
;;  ((string-equal platform MACOS)
;;   (defvar clang-format_bin "clang-format"))

;;  ((string-equal platform LINUX)
;;   (message "no implemented"))

;;  ((string-equal platform WSL)
;;   (defvar clang-format_bin "clang-format-10")))

;; (use-package! format
;;   :defer t
;;   :config
;;   (set-formatter! 'clang-format
;;     '(clang-format_bin
;;       "-style={BasedOnStyle: Google, SortIncludes: false}"
;;       ("-assume-filename=%S" (or buffer-file-name mode-result "")))
;;     :modes
;;     '((c-mode ".c")
;;       (c++-mode ".cpp")
;;       (java-mode ".java")
;;       (objc-mode ".m")
;;       (protobuf-mode ".proto")))

;;   (set-formatter! 'black "black -q -"
;;     :modes '(python-mode)))

(use-package! company
  :defer t
  :config
  (setq company-idle-delay 0
        company-echo-delay 0
        ;; Easy navigation to candidates with M-<n>
        company-show-numbers t
        company-require-match nil
        company-minimum-prefix-length 2
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

;; (use-package! lsp-mode
;;   :defer t
;;   :config
;;   (setq read-process-output-max (* 1024 1024))

;;   (setq lsp-keymap-prefix "C-c l"
;;         lsp-idle-delay 0.1                 ;; lazy refresh
;;         lsp-log-io nil                     ;; enable log only for debug
;;         lsp-enable-folding nil             ;; use `evil-matchit' instead
;;         lsp-diagnostic-package :flycheck   ;; prefer flycheck
;;         lsp-lens-auto-enable nil           ;; disable lens
;;         lsp-flycheck-live-reporting nil    ;; obey `flycheck-check-syntax-automatically'
;;         lsp-prefer-capf t                  ;; using `company-capf' by default
;;         lsp-enable-snippet nil             ;; no snippet
;;         lsp-enable-file-watchers nil       ;; turn off for better performance
;;         lsp-enable-text-document-color nil ;; as above
;;         lsp-enable-symbol-highlighting nil ;; as above
;;         lsp-enable-indentation nil         ;; indent by ourself
;;         lsp-enable-on-type-formatting nil  ;; disable formatting on the fly
;;         lsp-auto-guess-root t              ;; auto guess root
;;         lsp-keep-workspace-alive nil       ;; auto kill lsp server
;;         lsp-enable-xref t
;;         lsp-eldoc-enable-hover nil         ;; disable eldoc hover displays in minibuffer, lsp-ui shows it
;;         lsp-signature-auto-activate t      ;; show function signature
;;         lsp-signature-doc-lines 1)         ;; but dont take up more lines
;;   (add-to-list 'exec-path (concat conda_home "envs/common_dev_python3.8/bin/")))

;; (use-package! lsp-ui
;;   :after lsp-mode
;;   :config
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-sideline-show-hover nil
;;         lsp-ui-sideline-show-diagnostics nil
;;         lsp-ui-sideline-ignore-duplicate t
;;         lsp-ui-sideline-delay 0.1

;;         lsp-ui-peek-enable nil
;;         lsp-ui-peek-fontify 'always

;;         lsp-ui-doc-enable nil
;;         lsp-ui-doc-use-webkit nil
;;         lsp-ui-doc-delay 0.1
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-border (face-foreground 'default)

;;         lsp-ui-imenu-enable nil
;;         lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
;;                               ,(face-foreground 'font-lock-string-face)
;;                               ,(face-foreground 'font-lock-constant-face)
;;                               ,(face-foreground 'font-lock-variable-name-face))))

;; (use-package! lsp-treemacs
;;   :after lsp-mode)

;; (use-package! ccls
;;   :after lsp-mode
;;   :config
;;   (setq ccls-sem-highlight-method 'font-lock)
;;   (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
;;   (ccls-use-default-rainbow-sem-highlight)

;;   (setq ccls-executable "~/Documents/workspace/github/ccls/Release/ccls"
;;         ccls-args '("--log-file=/tmp/ccls-emacs.log")
;;         ccls-initialization-options `(:capabilities (:foldingRangeProvider :json-false)
;;                                                     :cache (:directory ".ccls-cache")
;;                                                     :completion (:caseSensitivity 0)
;;                                                     :compilationDatabaseDirectory "cmake-build"
;;                                                     :codeLens (:localVariables :json-false)
;;                                                     :client (:snippetSupport t)
;;                                                     :diagnostics (:onChang 100
;;                                                                            :onOpen 100
;;                                                                            :onSave 100)
;;                                                     :highlight (:lsRanges t)
;;                                                     :index (:threads 5)))
;;   (evil-set-initial-state 'ccls-tree-mode 'emacs))

(use-package! modern-cpp-font-lock
  :after ccls
  :hook (c++-mode . modern-c++-font-lock-mode))

(use-package! pyim
  :demand t
  :config
  (setq pyim-default-scheme 'quanpin
        default-input-method "pyim"
        ;; pyim-isearch-mode 1
        pyim-page-tooltip 'posframe
        pyim-page-length 5
        pyim-fuzzy-pinyin-alist '(("an" "ang")
                                  ("in" "ing")
                                  ("en" "eng")
                                  ("uan" "uang"))

        pyim-dcache-directory (concat doom-cache-dir "pyim"))

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 C-; 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions '(pyim-probe-dynamic-english
                                                      pyim-probe-isearch-mode
                                                      pyim-probe-program-mode
                                                      pyim-probe-org-structure-template)
                pyim-punctuation-half-width-functions '(pyim-probe-punctuation-line-beginning
                                                        pyim-probe-punctuation-after-punctuation))

  :bind
  (("C-;" . pyim-convert-string-at-point) ; 与 pyim-probe-dynamic-english 配合
   ("C-<f1>" . pyim-delete-word-from-personal-buffer)))

;; 激活 basedict 拼音词库，五笔用户请继续阅读 README
(use-package pyim-basedict
  :after pyim
  :config
  (pyim-basedict-enable))

(use-package! pyim-greatdict
  :after pyim
  :config
  (pyim-greatdict-enable))

(use-package! pangu-spacing
  :demand t
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t))

(use-package! awesome-tab
  :defer t
  :bind (("M-1" . awesome-tab-select-visible-tab)
         ("M-2" . awesome-tab-select-visible-tab)
         ("M-3" . awesome-tab-select-visible-tab)
         ("M-4" . awesome-tab-select-visible-tab)
         ("M-5" . awesome-tab-select-visible-tab)
         ("M-6" . awesome-tab-select-visible-tab)
         ("M-7" . awesome-tab-select-visible-tab)
         ("M-8" . awesome-tab-select-visible-tab)
         ("M-9" . awesome-tab-select-visible-tab)
         ("M-0" . awesome-tab-select-visible-tab))
  :config
  (awesome-tab-mode t)
  (setq awesome-tab-height 120))

(use-package! posframe
  :demand t)

(use-package! flycheck-posframe
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(use-package! expand-region
  :defer t
  :bind
  (("M-=" . er/expand-region)))
