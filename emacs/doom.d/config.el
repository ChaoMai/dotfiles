;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "chaomai"
      user-mail-address "loneymai@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
;; (setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.


;; ;;;;;;;;;; env
;; ;; https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; (add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

;; ;; check platform
;; (defconst MACOS "macos")
;; (defconst WSL "wsl")
;; (defconst LINUX "linux")

;; (cond
;;  ((string-equal system-type "darwin")
;;   (defvar platform MACOS))

;;  ((string-match "microsoft"
;;                 (with-temp-buffer (shell-command "uname -r" t)
;;                                   (goto-char (point-max))
;;                                   (delete-char -1)
;;                                   (buffer-string)))
;;   (defvar platform WSL))

;;  ((string-equal system-type "gnu/linux")
;;   (defvar platform LINUX)))

;; ;; disable double buffering in wsl
;; ;; https://github.com/hlissner/doom-emacs-private/blob/master/config.el
;; (cond
;;  ((string-equal platform WSL)
;;   (setq default-frame-alist
;;         (append default-frame-alist '((inhibit-double-buffering . t))))))

;; ;; org mode dir
;; (cond
;;  ((string-equal platform MACOS)
;;   (defvar org_dir "~/Documents/onedrive/Documents/workspace/chaomai.org/"))

;;  ((string-equal platform LINUX)
;;   (message "no implemented"))

;;  ((string-equal platform WSL)
;;   (defvar org_dir "/mnt/d/maichao/OneDrive/Documents/workspace/chaomai.org/")))

;; ;; conda home
;; (cond
;;  ((string-equal platform MACOS)
;;   (defvar conda_home "/usr/local/Caskroom/miniconda/base/")
;;   (defvar conda_env_home "/usr/local/Caskroom/miniconda/base/"))

;;  ((string-equal platform LINUX)
;;   (message "no implemented"))

;;  ((string-equal platform WSL)
;;   (defvar conda_home "/home/chaomai/Programs/opt/miniconda3/")
;;   (defvar conda_env_home "/home/chaomai/Programs/opt/miniconda3/")))

;; ;; wsl open
;; (defun wsl-browse-url (url &optional _new-window)
;;   ;; new-window ignored
;;   (interactive (browse-url-interactive-arg "URL: "))
;;   (let ((quotedUrl (format "start '%s'" url)))
;;     (apply 'call-process "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe" nil
;;            0 nil
;;            (list "-Command" quotedUrl)))
;;   (message "open link via powershell"))

;; (cond
;;  ((string-equal platform WSL)
;;   (setq browse-url-browser-function 'wsl-browse-url)))

;; ;; (cond
;; ;;  ((string-equal platform WSL)
;; ;;   (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
;; ;;         (cmd-args '("/c" "start")))
;; ;;     (when (file-exists-p cmd-exe)
;; ;;       (setq browse-url-generic-program  cmd-exe
;; ;;             browse-url-generic-args     cmd-args
;; ;;             browse-url-browser-function 'browse-url-generic)))))

;; ;; clang-format
;; ;; (cond
;; ;;  ((string-equal platform MACOS)
;; ;;   (defvar clang-format_bin "clang-format"))

;; ;;  ((string-equal platform LINUX)
;; ;;   (message "no implemented"))

;; ;;  ((string-equal platform WSL)
;; ;;   (defvar clang-format_bin "clang-format-10")))

;; (use-package! which-key
;;   :config
;;   (setq which-key-idle-delay 0.1))

;; ;;;;;;;;;; ui
;; (setq fancy-splash-image (concat doom-private-dir "nerv_logo.png"))

;; (cond
;;  ((string-equal platform MACOS)
;;   (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'regular)))

;;  ((string-equal platform LINUX)
;;   (message "no implemented"))

;;  ((string-equal platform WSL)
;;   (setq doom-font (font-spec :family "Fira Code" :size 14 :weight 'regular))))

;; (setq line-spacing 5
;;       display-line-numbers-type nil)

;; (use-package! doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   (load-theme 'doom-one-light t)

;;   ;; Enable flashing mode-line on errors
;;   (doom-themes-visual-bell-config)

;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   ;; (doom-themes-neotree-config)
;;   ;; or for treemacs users
;;   (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
;;   (doom-themes-treemacs-config)

;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

;; ;;;;;;;;;; basic
;; ;; pyim
;; (use-package! pyim
;;   :demand t
;;   :config
;;   (setq pyim-default-scheme 'quanpin
;;         default-input-method "pyim"
;;         ;; 开启拼音搜索功能
;;         ;; pyim-isearch-mode 1
;;         pyim-page-tooltip 'popup
;;         pyim-page-length 8
;;         pyim-fuzzy-pinyin-alist '(("an" "ang")
;;                                   ("in" "ing")
;;                                   ("en" "eng")
;;                                   ("uan" "uang"))

;;         pyim-dcache-directory (concat doom-cache-dir "pyim")))

;; (use-package! pyim-basedict
;;   :after pyim
;;   :config
;;   (pyim-basedict-enable))

;; (use-package! pyim-greatdict
;;   :after pyim
;;   :config
;;   (pyim-greatdict-enable))

;; (use-package! pangu-spacing
;;   :demand t
;;   :config
;;   (global-pangu-spacing-mode 1)
;;   (setq pangu-spacing-real-insert-separtor t))

;; ;; projectile
;; ;; project root is same with vim's configuration
;; (use-package! projectile
;;   :config
;;   (setq projectile-require-project-root t
;;         projectile-project-root-files '(".ccls-root" ".idea" "go.mod" ".bzr" "_darcs"
;;                                         "build.xml" ".project" ".root" ".svn" ".git"
;;                                         ".projectile")
;;         projectile-project-root-functions '(projectile-root-top-down
;;                                             projectile-root-top-down-recurring
;;                                             projectile-root-bottom-up
;;                                             projectile-root-local)))

;; ;; evil
;; (use-package! evil
;;   :defer t
;;   :config
;;   (setq evil-want-fine-undo t
;;         evil-disable-insert-state-bindings t
;;         evil-split-window-below t
;;         evil-vsplit-window-right t))

;; ;;;;;;;;;; ivy
;; (use-package! ivy
;;   :defer t
;;   :config
;;   (setq ivy-display-style 'fancy
;;         ivy-count-format "(%d/%d) "
;;         ivy-use-virtual-buffers t
;;         ivy-on-del-error-function 'ignore)

;;   (defun eh-ivy-cregexp (str)
;;     (let ((x (ivy--regex-plus str))
;;           (case-fold-search nil))
;;       (if (listp x)
;;           (mapcar (lambda (y)
;;                     (if (cdr y)
;;                         (list (if (equal (car y) "")
;;                                   ""
;;                                 (pyim-cregexp-build (car y)))
;;                               (cdr y))
;;                       (list (pyim-cregexp-build (car y)))))
;;                   x)
;;         (pyim-cregexp-build x))))

;;   (setq ivy-re-builders-alist '((t . eh-ivy-cregexp))))

;; (use-package! counsel
;;   :defer t
;;   :hook (ivy-mode . counsel-mode))

;; (use-package! swiper
;;   :defer t
;;   :config
;;   (setq swiper-action-recenter t))

;; ;;;;;;;;;; company
;; ;; https://emacs.stackexchange.com/questions/15246/how-add-company-dabbrev-to-the-company-completion-popup
;; ;; https://phenix3443.github.io/notebook/emacs/modes/company-mode.html
;; (use-package! company
;;   :defer t
;;   :config
;;   (setq company-idle-delay 0.0
;;         company-echo-delay 0.0
;;         ;; Easy navigation to candidates with M-<n>
;;         company-show-numbers t
;;         company-require-match nil
;;         company-minimum-prefix-length 2
;;         company-tooltip-align-annotations t
;;         ;; complete `abbrev' only in current buffer
;;         company-dabbrev-other-buffers nil
;;         ;; make dabbrev case-sensitive
;;         company-dabbrev-ignore-case nil
;;         company-dabbrev-downcase nil
;;         company-backends '((company-capf company-dabbrev-code company-keywords)
;;                            company-files
;;                            company-dabbrev)))

;; ;;;;;;;;;; org
;; ;; org-mode
;; (use-package! org
;;   :defer t
;;   :init
;;   (setq org-directory org_dir)
;;   :config
;;   (setq org-agenda-files (list (concat org_dir "project.org"))
;;         org-tags-column 0
;;         org-pretty-entities nil
;;         org-startup-indented t
;;         org-image-actual-width nil
;;         org-hide-leading-stars t
;;         org-hide-emphasis-markers t
;;         org-fontify-done-headline t
;;         org-fontify-whole-heading-line t
;;         org-fontify-quote-and-verse-blocks t
;;         org-catch-invisible-edits 'smart
;;         org-insert-heading-respect-content t
;;         ;; block switching the parent to done state
;;         org-enforce-todo-dependencies t
;;         org-enforce-todo-checkbox-dependencies t
;;         ;; org-ellipsis " -> "
;;         ;; gdt task status
;;         org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "WAITING(w@/!)" "|" "INACTIVE(i@/!)" "CANCELLED(c@/!)" "DONE(d!)"))

;;         ;; log
;;         org-log-done 'time
;;         org-log-repeat 'time
;;         org-log-redeadline 'note
;;         org-log-reschedule 'note
;;         org-log-into-drawer t
;;         org-log-state-notes-insert-after-drawers nil
;;         ;; refile
;;         org-refile-use-cache t
;;         org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
;;         org-refile-use-outline-path t
;;         org-outline-path-complete-in-steps nil
;;         org-refile-allow-creating-parent-nodes 'confirm
;;         ;; 配置归档文件的名称和 Headline 格式
;;         org-archive-location "%s_archive::date-tree"
;;         org-blank-before-new-entry '((heading . always)
;;                                      (plain-list-item . nil))
;;         org-startup-truncated nil))

;; ;; org-src
;; (use-package! org-src
;;   :after org
;;   :config
;;   (setq org-src-fontify-natively t
;;         org-src-tab-acts-natively t
;;         org-src-preserve-indentation t
;;         org-src-window-setup 'current-window
;;         org-confirm-babel-evaluate t
;;         org-edit-src-content-indentation 0
;;         org-babel-load-languages '((shell . t)
;;                                    (python . t)
;;                                    (ocaml . t)
;;                                    (emacs-lisp . t))))

;; ;; org-clock
;; (use-package! org-clock
;;   :after org
;;   :config
;;   (setq org-clock-in-resume t
;;         org-clock-idle-time 10
;;         org-clock-into-drawer t
;;         org-clock-out-when-done t
;;         org-clock-persist 'history
;;         org-clock-history-length 10
;;         org-clock-out-remove-zero-time-clocks t
;;         org-clock-report-include-clocking-task t)
;;   (org-clock-persistence-insinuate))

;; ;; org-superstar
;; (use-package! org-superstar
;;   :after org
;;   :hook (org-mode . org-superstar-mode)
;;   :config
;;   (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")))

;; ;; org-download
;; ;; make drag-and-drop image save in the same name folder as org file.
;; ;; example: `aa-bb-cc.org' then save image test.png to `aa-bb-cc_media/test.png'.
;; ;; https://coldnew.github.io/hexo-org-example/2018/05/22/use-org-download-to-drag-image-to-emacs/
;; (use-package! org-download
;;   :after org
;;   :hook ('dired-mode-hook 'org-download-enable)
;;   :config
;;   (defun my-org-download-method (link)
;;     (let ((filename
;;            (file-name-nondirectory
;;             (car (url-path-and-query
;;                   (url-generic-parse-url link)))))
;;           (dirname (concat (concat (file-name-sans-extension (buffer-name)) "_media/")
;;                            (format-time-string "%Y-%m-%d"))))
;;       ;; if directory not exist, create it
;;       (unless (file-exists-p dirname)
;;         (make-directory dirname))
;;       ;; return the path to save the download files
;;       (expand-file-name filename dirname)))

;;   (setq org-download-method 'my-org-download-method))

;; ;; valign
;; (use-package! valign
;;   :after org
;;   :hook (org-mode . valign-mode-hook))

;;;;;;;;;; utils
;; (use-package! undohist
;;   :demand t
;;   :config
;;   (setq undohist-directory (concat doom-cache-dir "undohist"))
;;   (undohist-initialize))

;; (use-package! saveplace
;;   :demand t
;;   :config
;;   (setq save-place-mode t)
;;   (save-place-mode 1))

;;;;;;;;;; references
;; https://practicalli.github.io/spacemacs/
;; https://scarletsky.github.io/2017/09/29/org-mode-in-spacemacs/
;; https://edward852.github.io/post/%E9%80%9A%E7%94%A8%E4%BB%A3%E7%A0%81%E7%BC%96%E8%BE%91%E5%99%A8spacemacs/
;; https://emacs-lsp.github.io/lsp-mode/
;; https://www.gtrun.org/custom/init.html
;; https://github.com/condy0919/emacs-newbie
;; https://github.com/condy0919/.emacs.d
;; https://alhassy.github.io/init/
;; https://huadeyu.tech/tools/emacs-setup-notes.html
;; https://emacs.nasy.moe/
;; https://github.com/lujun9972/emacs-document
