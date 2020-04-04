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
(setq doom-font (font-spec :family "Sarasa Mono SC" :size 14 :weight 'light))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
                                        ;(setq doom-theme 'doom-one)

(setq doom-theme 'spacemacs-dark
      spacemacs-theme-comment-bg nil
      spacemacs-theme-comment-italic t)

(setq-default line-spacing 5)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; projectile
;; project root is same with vim's configuration
(setq projectile-require-project-root t)
(setq projectile-project-root-files '(".ccls-root" ".idea" "go.mod" ".bzr" "_darcs"
                                      "build.xml" ".project" ".root" ".svn" ".git"))

(setq projectile-project-root-files-functions '(projectile-root-top-down
                                                projectile-root-top-down-recurring
                                                projectile-root-bottom-up
                                                projectile-root-local))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; format
(after! format
 (set-formatter! 'clang-format
   '("clang-format"
     "-style={BasedOnStyle: Google, IndentWidth: 4, SortIncludes: false}"
     ("-assume-filename=%S" (or buffer-file-name mode-result "")))
   ))

;; :modes
;; '((c-mode ".c")
;;   (c++-mode ".cpp")
;;   (java-mode ".java")
;;   (objc-mode ".m")
;;   (protobuf-mode ".proto"))))

;; (after! format
;;  (set-formatter!
;;    'black "black -q -" :modes '(python-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp, ccls
(after! ccls
  :config
  ;; overlay is slow
  ;; Use https://github.com/emacs-mirror/emacs/commits/feature/noverlay
  (setq ccls-sem-highlight-method 'font-lock)
  (add-hook 'lsp-after-open-hook #'ccls-code-lens-mode)
  (ccls-use-default-rainbow-sem-highlight)

  ;; https://github.com/maskray/ccls/blob/master/src/config.h
  (setq ccls-executable "~/Documents/workspace/github/ccls/Release/ccls"
        ccls-args '("--log-file=/tmp/ccls-emacs.log")
        ccls-initialization-options `(:capabilities (:foldingRangeProvider :json-false)
                                                    :cache (:directory ".ccls-cache")
                                                    :completion (:caseSensitivity 0)
                                                    :compilationDatabaseDirectory "cmake-build"
                                                    :client (:snippetSupport t)
                                                    :codeLens (:localVariables t)
                                                    :diagnostics (:onChang 100
                                                                           :onOpen 100
                                                                           :onSave 100)
                                                    :highlight (:lsRanges t)
                                                    :index (:threads 4)))
  (evil-set-initial-state 'ccls-tree-mode 'emacs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company
(setq company-idle-delay 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modern-cpp-font-lock
(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))
