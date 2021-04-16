;;; init-startup.el --- The startup dashboard -*- lexical-binding: t -*-

;;; Commentary:
;;

;;; Code:


(use-package all-the-icons
  :ensure t)

;; Project management
(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
         ("C-c p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy
        projectile-indexing-method 'hybrid
        projectile-read-command nil ;; no prompt in projectile-compile-project
        projectile-globally-ignored-file-suffixes '(".elc" ".pyc" ".o" ".swp" ".so")
        projectile-ignored-projects '("/tmp/"))

  ;; cmake project build
  (projectile-register-project-type 'cmake '("CMakeLists.txt")
                                    :configure "cmake -Bbuild"
                                    :compile "cmake --build build"
                                    :test "cd build && ctest --output-on-failure")

  ;; bazel project builds
  (projectile-register-project-type 'bazel '("WORKSPACE")
                                    :compile "bazel build //..."
                                    :test "bazel test //...")

  (dolist (dir '(".ccls-cache"
                 ".clangd"
                 "bazel-bin"
                 "bazel-out"
                 "bazel-testlogs"))
    (push dir projectile-globally-ignored-directories))
)

;; Comprehensive ivy integration for projectile
(use-package counsel-projectile
  :ensure t
  :after counsel projectile
  :hook (prog-mode . counsel-projectile-mode))

(use-package recentf
  :ensure nil
  :hook ((after-init . recentf-mode)
         (focus-out-hook . (recentf-save-list recentf-cleanup)))
  :config
  (setq recentf-max-saved-items 300
        recentf-auto-cleanup 'never
        recentf-exclude '((expand-file-name package-user-dir)
                          no-littering-var-directory
                          no-littering-etc-directory
                          ".cache"
                          "cache"
                          "recentf"
                          "^/tmp/"
                          "/ssh:"
                          "^/usr/include/"
                          "bookmarks"
                          "COMMIT_EDITMSG\\'")))

(use-package page-break-lines
  :ensure t
  :hook ((emacs-lisp-mode compilation-mode help-mode) . page-break-lines-mode))

(use-package dashboard
  :ensure t
  :hook (after-init . dashboard-setup-startup-hook)
  :custom-face
  (dashboard-heading ((t (:foreground "#f1fa8c" :weight bold))))
  :config
  (setq dashboard-banner-logo-title "Welcome to Emacs Dashboard"
        dashboard-startup-banner 'logo
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-set-init-info t
        dashboard-set-navigator t
        dashboard-items '((recents . 10)
                           (projects . 5)
                           (bookmarks . 5))))

(provide 'init-startup)

;;; init-startup.el ends here
