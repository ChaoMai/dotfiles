;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
                      auto-completion-enable-snippets-in-popup t)
     (c-c++ :variables
            c-c++-backend 'lsp-ccls)
     (python :variables
             python-backend 'lsp)
     (version-control :variables
                      version-control-diff-side 'left
                      version-control-global-margin t)
     better-defaults
     chinese
     cmake
     conda
     copy-as-format
     csv
     dap
     emacs-lisp
     git
     go
     graphviz
     helpful
     html
     ivy
     json
     lsp
     lua
     markdown
     multiple-cursors
     org
     quickurl
     rust
     shell-scripts
     sql
     syntax-checking
     tabs
     treemacs
     vimscript)


   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(flycheck-posframe
                                      posframe
                                      modern-cpp-font-lock)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 4096)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 10))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 10.0
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%a@%t"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'changed

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs t

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  ;; staratup banner
  (setq dotspacemacs-startup-banner '"~/Documents/workspace/dotfiles/emacs/spacemacs.d/nerv_logo.png")

  ;; check platform
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

  ;; disable double buffering in wsl
  ;; https://github.com/hlissner/doom-emacs-private/blob/master/config.el
  (cond
   ((string-equal platform WSL)
    (setq default-frame-alist
          (append default-frame-alist '((inhibit-double-buffering . t))))))

  ;; org mode dir
  (cond
   ((string-equal platform MACOS)
    (defvar org_dir "~/Documents/onedrive/Documents/workspace/chaomai.org/"))

   ((string-equal platform LINUX)
    (message "no implemented"))

   ((string-equal platform WSL)
    (defvar org_dir "/mnt/d/maichao/OneDrive/Documents/workspace/chaomai.org/")))

  ;; conda home
  (cond
   ((string-equal platform MACOS)
    (defvar conda_home "/usr/local/Caskroom/miniconda/base/")
    (defvar conda_env_home "/usr/local/Caskroom/miniconda/base/"))

   ((string-equal platform LINUX)
    (message "no implemented"))

   ((string-equal platform WSL)
    (defvar conda_home "/home/chaomai/Programs/opt/miniconda3/")
    (defvar conda_env_home "/home/chaomai/Programs/opt/miniconda3/")))

  ;; wsl open
  (cond
   ((string-equal platform WSL)
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic)))))

  ;; clang-format
  (cond
   ((string-equal platform MACOS)
    (defvar clang-format_bin "clang-format"))

   ((string-equal platform LINUX)
    (message "no implemented"))

   ((string-equal platform WSL)
    (defvar clang-format_bin "clang-format-10")))

  ;; font
  (cond
   ((string-equal platform MACOS)
    (setq dotspacemacs-default-font '("Cascadia Code PL"
                                      :size 14.0
                                      :weight normal
                                      :width normal)
          dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)))

   ((string-equal platform LINUX)
    (message "no implemented"))

   ((string-equal platform WSL)
    (setq dotspacemacs-default-font '("Cascadia Code PL"
                                      :size 12.0
                                      :weight normal
                                      :width normal)
          dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5))))

  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; references
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; basic
  ;; line spacing
  (setq-default line-spacing 9)

  ;; pyim
  (use-package pyim
    :demand t
    :config
    (setq pyim-default-scheme 'quanpin
          default-input-method "pyim"
          ;; 开启拼音搜索功能
          ;; pyim-isearch-mode 1
          pyim-page-tooltip 'posframe
          pyim-page-length 5
          pyim-fuzzy-pinyin-alist '(("an" "ang")
                                    ("in" "ing")
                                    ("en" "eng")
                                    ("uan" "uang"))

          pyim-dcache-directory (concat spacemacs-cache-directory "pyim"))

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
    (("C-;" . pyim-convert-string-at-point) ; 与 pyim-probe-dynamic-english
     ("C-<f1>" . pyim-delete-word-from-personal-buffer)))

  ;; pangu-spacing
  (use-package pangu-spacing
    :demand t
    :config
    (global-pangu-spacing-mode 1)
    (setq pangu-spacing-real-insert-separtor t))

  ;; projectile
  ;; project root is same with vim's configuration
  (use-package projectile
    :config
    (setq projectile-require-project-root t
          projectile-project-root-files '(".ccls-root" ".idea" "go.mod" ".bzr" "_darcs"
                                          "build.xml" ".project" ".root" ".svn" ".git"
                                          "index.org" ".projectile")
          projectile-project-root-files-functions '(projectile-root-top-down
                                                    projectile-root-top-down-recurring
                                                    projectile-root-bottom-up
                                                    projectile-root-local)))

  ;; centaur-tabs
  (use-package centaur-tabs
    :config
    (setq centaur-tabs-set-close-button nil))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil
  (use-package evil
    :defer t
    :bind (:map evil-normal-state-map
                ("<backspace>" . evil-ex-nohighlight)
                ("/" . swiper))
    :config
    (setq evil-want-fine-undo t
          evil-split-window-below t
          evil-vsplit-window-right t))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ivy
  (use-package ivy
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

  (use-package counsel
    :defer t
    :hook (ivy-mode . counsel-mode))

  (use-package swiper
    :defer t
    :config
    (setq swiper-action-recenter t))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; company

  (use-package company
    :defer t
    :config
    (setq company-idle-delay 0.0
          company-echo-delay 0.0
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org
  ;; org-mode
  (use-package org
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
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n!)" "PROJ(p!)" "WAITING(w@/!)" "|" "INACTIVE(i@/!)" "CANCELLED(c@/!)" "DONE(d!)"))

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

  ;; org-agenda
  ;; 1. https://emacs-china.org/t/org-agenda/8679/3
  ;; 2. https://www.lijigang.com/blog/2018/08/08/%E7%A5%9E%E5%99%A8-org-mode/
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

  ;; org-src
  (use-package org-src
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

  ;; org-clock
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

  ;; org-superstar
  (use-package org-superstar
    :after org
    :hook (org-mode . org-superstar-mode)
    :config
    (setq org-superstar-headline-bullets-list '("☰" "☱" "☲" "☳" "☴" "☵" "☶" "☷" "☷" "☷" "☷")))

  ;; org-download
  ;; make drag-and-drop image save in the same name folder as org file.
  ;; example: `aa-bb-cc.org' then save image test.png to `aa-bb-cc_media/test.png'.
  ;; https://coldnew.github.io/hexo-org-example/2018/05/22/use-org-download-to-drag-image-to-emacs/
  (use-package org-download
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp
  ;; lsp-mode
  ;; 1. https://github.com/MaskRay/Config/blob/master/home/.config/doom/modules/private/my-cc/autoload.el
  ;; 2. https://github.com/MaskRay/ccls/wiki/lsp-mode
  (use-package lsp-mode
    :commands lsp
    :config
    (setq lsp-keymap-prefix "C-c l"
          lsp-idle-delay 0.500               ;; lazy refresh
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
          lsp-headerline-breadcrumb-segments t
          lsp-eldoc-enable-hover nil         ;; disable eldoc hover displays in minibuffer
          lsp-signature-auto-activate t      ;; show function signature
          lsp-signature-doc-lines 1)         ;; but dont take up more lines
    (add-to-list 'exec-path (concat conda_home "envs/common_dev_python3.8/bin/")))

  (use-package lsp-ui
    :after lsp-mode
    :config
    (setq lsp-ui-sideline-enable t
          lsp-ui-sideline-delay 0.1
          lsp-ui-sideline-ignore-duplicate t
          lsp-ui-sideline-show-code-actions nil
          lsp-ui-sideline-show-diagnostics t
          lsp-ui-sideline-show-hover nil

          lsp-ui-peek-enable nil
          lsp-ui-peek-fontify 'always

          lsp-ui-doc-enable nil
          lsp-ui-doc-use-webkit nil
          lsp-ui-doc-delay 0.1
          lsp-ui-doc-include-signature t
          lsp-ui-doc-position 'top
          lsp-ui-doc-border (face-foreground 'default)

          lsp-ui-imenu-enable t
          lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                ,(face-foreground 'font-lock-string-face)
                                ,(face-foreground 'font-lock-constant-face)
                                ,(face-foreground 'font-lock-variable-name-face))))

  ;; ccls
  (use-package ccls
    :after lsp-mode
    :config
    (setq ccls-sem-highlight-method 'font-lock)
    (ccls-use-default-rainbow-sem-highlight)

    (setq ccls-executable "~/Documents/workspace/github/ccls/Release/ccls"
          ccls-args '("--log-file=/tmp/ccls-emacs.log")
          ccls-initialization-options `(:capabilities (:foldingRangeProvider :json-false)
                                                      :cache (:directory ".ccls-cache")
                                                      :completion (:caseSensitivity 0)
                                                      :compilationDatabaseDirectory "cmake-build"
                                                      :codeLens (:localVariables :json-false)
                                                      :client (:snippetSupport t)
                                                      :diagnostics (:onChang 100
                                                                             :onOpen 100
                                                                             :onSave 100)
                                                      :highlight (:lsRanges t)
                                                      :index (:threads 5)))
    (evil-set-initial-state 'ccls-tree-mode 'emacs))

  ;; modern-cpp-font-lock
  (use-package modern-cpp-font-lock
    :after ccls
    :config
    (modern-c++-font-lock-global-mode t))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; format
  ;; (use-package format
  ;;   :demand t
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; misc
  ;; posframe
  (use-package posframe
    :demand t)

  ;; flycheck-posframe
  (use-package flycheck-posframe
    :after flycheck
    :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))


  ;; conda
  ;; https://github.com/necaris/conda.el/issues/39#issuecomment-554802379
  (use-package conda
    :defer t
    :config
    (setq conda-anaconda-home conda_home)
    (setq conda-env-home-directory conda_env_home))
    ;; (setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format)))

  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(pyim-dicts
   '((:name "pyim-greatdict" :file "~/Documents/workspace/dotfiles/emacs/spacemacs.d/pyim-greatdict.pyim"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
