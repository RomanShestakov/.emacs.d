;; Common Lisp
(require 'cl)

;; root of all emacs-related stuff
(defvar emacs-root (if (or (eq system-type 'cygwin)
                           (eq system-type 'gnu/linux)
                           (eq system-type 'linux)
                           (eq system-type 'darwin))
                       "~/.emacs.d/"    "z:/.emacs.d/"))

;; path to where plugins are kept
(setq plugin-path (concat emacs-root "el-get"))

;; add paths to various configuration modes
(cl-labels ((add-path (p)
                   (add-to-list 'load-path (concat emacs-root p))))
  (add-path  ".")
  (add-path  "settings")
  (add-path  "site-lisp")
  (add-path  "scala") ;; scala mode related code
  (add-path  "erlang") ;; Configuration for Erlang mode
  (add-path  "exec-path-from-shell") ;; allows setting PATH, PYTHONPATH from .profile 
  )

;; now load various configs
;; set PATH, because we don't load .bashrc
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'custom-functions)
(require 'el-get-settings) ;; load plugins with el-get 
(require 'general-settings)
(require 'color-theme-settings)

;; tramp-mode
(include-plugin "tramp")
(require 'tramp)

(require 'scala_mode_config)
(require 'erlang-settings) ;; add settings for erlang mode
(require 'python-settings) ;; add settings for python mode

(require 'flymake)

;; git 
(include-plugin "magit")
(require 'magit)

;; tree - required for dirtree
(include-plugin "tree-mode")
(require 'tree-mode)

;; dirtree
(include-plugin "dirtree")
(require 'dirtree)

;; move-text
(include-plugin "move-text")
(require 'move-text)
(move-text-default-bindings)

;; puppet-mode to edit puppet definitions
(include-plugin "puppet-mode")
(require 'puppet-mode)

(require 'drag-and-drop-settings)
(require 'key-binding-settings)
(require 'auto-complete-settings)
(require 'fill-column-indicator-settings)

;; assosiate .pp file extension with puppet-mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))
