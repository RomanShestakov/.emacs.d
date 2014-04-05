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

;; load a path to elang list as distel install in el-get distel package depends on it
(setq load-path  (cons "/usr/local/lib/erlang/lib/tools-2.6.13/emacs" load-path))

;; now load various configs
;; set PATH, because we don't load .bashrc
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; some custom helper funcs
(require 'custom-functions)

;; load plugins with el-get 
(require 'el-get-settings) 

;; apply general customisation settings
(require 'general-settings)

;; tramp-mode
(include-plugin "tramp")
(require 'tramp)

;; git 
(include-plugin "magit")
(require 'magit)

;; move-text
(include-plugin "move-text")
(require 'move-text)
(move-text-default-bindings)

;; add modes with customized settings
;; add settings for scala mode
(require 'scala_mode_config)
;; add settings for erlang mode
(require 'erlang-settings) 
;; add settings for python mode
(require 'python-settings) 
(require 'flymake)
(require 'drag-and-drop-settings)
(require 'key-binding-settings)
(require 'auto-complete-settings)
(require 'fill-column-indicator-settings)
(require 'puppet-mode-settings)
(require 'color-theme-settings)
(require 'helm-settings)
(require 'ctag-settings)
(require 'dirtree-settings)
(require 'yasnippet-settings)
