;;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

(with-no-warnings
  (require 'cl))

;; root of all emacs-related stuff
(defvar emacs-root (if (or (eq system-type 'cygwin)
                           (eq system-type 'gnu/linux)
                           (eq system-type 'linux)
                           (eq system-type 'darwin))
                       "~/.emacs.d/"    "z:/.emacs.d/")
  "*Path to EMACS root.")

;; path to where plugins are kept
(defvar plugin-path (concat emacs-root "el-get")
  "*Path to el-get plugins.")

;; add paths to various configuration modes
(cl-labels ((add-path (p) (add-to-list 'load-path (concat emacs-root p))))
  (add-path  ".")
  (add-path  "settings")
  (add-path  "site-lisp")
  (add-path  "erlang") ;; Configuration for Erlang mode
  (add-path  "exec-path-from-shell") ;; allows setting PATH, PYTHONPATH from .profile
  ;; (add-path  "scala") ;; scala mode related code
  )

;; load a path to elang list as distel install in el-get distel package depends on it
(add-to-list 'load-path "/usr/local/lib/erlang/lib/tools-2.6.13/emacs")

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
;; (require 'scala_mode_config)
;; add settings for erlang mode
(require 'erlang-settings)
;; add settings for python mode
(require 'python-settings)
(require 'drag-and-drop-settings)
(require 'key-binding-settings)
(require 'fill-column-indicator-settings)
(require 'puppet-mode-settings)
(require 'color-theme-settings)
(require 'helm-settings)
(require 'ctag-settings)
(require 'dirtree-settings)
(require 'yasnippet-settings)
(require 'flycheck-mode-settings)

;;; init.el ends here
