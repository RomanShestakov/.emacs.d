;;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

;; root of all emacs-related stuff
(defvar emacs-root
  (if (or (eq system-type 'cygwin)
          (eq system-type 'gnu/linux)
          (eq system-type 'linux)
          (eq system-type 'darwin))
      "~/.emacs.d/"    "z:/.emacs.d/")
  "Path to where EMACS configuration root is.")

;; path to where plugins are kept
(defvar plugin-path (concat (file-name-as-directory emacs-root) "el-get")
  "*Path to el-get plugins.")

;; add subdirectories of root into load path
(let ((default-directory emacs-root))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

;; set PATH and PYTHONPATH from env
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "PYTHONPATH")

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
(require 'erlang-settings)
(require 'python-settings)
(require 'drag-and-drop-settings)
(require 'key-binding-settings)
;;(require 'fill-column-indicator-settings)
(require 'puppet-mode-settings)
(require 'color-theme-settings)
(require 'helm-settings)
(require 'ctag-settings)
(require 'dirtree-settings)
(require 'yasnippet-settings)
(require 'flycheck-mode-settings)
(require 'org-mode-settings)
(require 'projectile-settings)
(require 'ido-settings)
(require 'elisp-slime-nav-settings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((checkdoc-minor-mode . t) (require-final-newline . t) (mangle-whitespace . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
