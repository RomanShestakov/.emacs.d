;;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

;; time emacs load time
(defvar *emacs-load-start* (current-time))

(defun anarcat/time-to-ms (time)
  "*Used to calc load TIME."
  (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))

(defun anarcat/display-timing ()
  "*Display load TIME."
  (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms (current-time))
                                         (anarcat/time-to-ms *emacs-load-start*)) 1000000.0)))
(add-hook 'after-init-hook 'anarcat/display-timing t)

(eval-when-compile
  (require 'cl))

;; add root to all emacs-related stuff
;; and add load-paths to packages
;; see http://stackoverflow.com/questions/23324760/emacs-byte-compile-errors-on-the-first-require-statement
;; for the use of eval-and-compile
(eval-and-compile
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
    (normal-top-level-add-subdirs-to-load-path)))

;; set PATH and PYTHONPATH from env
(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "PATH")
(exec-path-from-shell-copy-env "PYTHONPATH")

;; load plugins with el-get
(require 'el-get-settings)

;; apply general customisation settings
(require 'general-settings)

;; tramp-mode
(require 'tramp)

;; git
(require 'magit)

;; move-text
(require 'move-text)
(move-text-default-bindings)

;; add modes with customized settings
(require 'erlang-settings)
(require 'elixir-settings)
(require 'python-settings)
(require 'drag-and-drop-settings)
(require 'key-binding-settings)
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
;;(require 'fill-column-indicator-settings)

(setq custom-file (concat (file-name-as-directory emacs-root) ".emacs-custom.el"))
(load custom-file 'noerror)

;;; init.el ends here
