;;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

(eval-when-compile (require 'cl))

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
;; (require 'exec-path-from-shell)
(eval-after-load "exec-path-from-shell"
  '(progn
     (exec-path-from-shell-copy-env "PATH")
     (exec-path-from-shell-copy-env "PYTHONPATH")))

;; load settings/loaddefs if available, if not generate it first
;; this file contains autoloads from packages in settings dir
(autoload 'update-autoloads-in-package-area "update-auto-loads" t)
(let ((loaddef (concat (file-name-as-directory emacs-root) "settings/loaddefs.el")))
  (if (file-exists-p loaddef)
      (progn
        (load-file loaddef))
    (update-autoloads-in-package-area)
    (load-file loaddef)))

;; load plugins with el-get
(require 'el-get-settings)

;; apply general customisation settings
(require 'general-settings)

;; move-text mode
(move-text-default-bindings)

;; add modes with customized settings
(require 'erlang-settings)
(require 'elixir-settings)
(require 'python-settings)
(require 'key-binding-settings)
(require 'color-theme-settings)
(require 'helm-settings)
(require 'dirtree-settings)
(require 'flycheck-mode-settings)
(require 'org-mode-settings)
(require 'ido-settings)
(require 'elisp-slime-nav-settings)
(require 'yasnippet-settings)
;(require 'ctag-settings)
;(require 'projectile-settings)
;;(require 'fill-column-indicator-settings)

;; see #7 from http://a-nickels-worth.blogspot.co.uk/2007/11/effective-emacs.html
;; load custom-file
(setq custom-file (concat (file-name-as-directory emacs-root) ".emacs-custom.el"))
(load custom-file 'noerror)

;; make sure that loaddefs.el is updated on emacs exit
;; http://stackoverflow.com/questions/4189159/emacs23-elisp-how-to-properly-autoload-this-library
(add-hook 'kill-emacs-hook 'update-autoloads-in-package-area)

;;; init.el ends here
