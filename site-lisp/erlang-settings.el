

;;; Commentary:
;;; erlang-settings.el --- provide settings for erlang mode.

;;; Code:

;; set path to erlang install load a path to elang lisp as distel
;; install in el-get distel package depends on it if root is nil or
(defvar erlang-root (or (getenv "ERLANG_HOME") "/usr/lib/erlang")
  "*Path to Erlang installation.
Env var ERLANG_HOME needs to be set in bash environment.")
;(defvar erlang-root "/usr/local/lib/erlang")

;; add erlang bin dir to exec-path
(setq exec-path (cons (concat (file-name-as-directory erlang-root) "bin") exec-path))

;; add "lib" to erlang-root
(defun get-erlang-root-dir()
  "*Path to erlang lib."
  (concat (file-name-as-directory erlang-root) "lib"))

;; find the name of tools- directory
(defun get-erlang-tools-dir-name ()
  "*Get the name of tools dir in current erlang installation."
  (file-name-as-directory (car (file-expand-wildcards (concat (file-name-as-directory (get-erlang-root-dir)) "tools-*")))))

;; build the full absolute name to erlang tools
(defun get-full-path-to-erlang-tools-dir ()
  "*Get full name of erlang tools dir."
  (concat (file-name-as-directory
           (concat (get-erlang-tools-dir-name) "emacs"))))

(defun erlang-path-init()
  "*Set the paths to erlang mode."
  (add-to-list 'load-path (get-full-path-to-erlang-tools-dir)))

;; add path to OTP installation to load-path
(erlang-path-init)

(use-package ivy-erlang-complete
  :ensure t)

(use-package erlang
;;  :load-path ("<PATH TO OTP>/lib/erlang/lib/tools-3.0/emacs/")
  :hook (after-save . ivy-erlang-complete-reparse)
  :custom (ivy-erlang-complete-erlang-root erlang-root)
  :config (ivy-erlang-complete-init)
  :mode (("\\.erl?$" . erlang-mode)
	 ("rebar\\.config$" . erlang-mode)
	 ("relx\\.config$" . erlang-mode)
	 ("sys\\.config\\.src$" . erlang-mode)
	 ("sys\\.config$" . erlang-mode)
	 ("\\.config\\.src?$" . erlang-mode)
	 ("\\.config\\.script?$" . erlang-mode)
	 ("\\.hrl?$" . erlang-mode)
	 ("\\.app?$" . erlang-mode)
	 ("\\.app.src?$" . erlang-mode)
	 ("\\Emakefile" . erlang-mode)))

(use-package eglot
  :ensure nil
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  (projectile-mode t)
  ;;(add-hook 'erlang-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:willSaveWaitUntil :textDocumentSync)))

(add-hook 'erlang-mode-hook 'eglot-ensure)

(use-package delight
  :ensure t)

(use-package flycheck
  :ensure t
  :delight
  :config (global-flycheck-mode))

;; (use-package flycheck
;;   :diminish flycheck-mode
;;   :config
;;   (add-hook 'after-init-hook 'global-flycheck-mode)
;;   (setq
;;         ;; flycheck-display-errors-function nil
;;         flycheck-erlang-include-path '("../include")
;;         flycheck-erlang-library-path '()
;;         flycheck-check-syntax-automatically '(save)))

(provide 'erlang-settings)

;;; erlang-settings.el ends here
