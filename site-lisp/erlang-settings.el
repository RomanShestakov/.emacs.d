

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
(defvar erlang-root-dir
  (concat (file-name-as-directory erlang-root) "lib")
  "*Path to erlang lib.")

;; find the name of tools- directory
(defun get-erlang-tools-dir-name ()
  "*Get the name of tools dir in current erlang installation."
  (file-name-as-directory (car (file-expand-wildcards (concat (file-name-as-directory erlang-root-dir) "tools-*")))))

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

(use-package erlang
  :init
  ;; define auto erlang mode for these files/extensions.
  (add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*\\.erl\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
  (add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
  :config
  (add-hook 'erlang-mode-hook
            (lambda()
              (setq mode-name "erl"
                    erlang-compile-extra-opts '((i . "../include"))
                    erlang-root-dir "/usr/lib/erlang")))
  )


;; completion
(use-package company :ensure t)

(use-package yasnippet
  :ensure t
  :hook ((erlang-mode . yas-minor-mode)
	       (snippet-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :ensure t
  :after (yasnippet))

(use-package eglot
  :ensure t
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  (projectile-mode t)
  (add-hook 'erlang-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:willSaveWaitUntil :textDocumentSync))
  )

(use-package flycheck
  :diminish flycheck-mode
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq
        ;; flycheck-display-errors-function nil
        flycheck-erlang-include-path '("../include")
        flycheck-erlang-library-path '()
        flycheck-check-syntax-automatically '(save)))

(provide 'erlang-settings)

;;; erlang-settings.el ends here
