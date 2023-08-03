

;;; Commentary:
;;; elixir-settings.el --- provide settings for elixir mode.

;;; Code:


(use-package elixir-mode
    :ensure t )
  ;; :init
  ;; define auto erlang mode for these files/extensions.
  ;; (add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*\\.erl\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
  ;; (add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
  ;; :config
  ;; (add-hook 'erlang-mode-hook
  ;;           (lambda()
  ;;             (setq mode-name "erl"
  ;;                   erlang-compile-extra-opts '((i . "../include"))
  ;;                   erlang-root-dir "/usr/lib/erlang")))
;;  )


;; ;; completion
;; (use-package company :ensure t)

;; (use-package yasnippet
;;   :ensure t
;;   :hook ((erlang-mode . yas-minor-mode)
;; 	       (snippet-mode . yas-minor-mode)))

;; (use-package yasnippet-snippets
;;   :ensure t
;;   :after (yasnippet))

(use-package eglot
  :ensure t
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  (projectile-mode t)
  (add-hook 'elixir-mode-hook 'eglot-ensure)
  (setq eglot-ignored-server-capabilities '(:willSaveWaitUntil :textDocumentSync))
  )

;; (use-package flycheck
;;   :diminish flycheck-mode
;;   :config
;;   (add-hook 'after-init-hook 'global-flycheck-mode)
;;   (setq
;;         ;; flycheck-display-errors-function nil
;;         flycheck-erlang-include-path '("../include")
;;         flycheck-erlang-library-path '()
;;         flycheck-check-syntax-automatically '(save)))

(provide 'elixir-settings)

;;; erlang-settings.el ends here
