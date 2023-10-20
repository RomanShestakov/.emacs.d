
;;; Commentary:
;;; typescript-settings.el --- provide settings for typescript.

;;; npm install -g typescript-language-server
;;; npm install typescript-eslint-language-service -D
;;; https://notes.alexkehayias.com/setting-up-typescript-and-eslint-with-eglot/

;;; Code:

(defvar ts-lang-server (executable-find "typescript-language-server")
  "TypeScript Language server executable path.")

(use-package treesit-auto
  :config
  (global-treesit-auto-mode))

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
         ("\\.js\\'" . typescript-ts-mode))

(use-package eglot
  :ensure t
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (projectile-mode t)
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio")))
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook 'eglot-ensure))

(provide 'web-settings)

;;; typescript-settings.el ends here
