

;;; Commentary:
;;; web-settings.el --- provide settings for web dev.

;;; npm install -g typescript-language-server
;;; npm install typescript-eslint-language-service -D
;;; https://notes.alexkehayias.com/setting-up-typescript-and-eslint-with-eglot/

;;; Code:

(defvar ts-lang-server (executable-find "typescript-language-server")
  "TypeScript Language server executable path.")

(use-package web-mode
    :ensure t
    :mode (("\\.ts\\'" . web-mode)
           ("\\.js\\'" . web-mode)
           ("\\.mjs\\'" . web-mode)
           ("\\.tsx\\'" . web-mode)
           ("\\.jsx\\'" . web-mode))
    :config
    (setq web-mode-content-types-alist
   '(("jsx" . "\\.js[x]?\\'"))))

(use-package eglot
  :ensure t
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (projectile-mode t)
;;  (add-to-list 'eglot-server-programs '((web-mode) ts-lang-server "--stdio" ))
  (add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(web-mode . ("typescript-language-server" "--stdio")))
  (add-hook 'js-mode-hook 'eglot-ensure)
  (add-hook 'web-mode-hook 'eglot-ensure))

(provide 'web-settings)

;;; web-settings.el ends here
