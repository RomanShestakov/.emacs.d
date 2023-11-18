
;;; Commentary:
;;; typescript-settings.el --- provide settings for typescript.

;;; npm install -g typescript-language-server
;;; npm install typescript-eslint-language-service -D
;;; https://notes.alexkehayias.com/setting-up-typescript-and-eslint-with-eglot/
;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
;;; Code:

(defvar ts-lang-server (executable-find "typescript-language-server")
  "TypeScript Language server executable path.")

(use-package typescript-ts-mode
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
         ;;("\\.tsx\\'" . typescript-ts-mode))
         ("\\.js\\'" . typescript-ts-mode))

(use-package eglot
  :ensure nil
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  (add-to-list 'eglot-server-programs '(tsx-ts-mode . ("typescript-language-server" "--stdio")))
  (add-to-list 'eglot-server-programs '(typescript-ts-mode . ("typescript-language-server" "--stdio"))))

(add-hook 'js-mode-hook #'eglot-ensure)
(add-hook 'typescript-ts-mode-hook #'eglot-ensure)
(add-hook 'tsx-ts-mode-hook #'eglot-ensure)

(setq js-indent-level 2)

(provide 'typescript-settings)

;;; typescript-settings.el ends here
