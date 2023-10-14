

;;; Commentary:
;;; elixir-settings.el --- provide settings for elixir mode.

;;; https://medium.com/@victor.nascimento/elixir-emacs-development-2023-edition-1a6ccc40629

;;; Code:


;; (use-package elixir-mode
;;   :ensure t )




(use-package
 emacs
 :ensure nil
 :custom

 ;; Should use:
;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))
 ;; at least once per installation or while changing this list
 (treesit-language-source-alist
  '((heex "https://github.com/phoenixframework/tree-sitter-heex")
    (elixir "https://github.com/elixir-lang/tree-sitter-elixir")))

 (major-mode-remap-alist
  '((elixir-mode . elixir-ts-mode)))
)



(use-package
 eglot
 :ensure nil
 :config
 (add-to-list 'eglot-server-programs '(elixir-ts-mode "language_server.sh"))
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
 )


(use-package
 elixir-ts-mode
 :hook (elixir-ts-mode . eglot-ensure)
 (elixir-ts-mode
  .
  (lambda ()
    (push '(">=" . ?\u2265) prettify-symbols-alist)
    (push '("<=" . ?\u2264) prettify-symbols-alist)
    (push '("!=" . ?\u2260) prettify-symbols-alist)
    (push '("==" . ?\u2A75) prettify-symbols-alist)
    (push '("=~" . ?\u2245) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("->" . ?\u2192) prettify-symbols-alist)
    (push '("<-" . ?\u2190) prettify-symbols-alist)
    (push '("|>" . ?\u25B7) prettify-symbols-alist)))
 (before-save . eglot-format))


(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)
         ("C-c i R" . 'inf-elixir-reload-module)))

(provide 'elixir-settings)

;;; elixir-settings.el ends here
