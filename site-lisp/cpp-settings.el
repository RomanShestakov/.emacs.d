;;; cpp-settings.el --- customization for cpp-mode

;;; Commentary:
;; this configuration is inspired by the following blog post : https://ddavis.io/blog/eglot-cpp-ide/

;;; Code:

(defvar clangd-exe (executable-find "clangd")
  "Clangd executable path.")

;; https://edoput.it/2022/07/19/use-package.html
(use-package eglot
  ;; use nil to use builtin eglot package
  :ensure nil
  :defines company-backends
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'xref-go-back)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  ;;(setq company-backends 'company-complete (remove 'company-clang company-backends)))

  (add-to-list 'eglot-server-programs `((c++-ts-mode), clangd-exe))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
  ;; stop eldoc from poping up window
  (add-to-list 'eglot-ignored-server-capabilities :hoverProvider)
  ;; use flymake in favour of flycheck
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode t)))
  (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'eglot-managed-mode-hook (lambda () (add-to-list 'company-backends '(company-capf :with company-yasnippet)))))

(use-package clang-format+
  :ensure t
  :config
  (bind-key "C-c u" 'clang-format-buffer)
  (bind-key "C-c i" 'clang-format-region)
  (setq clang-format-style-option "llvm")
  )

;; declare c-ts-mode--indent-styles o avoid warning from byte-compiler in my-indent-style
;;(declare-function c-ts-mode--indent-styles c-or-c++-ts-mode)

;; ;; https://emacs.stackexchange.com/questions/77232/c-c-with-tree-sitter-how-to-change-indent
;; (defun my-indent-style()
;;   "Override the built-in BSD indentation style with some additional rules."
;;   `(;; Here are your custom rules
;;     ((node-is ")") parent-bol 0)
;;     ((match nil "argument_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
;;     ((parent-is "argument_list") prev-sibling 0)
;;     ((match nil "parameter_list" nil 1 1) parent-bol c-ts-mode-indent-offset)
;;     ((parent-is "parameter_list") prev-sibling 0)
;;     ;; Append here the indent style you want as base
;;    ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp))))

(use-package c-ts-mode
  :ensure nil
  :if (and (treesit-language-available-p 'c) (treesit-language-available-p 'cpp))
  :custom
  (c-ts-mode-indent-offset 2)
  ;;  (c-ts-mode-indent-style #'my-indent-style)
  :init
  ;; Remap the standard C/C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  ;; enale clang-format
  (add-hook 'c++-ts-mode-hook #'clang-format+-mode)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-or-c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace))))


;;(with-eval-after-load "eglot" (add-to-list 'eglot-stay-out-of 'eldoc))

(provide 'cpp-settings)

;;; cpp-settings.el ends here
