;;; cpp-settings.el --- customization for cpp-mode

;;; Commentary:
;;; http://nilsdeppe.com/posts/emacs-c++-ide

;;; depends on :
;;; install rust ;
;;; curl https://sh.rustup.rs -sSf | sh
;;; 1. rustfmt - to intall
;;; cargo intall rustfmt
;;; 2. racer - for code completion
;;; cargo install racer
;;; add source code for rust:
;;; git clone git@github.com:rust-lang/rust.git
;;; _build/default/lib/erlrexec/
;;; http://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
;;; http://syamajala.github.io/c-ide.html
;;; http://irreal.org/blog/?p=6028
;;; http://syamajala.github.io/c-ide.html (EMACS as C++ IDE based on rtags)

;; install clang/llvm
;; brew install llvm --with-libcxx --with-clang --without-assertions --with-rtti
;; brew link llvm
;; git clone git@github.com:Andersbakken/rtags.git; cd rtags; git submodule init; git submodule update
;; rtags + OSX
;; https://gist.github.com/floatplane/68f2006186cef4d3e165

;; how to crear=e .clang-format
;; clang-format -style=llvm -dump-config > .clang-format

;; https://github.com/philippe-grenet/exordium

;; Add these to the PATH so that proper executables are found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setq exec-path (append exec-path '("/usr/texbin")))
;; (setq exec-path (append exec-path '("/usr/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))

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
  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  ;;(setq company-backends 'company-complete (remove 'company-clang company-backends)))

  (add-to-list 'eglot-server-programs `((c++-ts-mode), clangd-exe))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-ts-mode))
  ;; stop eldoc from poping up window
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  ;; use flymake in favour of flycheck
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode t)))
  (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'eglot-managed-mode-hook (lambda () (add-to-list 'company-backends '(company-capf :with company-yasnippet)))))

(use-package clang-format+
  :ensure t)

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

(use-package c++-ts-mode
  :if (treesit-language-available-p 'c)
  :custom
  (c-ts-mode-indent-offset 4)
  ;;  (c-ts-mode-indent-style #'my-indent-style)
  :init
  ;; Remap the standard C/C++ modes
  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  (add-hook 'c++-ts-mode-hook #'clang-format+-mode)
  (add-hook 'c-ts-mode-hook 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook 'eglot-ensure)
  (add-hook 'c-or-c++-ts-mode 'eglot-ensure)
  (add-hook 'c++-ts-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace))))


;;(with-eval-after-load "eglot" (add-to-list 'eglot-stay-out-of 'eldoc))

(provide 'cpp-settings)

;;; cpp-settings.el ends here
