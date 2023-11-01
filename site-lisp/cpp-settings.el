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

;; https://github.com/philippe-grenet/exordium

;; Add these to the PATH so that proper executables are found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setq exec-path (append exec-path '("/usr/texbin")))
;; (setq exec-path (append exec-path '("/usr/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))

;;; Code:

;; set 4 space indent
(setq-default c-basic-offset 4)
(defvar clangd-exe (executable-find "clangd")
  "Clangd executable path.")



;; not really neede to use this re-mapping as
;; in tree-sitter-settings
;; (use-package c-ts-mode
;;   :ensure t
;;   :init
;;   ;; Remap the standard C/C++ modes
;;   (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
;;   (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode)))


;; https://edoput.it/2022/07/19/use-package.html
(use-package eglot
  ;; use nil to use builtin package
  :ensure nil
  :defines company-backends
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  ;;(setq company-backends 'company-complete (remove 'company-clang company-backends)))

  (projectile-mode t)
  (add-to-list 'eglot-server-programs `((c++-mode), clangd-exe))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  ;; stop eldoc from poping up window
  (add-to-list 'eglot-ignored-server-capabilites :hoverProvider)
  ;; use flymake in favour of flycheck
  (add-hook 'eglot-managed-mode-hook (lambda () (flymake-mode t)))
  (add-hook 'eglot-managed-mode-hook (lambda () (flycheck-mode -1)))
  (add-hook 'eglot-managed-mode-hook (lambda () (add-to-list 'company-backends '(company-capf :with company-yasnippet)))))

;; start eglot
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;;(with-eval-after-load "eglot" (add-to-list 'eglot-stay-out-of 'eldoc))

(provide 'cpp-settings)

;;; cpp-settings.el ends here
