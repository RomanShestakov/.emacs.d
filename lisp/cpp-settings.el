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
;; git clone git@github.com:Andersbakken/rtags.git; cd rtags; git submodule init; git submodule update
;; rtags + OSX
;; https://gist.github.com/floatplane/68f2006186cef4d3e165


;; Add these to the PATH so that proper executables are found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setq exec-path (append exec-path '("/usr/texbin")))
;; (setq exec-path (append exec-path '("/usr/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))


;; (use-package async
;;   :ensure t
;;   :defer t)

;; (use-package auctex
;;   :ensure t
;;   :defer t)

;; (use-package auto-complete
;;   :ensure t
;;   :defer t)

;; (use-package autopair
;;   :ensure t
;;   :defer t)

;; (use-package clang-format
;;   :ensure t
;;   :defer t)

(use-package cmake-mode
  :ensure t
  :defer t)

;; (use-package company-irony
;;   :ensure t
;;   :defer t)

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :defer t)

;; (use-package dash
;;   :ensure t
;;   :defer t)

;; (use-package elp
;;   :ensure t
;;   :defer t)

;; (use-package flycheck-irony
;;   :ensure t
;;   :defer t)

;; (use-package google-c-style
;;   :ensure t
;;   :defer t)

;; (use-package helm-ctest
;;   :ensure t
;;   :defer t)

;; (use-package helm-flycheck
;;   :ensure t
;;   :defer t)

;; (use-package helm-flyspell
;;   :ensure t
;;   :defer t)

;; (use-package helm-ls-git
;;   :ensure t
;;   :defer t)

;; (use-package hungry-delete
;;   :ensure t
;;   :defer t)

(use-package irony
  :ensure t
  :defer t)

;; (use-package let-alist
;;   :ensure t
;;   :defer t)

;; (use-package pkg-info
;;   :ensure t
;;   :defer t)

;; (use-package popup
;;   :ensure t
;;   :defer t)

;; (use-package rtags
;;   :ensure t
;;   :defer t)

;; (use-package seq
;;   :ensure t
;;   :defer t)

;; (use-package vlf
;;   :ensure t
;;   :defer t)

;; (use-package autopair
;;   :ensure t
;;   :defer t)

;; (use-package rtags
;;   :ensure t
;;   :defer t)

;; (use-package company-rtags
;;   :ensure t
;;   :defer t)


(use-package rtags
  :ensure t
  :defer t
  :config (add-hook 'c++-mode-hook
                    (lambda ()
                      (rtags-enable-standard-keybindings c-mode-base-map)
                      (setq rtags-completions-enabled t)
                      (rtags-diagnostics))))

(require 'company-rtags)

;; (use-package company-rtags
;;   :ensure t
;;   :defer t
;;   :config (progn (add-hook
;;                   'c++-mode-hook
;;                   (lambda()
;;                     ;; (set (make-local-variable 'company-backends) '(company-rtags))
;;                     (setq company-rtags-begin-after-member-access t)))))

;; (use-package irony
;;   :ensure t
;;   :config
;;   (use-package company-irony
;;     :ensure t
;;     :config
;;     (add-to-list 'company-backends 'company-irony))
;;   (use-package company-irony-c-headers
;;     :ensure t
;;     :config
;;     (add-to-list 'company-backends 'company-irony-c-headers))
;;   (add-hook 'c++-mode-hook 'irony-mode)
;;   (add-hook 'c-mode-hook 'irony-mode)
;;   (add-hook 'objc-mode-hook 'irony-mode)
;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's function
;;   (defun my-irony-mode-hook ()
;;     (define-key irony-mode-map [remap completion-at-point]
;;       'irony-completion-at-point-async)
;;     (define-key irony-mode-map [remap complete-symbol]
;;       'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package company
  :ensure t
  :init
  (global-company-mode)
  :bind (("<backtab>" . company-complete-common-or-cycle))
  :config
  (delete 'company-backends 'company-clang))

(use-package cmake-ide
  :ensure t
  :init
  (bind-key [f9] 'cmake-ide-compile)
  (use-package semantic/bovine/gcc)
  ;; (setq cmake-ide-flags-c++ (append '("-std=c++11")
  ;;                                   (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c++"))))
  ;; (setq cmake-ide-flags-c (append (mapcar (lambda (path) (concat "-I" path)) (semantic-gcc-get-include-paths "c"))))
  ;; (put 'cmake-ide-build-dir 'safe-local-variable #'stringp)
  ;; (global-set-key (kbd "C-c m") 'cmake-ide-compile)
  ;; (define-key rust-mode-map [f9] 'cargo-process-run))
  (cmake-ide-setup))

;;(define-key c++-mode-map [f9] 'cmake-ide-compile))

(add-hook 'c++-mode-hook
          (lambda () (setq flycheck-clang-language-standard "c++11"))
       ;   (define-key c++-mode-map [f9] 'cmake-ide-compile))
          )

(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; Load rtags and start the cmake-ide-setup process
;; (require 'rtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup cmake-ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'cmake-ide)
;;(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
;;(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; We want to be able to compile with a keyboard shortcut
;;(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html
;; (setq rtags-autostart-diagnostics t)
;; (rtags-diagnostics)
;; (setq rtags-completions-enabled t)
;; (rtags-enable-standard-keybindings)

;; ;; clang-format can be triggered using C-M-tab
;; (require 'clang-format)
;; (global-set-key [C-M-tab] 'clang-format-region)
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format


(provide 'cpp-settings)





;;; cpp-settings.el ends here
