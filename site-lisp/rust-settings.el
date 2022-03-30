;;; rust-settings.el --- customization for rust-mode

;;; Commentary:
;;; http://julienblanchard.com/2016/fancy-rust-development-with-emacs/
;;; https://github.com/racer-rust/emacs-racer

;;; depends on :
;;; install rust ;
;;; curl https://sh.rustup.rs -sSf | sh
;;; 1. rustfmt - to intall
;;; cargo intall rustfmt
;;; 2. racer - for code completion
;;; cargo install racer
;;; add source code for rust:
;;; git clone git@github.com:rust-lang/rust.git


;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;; Code:

(use-package rust-mode
  :ensure t
  :defer t)


(use-package eglot
  :ensure t
  :config
;;  (bind-key "M-." 'xref-find-definitions)
;;  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (projectile-mode t)
  (add-to-list 'eglot-server-programs '(rust-mode . "rust-analyzer"))
  (add-hook 'rust-mode-hook 'eglot-ensure))



;; (use-package cargo
;;   :ensure t
;;   :defer t)

;; (use-package flycheck-rust
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; (use-package rust-analyzer
;;   :ensure t
;;   :init
;;   (bind-key "M-." #'racer-find-definition)
;;   ;;(bind-key "M-," 'jedi:goto-definition-pop-marker python-mode-map)
;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   ;; Rustup binaries PATH
;;   ;;(setq racer-cmd "~/.cargo/bin/racer")
;;   ;; Rust source code PATH
;; ;;  (setq racer-rust-src-path "/Users/romanshestakov/.cargo/")
;;   )


;; (use-package rustfmt
;;   :config
;;   (define-key rust-mode-map (kbd "C-c C-f") #'rustfmt-format-buffer))

;; (add-hook 'rust-mode-hook 'cargo-minor-mode)

;; (defun my-rust-mode-hook ()
;;   ;;(define-key rust-mode-map [f9] 'cargo-process-build)
;;   (define-key rust-mode-map [f9] 'cargo-process-run))

;; (add-hook 'rust-mode-hook 'flycheck-mode)
;; ;;(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; (add-hook 'rust-mode-hook 'my-rust-mode-hook)

;; ;; completion
;; (add-hook 'rust-mode-hook #'company-mode)
;; (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
;; (setq company-tooltip-align-annotations t)

;; Key binding to jump to method definition
;(local-set-key (kbd "M-.") #'racer-find-definition)

(provide 'rust-settings)

;;; rust-settings.el ends here
