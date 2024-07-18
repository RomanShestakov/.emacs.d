;;; rust-settings.el --- customization for rust-mode

;;; Commentary:

;;; Code:

;;; This dpends on rust-analayer being installed
;;; rust-analyser is LSP for rust
;;; https://rust-analyzer.github.io/manual.html#installation

(use-package eglot
  :hook ((rust-mode nix-mode) . eglot-ensure)
  :config (add-to-list 'eglot-server-programs
                       `(rust-mode . ("rust-analyzer" :initializationOptions
                                     ( :procMacro (:enable t)
                                       :cargo ( :buildScripts (:enable t)
                                                :features "all"))))))

;; (use-package rustic
;;   :ensure t
;;   :config
;;   (setq rustic-format-on-save t)
;;   (setq rustic-lsp-client 'eglot))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
         ("C-j" . corfu-next)
         ("C-k" . corfu-previous)
         ("TAB" . corfu-insert)
         ("RET" . nil))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  (global-set-key (kbd "M-i") #'completion-at-point))


(provide 'rust-settings)

;;; rust-settings.el ends here











