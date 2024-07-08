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



(provide 'rust-settings)

;;; rust-settings.el ends here











