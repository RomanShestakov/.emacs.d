;; auto-complete-settings.el

;; auto-complete mode: https://github.com/auto-complete/auto-complete
;;(add-path  "auto-complete") ;; add auto-complete mode: https://github.com/auto-complete/auto-complete

(include-plugin "auto-complete")
(require 'auto-complete)

(add-to-list  'ac-dictionary-directories (concat emacs-root "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(provide 'auto-complete-settings)
