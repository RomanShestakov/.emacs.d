;; flyckeck-mode-settings.el

;; enable global mode for flycheck
(include-plugin "flycheck")
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'flycheck-mode-settings)
