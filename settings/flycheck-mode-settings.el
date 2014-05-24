;;; flyckeck-mode-settings.el --- a wrapper for flycheck mode

;;; Commentary:
;; enable global mode for flycheck

;;; Code:

(eval-after-load "flycheck"
  '(progn
    ;(add-hook 'after-init-hook #'global-flycheck-mode)
    ;; redefine the key binding for C-C ! to C-c f
    ;; see comment in flyckeck.el
    ;; because it is shadowing py-shell
     (define-key flycheck-mode-map flycheck-keymap-prefix nil)
     (setq flycheck-keymap-prefix (kbd "C-c f"))
     (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
     ))

(provide 'flycheck-mode-settings)

;;; flycheck-mode-settings.el ends here
