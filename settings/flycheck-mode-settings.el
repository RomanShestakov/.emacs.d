;;; flyckeck-mode-settings.el --- a wrapper for flycheck mode

;;; Commentary:

;; enable global mode for flycheck
;; python-mode uses keys C-c ! to start python interpreter
;; flycheck-mode uses the same bindings so it shadows
;; we redefine C-c ! to C-c f

;;; Code:

;; load flycheck
(autoload 'global-flycheck-mode "flycheck" nil)
(add-hook 'after-init-hook #'global-flycheck-mode)

(eval-after-load "flycheck"
  '(progn
    ;(add-hook 'after-init-hook #'global-flycheck-mode)
    ;; redefine the key binding for C-c ! to C-c f
    ;; see comment in flyckeck.el
    ;; because it is shadowing py-shell
     (define-key flycheck-mode-map flycheck-keymap-prefix nil)
     (setq flycheck-keymap-prefix (kbd "C-c f"))
     (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
     ))

(provide 'flycheck-mode-settings)

;;; flycheck-mode-settings.el ends here
