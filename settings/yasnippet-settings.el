;;; yasnippet-settings.el --- settings for yasnippet

;;; Commentary:

;;; Code:

(require 'yasnippet)
(yas-global-mode 1)

;; http://orgmode.org/manual/Conflicts.html
(defun yas/org-very-safe-expand ()
  "*Fix conflict of yasnipper wiht 'org-mode'."
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(provide 'yasnippet-settings)

;;; yasnippet-settings.el ends here
