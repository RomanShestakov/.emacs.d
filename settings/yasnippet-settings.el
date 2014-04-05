;; yasnippet-settings.el

(include-plugin "yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; fix conflict of yasnipper wiht org-mode
;; http://orgmode.org/manual/Conflicts.html
(defun yas/org-very-safe-expand ()
  (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

(add-hook 'org-mode-hook
          (lambda ()
            (make-variable-buffer-local 'yas/trigger-key)
            (setq yas/trigger-key [tab])
            (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
            (define-key yas/keymap [tab] 'yas/next-field)))

(provide 'yasnippet-settings)
