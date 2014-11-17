;;; multi-term-settings.el --- multi-term

;;; Commentary:
;;; multi-term mode customisation
;;; http://www.emacswiki.org/MultiTerm
;;; Code:

;;; keys shortcuts
;; M-r - (same as Ctr-r) search history.

;; setup shell program to be used with multi-term

(eval-after-load "multi-term"
  '(progn
     (setq multi-term-program "/bin/bash")
     ))

(global-set-key (kbd "C-c t") 'multi-term-next)
(global-set-key (kbd "C-c T") 'multi-term) ;; create a new one

(provide 'multi-term-settings)

;;; multi-term-settings.el ends here
