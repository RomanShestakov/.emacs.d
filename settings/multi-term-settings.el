;;; multi-term-settings.el --- multi-term

;;; Commentary:
;;; multi-term mode customisation
;;; http://www.emacswiki.org/MultiTerm
;;; Code:

;;; keys shortcuts
;; M-r - (same as Ctr-r) search history.

;; some interesting customisation for multi-term
;; https://github.com/daimrod/Emacs-config/blob/master/config/config-multi-term.el

;; setup shell program to be used with multi-term

(eval-after-load "multi-term"
  '(progn
     (setq multi-term-program "/bin/bash"
           term-unbind-key-list '("C-x"
                                  "C-h"
                                  "M-x"
                                  "C-z")
           term-term-name "xterm-256color" )

     ;; create a new term or switch to existing one
     (global-set-key (kbd "C-c t") 'multi-term-next)

     ;; create a new one unconditionally
     (global-set-key (kbd "C-c T") 'multi-term)

     ;; ;; disable yasnippet
     (add-hook 'term-mode-hook (lambda()
                                 (yas-minor-mode -1)))
     ))


(provide 'multi-term-settings)

;;; multi-term-settings.el ends here
