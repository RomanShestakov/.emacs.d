;;; elisp-slime-nav-settings.el --- elisp slime nav

;;; Commentary:
;; enable slime nav mode for elisp
;; https://github.com/purcell/elisp-slime-nav

;; key bindings:
;; 'M-.' jump in
;; 'M-,' jump out
;; 'C-c C-d d' - slime describe symbol

;;; Code:

(eval-after-load "elisp-slime-nav"
  '(progn
     (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
       (add-hook hook 'elisp-slime-nav-mode))))

(provide 'elisp-slime-nav-settings)

;;; elisp-slime-nav-settings.el ends here
