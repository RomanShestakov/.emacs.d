;;; org-mode-settings.el --- customisations for org-mode

;;; Commentary:

;;; Code:

(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)
   (ruby . t)
   (sh . t)
   (python . t)
   (emacs-lisp . t)
   ;;(erlang . t)
   ))

(defun nolinum ()
  "*Disable nulum in 'org-mode'."
  (global-linum-mode 0))

(add-hook 'org-mode-hook 'nolinum)

(provide 'org-mode-settings)

;;; org-mode-settings.el ends here
