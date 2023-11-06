

;;; Commentary:
;;; elisp-settings.el --- provide settings for elisp mode.

;;; Code:

;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :functions elisp-slime-nav-mode
  :bind (("M-." . elisp-slime-nav-find-elisp-thing-at-point)
         ("M-," . pop-tag-mark))
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))



(provide 'elisp-settings)

;;; elisp-settings.el ends here
