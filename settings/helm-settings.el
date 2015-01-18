;;; helm-settings.el --- wrapper for helm settings

;;; Commentary:
;; https://github.com/emacs-helm/helm/wiki

;;; Code:


;; to get around issue with renamed function
;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00351.html
(defun class-slot-initarg (class-name slot)
  (eieio--class-slot-initarg (eieio--class-v class-name) slot))

(autoload 'helm-mode "helm-mode" nil)
(helm-mode 1)

(eval-after-load "helm"
  '(progn
    (fset 'describe-bindings 'helm-descbinds)
    (global-set-key (kbd "C-c h") 'helm-mini)

    ;; http://amitp.blogspot.co.uk/2012/10/emacs-helm-for-finding-files.html
    ;; find files
    (setq helm-idle-delay 0.1)
    (setq helm-input-idle-delay 0.1)

    (setq helm-locate-command "locate-with-mdfind %.0s %s")
    (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
          do(add-to-list 'helm-boring-file-regexp-list ext))

    ;; bind <M-t> to helm find files
    (global-set-key (kbd "M-t") 'helm-for-files)

    ;; http://www.emacswiki.org/emacs/ShiftedKeys
    ;; http://stackoverflow.com/questions/6156286/emacs-lisp-call-function-with-prefix-argument-programmatically
    ;; add 'glimpse' - grep recursively in files
    (global-set-key (kbd "S-<f12>")
                    (lambda() (interactive)
                      (setq current-prefix-arg '(4))
                      (call-interactively 'helm-do-grep)))
    ))

(provide 'helm-settings)

;;; helm-settings.el ends here
