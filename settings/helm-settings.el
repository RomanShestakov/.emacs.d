;;; helm-settings.el --- wrapper for helm settings

;;; Commentary:
;; https://github.com/emacs-helm/helm/wiki

;;; Code:


;; ;; to get around issue with renamed function
;; ;; http://lists.gnu.org/archive/html/bug-gnu-emacs/2015-01/msg00351.html
;; (if (>= emacs-major-version 24)
;;     (progn
;;       (defun class-slot-initarg (class-name slot)
;;         (eieio--class-slot-initarg (eieio--class-v class-name) slot))))

;; (autoload 'helm-mode "helm-mode" nil)
;; (helm-mode 1)

;; (eval-after-load "helm"
;;   '(progn
;;     (fset 'describe-bindings 'helm-descbinds)
;;     (global-set-key (kbd "C-c h") 'helm-mini)

;;     ;; http://amitp.blogspot.co.uk/2012/10/emacs-helm-for-finding-files.html
;;     ;; find files
;;     (setq helm-idle-delay 0.1)
;;     (setq helm-input-idle-delay 0.1)

;;     (setq helm-locate-command "locate-with-mdfind %.0s %s")
;;     (loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
;;           do(add-to-list 'helm-boring-file-regexp-list ext))

;;     ;; bind <M-t> to helm find files
;;     (global-set-key (kbd "M-t") 'helm-for-files)

;;     ;; http://www.emacswiki.org/emacs/ShiftedKeys
;;     ;; http://stackoverflow.com/questions/6156286/emacs-lisp-call-function-with-prefix-argument-programmatically
;;     ;; add 'glimpse' - grep recursively in files
;;     (global-set-key (kbd "S-<f12>")
;;                     (lambda() (interactive)
;;                       (setq current-prefix-arg '(4))
;;                       (call-interactively 'helm-do-grep)))
;;     ))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    ;; update fast sources immediately (doesn't).
    (setq helm-idle-delay 0.0
          ;; this actually updates things
          helm-input-idle-delay 0.01  
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("M-t" . helm-for-files)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

;; Turn off ido mode in case I enabled it accidentally
;;(ido-mode -1) 

(provide 'helm-settings)

;;; helm-settings.el ends here
