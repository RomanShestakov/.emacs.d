;;; helm-settings.el --- wrapper for helm settings

;;; Commentary:
;; https://github.com/emacs-helm/helm/wiki

;;; Code:
(require 'helm)
(require 'helm-descbinds)
(require 'helm-files)

(fset 'describe-bindings 'helm-descbinds)
(helm-mode 1)

(global-set-key (kbd "C-c h") 'helm-mini)

;; http://amitp.blogspot.co.uk/2012/10/emacs-helm-for-finding-files.html
;; find files
(setq helm-idle-delay 0.1)
(setq helm-input-idle-delay 0.1)

(setq helm-locate-command "locate-with-mdfind %.0s %s")
(loop for ext in '("\\.swf$" "\\.elc$" "\\.pyc$")
      do(add-to-list 'helm-boring-file-regexp-list ext))

(global-set-key (kbd "M-t") 'helm-for-files)

(provide 'helm-settings)

;;; helm-settings.el ends here
