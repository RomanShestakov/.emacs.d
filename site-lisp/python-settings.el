;;; python-settings.el --- customization for python-mode

;;; Commentary:
;; http://damcb.com/setting-up-a-scientific-python-working-environment.html
;; http://pedrokroger.net/configuring-emacs-python-ide/
;; https://github.com/jhamrick/emacs/blob/macs-and-emacs-post/.emacs.d/settings/python-settings.el
;; https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/python-settings.el
;; http://stackoverflow.com/questions/17255940/clear-steps-to-install-pymacs-with-emacs-24
;; http://www.idryman.org/blog/2013/03/13/python-setup-on-mac-osx/
;; http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co

;; to add flycheck - need to install flake8 and pylint
;; http://stackoverflow.com/questions/19803033/emacs-flycheck-configured-syntax-checker-python-flake8-cannot-be-used

;; https://github.com/flycheck/flycheck
;; sudo pip install flake8
;; sudo pip install pylint

;; for first run Jedi need to setup virtualenv
;; http://tkf.github.io/emacs-jedi/latest/
;; run : M-x jedi:install-server for first time to setup virtualenv

;;; Code:

(require 'python)

;; how to fix ipython broken output
;; http://emacs.stackexchange.com/questions/24453/weird-shell-output-when-using-ipython-5?newreg=7e15e274404d40d3bf722e1e310ee278
;;(setq python-shell-interpreter "ipython")
(setq python-shell-interpreter "python3")

(use-package eglot
  :ensure nil
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark)
  ;;(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
  (projectile-mode t)
  ;;(add-hook 'python-mode-hook 'eglot-ensure)
  )

(add-hook 'python-mode-hook 'eglot-ensure)

;; add F9 and S-F9 binding to eval a buffer or selected expr
(defun my-python-mode-hook ()
  (define-key python-mode-map [f9] 'python-shell-send-buffer)
  (define-key python-mode-map [S-f9] 'python-shell-send-region)
  (define-key python-mode-map [S-f1] 'python-insert-breakpoint)
  (setq indent-tabs-mode nil)
  (setq python-indent-offset 4)
  (setq tab-width 4)
  (setq python-indent 4)
  (setq flycheck-checker 'python-pylint
        flycheck-pylintc "~/.pylintrc"))

;; add hooks to python-mode
(add-hook 'python-mode-hook 'flycheck-mode)
;;(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(provide 'python-settings)

;;; python-settings.el ends here
