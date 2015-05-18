;;; python-settings.el --- customization for python-mode

;;; Commentary:
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

;; ;; ;; set PATHONPATH from env
;; (require 'exec-path-from-shell)
;; (exec-path-from-shell-copy-env "PATHONPATH")

(require 'python)

(setq python-shell-interpreter "ipython")
;; (setq python-shell-interpreter-args
;;       (if (system-is-mac)
;;           "--matplotlib=osx --colors=Linux"
;;         (if (system-is-linux)
;;             "--gui=wx --matplotlib=wx --colors=Linux")))
(setq python-shell-interpreter-args "")
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(setq python-shell-completion-setup-code  "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; add F9
(add-hook 'python-mode-hook 'my-python-mode-hook)
(defun my-python-mode-hook ()
  "*Compile file with F9."
  (define-key python-mode-map (kbd "<f9>")
    (lambda()
      (interactive)
      (progn
        (python-shell-send-buffer))))

  (define-key python-mode-map (kbd "<f8>")
    (lambda()
      (interactive)
      (progn
        (python-shell-send-region))))
  )

;; pydoc info
;;(include-plugin "pydoc-info-0.2")
;;(require 'pydoc-info)

(use-package jedi
  :ensure t
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :bind (("C-." . jedi:goto-definition)
         ("C-c r" . jedi:related-names)
         ("C-?" . jedi:show-doc)))

;; (require 'jedi)
;; (autoload 'jedi-setup "jedi" t)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(provide 'python-settings)

;;; python-settings.el ends here
