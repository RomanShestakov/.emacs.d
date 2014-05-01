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

(require 'custom-functions)

;; Set PYTHONPATH, because we don't load .bashrc
;; (setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")
;; (exec-path-from-shell-copy-env "PYTHONPATH")

(require 'python-mode)

;; add path to python-mode and pymacs
(add-to-list 'load-path (concat emacs-root "el-get/pymacs"))
(add-to-list 'load-path (concat emacs-root "el-get/python-mode"))
(setq py-install-directory (concat emacs-root "el-get/python-mode"))

;; use python-mode for .py files
(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))

(setq-default py-shell-name "/usr/local/bin/ipython")
(setq-default py-which-bufname "IPython")

(setq-default py-python-command-args
              (if (system-is-mac)
                  '("--gui=osx" "--pylab=osx" "--colors" "Linux")
                (if (system-is-linux)
                 '("--gui=wx" "--pylab=wx" "--colors" "Linux")
                 '())))

(setq py-force-py-shell-name-p 1)

;; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p nil)

;; try to automagically figure out indentantion
(setq py-smart-indentation t)

;; add F9 and S-F9 keybindings
(add-hook 'python-mode-hook 'my-python-mode-hook)
(defun my-python-mode-hook ()
  "*Compile file with F9."
  (define-key python-mode-map [f9]
    (lambda()
      (interactive)
      (progn
        (py-execute-buffer)))))

;; pydoc info
;(include-plugin "pydoc-info-0.2")
;(require 'pydoc-info)

;;(include-plugin "jedi")
(require 'jedi)

(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(provide 'python-settings)

;;; python-settings.el ends here
