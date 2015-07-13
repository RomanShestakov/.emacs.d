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

(require 'python)

;(setq python-shell-interpreter "ipython")
;; (setq python-shell-interpreter-args
;;       (if (system-is-mac)
;;           "--matplotlib=osx --colors=Linux"
;;         (if (system-is-linux)
;;             "--gui=wx --matplotlib=wx --colors=Linux")))
(setq python-shell-interpreter-args "")
(setq python-shell-prompt-regexp "In \\[[0-9]+\\]: ")
(setq python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
(setq python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n")
(setq python-shell-completion-setup-code  "from IPython.core.completerlib import module_completion")
(setq python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

;; setup virtualenvwrapper
(use-package virtualenvwrapper
  :ensure t
  :init
  (bind-key "C-c v" 'venv-workon python-mode-map)
  (bind-key "C-c d" 'venv-diactivate python-mode-map)
  (bind-key "C-c l" 'venv-lsvirtualenv python-mode-map)
  (bind-key "C-c m" 'venv-mkvirtualenv python-mode-map)
  (bind-key "C-c c" 'venv-cdvirtualenv python-mode-map)
  :config
  (progn
    ;; Used by virtualenvwrapper.el to store virtualenvs
    (setq venv-location "~/.virtualenvs")
    ;; Used python-environment.el and by extend jedi.el
    (setq python-environment-directory venv-location)
    (venv-initialize-interactive-shells)
    ;; show the name of env in status line
    (setq-default mode-line-format (cons '(:exec venv-current-name) mode-line-format))))

;; ;; get auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :defer t)

;; get epc
(use-package epc
  :ensure t
  :defer t)

;; get jedi
;; note that jedi depends on virtualenvwrapper and
;; on (setq python-environment-directory venv-location)
(use-package jedi
  :ensure t
  :preface
  (declare-function jedi:goto-definition jedi nil)
  (declare-function jedi:related-names jedi nil)
  (declare-function jedi:show-doc jedi nil)
  :init
  (bind-key "M-." 'jedi:goto-definition python-mode-map)
  (bind-key "M-," 'jedi:goto-definition-pop-marker python-mode-map)
  (bind-key "M-/" 'jedi:get-in-function-call python-mode-map)
  (bind-key "M-?" 'jedi:show-doc python-mode-map)
  :config
  (progn
    (setq jedi:complete-on-dot t)))


;; highlight breakpoint
;; borrowed from https://www.masteringemacs.org/article/compiling-running-scripts-emacs
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(defvar python--pdb-breakpoint-string "import ipdb; ipdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `ipdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))

;; add F9 and S-F9 binding to eval a buffer or selected expr
(defun my-python-mode-hook ()
  (define-key python-mode-map [f9] 'python-shell-send-buffer)
  (define-key python-mode-map [S-f9] 'python-shell-send-region)
  (define-key python-mode-map [S-f1] 'python-insert-breakpoint))

;; add hooks to python-mode
(add-hook 'python-mode-hook 'flycheck-mode)
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; install common package when creating a new virtualenv
;; add debug highlighting
(add-hook 'python-mode-hook 'python--add-debug-highlight)

;; pydoc info
;;(include-plugin "pydoc-info-0.2")
;;(require 'pydoc-info)

;; shut up Can't guess python-indent-offset warnings
;; http://stackoverflow.com/questions/18778894/emacs-24-3-python-cant-guess-python-indent-offset-using-defaults-4
;;(python-indent-guess-indent-offset nil)

(provide 'python-settings)

;;; python-settings.el ends here
