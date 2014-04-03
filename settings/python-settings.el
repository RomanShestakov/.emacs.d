;; python-settings.el

;; http://pedrokroger.net/configuring-emacs-python-ide/
;; https://github.com/jhamrick/emacs/blob/macs-and-emacs-post/.emacs.d/settings/python-settings.el
;; https://github.com/jhamrick/emacs/blob/master/.emacs.d/settings/python-settings.el
;; http://stackoverflow.com/questions/17255940/clear-steps-to-install-pymacs-with-emacs-24
;; http://www.idryman.org/blog/2013/03/13/python-setup-on-mac-osx/
;; http://stackoverflow.com/questions/1259873/how-can-i-use-emacs-flymake-mode-for-python-with-pyflakes-and-pylint-checking-co

;; Set PYTHONPATH, because we don't load .bashrc
;;(setenv "PYTHONPATH" "/usr/local/lib/python2.7/site-packages:")
(exec-path-from-shell-copy-env "PYTHONPATH")

(include-plugin "python-mode")
(require 'python-mode)

(include-plugin "pymacs")
(require 'pymacs)

;; add path to python-mode and pymacs
(add-to-list 'load-path (concat emacs-root "el-get/pymacs"))
(add-to-list 'load-path (concat emacs-root "el-get/python-mode"))
(setq py-install-directory (concat emacs-root "el-get/python-mode"))

;; use python-mode for .py files 
(add-to-list 'auto-mode-alist '("\.py\'" . python-mode))

(setq-default py-shell-name "/usr/local/bin/ipython")
(setq-default py-which-bufname "IPython")

;; (setq-default py-python-command-args
;;               (if (system-is-mac)
;;                   '("--gui=osx" "--pylab=osx" "--colors" "Linux")
;;                 (if (system-is-linux)
;;                     '("--gui=wx" "--pylab=wx" "--colors" "Linux")
;;                   '())))

(setq py-force-py-shell-name-p 1)

;; ;; switch to the interpreter after executing code
;; (setq py-shell-switch-buffers-on-execute-p t)
;; (setq py-switch-buffers-on-execute-p t)
;; ;; don't split windows
;; (setq py-split-windows-on-execute-p nil)

;; ;; try to automagically figure out indentantion
;; (setq py-smart-indentation t)

;; pymacs
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" -1 t)
(autoload 'pymacs-exec "pymacs" -1 t)
(autoload 'pymacs-load "pymacs" -1 t)
(autoload 'pymacs-autoload "pymacs")
(setq py-load-pymacs-p 1)
(setq py-complete-set-keymap-p 1)

;; ropemacs
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")

;; pyflakes flymake integration
;; http://stackoverflow.com/a/1257306/347942
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list (concat emacs-root "bin/pycheckers") (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (unless (eq buffer-file-name nil) (flymake-mode 1))))

;; add F9 and S-F9 keybindings
(add-hook 'python-mode-hook 'my-python-mode-hook)
;; (defun my-python-mode-hook ()
;;   ;; compile file with F9
;;   (define-key python-mode-map (kbd "f9") 'py-execute-buffer)
;;   ;; compile file with S-F9
;;   (define-key python-mode-map (kbd "S-<f9>") 'py-execute-region)
;;   )

;; add F9 and S-F9 keybindings
(add-hook 'python-mode-hook 'my-python-mode-hook)
(defun my-python-mode-hook ()
  ;; compile file with F9
  (define-key python-mode-map [f9]
    (lambda()
      (interactive)
      (progn
        (py-execute-buffer)))))

(provide 'python-settings)


