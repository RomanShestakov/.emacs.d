;;; virtualenv-settings.el --- emacs virtualenvwrapper

;;; Commentary:
;;; https://github.com/porterjamesj/virtualenvwrapper.el
;;; youtube videos on python powertools:
;;; https://www.youtube.com/watch?v=IX-v6yvGYFg
;;; https://www.youtube.com/watch?v=UcbUXq0wd-8
;;; Code:

;; autoload venv-with-virtualenv because otherwise venv-mkenv fails
(autoload 'venv-with-virtualenv "virtualenvwrapper" 'macro)

(eval-after-load "virtualenvwrapper"
  '(progn
     (venv-initialize-interactive-shells)
     (setq venv-location (concat (file-name-as-directory (getenv "HOME")) ".virtualenvs"))
     ;; add a hook to python-mode to active virt-env for this project
     ;; see section Automatically activating a virtualenv in a particular project
     ;; https://github.com/porterjamesj/virtualenvwrapper.el.
     ;; Need to add file .dir-locals.el in the python project root dir
     ;; ((python-mode . ((project-venv-name . "myproject-env"))))
     (add-hook 'python-mode-hook (lambda()
                                   (hack-local-variables)
                                   (when (boundp 'project-venv-name)
                                     (venv-workon project-venv-name))))

     ;; install ipython and ipdb for each virtual-env
     (add-hook 'venv-mkvirtualenv
               (lambda() (shell-command "pip install ipython ipdb")))
))

(provide 'virtualenv-settings)

;;; virtualenv-settings.el ends here
