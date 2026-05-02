;;; python-settings.el --- customization for python-mode

;;; Commentary:
;; inspired by : https://ddavis.io/blog/python-emacs-4/
;; npm install -g basedpyright
;;; Code:

(require 'python)

(setq python-shell-interpreter "python3")

(add-to-list 'exec-path "/home/romanshestakov/anaconda3/bin")

;; corfu for in-buffer completion popup (works with eglot via completion-at-point)
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  :init
  (global-corfu-mode 1))

;; corfu uses child frames which don't work in terminal mode (-nw)
;(require 'quelpa-use-package)
(use-package corfu-terminal
  ;; :quelpa (corfu-terminal
  ;;          :fetcher git
  ;;          :url "https://codeberg.org/akib/emacs-corfu-terminal.git")
  :after corfu
  :demand t
  :config
  (unless (display-graphic-p)
    (corfu-terminal-mode 1)))

;; eglot is built-in since Emacs 29; no :ensure needed.
(use-package eglot
  :ensure nil
  :config
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'xref-go-back)
  (add-to-list 'eglot-server-programs
               `(python-base-mode
                 . ,(eglot-alternatives '(("basedpyright-langserver" "--stdio")
                                          ("pyright-langserver" "--stdio"))))))

;; ruff for formatting and import sorting on save
(use-package reformatter
  :ensure t
  :config
  (reformatter-define python-ruff-format
    :program "uvx"
    :args `("ruff" "format" "--stdin-filename" ,buffer-file-name "-"))
  (reformatter-define python-ruff-sort
    :program "uvx"
    :args `("ruff" "check" "--select" "I" "--fix"
            "--stdin-filename" ,buffer-file-name "-")))

;; auto-activate .venv in project root
(use-package pyvenv
  :ensure t)

(defun my-python-init ()
  (let* ((project (project-current))
         (project-root (when project (project-root project)))
         (venv-path (when project-root
                      (expand-file-name ".venv" project-root))))
    (when (and venv-path (file-directory-p venv-path))
      (pyvenv-activate venv-path)))
  (python-ruff-format-on-save-mode +1)
  (python-ruff-sort-on-save-mode +1))

(defun my-python-mode-hook ()
  (define-key python-mode-map [f9]   'python-shell-send-buffer)
  (define-key python-mode-map [S-f9] 'python-shell-send-region)
  (define-key python-mode-map [S-f1] 'python-insert-breakpoint)
  (setq-local indent-tabs-mode nil)
  (setq-local python-indent-offset 4)
  (setq-local tab-width 4))

(add-hook 'python-base-mode-hook 'eglot-ensure)
(add-hook 'python-base-mode-hook 'my-python-init)
(add-hook 'python-mode-hook 'my-python-mode-hook)
(add-hook 'python-mode-hook
          (lambda () (add-hook 'before-save-hook 'delete-trailing-whitespace nil t)))

;; helm-mode overrides completion-in-region-function globally; restore corfu's in python buffers
(add-hook 'python-base-mode-hook
          (lambda ()
            (setq-local completion-in-region-function #'corfu--complete-in-region)))

(provide 'python-settings)

;;; python-settings.el ends here
