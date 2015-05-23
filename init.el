;;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

(eval-when-compile (require 'cl))

;; Bootstrap package management
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))

;; add root to all emacs-related stuff
;; and add load-paths to packages
;; see http://stackoverflow.com/questions/23324760/emacs-byte-compile-errors-on-the-first-require-statement
;; for the use of eval-and-compile
(eval-and-compile
  (defvar emacs-root
    (if (or (eq system-type 'cygwin)
            (eq system-type 'gnu/linux)
            (eq system-type 'linux)
            (eq system-type 'darwin))
        "~/.emacs.d/"    "z:/.emacs.d/")
    "Path to where EMACS configuration root is.")

  ;; set path to "~/.emacs/lisp dir for custom packages
  (defvar my-lisp-dir
    (concat (file-name-as-directory emacs-root) "lisp")
    "*Path to custom lisp lib.")

  ;; add subdirectories of root into load path
  (let ((default-directory emacs-root))
    (normal-top-level-add-subdirs-to-load-path)))

;; set PATH from env
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "ERL_TOP")
  (exec-path-from-shell-copy-env "PATHONPATH"))

;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-support-shift-select t)
  (setq org-completion-use-ido t)
  (use-package org-repo-todo :ensure t))

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
(setq magit-last-seen-setup-instructions "1.4.0")

;; display windows numbers
(use-package window-number
  :ensure t
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

;; enable multi-term
(use-package multi-term
  :ensure t
  :defer t
  :bind ("C-c t" . multi-term-next)
  :init
  (setq multi-term-program-switches "--login")
  (add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
  (setq multi-term-program "/bin/bash"
        term-unbind-key-list '("C-x"
                               "C-h"
                               "M-x"
                               "C-z")
        term-term-name "xterm-256color"))

;; https://github.com/ramnes/move-border
(use-package move-border
  :load-path "lisp/move-border")

;; enable flycheck
(use-package flycheck
  :ensure t
  :defer 5
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

;; move-text mode
(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

;; flx-ido
(use-package flx-ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1)
  ;; disable ido faces to see flx highlights
  (setq ido-use-faces nil)
  (setq ido-use-filename-at-point 'guess)
  (setq ido-create-new-buffer 'always)
  (setq ido-file-extensions-order '(".scala" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
  ;; You can disable the merging (the "looking in other directories" in ido vulgo) with
  ;; http://stackoverflow.com/questions/7479565/emacs-ido-mode-and-creating-new-files-in-directories-it-keeps-changing-the-dire
  (setq ido-auto-merge-work-directories-length -1)
  (flx-ido-mode 1))

;; projectile
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(use-package projectile
  :ensure t
  ;; :bind (("s-p" . projectile-find-file)
  ;;        ("s-b" . projectile-switch-to-buffer)
  ;;        ("s-F" . projectile-ag))
  :init (projectile-global-mode)
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t)
    (setq projectile-require-project-root nil)
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'helm)))
(use-package helm-projectile
  :ensure t
  :defer t)

;; apply general customisation settings
(require 'general-settings)

;; add modes with customized settings
(require 'erlang-settings)
(require 'python-settings)
(require 'key-binding-settings)
(require 'color-theme-settings)
(require 'helm-settings)

;; (require 'ido-settings)
;; (require 'elisp-slime-nav-settings)
;; (require 'yasnippet-settings)
;; (require 'virtualenv-settings)

;;(require 'prolog-settings)
;;(require 'elixir-settings)
;;(require 'ctag-settings)
;;(require 'projectile-settings)
;;(require 'fill-column-indicator-settings)

;;; init.el ends here
