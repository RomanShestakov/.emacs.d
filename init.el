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
;; allows to move border between windows
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

;; apply general customisation settings
(require 'general-settings)

;; add modes with customized settings
(require 'erlang-settings)
(require 'python-settings)
(require 'key-binding-settings)
(require 'color-theme-settings)
(require 'helm-settings)
(require 'ido-settings)

;; (require 'org-mode-settings)
;; (require 'elisp-slime-nav-settings)
;; (require 'flycheck-mode-settings)
;; (require 'yasnippet-settings)
;; (require 'magit)
;; (require 'virtualenv-settings)
;; (require 'multi-term-settings)

;;(require 'prolog-settings)
;;(require 'elixir-settings)
;;(require 'ctag-settings)
;;(require 'projectile-settings)
;;(require 'fill-column-indicator-settings)

;;; init.el ends here

