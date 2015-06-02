;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

(eval-when-compile (require 'cl))

;; Bootstrap package management
(require 'package)
(setq package-enable-at-startup nil)
;; melpa url must have a trailing "/" at the end
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
;; allow to remove minor modes from status line
(require 'diminish)
(require 'bind-key)

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
;; allows to move borders between windows
(use-package move-border
  :load-path "lisp/move-border"
  :bind (("M-S-<up>" . move-border-up)
         ("M-S-<down>" . move-border-down)
         ("M-S-<left>" . move-border-left)
         ("M-S-<right>" . move-border-right)))

;; enable flycheck
(use-package flycheck
  :ensure t
  :defer 5
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

;; move-text mode
;; move a line with M-up/down
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

;; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    ;; update fast sources immediately (doesn't).
    (setq helm-idle-delay 0.0
          ;; this actually updates things
          helm-input-idle-delay 0.01
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("M-t" . helm-for-files)
         ("C-x c o" . helm-occur)
         ;; TODO: install helm-swoop
         ;;("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

;; projectile
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(use-package projectile
  :ensure t
  :init (projectile-global-mode)
  :diminish projectile-mode
  :config
  (progn
    (setq projectile-enable-caching t)
    ;;(setq projectile-require-project-root nil)
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (define-key projectile-command-map (kbd "g") #'projectile-grep)
    (setq projectile-completion-system 'helm)))
(use-package helm-projectile
  :ensure t
  :defer t)

;; Elisp go-to-definition with M-. and back again with M-,
(use-package elisp-slime-nav
  :ensure t
  :defer t
  :diminish elisp-slime-nav-mode
  :bind (("M-." . elisp-slime-nav-find-elisp-thing-at-point)
         ("M-," . pop-tag-mark))
  :config
  (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t))))

;; Paredit
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)))

;; apply general emacs customisation settings
(use-package general-settings)
(use-package erlang-settings)
(use-package python-settings)
(use-package color-theme-settings)

;; (require 'ido-settings)
;; (require 'elisp-slime-nav-settings)
;; (require 'yasnippet-settings)
;; (require 'virtualenv-settings)
;; (require 'prolog-settings)
;; (require 'elixir-settings)
;; (require 'ctag-settings)
;; (require 'projectile-settings)
;; (require 'fill-column-indicator-settings)

;; http://stackoverflow.com/questions/26171265/emacs-keyboard-bindings-on-os-x-iterm2
;; hardcode keybinding to make emacs work with iTerm2
;; otherwise moving border between windows doesn't work in linux terminal started from iterm
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/tmp/autosaves/\\1" t))))
 '(safe-local-variable-values (quote ((project-venv-name . "coursera-python")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
