; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

;; add root to all emacs-related stuff
;; and add load-paths to packages
;; see http://stackoverflow.com/questions/23324760/emacs-byte-compile-errors-on-the-first-require-statement
;; for the use of eval-and-compile
(eval-and-compile

  ;; user-emacs-directory is set by starting emacs with:
  ;; emacs --init-directory=~/.emacs.d_29 -nw
  ;; make emacs-root as a alias to user-emacs-directory
  (defvaralias 'emacs-root 'user-emacs-directory "Path to where EMACS configuration root is.")

  ;; set path to "~/.emacs/site-lisp dir for custom packages
  (defvar my-lisp-dir
    (concat (file-name-as-directory emacs-root) "site-lisp")
    "*Path to custom site-lisp lib.")

  ;; add subdirectories of root into load path
  (let ((default-directory emacs-root))
    (normal-top-level-add-subdirs-to-load-path)))

;; Bootstrap package management
(require 'package)
(setq package-enable-at-startup nil)
(require 'use-package)

;; use local melpa mirror
;; to create local repo:
;; M-x elpamr-create-mirror-for-installed
;; url must have a trailing "/" at the end
;; by default use local melpa - this forces to use local mirror of packages instead of melpa to avoid breaks
(defvar use-local-package-archive nil "Set to t if want to use local myelpa as package archive or nil otherwise.")

(use-package elpa-mirror
  :ensure nil
  :config
  (setq elpamr-default-output-directory (concat (file-name-as-directory emacs-root) "myelpa/"))
  (setq package-archives
        (if use-local-package-archive
	          `(("myelpa" . ,elpamr-default-output-directory))
          '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))))

;; allow to remove minor modes from status line
;;(require 'diminish)
(require 'bind-key)

;; see http://elpa.gnu.org/packages/gnu-elpa-keyring-update.html
;; enable gnu-elpa-keyring-update
;; to update signature keys
;; temporaraly turn off signature check
;;(setq package-check-signature nil)
(use-package gnu-elpa-keyring-update
  :ensure t)

;; set PATH from env
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "PATH")
  ;; (exec-path-from-shell-copy-env "ERL_TOP")
  ;; (exec-path-from-shell-copy-env "OCAML_TOPLEVEL_PATH")
  (exec-path-from-shell-copy-env "PYTHONPATH"))

;; org-mode
(use-package org
  :ensure t
  :config
  (setq org-support-shift-select t)
  ;(setq org-completion-use-ido t)
  (use-package org-repo-todo :ensure t))

;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
;; ;;(setq magit-last-seen-setup-instructions "1.4.0")

;; display windows numbers
;; move between windows with M-<number>
;; winum is a replacement for window-number mode
(use-package winum
  :ensure t
  :bind( :map winum-keymap
         ("M-0" . 'winum-select-window-0-or-10)
         ("M-1" . 'winum-select-window-1)
         ("M-2" . 'winum-select-window-2)
         ("M-3" . 'winum-select-window-3)
         ("M-4" . 'winum-select-window-4)
         ("M-5" . 'winum-select-window-5))
  :config (winum-mode)
  (setq winum-format "[ %s ]")
  (set-face-attribute 'winum-face nil :weight 'bold)
  (set-face-background 'winum-face "darkred"))

;; enable flycheck
(use-package flycheck
  :ensure t
  ;;:defer 5
  :init (global-flycheck-mode)
  :config
  (defalias 'flycheck-show-error-at-point-soon 'flycheck-show-error-at-point))

;; helm
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
;;    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; this was to fix issue with helm-find-files as file notification system is missing
    ;; FIXME - compile emacs --with-file-notification=inotify option
    ;; (setq helm-ff-use-notify nil)
    ;; From https://gist.github.com/antifuchs/9238468
    ;; update fast sources immediately (doesn't).
    ;; (setq helm-idle-delay 0.0
    ;;       ;; this actually updates things
    ;;       helm-input-idle-delay 0.01
    ;;       helm-yas-display-key-on-candidate t
    ;;       helm-quick-update t
    ;;       helm-M-x-requires-pattern nil
    ;;       helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("M-t" . helm-for-files)
         ("C-x c o" . helm-occur)
         ("C-x C-f" . helm-find-files)
         ;; TODO: install helm-swoop
         ;;("C-x c s" . helm-swoop)
         ;; ("C-x c y" . helm-yas-complete)
         ;; ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

;; get company mode
;; company is text completion mode
(use-package company
  :ensure t
  :config
  (global-company-mode t))

;;Eglot provides template based completion if the server supports snippet completion
;; and yasnippet is enabled before Eglot connects to the server. T
(use-package yasnippet
  :ensure t)

;; yaml mode
(use-package yaml-mode
  :ensure t
  :defer t)

;; apply general emacs customisation settings
(use-package general-settings)
(use-package color-theme-settings)
(use-package elisp-settings)
;(use-package tree-sitter-settings)

;; requires setting "ERLANG_HOME"
(use-package erlang-settings)
(use-package elixir-settings)
;(use-package ocaml-settings)
;;(use-package python-settings)
(use-package rust-settings)
(use-package cpp-settings)
(use-package typescript-settings)
;; (require 'prolog-settings)
(use-package ponylang-settings)

;; http://stackoverflow.com/questions/26171265/emacs-keyboard-bindings-on-os-x-iterm2
;; hardcode keybinding to make emacs work with iTerm2
;; otherwise moving border between windows doesn't work in linux terminal started from iterm
;; how to use all functional keys over putty: http://emacswiki.org/emacs/PuTTY
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])
(define-key input-decode-map "\e[U" [f9])
(define-key input-decode-map "\e[g" [S-f9])
(define-key input-decode-map "\e[s" [C-f9])

;; ;; Use M-i for imenu to show func definitions
;; (global-set-key (kbd "M-i") 'imenu)
;; ;; bind C-x r to rgrep command
;; (global-set-key (kbd "C-x r") 'rgrep)

;; ;; copy / cut lines without selecting them first
;; ;; http://emacs-fu.blogspot.de/2009/11/copying-lines-without-selecting-them.html
;; ;; if there is not selection, assume the operation should be applied to the whole line.
;; ;; in case of selection use the usual behavour
;; ;; Binded to M-w
;; (defadvice kill-ring-save (before slick-copy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (message "Copied line")
;;      (list (line-beginning-position) (line-beginning-position 2)))))

;; ;; Binded to C-w
;; (defadvice kill-region (before slick-cut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;     (if mark-active (list (region-beginning) (region-end))
;;       (list (line-beginning-position)
;;         (line-beginning-position 2)))))

;; uncomment in case if backspace is not working correctly
;; (normal-erase-is-backspace-mode 0)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((eval when (fboundp 'rainbow-mode) (rainbow-mode 1))))
 '(package-selected-packages
   '(toml-mode rust-playground lsp-ui lsp-mode rustic hl-todo elixir-ts-mode ivy-erlang-complete multiple-cursors elisp-slime-nav rainbow-mode yaml-mode yasnippet company helm flycheck winum magit org-repo-todo exec-path-from-shell gnu-elpa-keyring-update))
 '(safe-local-variable-values
   '((cmake-ide-build-dir . "/Users/romanshestakov/development/cpp/temp-conversion")))
 '(warning-suppress-types '((comp) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
