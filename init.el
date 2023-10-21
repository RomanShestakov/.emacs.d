;; init.el --- entry point for configuration

;;; Commentary:
;; the root tree of calls to various modes

;;; Code:

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

;; use local melpa mirror
;; to create local repo:
;; M-x elpamr-create-mirror-for-installed
(require 'elpa-mirror)
(setq elpamr-default-output-directory (concat (file-name-as-directory emacs-root) "myelpa/"))
;; url must have a trailing "/" at the end
;; by default use local melpa - this forces to use local mirror of packages instead of melpa to avoid breaks
(setq package-archives '(("myelpa" . "~/.emacs.d/myelpa/")))
(setq package-archives '(("myelpa" . (symbol-value 'elpamr-default-output-directory))))
;; uncomment below if need to reload packages from global melpa
(setq package-archives '(("melpa" . "https://melpa.org/packages/")))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

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

;; ;; magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))
;; ;;(setq magit-last-seen-setup-instructions "1.4.0")

;; display windows numbers
;; move between windows with M-<number>
(use-package window-number
  :ensure t
  :config
  (window-number-mode 1)
  (window-number-meta-mode 1))

;; enable flycheck
(use-package flycheck
  :ensure t
  :defer 5
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
         ("C-x C-f" . helm-find-files)
         ;; TODO: install helm-swoop
         ;;("C-x c s" . helm-swoop)
         ;; ("C-x c y" . helm-yas-complete)
         ;; ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))

;; projectile
;; https://github.com/lunaryorn/.emacs.d/blob/master/init.el
(use-package projectile
  :ensure t
  :init (projectile-mode)
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


;; get company mode
;; company is text completion mode
(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode t)
  ;;(setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

;; yaml mode
(use-package yaml-mode
  :ensure t
  :defer t)

;; apply general emacs customisation settings
(use-package general-settings)
(use-package color-theme-settings)
(use-package elisp-settings)
;; run once to install tree-sitter
;;(use-package tree-sitter-settings)
;; requires setting "ERLANG_HOME"
(use-package erlang-settings)
;;(use-package elixir-settings)
;(use-package ocaml-settings)
(use-package python-settings)
;;(use-package rust-settings)
(use-package cpp-settings)
(use-package typescript-settings)
;; (require 'prolog-settings)

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
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(auto-save-file-name-transforms (quote ((".*" "~/tmp/autosaves/\\1" t))))
;;  '(package-selected-packages
;;    (quote
;;     (rtags company-rtags flycheck-rtags edts cmake-ide cargo window-number utop use-package tuareg racer org-repo-todo opam ocp-indent multi-term move-text magit jedi helm-projectile flycheck-rust flycheck-ocaml flx-ido exec-path-from-shell elisp-slime-nav)))
;;  '(safe-local-variable-values
;;    (quote
;;     ((cmake-ide-build-dir . "/Users/romanshestakov/development/cpp/temp-conversion")
;;      (project-venv-name . "ros")
;;      (project-venv-name . "coursera-python"))))
;;  '(send-mail-function (quote mailclient-send-it)))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line
;; (put 'upcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rainbow-mode treesit-auto typescript-mode web-mode json-mode ivy-erlang-complete cmake-mode inf-elixir elixir-ts-mode elixir-mode yasnippet-snippets rustic gnu-elpa-keyring-update json-rpc flymake-cppcheck eglot-jl flymake-go cmake-project yaml-mode window-number org-repo-todo multi-term move-text magit jedi helm-projectile flycheck-rtags flycheck-irony flx-ido exec-path-from-shell elisp-slime-nav edts company-irony))
 '(safe-local-variable-values
   '((cmake-ide-build-dir . "/Users/romanshestakov/development/cpp/temp-conversion")))
 '(warning-suppress-types '((comp) (use-package) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
