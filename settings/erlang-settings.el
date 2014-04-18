;;; erlang-settings.el --- provide settings for erlang mode.

;;; Commentary:
;; https://github.com/massemanet/distel.git

;;; Code:

;; set paths to erlang libs
(defvar erlang-root-dir "/usr/local/lib/erlang/lib")
(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))
;; moved this to init as el-get distel package depends on this
;;(setq load-path  (cons "/usr/local/lib/erlang/lib/tools-2.6.13/emacs" load-path))

(require 'erlang-start)

;; set path to distel root
(include-plugin "distel")
(let ((distel-dir (concat emacs-root "el-get/distel/elisp")))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; define auto erlang mode for these files/extensions.
(add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
(add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
      (lambda ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")
))

(setq erl-nodename-cache
      (make-symbol
       (concat
        "emacs")))
        ;; Mac OS X uses "name.local" instead of "name", this should work
        ;; pretty much anywhere without having to muck with NetInfo
        ;; ... but I only tested it on Mac OS X.
       ;(car (split-string (shell-command-to-string "hostname"))))))

;; A number of the erlang-extended-mode key bindings are useful in the shell too
(defconst distel-shell-keys
  '(("\C-\M-i"   erl-complete)
    ("\M-?"      erl-complete)
    ("\M-."      erl-find-source-under-point)
    ("\M-,"      erl-find-source-unwind)
    ("\M-*"      erl-find-source-unwind)
    )
  "Additional keys to bind when in Erlang shell.")

(add-hook 'erlang-shell-mode-hook
          (lambda ()
            ;; add some Distel bindings to the Erlang shell
            (dolist (spec distel-shell-keys)
              (define-key erlang-shell-mode-map (car spec) (cadr spec)))))

;; http://stackoverflow.com/questions/4356472/emacs-per-file-customization
(defmacro* when-let ((var value) &rest body)
  `(let ((,var ,value))
     (when ,var ,@body)))

;; http://stackoverflow.com/questions/6367743/emacs-find-file-without-changing-working-directory
(defun my-erlang-shell-display()
  "*Override existing erlang-shell-display to make sure that
erlang shell is always started from the root of the project. Root
project should have .erlang in it."
  (interactive)
  (when-let (default-directory (locate-dominating-file default-directory ".erlang"))
            (erlang-shell-display)))

(defun my-erlang-compile()
  "*Override existing erlang-compile to make sure that
erlang shell is always started from the root of the project. Root
project should have .erlang in it."
  (interactive)
  (when-let (default-directory (locate-dominating-file default-directory ".erlang"))
            (erlang-compile)))

;; add hooks to erlang-mode
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  "*When starting an Erlang shell in Emacs, default in the node name."
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; compile file with F9
  (define-key erlang-mode-map [f9] 'my-erlang-compile)
  (define-key erlang-mode-map (kbd "C-c C-z") 'my-erlang-shell-display)
  )
   
;; disable flycheck mode for erlang as flycheck doesnt' recognise includes
(setq flycheck-disabled-checkers '(erlang))

;; add include directory to default compile path.
(defvar erlang-compile-extra-opts
  '(bin_opt_info debug_info (i . "../include") (i . "../deps") (i . "../../") (i . "../../../deps")))

;; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;; flymake syntax checking.
;; setup syntaxerl to do error checking
;; https://github.com/ten0s/syntaxerl
(require 'flymake)
;;(setq flymake-log-level 3)

(defun flymake-compile-script-path (path)
  "*PATH to compile script."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

(defun flymake-syntaxerl ()
  "*Script used to compile."
  (flymake-compile-script-path (concat emacs-root "bin/syntaxerl")))

(add-hook 'erlang-mode-hook
  '(lambda()
     (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.hrl\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.app.src\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.config\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.rel\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.script\\'" flymake-syntaxerl))
     (add-to-list 'flymake-allowed-file-name-masks '("\\.escript\\'" flymake-syntaxerl))
     ;; should be the last.
     (flymake-mode 1)
))

(provide 'erlang-settings)

;;; erlang-settings.el ends here
