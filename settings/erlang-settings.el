;;; erlang-settings.el --- provide settings for erlang mode.

;;; Commentary:
;; https://github.com/massemanet/distel.git

;;; Code:

;; set paths to erlang libs
(require 'erlang-path-settings)
(require 'erlang-start)

;; set path to distel root
(let ((distel-dir (concat emacs-root "el-get/distel/elisp")))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))

(autoload 'distel "distel" t)
(eval-after-load 'distel (distel-setup))

;; Some Erlang customizations
(add-hook 'erlang-mode-hook
      (lambda ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")))

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
    ("\M-*"      erl-find-source-unwind))
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

(defun my-erlang-mode-hook ()
  "*When starting an Erlang shell in Emacs, default in the node name."
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; compile file with F9
  (define-key erlang-mode-map [f9] 'my-erlang-compile)
  (define-key erlang-mode-map (kbd "C-c C-z") 'my-erlang-shell-display))

;; add hooks to erlang-mode
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;; disable flycheck mode for erlang as flycheck doesnt' recognise includes
(setq flycheck-disabled-checkers '(erlang))

;; define where put beam files.
(setq erlang-compile-outdir "../ebin")

;; flymake syntax checking.
;; setup syntaxerl to do error checking
;; https://github.com/ten0s/syntaxerl
(autoload 'flymake "flymake" t)
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

(provide 'erlang-settings)

;;; erlang-settings.el ends here



