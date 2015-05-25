;;; erlang-settings.el --- provide settings for erlang mode.

;;; Commentary:
;; https://github.com/massemanet/distel.git

;;; Code:

(eval-when-compile (require 'cl))

;; http://stackoverflow.com/questions/4356472/emacs-per-file-customization
(defmacro* when-let ((var value) &rest body)
  `(let ((,var ,value))
     (when ,var ,@body)))

;; set path to erlang install load a path to elang lisp as distel
;; install in el-get distel package depends on it if root is nil or
(defvar erlang-root (getenv "ERL_TOP")
  "*Path to Erlang installation. Env var ERL_TOP needs to be set in bash environment.")
;(defvar erlang-root "/usr/local/lib/erlang")

;; add erlang bin dir to exec-path
(setq exec-path (cons (concat (file-name-as-directory erlang-root) "bin") exec-path))

;; add "lib" to erlang-root
(defvar erlang-root-dir
  (concat (file-name-as-directory erlang-root) "lib")
  "*Path to erlang lib.")

;; find the name of tools- directory
(defun get-erlang-tools-dir-name ()
  "*Get the name of tools dir in current erlang installation."
  (car (let ((default-directory erlang-root-dir))
         (file-expand-wildcards "tools-*"))))

;; build the full absolute name to erlang tools
(defun get-full-path-to-erlang-tools-dir ()
  "*Get full name of erlang tools dir."
  (concat (file-name-as-directory
           (concat (file-name-as-directory erlang-root-dir) (get-erlang-tools-dir-name)))
          "emacs"))

(defun erlang-path-init()
  "*Sets the paths to erlang mode."
  (add-to-list 'load-path (get-full-path-to-erlang-tools-dir)))

;; ;; functions to locate project root

;; (defvar erlang-settings-project-root-files-bottom-up
;;   '(".projectile" ; projectile project marker
;;     ".erlang"     ; erlang
;;     )
;;   "A list of files considered to mark the root of a project.
;; This root files pattern overrides discovery of any root files
;; pattern that would have found a project root in a subdirectory.")

;; (defvar erlang-settings-project-root-files-functions
;;   '(erlang-setting-project-root-bottom-up
;;     ;;root-top-down
;;     ;;root-top-down-recurring
;;     )
;;   "A list of functions for finding project roots.")

;; (defun erlang-settings-project-root-bottom-up (dir &optional list)
;;   "Identify a project root in DIR by looking at `erlang-settings-project-root-files-bottom-up'.
;; Returns a project root directory path or nil if not found."
;;   (--reduce-from
;;    (or acc
;;        (locate-dominating-file dir it))
;;    nil
;;    (or list erlang-settings-project-root-files-bottom-up)))


;; (defun erlang-settings-project-root ()
;;   "Retrieves the root directory of a project if available.
;; The current directory is assumed to be the project's root otherwise."
;;   (erlang-settings-project-root-files-functions))


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

(defun flymake-compile-script-path (path)
  "*PATH to compile script."
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list path (list local-file))))

;; path to syntaxerl binary used by flymake for erlang syntax checking
(defun flymake-syntaxerl ()
  "*Syntaxerl script used for erlang syntax checking."
  (flymake-compile-script-path (concat emacs-root "bin/syntaxerl")))

;; load erlang paths and init erlang-mode
(erlang-path-init)
(load "erlang-start" 'no-error)

(eval-after-load "erlang"
  '(progn
     (load "distel" :noerror)
     (eval-after-load "distel"
       '(progn
          (distel-setup)
          (add-hook 'erlang-mode-hook
                    (lambda ()
                      ;; when starting an Erlang shell in Emacs, default in the node name
                      (setq inferior-erlang-machine-options '("-sname" "emacs"))))

     (setq erl-nodename-cache (make-symbol (concat "emacs")))

     ;; A number of the erlang-extended-mode key bindings are useful in the shell too
     (defconst distel-shell-keys
       '(("\C-\M-i"   erl-complete)
         ("\M-?"      erl-complete)
         ("\M-."      erl-find-source-under-point)
         ("\M-,"      erl-find-source-unwind)
         ("\M-*"      erl-find-source-unwind))
       "Additional keys to bind when in Erlang shell.")

     ;; define auto erlang mode for these files/extensions.
     (add-to-list 'auto-mode-alist '(".*\\.app\\'" . erlang-mode))
     (add-to-list 'auto-mode-alist '(".*app\\.src\\'" . erlang-mode))
     (add-to-list 'auto-mode-alist '(".*\\.config\\'" . erlang-mode))
     (add-to-list 'auto-mode-alist '(".*\\.rel\\'" . erlang-mode))
     (add-to-list 'auto-mode-alist '(".*\\.script\\'" . erlang-mode))
     (add-to-list 'auto-mode-alist '(".*\\.escript\\'" . erlang-mode))
     
     (add-hook 'erlang-shell-mode-hook
               (lambda ()
                 ;; add some Distel bindings to the Erlang shell
                 (dolist (spec distel-shell-keys)
                   (define-key erlang-shell-mode-map (car spec) (cadr spec)))))))

     ;; add hooks to erlang-mode
     (add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

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
     ))

(provide 'erlang-settings)

;;; erlang-settings.el ends here
