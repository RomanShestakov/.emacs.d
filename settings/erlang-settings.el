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

(include-plugin "distel")
(let ((distel-dir (concat emacs-root "el-get/distel/elisp")))
  (unless (member distel-dir load-path)
    (setq load-path (append load-path (list distel-dir)))))

(require 'distel)
(distel-setup)

;; https://groups.google.com/forum/#!topic/erlang-russian/Y1PlEMyJ-P0
(defun directory-sub-dirs (dir suffix)
  "*Find all sub-directories of DIR, append SUFFIX to them, return as a list."
  (if (file-accessible-directory-p dir)
      (let ((dir (directory-file-name dir))
            (dirs '())
            (files (directory-files dir nil nil t)))
        (dolist (file files)
          (unless (member file '("." ".."))
            (let ((file (concat dir "/" file "/" suffix)))
              (when (file-directory-p file)
                (setq dirs (cons file dirs))))))
        dirs)
    ()))

;; setup flymake as flyckeck does not include dirs
;; https://github.com/flycheck/flycheck/pull/178
;; (require 'erlang-flymake)
;; (setq erlang-flymake-get-include-dirs-function
;;       (lambda ()
;;         (append
;;          (directory-sub-dirs (erlang-flymake-get-app-dir) "")
;;          (directory-sub-dirs (concat (erlang-flymake-get-app-dir) "deps") "include"))))

;; https://github.com/legoscia/dotemacs/blob/master/dotemacs.org
(defvar mh-erlang-flymake-code-path-dirs (list "../../*/ebin")
  "List of directories to add to code path for Erlang Flymake.
Wildcards are expanded.")

(defun mh-simple-get-deps-code-path-dirs ()
  "*Why complicate things?"
  (and (buffer-file-name)
       (let ((default-directory (file-name-directory (buffer-file-name))))
         (apply 'append (mapcar 'file-expand-wildcards mh-erlang-flymake-code-path-dirs)))))

(defun mh-simple-get-deps-include-dirs ()
  "*Add include."
  (list "../include" "../src"))

(setq erlang-flymake-get-code-path-dirs-function 'mh-simple-get-deps-code-path-dirs
      erlang-flymake-get-include-dirs-function 'mh-simple-get-deps-include-dirs)


;; Some Erlang customizations
(add-hook 'erlang-mode-hook
      (lambda ()
        ;; when starting an Erlang shell in Emacs, default in the node name
        (setq inferior-erlang-machine-options '("-sname" "emacs"))
        ;; add Erlang functions to an imenu menu
        (imenu-add-to-menubar "imenu")
        ;;(define-key erlang-mode-map [f5] 'compile)
        ;;(define-key erlang-mode-map (kbd "\C-c\C-dH") 'erlang-man-function)
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

(defun my-erlang-shell-display()
  "*Override existing erlang-shell-display to make sure that
erlang shell is always started from the root of the project. Root
project should have .erlang in it."
  (interactive)
  (erlang-shell-display)
  )

;; add hooks to erlang-mode
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(defun my-erlang-mode-hook ()
  "*When starting an Erlang shell in Emacs, default in the node name."
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; compile file with F9
  (define-key erlang-mode-map [f9] 'erlang-compile)
  (define-key erlang-mode-map (kbd "C-c C-z") 'my-erlang-shell-display)
  )

(provide 'erlang-settings)

;;; erlang-settings.el ends here
