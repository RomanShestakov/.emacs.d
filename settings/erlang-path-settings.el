;;; erlang-path-settings.el --- sets paths to erlang mode.

;;; Commentary:
;; there is a split between erlang-path-settings and erlang settings because
;; installatin of distel mode depends on correct erlang paths
;; distel-mode is intalled with el-get.
;; So, first init.el runs erlang-path-settings, then el-gets installs
;; all required modes, including distel.  Then init runs erlang-settings
;; which sets all the required settings for erlang-mode

;;; Code:

(eval-when-compile (require 'cl))

;; read value of env variable "ERL_TOP"
;; this var should be set in env
; export ERL_TOP=/usr/local/lib/erlang

(require 'exec-path-from-shell)
(exec-path-from-shell-copy-env "ERL_TOP")

;; set path to erlang install load a path to elang lisp as distel
;; install in el-get distel package depends on it if root is nil or
;; empty (because env ERL_TOP is not setup, default to standard erlang
;; location "/usr/local/lib/erlang"
(defvar erlang-root
  (let ((root (getenv "ERL_TOP")))
    (if (and (not root)
             (not (equal "" root)))
        root
      "/usr/local/lib/erlang"))
  "*Path to Erlang installation.
Env var ERL_TOP needs to be set in bash environment.  If ERL_TOP is not set, return  /usr/local/lib/erlang.")

;; add lib to erlang-root
(defvar erlang-root-dir
  (concat (file-name-as-directory erlang-root) "lib")
  "*Path to erlang lib.")

;; add erlang bin dir to exec-path
(setq exec-path (cons (concat (file-name-as-directory erlang-root) "bin") exec-path))

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

;; add erlang tools to load-path
(add-to-list 'load-path (get-full-path-to-erlang-tools-dir))

(provide 'erlang-path-settings)

;;; erlang-path-settings.el ends here
