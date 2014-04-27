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

;; set path to erlang install
;; load a path to elang lisp as distel install in el-get distel package depends on it
(defvar erlang-root
  (let ((root (getenv "ERL_TOP")))
    (if (and (not root)
             (not (equal "" root)))
        root
      "/usr/local/lib/erlang"))
  "*Path to Erlang installation.
Env var ERL_TOP needs to be set in bash environment.  If ERL_TOP is not set, return  /usr/local/lib/erlang.")

(defvar erlang-root-dir
  (concat erlang-root "/lib")
  "*Path to erlang lib.")
(setq exec-path (cons (concat erlang-root "/bin") exec-path))
(add-to-list 'load-path (concat erlang-root "/lib/tools-2.6.14/emacs"))


(provide 'erlang-path-settings)
(require 'erlang-path-settings)

;;; erlang-path-settings.el ends here



