;;; erlang-path-settings.el --- sets paths to erlang mode.

;;; Commentary:
;; there is a split between erlang-path-settings and erlang settings because
;; installatin of distel mode depends on correct erlang paths
;; distel-mode is intalled with el-get.
;; So, first init.el runs erlang-path-settings, then el-gets installs
;; all required modes, including distel.  Then init runs erlang-settings
;; which sets all the required settings for erlang-mode

;;; Code:

;; paths to erlang libs are set in init.el

;; set path to erlang install
;; load a path to elang lisp as distel install in el-get distel package depends on it
(defvar erlang-root  "/usr/local/lib/erlang"
  ;(getenv "ERL_TOP")
  "*Path to Erlang installation.  Env var ERL_TOP needs to be set in bash environment.")
(defvar erlang-root-dir
  (concat erlang-root "/lib")
  "*Path to erlang lib.")
(setq exec-path (cons (concat erlang-root "/bin") exec-path))
(add-to-list 'load-path (concat erlang-root "/lib/tools-2.6.14/emacs"))


(provide 'erlang-path-settings)

;;; erlang-path-settings.el ends here



