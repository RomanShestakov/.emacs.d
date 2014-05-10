;;; elixir-settings.el --- elixir-mode settings

;;; Commentary:

;; elixir mode and mix integration
;; https://github.com/elixir-lang/emacs-elixir
;; https://github.com/tonini/elixir-mix.el
;;; Code:

(require 'elixir-mode)

;; Highlights *.elixir2 as well
(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))

;; enable elixir-mix
(require 'elixir-mix)
(global-elixir-mix-mode)

(setq elixir-mix-command "/usr/local/bin/mix")
(setq elixir-mix-buffer-name "*mix*")

;; add hooks to elixir-mode
(add-hook 'elixir-mode-hook 'my-elixir-mode-hook)
(defun my-elixir-mode-hook ()
  "*When starting an elixir shell in Emacs, default in the node name."
  ;;(setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; compile file with F9
  ;;(define-key elixir-mode-map [f9] 'my-elixir-compile)
  (define-key elixir-mode-map (kbd "C-c C-z") 'elixir-mode-iex)
  )

(provide 'elixir-settings)

;;; elixir-settings.el ends here
