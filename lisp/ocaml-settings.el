;;; ocaml-settings.el --- provide settings for ocaml tuareg mode.

;;; Commentary:
;;; https://github.com/diml/utop
;;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
;;; https://github.com/the-lambda-church/merlin/wiki/emacs-from-scratch

;;; Code:

;; Ocaml
(use-package utop
  :ensure t)

(use-package merlin
  :ensure t)

(use-package tuareg
  :ensure t
  :config
  (setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                auto-mode-alist))
  (autoload 'utop "utop" "Toplevel for Ocaml" t)
  (add-hook 'tuareg-mode-hook 'merlin-mode)
  (setq merlin-use-auto-complete-mode t)
  (setq merlin-error-after-save nil))

;; -- Tweaks for OS X -------------------------------------
;; Tweak for problem on OS X where Emacs.app doesn't run the right
;; init scripts when invoking a sub-shell
(cond
 ;; macosx
 ((eq window-system 'ns)
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

;; opam and utop setup
(dolist
    (var (car (read-from-string
               (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; Update the emacs path
(setq exec-path (split-string (getenv "PATH") path-separator))

;; Update the emacs load path
(push (concat (getenv "OCAML_TOPLEVEL_PATH") "/../../share/emacs/site-lisp") load-path)

(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)
(setq tuareg-font-lock-symbols t)

;; Indent `=' like a standard keyword.
(setq tuareg-lazy-= t)
;; Indent [({ like standard keywords.
(setq tuareg-lazy-paren t)
;; No indentation after `in' keywords.
(setq tuareg-in-indent 0)

;; -- merlin setup ---------------------------------------
(setq opam-share (substring (shell-command-to-string "opam config var share") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

;; Enable Merlin for ML buffers
(add-hook 'tuareg-mode-hook 'merlin-mode)

;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
;; by spaces.
(define-key merlin-mode-map
  (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
(define-key merlin-mode-map
  (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
(set-face-background 'merlin-type-face "#88FF44")

;; -- enable auto-complete -------------------------------
;; Not required, but useful along with merlin-mode
(require 'auto-complete)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)

;; add F9 and S-F9 binding to eval a buffer or selected expr
(defun my-ocaml-mode-hook ()
  ;;(define-key tuareg-mode-map [f9] 'tuareg-eval-buffer))
  (define-key python-mode-map [S-f9] 'utop-eval-region)
  (define-key tuareg-mode-map [f9] 'utop-eval-buffer))

;; add hooks to tuareg-mode
(add-hook 'tuareg-mode-hook 'my-ocaml-mode-hook)

(provide 'ocaml-settings)

;;; ocaml-settings.el ends here
