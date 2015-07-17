;;; ocaml-settings.el --- provide settings for ocaml tuareg mode.

;;; Commentary:
;;; https://github.com/diml/utop
;;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
;;; https://github.com/the-lambda-church/merlin/wiki/emacs-from-scratch
;;; http://the-lambda-church.github.io/merlin/
;;; http://mort.io/blog/2013/10/13/21st-century-ide/
;;; http://www.lunaryorn.com/2014/12/03/generic-syntax-checkers-in-flycheck.html

;;; Code:

;; Ocaml
(use-package opam
  :ensure t
  :init (opam-init))

(use-package utop
  :ensure t)

(use-package ocp-indent
  :ensure t)

(use-package merlin
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook #'merlin-mode)
  :config
  (progn
    (setq merlin-use-auto-complete-mode 'easy)
    ;;(setq merlin-report-warnings nil)
    (setq merlin-error-after-save nil)))

;; Check OCaml code with Merlin
(use-package flycheck-ocaml
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'merlin
    (flycheck-ocaml-setup)))

(use-package tuareg
  :ensure t
  :defer t
  :config
  (progn
    (setq auto-mode-alist
          (append '(("\\.ml[ily]?$" . tuareg-mode)
                  ("\\.topml$" . tuareg-mode))
                  auto-mode-alist))
    ;; (setq tuareg-in-indent 0)
    ;; (setq tuareg-use-smie nil)
    ;; (setq indent-line-function 'ocp-indent-line)
    ;; (setq tuareg-font-lock-symbols t)
    (autoload 'utop "utop" "Toplevel for Ocaml" t)))


;; opam and utop setup
(dolist
    (var (car (read-from-string
               (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; add F9 and S-F9 binding to eval a buffer or selected expr
(defun my-ocaml-mode-hook ()
  ;;(define-key tuareg-mode-map [f9] 'tuareg-eval-buffer))
  (define-key python-mode-map [S-f9] 'utop-eval-region)
  (define-key tuareg-mode-map [f9] 'utop-eval-buffer))

;; add hooks to tuareg-mode
(add-hook 'tuareg-mode-hook 'my-ocaml-mode-hook)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(add-hook 'tuareg-mode-hook 'flycheck-mode)

(provide 'ocaml-settings)

;;; ocaml-settings.el ends here
