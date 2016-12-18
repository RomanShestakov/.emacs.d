;;; ocaml-settings.el --- provide settings for ocaml tuareg mode.

;;; Commentary:
;;; https://github.com/diml/utop
;;; https://github.com/realworldocaml/book/wiki/Installation-Instructions
;;; https://github.com/the-lambda-church/merlin/wiki/emacs-from-scratch
;;; http://the-lambda-church.github.io/merlin/
;;; http://mort.io/blog/2013/10/13/21st-century-ide/
;;; http://www.lunaryorn.com/2014/12/03/generic-syntax-checkers-in-flycheck.html
;;; https://gist.github.com/unhammer/c1ac7320141f09ac38e0
;;; http://blog.nullspace.io/beginners-guide-to-ocaml-beginners-guides.html

;;; Code:

;; (defun opam-shell-command-to-string (command)
;;   "Similar to shell-command-to-string, but returns nil unless the process
;;   returned 0 (shell-command-to-string ignores return value)"
;;   (let* ((return-value 0)
;;          (return-string
;;           (with-output-to-string
;;             (setq return-value
;;                   (with-current-buffer standard-output
;;                     (process-file shell-file-name nil t nil
;;                                   shell-command-switch command))))))
;;     (if (= return-value 0) return-string nil)))

;; (setq opam-share
;;   (let ((reply (opam-shell-command-to-string "opam config var share")))
;;     (when reply (substring reply 0 -1))))

(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))

;; (require 'ocp-indent)
;; (require 'merlin)
;; (add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; OPAM-installed tools automated detection and initialisation

;; (require 'ocp-indent)

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
  (bind-key "M-." 'merlin-locate)
  (bind-key "M-," 'merlin-pop-stack)
  :config
  (progn
    (setq merlin-ac-setup 'easy)
    (setq merlin-report-warnings nil)
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

;; Tweak for problem on OS X where Emacs.app doesn't run the right
;; init scripts when invoking a sub-shell
(cond
 ((eq window-system 'ns) ; macosx
  ;; Invoke login shells, so that .profile or .bash_profile is read
  (setq shell-command-switch "-lc")))

;; opam and utop setup
(dolist
    (var (car (read-from-string
               (shell-command-to-string "opam config env --sexp"))))
  (setenv (car var) (cadr var)))

;; add F9 and S-F9 binding to eval a buffer or selected expr
(defun my-ocaml-mode-hook ()
  ;;(define-key tuareg-mode-map [f9] 'tuareg-eval-buffer))
  (define-key tuareg-mode-map [S-f9] 'utop-eval-region)
  (define-key tuareg-mode-map [f9] 'utop-eval-buffer))

;; add hooks to tuareg-mode
(add-hook 'tuareg-mode-hook 'my-ocaml-mode-hook)
(add-hook 'tuareg-mode-hook 'auto-complete-mode)
(add-hook 'tuareg-mode-hook 'flycheck-mode)

(provide 'ocaml-settings)

;;; ocaml-settings.el ends here
