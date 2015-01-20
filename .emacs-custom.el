;;; .emacs-custom.el --- entries added by custom mode

;;; Commentary:
;;

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/tmp/autosaves/\\1" t))))
 '(safe-local-variable-values
   (quote
    ((eval when
           (fboundp
            (quote rainbow-mode))
           (rainbow-mode 1))
     (project-venv-name . "mdf")
     (project-venv-name . "proj1")
     (project-venv-name . "pyexp")
     (project-venv-name . "ufo")
     (checkdoc-minor-mode . t)
     (require-final-newline . t)
     (mangle-whitespace . t)))))

;;; .emacs-custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

