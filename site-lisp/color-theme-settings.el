;;; color-theme-settings.el --- custom color scheme

;;; Commentary:
;;; Code:

(defvar emacs-root)

;; to get rid of reference to free variable
(eval-when-compile (defvar my-lisp-dir))

;; load paths to favorite themes
(add-to-list 'custom-theme-load-path (file-name-as-directory my-lisp-dir))
(load-theme 'gruber-darker t)

;; start rainbow-mode so to see color codes in color theme
(use-package rainbow-mode
  :hook (emacs-lisp-mode text-mode lisp-mode))

;; set up fonts
;; http://superuser.com/questions/210555/emacs-font-settings-not-working-in-new-frame
(autoload 'system-is-mac "custom-functions" t)
(if (system-is-mac)
    (progn
      (add-to-list 'default-frame-alist
                   '(font . "Inconsolata-14"))
      (setq mac-allow-anti-aliasing t)))

(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
