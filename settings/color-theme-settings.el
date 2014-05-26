;;; color-theme-settings.el --- custom color scheme

;;; Commentary:
;;; Code:

(defvar emacs-root)

;; load gruber-darker
(if (>= emacs-major-version 23)
    '(progn
       ;; load paths to favorite themes
       (add-to-list 'custom-theme-load-path
                    (concat (file-name-as-directory emacs-root) "gruber-darker-theme"))
       (load-theme 'gruber-darker t))
  (load "color-theme-gruber-darker")
  (eval-after-load "color-theme-gruber-darker"
    '(progn
       (color-theme-gruber-darker))))

;; set up fonts
;; http://superuser.com/questions/210555/emacs-font-settings-not-working-in-new-frame
(autoload 'system-is-mac "custom-functions" t)
(if (system-is-mac)
    (progn
      (add-to-list 'default-frame-alist
                   '(font . "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1"))
      (setq mac-allow-anti-aliasing t)))

;;(set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
;;(set-default-font "Inconselata-13")
;;(set-default-font "Monaco-13")

(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
