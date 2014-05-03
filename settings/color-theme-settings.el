;;; color-theme-settings.el --- custom color scheme

;;; Commentary:
;;; Code:

(require 'custom-functions)

;; load paths to favorite themes
(add-to-list 'custom-theme-load-path (concat (file-name-as-directory emacs-root) "gruber-darker-theme"))

;; ;; at home use solarized (on mac), for anything else use gruber-dark
;; (if (system-is-mac)
;;     (progn
;;       (load-theme 'solarized-dark t)
;;       (setq solarized-termcolors 256))
;;   (load-theme 'gruber-darker t))

;; ;; load gruber-darker
(load-theme 'gruber-darker t)

;; set up fonts
;; http://superuser.com/questions/210555/emacs-font-settings-not-working-in-new-frame
(if (system-is-mac)
    (progn
      (add-to-list 'default-frame-alist
                   '(font . "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1"))
      (setq mac-allow-anti-aliasing t)))

;;(set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
;;(set-default-font "Inconselata-13")
;;(set-default-font "Monaco-13")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
