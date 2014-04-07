;;; color-theme-settings.el --- custom color scheme

;;; Commentary:
;;; Code:

(require 'custom-functions)

;; load paths to favorite themes
(add-to-list 'custom-theme-load-path (concat emacs-root "gruber-darker-theme"))

;; ;; at home use solarized (on mac), for anything else use gruber-dark
;; (if (system-is-mac)
;;     (progn
;;       (load-theme 'solarized-dark t)
;;       (setq solarized-termcolors 256))
;;   (load-theme 'gruber-darker t))

;; ;; load gruber-darker
(load-theme 'gruber-darker t)

;; set up fonts
(if (system-is-mac)
    (progn
      (set-default-font "-apple-inconsolata-medium-r-normal--13-130-72-72-m-130-iso10646-1")
      (setq mac-allow-anti-aliasing t)))

;;(set-default-font "Inconselata-13")
;;(set-default-font "Monaco-13")

(provide 'color-theme-settings)

;;; color-theme-settings.el ends here
