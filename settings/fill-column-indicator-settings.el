;;; fill-column-indicator-settings.el --- fill column indicator

;;; Commentary:
;; https://github.com/alpaker/Fill-Column-Indicator

;;; Code:

(include-plugin "fill-column-indicator")
(require 'fill-column-indicator)

(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)
(setq fci-rule-column 80)

(provide 'fill-column-indicator-settings)

;;; fill-column-indicator-settings.el ends here
