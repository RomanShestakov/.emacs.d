;;; flyckeck-mode-settings.el --- a wrapper for flycheck mode

;;; Commentary:
;; enable global mode for flycheck

;;; Code:

(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'flycheck-mode-settings)

;;; flycheck-mode-settings.el ends here
