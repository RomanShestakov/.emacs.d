;;; puppet-mode-settings.el --- puppet-mode to edit puppet definitions

;;; Commentary:

;;; Code:

(require 'custom-functions)
(include-plugin "puppet-mode")
(require 'puppet-mode)

;; assosiate .pp file extension with puppet-mode
(add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode))

(provide 'puppet-mode-settings)

;;; puppet-mode-settings.el ends here
