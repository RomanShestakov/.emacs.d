;;; projectile.el --- project management

;;; Commentary:

;; https://www.youtube.com/watch?v=qpv9i_I4jYU
;; https://github.com/bbatsov/projectile

;;; Code:

(require 'projectile)
(projectile-global-mode)
(add-hook 'python-mode 'erlang-mode)

(provide 'projectile-settings)

;;; projectile-settings.el ends here
