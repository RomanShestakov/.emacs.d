;;; projectile.el --- project management

;;; Commentary:

;;; Code:

(require 'custom-functions)

(include-plugin "projectile")
(projectile-global-mode)
(add-hook 'python-mode 'erlang-mode)

(provide 'projectile-settings)

;;; projectile-settings.el ends here
