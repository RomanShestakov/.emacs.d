;;; custom-function.el --- Custom functions

;;; Commentary:

;;; Code:

;;;###autoload
(defun system-is-mac ()
  "*Check is the system in darwin-based."
  (interactive)
  (string-equal system-type "darwin"))

;;;###autoload
(defun system-is-linux ()
  "*Check is the system is linux."
  (interactive)
  (string-equal system-type "gnu/linux"))

(provide 'custom-functions)

;;; custom-functions.el ends here
