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

;;;###autoload
(defun close-all-buffers ()
  "*Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;;;###autoload
(defun jump-match-paren (arg)
  "Go to the matching parenthesis."
  (interactive "p")
  (cond ((looking-at "\\s\(\\|\\s\[") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)\\|\\s\]") (forward-char 1) (backward-list 1))
        (t (back-to-indentation))
        ))

(provide 'custom-functions)

;;; custom-functions.el ends here
