;;; custom-functions2.el --- examples from gnu emacs extentions book

;;; Commentary:

;;; Code:


(provide 'custom-functions2)

;;; custom-functions2.el ends here

(defun read-only-if-symlink ()
  "*If a file is a symlink - make a buffer read-only."
  (if (file-symlink-p buffer-file-name)
      (progn
        (setq buffer-read-only t)
        (message "File %s is a symlink" buffer-file-name))))


(add-hook 'find-file-hook 'read-only-if-symlink)


;; 
(defun ros-trigger-advice()
  "*A func to trigger advice to test."
  (interactive)
  (message "in %s" (add-log-current-defun)))

(defun bad-advice ()
  (message "in %s - trigger error" (add-log-current-defun))
  (/ 1 0 ))

(advice-add 'ros-trigger-advice :before 'bad-advice)
(advice-remove 'ros-trigger-advice 'bad-advice)

;; M-x ros-trigger-advice to trigger debug

;; trigger debug on message
(setq debug-on-message "TEST")
(message "TEST")
