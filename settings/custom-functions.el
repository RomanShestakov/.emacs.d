;;; custom-function.el --- Custom functions

;;; Commentary:

;;; Code:

;; (defun unfill-paragraph ()
;;   "* Unfill a paragraph, make it so the text does not wrap in the paragraph where the cursor is."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-paragraph nil)))


;; (defun unfill-region ()
;;   "*Unfill a region, i.e., make is so the text in that region does not wrap."
;;   (interactive)
;;   (let ((fill-column (point-max)))
;;     (fill-region (region-beginning) (region-end) nil)))

(defun system-is-mac ()
  "*Check is the system in darwin-based."
  (interactive)
  (string-equal system-type "darwin"))

(defun system-is-linux ()
  "*Check is the system is linux."
  (interactive)
  (string-equal system-type "gnu/linux"))

;; (eval-when-compile (defvar plugin-path))
;; (defun make-plugin-path (plugin)
;;   "*Build a full path for a PLUGIN."
;;   (expand-file-name
;;    (concat plugin-path plugin)))

;; (defun include-plugin (plugin)
;;   "*Add a path to PLUGIN to 'load-path'."
;;   (add-to-list 'load-path (make-plugin-path plugin)))

(provide 'custom-functions)

;;; custom-functions.el ends here
