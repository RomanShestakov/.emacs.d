;;; drad-and-drop-settings.el --- drap and drop files

;;; Commentary:
;; open drag and droped files in the same window
;; from http://stackoverflow.com/questions/3805658/how-to-configure-emacs-drag-and-drop-to-open-instead-of-append-on-osx
;; Upon drag-and-drop: Find the file, w/shift insert filename; w/meta insert file contents
;; note that the EMACS window must be selected (CMD-TAB) for the modifiers to register.

;;; Code:
;;;###autoload
(defun ns-insert-filename ()
  "Insert contents of first element of `ns-input-file' at point."
  (interactive)
  (let ((f (pop ns-input-file)))
    (insert f))
  (if ns-input-file                     ; any more? Separate by " "
      (insert " ")))

;;;###autoload
(defun ns-find-file-in-frame ()
  "Do a `find-file' with the `ns-input-file' as argument; staying in frame."
  (interactive)
  (let ((ns-pop-up-frames nil))
    (ns-find-file)))

; Upon drag-and-drop: Find the file, w/shift insert filename; w/meta insert file contents
; note that the emacs window must be selected (CMD-TAB) for the modifiers to register
(define-key global-map [M-ns-drag-file] 'ns-insert-file)
(define-key global-map [S-ns-drag-file] 'ns-insert-filename)
(define-key global-map [ns-drag-file] 'ns-find-file-in-frame)

(provide 'drag-and-drop-settings)

;;; drag-and-drop-settings.el ends here
