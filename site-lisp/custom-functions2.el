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

;; ;; trigger debug on message
;; (setq debug-on-message "TEST")
;; (message "TEST")
m
;; (defvar ros-x 5)
;; (symbol-value 'ros-x)
;; (symbol-name 'ro


(defvar unscroll-to nil
  "Text position for next call to unscroll.")

(defvar unscroll-point nil
  "Cursor position for next call to unscroll.")

(defvar unscroll-window-start nil
  "Window start for next call to unscroll.")

(defvar unscroll-hscroll nil
  "Hscroll for next call to unscroll.")


(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from for unscroll."
  (if (not (eq last-command `scroll-up))
      (progn
        (setq unscroll-to (point)
              unscroll-window-start (window-start)
              unscroll-hscroll (window-start)))))

(defun unscroll ()
  "Jump to location specified by unscroll-to."
  (interactive)

  (if (not unscroll-point)
      (error "Can not unscroll yet"))
    (goto-char unscroll-to)
  (set-window-start nil unscroll-window-start)
  (set-window-hscroll nil unscroll-hscroll))


(defun insert-current-time ()
  "Insert current time."
  (interactive "*")
  (insert (current-time-string)))


;; lists

'(a b c)

(car '(a b c) )
(cdr '(a b c) )

(cdr '(x) )

(car '())
(cdr '())

(car '(nil))

(list 'a "b" 7)

(list '(x y z) 3)

(cons 'a '( a b c ))

(cons 'hello ())

(cons '(a b) '(c d))

(append '(a b) '(c d))

(append '(a '(e f)  b) '(c d))

(reverse '(1 2 3 4))

(cons 'a 'b)
(car (cons 'a 'b))

(cons 'a '(c b))

(cons 'a nil)

(consp '(a b))
(atom 'x)
(listp '(a b c))

(listp (cons 'a 'b) )
(null nil)
