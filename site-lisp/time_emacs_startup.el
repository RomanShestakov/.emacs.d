

;; ;; time emacs load time
;; (defvar *emacs-load-start* (current-time))

;; (defun anarcat/time-to-ms (time)
;;   "*Used to calc load TIME."
;;   (+ (* (+ (* (car time) (expt 2 16)) (car (cdr time))) 1000000) (car (cdr (cdr time)))))

;; (defun anarcat/display-timing ()
;;   "*Display load TIME."
;;   (message ".emacs loaded in %fms" (/ (- (anarcat/time-to-ms (current-time))
;;                                          (anarcat/time-to-ms *emacs-load-start*)) 1000000.0)))
;; (add-hook 'after-init-hook 'anarcat/display-timing t)

