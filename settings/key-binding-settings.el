;; key-binding-settings.el
;; key-bindings

;; setup global key bindings
(global-set-key (kbd "\C-x g") 'magit-status )

;; resize vertically split window with M-S-+/_ 
;; because UP/DOWN arrow combination don't work on Putty
(require 'window-resizing)
(global-set-key (kbd "M-+") 'move-border-up)
(global-set-key (kbd "M-_") 'move-border-down)

;;;; binding to resize window : http://www.emacswiki.org/emacs/WindowResize
;; (global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
;; (global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
;; (global-set-key (kbd "S-C-<down>") 'shrink-window)
;; (global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; ;; unbind arrow keys
;; (global-unset-key [left])
;; (global-unset-key [up])
;; (global-unset-key [right])
;; (global-unset-key [down])

(provide 'key-binding-settings)
