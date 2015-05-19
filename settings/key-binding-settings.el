;;; key-binding-settings.el --- custom key-bindings

;;; Commentary:

;;; Code:

;; setup global key bindings
;;;###autoload

;;(global-set-key (kbd "\C-x g") 'magit-status )

;; allows to move borders between windows
;; https://github.com/ramnes/move-border
(global-set-key (kbd "M-S-<up>") 'move-border-up)
(global-set-key (kbd "M-S-<down>") 'move-border-down)
(global-set-key (kbd "M-S-<left>") 'move-border-left)
(global-set-key (kbd "M-S-<right>") 'move-border-right)

;; http://stackoverflow.com/questions/26171265/emacs-keyboard-bindings-on-os-x-iterm2
;; hardcode keybinding to make emacs work with iTerm2
(define-key input-decode-map "\e[1;10A" [M-S-up])
(define-key input-decode-map "\e[1;10B" [M-S-down])
(define-key input-decode-map "\e[1;10C" [M-S-right])
(define-key input-decode-map "\e[1;10D" [M-S-left])
(define-key input-decode-map "\e[1;9A" [M-up])
(define-key input-decode-map "\e[1;9B" [M-down])
(define-key input-decode-map "\e[1;9C" [M-right])
(define-key input-decode-map "\e[1;9D" [M-left])

(provide 'key-binding-settings)

;;; key-binding-settings ends here
