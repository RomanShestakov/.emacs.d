;;; general-settings.el --- Global settings

;;; Commentary:

;;; Code:

;; set default directory
(setq default-directory "~")

;; language
(setq current-language-environment "English")

;; don't show startup screen
(setq inhibit-startup-screen 1)

;; switch off tool bar
(tool-bar-mode 0)

;; switch off menu-bar
(menu-bar-mode -1)

;; enable line numbering
;; (global-linum-mode 1)
;; (eval-after-load "linum"
;;   '(progn
;;      (setq linum-format "%d ")))

;; allow delete region by <DEL>
(delete-selection-mode 1)

;; show the current line and column numbers in the stats bar as well
(line-number-mode 1)
(column-number-mode 1)

;; disable beep
(setq visible-bell 1)

;; make frequently used commands short
;; http://ergoemacs.org/emacs/emacs_alias.html
(defalias 'qrr 'query-replace-regexp)

;; set project dir
;;(setq project-dir (getenv "PWD"))

;; ;; cua-mode is conflicting with org-mode and with python-mode
;; (cua-mode t)

;; enable electric-pair
;(electric-pair-mode t)

;; show matching paren without delay
;(autoload 'show-paren-mode "paren" t)
(show-paren-mode 1)
(defvar show-paren-delay)
(eval-when-compile (setq show-paren-delay 0))

;; use M-return to jump to the matching paren
;; https://gist.github.com/donghee/3937661
(autoload 'jump-match-paren "custom-functions" t)
(global-set-key (kbd "<M-RET>" ) #'jump-match-paren)


;; set left alt key with META and the right alt key with ALT,
;; use command as Meta as well as left Alt
(autoload 'system-is-mac "custom-functions" t)
(if (system-is-mac)
    (progn
      ;(setq mac-option-key-is-meta t)
      (setq mac-right-option-modifier nil)
      (setq mac-command-modifier 'meta)))

;; number of characters untill the fill column
(setq-default fill-column 70)

;; default window width and height at startup
(defun custom-set-frame-size()
  (add-to-list 'default-frame-alist '(height . 55))
  (add-to-list 'default-frame-alist '(width . 130)))
(custom-set-frame-size)
(add-hook 'before-make-frame-hook 'custom-set-frame-size)

;; switch off tabs, use spaces instead
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

;; ignore case while searching
(setq-default case-fold-search 1)

;; require final newline in files when they are saved
(setq require-final-newline 1)

;; add a new line when going to the next line
(setq next-line-add-newlines t)

;; short yes or no
(fset 'yes-or-no-p #'y-or-n-p)

;; don't blink the curson
;;(blink-cursor-mode 0)

;; ;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; ;; make backups by copying files
;; (setq backup-by-copying t)
;; ;; dir for backup files
;; (setq backup-directory-alist `(("." . "~/.saves")))
;; (setq delete-old-versions t
;;       kept-new-versions 6
;;       kept-old-versions 2
;;       version-control t)

;; stop creating backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; stop creating temp symbolic links
(setq create-lockfiles nil)

;; ;; autosave into ~/tmp/autosaves
;; (custom-set-variables
;;  '(auto-save-file-name-transforms '((".*" "~/tmp/autosaves/\\1" t))))
;; ;; create autosave dir if nessasary
;; (make-directory "~/tmp/autosaves" t)

;; make sure that dired is not trying to find alternative file
(put 'dired-find-alternate-file 'disabled nil)

;; ;; eval list buffer with F9
;; (add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
;; (defun my-lisp-mode-hook ()
;;   "* Make f9 to eval erlang buffer."
;;   (define-key emacs-lisp-mode-map [f9]
;;     (lambda()
;;       (interactive)
;;       (progn
;;         (eval-buffer)))))

(provide 'general-settings)

;;; general-settings.el ends here
