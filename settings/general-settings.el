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

;; cua-mode is conflicting with org-mode and with python-mode
(cua-mode t)

;; enable electric-pair
;(electric-pair-mode t)

;; show matching paren without delay
;(autoload 'show-paren-mode "paren" t)
(show-paren-mode 1)
(defvar show-paren-delay)
(eval-when-compile (setq show-paren-delay 0))

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

;; default window width and height
(defun custom-set-frame-size()
  (add-to-list 'default-frame-alist '(height . 100))
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

;; don't blink the curson
;;(blink-cursor-mode 0)

;; http://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;; make backups by copying files
(setq backup-by-copying t)
;; dir for backup files
(setq backup-directory-alist `(("." . "~/.saves")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; autosave into ~/tmp/autosaves
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/tmp/autosaves/\\1" t))))
;; create autosave dir if nessasary
(make-directory "~/tmp/autosaves" t)

;; display windows numbers
(autoload 'window-number-mode "window-number"
  "A global minor mode that enables selection of windows according to
 numbers with the C-x C-j prefix.  Another mode,
 `window-number-meta-mode' enables the use of the M- prefix."
  t)
(autoload 'window-number-meta-mode "window-number"
  "A global minor mode that enables use of the M- prefix to select
 windows, use `window-number-mode' to display the window numbers in
 the mode-line."
  t)
(window-number-mode 1)
(window-number-meta-mode 1)

;; make sure that dired is not trying to find alternative file
(put 'dired-find-alternate-file 'disabled nil)

;; eval list buffer with F9
(add-hook 'emacs-lisp-mode-hook 'my-lisp-mode-hook)
(defun my-lisp-mode-hook ()
  "* Make f9 to eval erlang buffer."
  (define-key emacs-lisp-mode-map [f9]
    (lambda()
      (interactive)
      (progn
        (eval-buffer)))))

(provide 'general-settings)

;;; general-settings.el ends here
