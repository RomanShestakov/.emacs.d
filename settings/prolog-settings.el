;;; prolog-settings.el --- provide settings for prolog mode.

;;; Commentary:
;; % -*- Mode: Prolog -*-

;;; Code:

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)

(eval-after-load "prolog"
  '(progn
     (setq prolog-system 'swi)
     (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                     ("\\.m$" . mercury-mode))
                                   auto-mode-alist))))

(provide 'prolog-settings)

;;; prolog-settings.el ends here


