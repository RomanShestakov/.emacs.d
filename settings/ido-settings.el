;;; ido-settings.el --- settings for ido mode

;;; Commentary:
;; https://github.com/lewang/flx

;;; Code:

(require 'custom-functions)

(include-plugin "flx")

;; setup ido mode and flx-mode
(require 'flx-ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights
(setq ido-use-faces nil)
(setq ido-use-filename-at-point 'guess)
(setq ido-create-new-buffer 'always)
(setq ido-file-extensions-order '(".scala" ".org" ".txt" ".py" ".emacs" ".xml" ".el" ".ini" ".cfg" ".cnf"))
;; You can disable the merging (the "looking in other directories" in ido vulgo) with
(setq ido-auto-merge-work-directories-length -1)

(provide 'ido-settings)

;;; ido-settings.el ends here
