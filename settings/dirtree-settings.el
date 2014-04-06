;;; dirtree-settings.el --- tree based directory viewer

;;; Commentary:
;;; Code:

;; tree - required for dirtree
(include-plugin "tree-mode")
(require 'tree-mode)

(include-plugin "windata")
(require 'windata)

;; dirtree
(include-plugin "emacs-dirtree")
(require 'dirtree)

(provide 'dirtree-settings)

;;; dirtree-settings.el ends here
