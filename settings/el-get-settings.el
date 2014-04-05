;; el-get-settings.el

;; set the el-get path, and create it if it doesn't exist
(setq elget-path plugin-path)
(unless (file-exists-p elget-path)
  (make-directory elget-path))

;; add el-get to the load path, and install it if it doesn't exist
(add-to-list 'load-path (concat emacs-root "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; packages to install
(setq 
 my-packages '(
               ;;auctex
               ;;exec-path-from-shell
               auto-complete
               ;;color-theme-solarized
               ;;ein
               magit
               ;;markdown-mode
               ;;matlab-mode
               ;;nxhtml
               pydoc-info
               ;;scss-mode
               ;;popup
               ;;jedi
               ;;nyan-mode
               window-number
               tramp
               tree-mode
               windata
               emacs-dirtree
               move-text
               puppet-mode
               fill-column-indicator
               pymacs
               python-mode
               ace-jump-mode
               multiple-cursors
               dired-details
               distel
               helm
               helm-descbinds
               eproject
               etags-select
               ))   

;; first enable shallow clone, so we don't need to clone the entire
;; history of every project
(setq el-get-git-shallow-clone t)

;; then intsall!
(el-get 'sync my-packages)

(provide 'el-get-settings)
