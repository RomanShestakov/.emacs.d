;;; el-get-settings.el --- define el-get packages

;;; Commentary:

;;; Code:

;; "Set the el-get path, and create it if it doesn't exist."
(defvar elget-path plugin-path)
(unless (file-exists-p elget-path)
  (make-directory elget-path))

(defun make-elget-path (plugin)
  "*Make a path to PLUGIN."
  (expand-file-name
   (concat elget-path plugin)))

(defun include-elget-plugin (plugin)
  "*Include PLUGIN."
  (add-to-list 'load-path (make-elget-path plugin)))

;; set correct paths to erlang installation as distel mode
;; depends on it
(autoload 'erlang-path-init "erlang-path-settings" t)
(erlang-path-init)

;; add el-get to the load path, and install it if it doesn't exist
(eval-when-compile (defvar emacs-root))
(add-to-list 'load-path (concat (file-name-as-directory emacs-root) "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; https://searchcode.com/codesearch/view/58622862/
(eval-after-load "el-get-custom"
  '(progn
     (message "el-get-sources")
     (setq el-get-sources
           '(
             (:name json-mode
                    :type git
                    :url "git://github.com/joshwnj/json-mode.git")
             (:name json-reformat
                    :type git
                    :url "https://github.com/gongo/json-reformat.git")
             ))))

;; packages to install
(defvar
  my-packages '(
                ;;auctex
                ;;exec-path-from-shell
                ;;color-theme-solarized
                ;;ein
                magit
                ;;markdown-mode
                ;;matlab-mode
                ;;nxhtml
                ;;pydoc-info
                ;;scss-mode
                jedi
                flycheck
                window-number
                tramp
                ;; tree-mode
                ;; windata
                dired-details
                emacs-dirtree
                move-text
                puppet-mode
                ;;fill-column-indicator
                python-mode
                ace-jump-mode
                multiple-cursors
                distel
                helm
                helm-descbinds
                eproject
                etags-select
                yasnippet
                projectile
                flx
                flymake
                elisp-slime-nav
                ;; to be able to run color theme at emcs 23
                color-theme
                org-cua-dwim
                json-mode
               ; json-snatcher
                json-reformat
                ))

;; (require 'el-get-git)

;; (autoload 'el-get-git-shallow-clone "el-get-git" t)

;; first enable shallow clone, so we don't need to clone the entire
;; history of every project
(defvar el-get-git-shallow-clone)
(eval-after-load "el-get-git"
  (setq el-get-git-shallow-clone t))

;; then install!
(el-get 'sync my-packages)

(provide 'el-get-settings)

;;; el-get-settings.el ends here

