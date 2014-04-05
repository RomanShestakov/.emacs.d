;; ctag-settings.el
;; simplify use of etagging

;; build ctags
;; http://mattbriggs.net/blog/2012/03/18/awesome-emacs-plugins-ctags/

(require 'eproject)
(require 'etags-select)

(defun build-ctags ()
  (interactive)
  (message "building project tags")
  (let ((root (eproject-root)))
    (shell-command (concat "ctags -e -R --extra=+fq --exclude=db --exclude=test --exclude=.git --exclude=public -f " root "TAGS " root)))
  (visit-project-tags)
  (message "tags built succesfully"))

(defun visit-project-tags()
  (interactive)
  (let ((tags-file (concat (eproject-root) "TAGS")))
    (visit-tags-table tags-file)
    (message (concat "Loaded " tags-file))))

(defun my-find-tag ()
  (interactive)
  (if (file-exists-p (concat (eproject-root) "TAGS"))
      (visit-project-tags)
    (build-ctags))
  (etags-select-find-tag-at-point))

;; re-define M-. and M-,
(global-set-key (kbd "M-.") 'my-find-tag)
(global-set-key (kbd "M-,") 'pop-tag-mark)
(global-set-key (kbd "C-c M-b") 'build-ctags)

(provide 'ctag-settings)
