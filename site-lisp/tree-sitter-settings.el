

;;; Commentary:
;;; tree-sitter-settings.el --- provide settings for tree-sitter

;;;
;;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
;;; Code:

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; ;; this package loads all pre-compiled dll for a bunch of languages
;; however for some reason this doesn't work for c++
;; so instead need to run one-off
;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; this allows to auto-remap traditional modes to ts-mode
;; e.g. c++ mode is mapped to c++-ts-mode
(use-package treesit-auto
  :ensure t
  :config)

;; FIXME - add a check if treesitter dir exits and if so
;; don't execute the code below
;; ;; this allows to specify languages for which treesit dlls
;; ;; need to be compiled
;; ;; and them compile them - this needs to be done once manually
;; so -uncomment the section below and execute mapc command
;; to do one-off complilation of tree-sitter modes
;; (setq treesit-language-source-alist
;;    '(;(bash "https://github.com/tree-sitter/tree-sitter-bash")
;;      (cmake "https://github.com/uyha/tree-sitter-cmake")
;;      (css "https://github.com/tree-sitter/tree-sitter-css")
;;      (elisp "https://github.com/Wilfred/tree-sitter-elisp")
;;      (go "https://github.com/tree-sitter/tree-sitter-go")
;;      (html "https://github.com/tree-sitter/tree-sitter-html")
;;      (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;      (json "https://github.com/tree-sitter/tree-sitter-json")
;;      (make "https://github.com/alemuller/tree-sitter-make")
;;      (markdown "https://github.com/ikatyang/tree-sitter-markdown")
;;      (python "https://github.com/tree-sitter/tree-sitter-python")
;;      (toml "https://github.com/tree-sitter/tree-sitter-toml")
;;      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;      (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;      (c "https://github.com/tree-sitter/tree-sitter-c")
;;      (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
;;      (yaml "https://github.com/ikatyang/tree-sitter-yaml")
;;      ))
;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(provide 'tree-sitter-settings)

;;; tree-sitter-settings.el ends here
