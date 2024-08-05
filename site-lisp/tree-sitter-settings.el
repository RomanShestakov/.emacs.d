

;;; Commentary:
;;; tree-sitter-settings.el --- provide settings for tree-sitter

;;;
;;; https://www.masteringemacs.org/article/how-to-get-started-tree-sitter
;;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
;;; install VS code language servers: npm i -g vscode-langservers-extracted

;;; Code:

(require 'treesit)


;; ;; this allows to auto-remap traditional modes to ts-mode
;; ;; e.g. c++ mode is mapped to c++-ts-mode
;; (use-package treesit-auto
;;   :ensure t
;;   :functions global-treesit-auto-mode
;;   :config
;;   (global-treesit-auto-mode))

;; FIXME - add a check if treesitter dir exits and if so
;; don't execute the code below
;; ;; this allows to specify languages for which treesit dlls
;; ;; need to be compiled
;; ;; and them compile them - this needs to be done once manually
;; so -uncomment the section below and execute mapc command
;; to do one-off complilation of tree-sitter modes
(setq treesit-language-source-alist
   '(;(bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     ;;(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (c "https://github.com/tree-sitter/tree-sitter-c")
     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")
     ))
;;(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))


;; ;;; this is override to use local copy
;; (defun treesit--install-language-grammar-1
;;     (out-dir lang url &optional revision source-dir cc c++)
;;   "Override existing treesit--install-language-grammar-1 func.
;; To allow installing gramma from local dir."

;;   (let* ((lang (symbol-name lang))
;;          (default-directory (make-temp-file "treesit-workdir" t))
;;          (workdir (expand-file-name "repo"))
;;          (source-dir (expand-file-name (or source-dir "src") workdir))
;;          (cc (or cc (seq-find #'executable-find '("cc" "gcc" "c99"))
;;                  ;; If no C compiler found, just use cc and let
;;                  ;; `call-process' signal the error.
;;                  "cc"))
;;          (c++ (or c++ (seq-find #'executable-find '("c++" "g++"))
;;                   "c++"))
;;          (soext (or (car dynamic-library-suffixes)
;;                     (signal 'treesit-error '("Emacs cannot figure out the file extension for dynamic libraries for this system, because `dynamic-library-suffixes' is nil"))))
;;          (out-dir (or (and out-dir (expand-file-name out-dir))
;;                       (locate-user-emacs-file "tree-sitter")))
;;          (lib-name (concat "libtree-sitter-" lang soext)))
;;     (unwind-protect
;;         (with-temp-buffer
;;           (message "Compiling repository %s" url)
;; ;;          (treesit--call-process-signal "cp" nil t nil "-rv" url workdir)
;;           (message "copying %s into %s" url workdir)
;;           (copy-directory url workdir)
;;           ;;(debug)
;;           ;;cd "${sourcedir}"
;;           ;;          (source-dir (expand-file-name (or source-dir "src") workdir))
;;           (setq default-directory source-dir)
;;           (if (file-exists-p source-dir)
;;             (message "dir %s exists" source-dir)
;;             (message "dir %s missing" source-dir)
;;             )
;;           (message "Compiling library")
;;           ;; cc -fPIC -c -I. parser.c
;;           (treesit--call-process-signal
;;            cc nil t nil "-fPIC" "-c" "-I." "parser.c")
;;           ;; cc -fPIC -c -I. scanner.c
;;           (when (file-exists-p "scanner.c")
;;             (treesit--call-process-signal
;;              cc nil t nil "-fPIC" "-c" "-I." "scanner.c"))
;;           ;; c++ -fPIC -I. -c scanner.cc
;;           (when (file-exists-p "scanner.cc")
;;             (treesit--call-process-signal
;;              c++ nil t nil "-fPIC" "-c" "-I." "scanner.cc"))
;;           ;; cc/c++ -fPIC -shared *.o -o "libtree-sitter-${lang}.${soext}"
;;           (apply #'treesit--call-process-signal
;;                  (if (file-exists-p "scanner.cc") c++ cc)
;;                  nil t nil
;;                  `("-fPIC" "-shared"
;;                    ,@(directory-files
;;                       default-directory nil
;;                       (rx bos (+ anychar) ".o" eos))
;;                    "-o" ,lib-name))
;;           ;; Copy out.
;;           (unless (file-exists-p out-dir)
;;             (make-directory out-dir t))
;;           (let* ((library-fname (expand-file-name lib-name out-dir))
;;                  (old-fname (concat library-fname ".old")))
;;             ;; Rename the existing shared library, if any, then
;;             ;; install the new one, and try deleting the old one.
;;             ;; This is for Windows systems, where we cannot simply
;;             ;; overwrite a DLL that is being used.
;;             (if (file-exists-p library-fname)
;;                 (rename-file library-fname old-fname t))
;;             (copy-file lib-name (file-name-as-directory out-dir) t t)
;;             ;; Ignore errors, in case the old version is still used.
;;             (ignore-errors (delete-file old-fname)))
;;           (message "Library installed to %s/%s" out-dir lib-name))
;;       (when (file-exists-p workdir)
;;         (delete-directory workdir t)))))


;; (setq treesit-language-source-alist
;; ;;   '((cpp "/home/romanshestakov/development/tree-sitter-cpp/"))
;;       '((typescript "/home/romanshestakov/development/tree-sitter-typescript-master/" "" "typescript/src/")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(provide 'tree-sitter-settings)

;;; tree-sitter-settings.el ends here
