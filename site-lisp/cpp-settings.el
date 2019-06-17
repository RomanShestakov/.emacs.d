;;; cpp-settings.el --- customization for cpp-mode

;;; Commentary:
;;; http://nilsdeppe.com/posts/emacs-c++-ide

;;; depends on :
;;; install rust ;
;;; curl https://sh.rustup.rs -sSf | sh
;;; 1. rustfmt - to intall
;;; cargo intall rustfmt
;;; 2. racer - for code completion
;;; cargo install racer
;;; add source code for rust:
;;; git clone git@github.com:rust-lang/rust.git
;;; _build/default/lib/erlrexec/
;;; http://parbo.github.io/blog/2016/05/10/configuring-emacs-for-cpp/
;;; http://syamajala.github.io/c-ide.html
;;; http://irreal.org/blog/?p=6028
;;; http://syamajala.github.io/c-ide.html (EMACS as C++ IDE based on rtags)

;; install clang/llvm
;; brew install llvm --with-libcxx --with-clang --without-assertions --with-rtti
;; brew link llvm
;; git clone git@github.com:Andersbakken/rtags.git; cd rtags; git submodule init; git submodule update
;; rtags + OSX
;; https://gist.github.com/floatplane/68f2006186cef4d3e165

;; https://github.com/philippe-grenet/exordium

;; Add these to the PATH so that proper executables are found
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/bin"))
;; (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
;; (setq exec-path (append exec-path '("/usr/texbin")))
;; (setq exec-path (append exec-path '("/usr/bin")))
;; (setq exec-path (append exec-path '("/usr/local/bin")))

;;; Code:

;; set 4 space indent
(setq-default c-basic-offset 4)
(defvar clangd-exe (executable-find "clangd")
  "clangd executable path")

(use-package eglot
  :ensure t
  :config
  (require 'eglot))
(setq company-backends (cons 'company-capf (remove 'company-capf company-backends)))
;;(projectile-mode t)
;;(with-eval-after-load 'project (add-to-list 'project-find-functions 'ddavis/projectile-proj-find-function))
(add-to-list 'eglot-server-programs `((c++-mode), clangd-exe))
(add-hook 'c++-mode-hook 'eglot-ensure)

;; (defvar rtags-root-dir (getenv "RTAGS_ROOT")
;;   "*Path to RTAGS installation.  Location of rtags dir 'export RTAGS_ROOT=/opt/rtags-v2.10' needs to be set in bash environment.")

;; (defun rtags-path-init()
;;   "*Sets the paths to rtags elisp files."
;;   (add-to-list 'load-path (concat rtags-root-dir "/share/emacs/site-lisp/rtags/")))
q
;; (defvar irony-root-dir (getenv "IRONY_ROOT")
;;   "*Path to IRONY installation.  Location of irony dir 'export IRONY_ROOT=/opt/irony-v1.2.1' needs to be set in bash environment.")

;; (defun irony-path-init()
;;   "*Sets the paths to irony elisp files."
;;   (add-to-list 'load-path (concat irony-root-dir "/share/emacs/site-lisp/irony/")))

;; ;; load paths
;; (rtags-path-init)
;; (irony-path-init)

;; ;; note rtags *is not installed* from MELPA (missing :ensure t)
;; ;; instead it is used from path initialized by rtags-path-init()
;; (use-package rtags
;;   ;; don't use ensure as we want to  load rtags.erl from the
;;   ;; current installation of rtags
;;   ;; :defer t
;;   :config
;;   (bind-key "M-." 'rtags-find-symbol-at-point)
;;   (bind-key "M-," 'rtags-location-stack-back)
;;   (setq rtags-autostart-diagnostics t)
;;   (setq rtags-completions-enabled t)
;;   (setq rtags-use-helm t)
;;   (setq rtags-display-current-error-as-tooltip t)
;;   (setq rtags-show-containing-function t)
;;   (add-hook 'find-file-hook 'rtags-start-process-maybe)
;;   (add-hook 'c++-mode-hook
;;             (lambda ()
;;               (rtags-start-process-unless-running)
;;               (rtags-enable-standard-keybindings)
;;               (push '(company-rtags) company-backends)
;;               )))

;; (defun my-flycheck-rtags-setup ()
;;   "RTags create more accurate overlays than Flychceck."
;;   (flycheck-select-checker 'rtags)
;;   (setq-local flycheck-highlighting-mode nil)
;;   (setq-local flycheck-check-syntax-automatically nil))

;; (use-package flycheck-rtags
;;   :ensure t
;;   :init
;;   (add-hook 'c++-mode-hook 'my-flycheck-rtags-setup))

;; ;; (use-package company-rtags
;; ;;   :ensure t
;; ;;   :defer t)

;; ;(add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)

;; ;; (use-package company-rtags
;; ;;   :ensure t
;; ;;   :defer t
;; ;;   :config
;; ;;   (add-hook 'c++-mode-common-hook
;; ;;             (lambda()
;; ;;               (set (make-local-variable 'company-backends) '(company-rtags))
;; ;;               (setq company-rtags-begin-after-member-access t)))))

;; (use-package irony
;;   ;:ensure t
;;   :config
;;   (use-package company-irony
;;     :ensure t
;;     :config
;;     (add-to-list 'company-backends 'company-irony))
;;   (add-hook 'c++-mode-hook 'irony-mode)
;; ;;  (add-hook 'c-mode-hook 'irony-mode)
;;   ;; replace the `completion-at-point' and `complete-symbol' bindings in
;;   ;; irony-mode's buffers by irony-mode's function
;;   (defun my-irony-mode-hook ()
;; ;;    (setq company-backends '(company-irony-c-headers company-irony))
;;     (setq irony-additional-clang-options '("-std=c++14")))
;;     ;; (define-key irony-mode-map [remap completion-at-point]
;;     ;;   'irony-completion-at-point-async)
;;     ;; (define-key irony-mode-map [remap complete-symbol]
;;     ;;   'irony-completion-at-point-async))
;;   (add-hook 'irony-mode-hook 'my-irony-mode-hook)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

;; (use-package flycheck-irony
;;   :ensure t
;;   :commands flycheck-irony-setup
;;   :init
;;   (add-hook 'c++-mode-hook 'flycheck-irony-setup)
;;   (add-hook 'c-mode-hook 'flycheck-irony-setup))

;; (defvar llvm-root-dir (getenv "LLVM_ROOT")
;;   "*Path to LLVM installation.  \
;; Location of llvm dir 'export LLVM_ROOT=/opt/llvm-4.0' needs to be set in bash environment.")

;; ;; need to run teh following command from the cmd
;; ;; cmake -DCMAKE_INSTALL_PREFIX\=/home/vagrant/.emacs.d/irony/ -DLIBCLANG_LIBRARY\=/opt/llvm-4.0/lib/libclang.so -DLIBCLANG_INCLUDE_DIR\=/opt/llvm-4.0/include -DCMAKE_INSTALL_RPATH=/opt/llvm-4.0/lib \
;; ;; -DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE  /home/vagrant/.emacs.d/elpa/irony-20180519.422/server && cmake --build . --use-stderr --config Release --target install
;; (defun irony-install-server (command)
;;   "Replace standard Irony with the one which takes extra arguments.
;; Install or reinstall the Irony server.
;; The installation requires CMake and the libclang developpement package."

;;   (interactive
;;    (list (let ((command
;;                 (format
;;                  (concat "%s %s %s %s %s %s && %s --build . "
;;                          "--use-stderr --config Release --target install")
;;                  (shell-quote-argument irony-cmake-executable)
;;                  (shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
;;                                                (expand-file-name
;;                                                 irony-server-install-prefix)))
;;                  (shell-quote-argument (concat "-DLIBCLANG_LIBRARY="
;;                                                (expand-file-name
;;                                                 (concat llvm-root-dir "/lib/libclang.so"))))
;;                  (shell-quote-argument (concat "-DLIBCLANG_INCLUDE_DIR="
;;                                                 (concat llvm-root-dir "/include")))
;;                  (shell-quote-argument "-DCMAKE_INSTALL_RPATH_USE_LINK_PATH=TRUE")
;;                  (shell-quote-argument irony-server-source-dir)
;;                  (shell-quote-argument irony-cmake-executable))))
;;            (irony--install-server-read-command command))))
;;   (let ((build-dir (or irony-server-build-dir
;;                        (concat
;;                         (file-name-as-directory temporary-file-directory)
;;                         (file-name-as-directory (format "build-irony-server-%s"
;;                                                         (irony-version)))))))
;;     (make-directory build-dir t)
;;     (let ((default-directory build-dir))
;;       ;; we need to kill the process to be able to install a new one,
;;       ;; at least on Windows
;;       (irony-server-kill)
;;       (with-current-buffer (compilation-start command nil
;;                                               #'(lambda (maj-mode)
;;                                                   "*irony-server build*"))
;;         (setq-local compilation-finish-functions
;;                     '(irony--server-install-finish-function))))))


;; Load rtags and start the cmake-ide-setup process
;; (require 'rtags)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup cmake-ide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'cmake-ide)
;;(cmake-ide-setup)
;; Set cmake-ide-flags-c++ to use C++11
;;(setq cmake-ide-flags-c++ (append '("-std=c++11")))
;; We want to be able to compile with a keyboard shortcut
;;(global-set-key (kbd "C-c m") 'cmake-ide-compile)

;; Set rtags to enable completions and use the standard keybindings.
;; A list of the keybindings can be found at:
;; http://syamajala.github.io/c-ide.html
;; (setq rtags-autostart-diagnostics t)
;; (rtags-diagnostics)
;; (setq rtags-completions-enabled t)
;; (rtags-enable-standard-keybindings)

;; ;; clang-format can be triggered using C-M-tab
;; (require 'clang-format)
;; (global-set-key [C-M-tab] 'clang-format-region)
;; Create clang-format file using google style
;; clang-format -style=google -dump-config > .clang-format


(provide 'cpp-settings)

;;; cpp-settings.el ends here
