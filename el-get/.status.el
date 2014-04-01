((ace-jump-mode status "installed" recipe
		(:name ace-jump-mode :website "https://github.com/winterTTr/ace-jump-mode/wiki" :description "A quick cursor location minor mode for emacs" :type github :pkgname "winterTTr/ace-jump-mode" :features ace-jump-mode))
 (auto-complete status "installed" recipe
		(:name auto-complete :website "https://github.com/auto-complete/auto-complete" :description "The most intelligent auto-completion extension." :type github :pkgname "auto-complete/auto-complete" :depends
		       (popup fuzzy)))
 (cl-lib status "installed" recipe
	 (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (dired-details status "installed" recipe
		(:name dired-details :description "Make file details hide-able in dired" :type emacswiki :features dired-details))
 (dirtree status "installed" recipe
	  (:name dirtree :description "Functions for building directory-tree lists" :type http :url "http://www.splode.com/~friedman/software/emacs-lisp/src/dirtree.el" :features dirtree))
 (distel status "installed" recipe
	 (:name distel :website "https://github.com/massemanet/distel" :description "Distributed Emacs Lisp for Erlang." :type github :pkgname "massemanet/distel" :info "doc" :build `,(mapcar
																							 (lambda
																							   (target)
																							   (list "make" target
																								 (format "EMACS=%s" el-get-emacs)))
																							 '("clean" "all"))
		:load-path
		("elisp")
		:features distel))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :load "el-get.el"))
 (fill-column-indicator status "installed" recipe
			(:name fill-column-indicator :type github :website "https://github.com/alpaker/Fill-Column-Indicator#readme" :description "An Emacs minor mode that graphically indicates the fill column." :pkgname "alpaker/Fill-Column-Indicator" :features fill-column-indicator))
 (fuzzy status "installed" recipe
	(:name fuzzy :website "https://github.com/auto-complete/fuzzy-el" :description "Fuzzy matching utilities for GNU Emacs" :type github :pkgname "auto-complete/fuzzy-el"))
 (git-modes status "installed" recipe
	    (:name git-modes :description "GNU Emacs modes for various Git-related files" :type github :pkgname "magit/git-modes"))
 (magit status "installed" recipe
	(:name magit :website "https://github.com/magit/magit#readme" :description "It's Magit! An Emacs mode for Git." :type github :pkgname "magit/magit" :depends
	       (cl-lib git-modes)
	       :info "." :build
	       (if
		   (version<= "24.3" emacs-version)
		   `(("make" ,(format "EMACS=%s" el-get-emacs)
		      "all"))
		 `(("make" ,(format "EMACS=%s" el-get-emacs)
		    "docs")))
	       :build/berkeley-unix
	       (("touch" "`find . -name Makefile`")
		("gmake"))))
 (move-text status "installed" recipe
	    (:name move-text :auto-generated t :type emacswiki :description "Move current line or region with M-up or M-down." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/move-text.el"))
 (multiple-cursors status "installed" recipe
		   (:name multiple-cursors :description "An experiment in adding multiple cursors to emacs" :type github :pkgname "magnars/multiple-cursors.el" :features multiple-cursors))
 (popup status "installed" recipe
	(:name popup :website "https://github.com/auto-complete/popup-el" :description "Visual Popup Interface Library for Emacs" :type github :submodule nil :pkgname "auto-complete/popup-el"))
 (puppet-mode status "installed" recipe
	      (:name puppet-mode :description "A simple mode for editing puppet manifests" :type github :pkgname "lunaryorn/puppet-mode" :website "https://github.com/lunaryorn/puppet-mode" :prepare
		     (progn
		       (autoload 'puppet-mode "puppet-mode" "Major mode for editing puppet manifests" t)
		       (add-to-list 'auto-mode-alist
				    '("\\.pp$" . puppet-mode)))))
 (pydoc-info status "installed" recipe
	     (:name pydoc-info :description "Emacs package for searching Python documentation in Info" :website "https://bitbucket.org/jonwaltman/pydoc-info" :type hg :url "https://bitbucket.org/jonwaltman/pydoc-info"))
 (pymacs status "installed" recipe
	 (:name pymacs :description "Interface between Emacs Lisp and Python" :type github :pkgname "pinard/Pymacs" :prepare
		(progn
		  (el-get-envpath-prepend "PYTHONPATH" default-directory)
		  (autoload 'pymacs-load "pymacs" nil t)
		  (autoload 'pymacs-eval "pymacs" nil t)
		  (autoload 'pymacs-exec "pymacs" nil t)
		  (autoload 'pymacs-call "pymacs")
		  (autoload 'pymacs-apply "pymacs"))
		:build
		("make")))
 (python-mode status "installed" recipe
	      (:name python-mode :description "Major mode for editing Python programs" :type bzr :url "lp:python-mode" :load-path
		     ("." "test")
		     :compile nil :prepare
		     (progn
		       (autoload 'python-mode "python-mode" "Python editing mode." t)
		       (autoload 'doctest-mode "doctest-mode" "Doctest unittest editing mode." t)
		       (setq py-install-directory
			     (el-get-package-directory "python-mode"))
		       (add-to-list 'auto-mode-alist
				    '("\\.py$" . python-mode))
		       (add-to-list 'interpreter-mode-alist
				    '("python" . python-mode)))))
 (tramp status "installed" recipe
	(:name tramp :description "Transparent Remote Access, Multiple Protocols." :website "http://www.gnu.org/s/tramp/" :type git :url "git://git.savannah.gnu.org/tramp.git" :build
	       `(("autoconf")
		 ("./configure" ,(concat "--with-emacs=" el-get-emacs)
		  "--with-contrib" ,(concat "--prefix="
					    (expand-file-name
					     (el-get-package-directory "tramp"))))
		 ("make")
		 ("make" "install"))
	       :load-path
	       ("./lisp")
	       :autoloads
	       ("trampver.el" "tramp-loaddefs.el")
	       :info "share/info"))
 (tree-mode status "installed" recipe
	    (:name tree-mode :auto-generated t :type emacswiki :description "A mode to manage tree widgets" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/tree-mode.el"))
 (window-number status "installed" recipe
		(:name window-number :auto-generated t :type emacswiki :description "Select windows by numbers." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/window-number.el")))
