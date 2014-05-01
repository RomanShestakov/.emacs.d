#  http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory

SHELL:=/bin/bash
TMP:=~/tmp

.PHONY: tar

all: compile

compile:
	emacs --batch --eval "(byte-recompile-directory \".\" 0)"

tar:
	rm -rf ${TMP}/tar
	rm -rf ./tar;mkdir tar
	mkdir -p ${TMP}/tar
	cp -Rf ../.emacs.d ${TMP}/tar
	cd ${TMP}/tar/.emacs.d
	find ${TMP}/tar/.emacs.d -name ".git*" -print | xargs rm -fr
	find ${TMP}/tar/.emacs.d -name ".bzr*" -print | xargs rm -fr
	rm -rf ${TMP}/tar/.emacs.d/el-get/el-get
	cp -Rf ${TMP}/tar/.emacs.d/el-get/* ${TMP}/tar/.emacs.d/site-lisp/
	rm -rf ${TMP}/tar/.emacs.d/el-get
	tar czf ${TMP}/tar/emacs.tar.gz ${TMP}/tar/.emacs.d
	cp ${TMP}/tar/emacs.tar.gz $(CURDIR)/tar/.

clean:
	find . -name "*.elc" -print | xargs rm -f
