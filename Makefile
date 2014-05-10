#  http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory

SHELL:=/bin/bash
TMP:=~/tmp

.PHONY: tar

all: compile

compile:
	emacs --batch --eval "(byte-recompile-directory \".\" 0)"

## create tar file for entire .emacs.d
## and update https://github.com/RomanShestakov/em1.git
## in case if I need to run emacs on the system which can't
## install packages via el-get (because of firewall)
tar:
	rm -rf ${TMP}/tar
	mkdir -p ${TMP}/tar
	cp -Rf ../.emacs.d ${TMP}/tar
	cd ${TMP}/tar/.emacs.d
	find ${TMP}/tar/.emacs.d -name ".git*" -print | xargs rm -fr
	find ${TMP}/tar/.emacs.d -name ".bzr*" -print | xargs rm -fr
	rm -rf ${TMP}/tar/.emacs.d/el-get/el-get
	cp -Rf ${TMP}/tar/.emacs.d/el-get/* ${TMP}/tar/.emacs.d/site-lisp/
	rm -rf ${TMP}/tar/.emacs.d/el-get
	tar czf ${TMP}/tar/emacs.tar.gz ${TMP}/tar/.emacs.d
	## update git
	rm -rf ${TMP}/em
	git clone git@github.com:RomanShestakov/em1 ${TMP}/em
	cp -f ${TMP}/tar/emacs.tar.gz ${TMP}/em/.
	## change to dir and push update to git, all in the same sub-process
	cd ${TMP}/em;git add emacs.tar.gz;git commit -m"update";git push

clean:
	find . -name "*.elc" -print | xargs rm -f
