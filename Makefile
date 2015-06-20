#  http://stackoverflow.com/questions/1217180/how-do-i-byte-compile-everything-in-my-emacs-d-directory

SHELL:=/bin/bash
TMP:=~/tmp

.PHONY: tar

all: compile

compile:
	emacs --batch --eval "(byte-recompile-directory \".\" 0)"
	make -C lisp/distel

clean:
	find . -name "*.elc" -print | xargs rm -f
