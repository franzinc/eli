.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

emacs = /usr/local/emacs

depend: modes.elc subprocess.elc subprocess-lisp.elc\
	subprocess-filec.elc subprocess-ring.elc\
	lisp-indent.elc aux.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

e = /usr/emacs

tags:
	(cd /usr/emacs; etags ${e}/lisp/fi/*.el ${e}/lisp/local/*.el\
		${e}/lisp/*.el ${e}/src/*.[hc])

backup:
	rdist -Rc . binky:emacs.save/fi

doc.n:	list.n
	$(emacs) -batch -q -l Makedoc.el
