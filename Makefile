.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    doc.elc rlogin.elc shell.elc

all:	depend spec.out

depend: ${elc-files}

spec.out:	spec.n ${elc-files} doc.el
	$(emacs) -batch -q -l doc.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

e = /usr/emacs

tags:
	(cd /usr/emacs; etags ${e}/lisp/fi/*.el ${e}/lisp/local/*.el\
		${e}/lisp/*.el ${e}/src/*.[hc])

backup:
	rdist -Rc . binky:emacs.save/fi
