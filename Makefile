# $Header: /repo/cvs.copy/eli/Makefile,v 1.18 1988/05/25 10:59:02 layer Exp $

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    doc.elc rlogin.elc shell.elc keys.elc tcplisp.elc utils.elc\
	    ltags.elc

all:	depend spec.out

depend: ${elc-files}

spec.out:	spec.n ${elc-files} doc.el
	$(emacs) -batch -q -l doc.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

tags:
	(cd ../..; etags lisp/fi/*.el lisp/local/*.el lisp/*.el src/*.[ch])

verify:
	verify *.el spec.n Makefile examples/*
