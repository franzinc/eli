# $Header: /repo/cvs.copy/eli/Makefile,v 1.22 1988/11/03 23:52:40 layer Exp $

cl = /usr/local/cl-3.0

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    tools/doc.elc rlogin.elc shell.elc keys.elc tcplisp.elc utils.elc\
	    ltags.elc

all:	ipc.fasl depend spec.out
	@rm -f stamp; date > stamp

ipc.fasl : ipc.cl
	echo '(require :process)(require :foreign)(require :cstructs)(compile-file "ipc")' | $(cl) -qq -batch

depend: ${elc-files}

spec.out:	spec.n ${elc-files} tools/doc.el
	$(emacs) -batch -q -l tools/doc.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

tags:
	(cd ../..; etags lisp/fi/*.el lisp/local/*.el lisp/*.el src/*.[ch])

verify:
	@verify *.el spec.out README examples/*
