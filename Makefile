# $Header: /repo/cvs.copy/eli/Makefile,v 1.23 1988/11/17 12:22:34 layer Exp $

cl = /usr/local/cl3.0

.SUFFIXES:
.SUFFIXES: .el .elc .cl .fasl
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

case-mode = (set-case-mode :case-sensitive-lower)

.cl.fasl : 
	echo '$(case-mode)(require :process)(require :foreign)(require :cstructs)(compile-file "$*.cl")' | $(cl) -qq -batch

emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    tools/doc.elc rlogin.elc shell.elc keys.elc tcplisp.elc utils.elc\
	    ltags.elc

all:	ipc.fasl emacs.fasl depend spec.out
	@rm -f stamp; date > stamp

depend: ${elc-files}

spec.out:	spec.n ${elc-files} tools/doc.el
	$(emacs) -batch -q -l tools/doc.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

tags:
	(cd ../..; etags lisp/fi/*.el lisp/local/*.el lisp/*.el src/*.[ch])

verify:
	@verify *.el spec.out README examples/*
