# $Header: /repo/cvs.copy/eli/Makefile,v 1.36 1989/07/22 14:15:52 layer Exp $

cl = /usr/local/cl
emacs = /usr/local/emacs

elc-files = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    rlogin.elc shell.elc keys.elc tcplisp.elc\
	    utils.elc ltags.elc clman.elc\
	    ../../tools/doc.elc

precompile = (set-case-mode :case-sensitive-lower)(require :process)(require :foreign)(require :cstructs)

.SUFFIXES:
.SUFFIXES: .el .elc .cl .fasl
.el.elc: 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

.cl.fasl:
	echo '$(precompile)(compile-file "$*.cl")' | $(cl) -qq -batch

all:	elcs fasls spec.out

fasls: ipc.fasl emacs.fasl 

elcs: ${elc-files}

spec.out:	../../doc/spec.n ${elc-files}
	$(emacs) -batch -q -l ../../tools/doc.elc

tags:;	(cd ../..; etags lisp/fi/*.el lisp/local/*.el)
