# $Header: /repo/cvs.copy/eli/Makefile,v 1.37 1989/07/22 14:33:29 layer Exp $

cl = /usr/local/cl
emacs = /usr/local/emacs

fasls = ipc.fasl emacs.fasl

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
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

fasls: ${fasls}

elcs: ${elcs}

spec.out:	../../doc/spec.n ${elcs}
	$(emacs) -batch -q -l ../../tools/doc.elc

tags:;	(cd ../..; etags lisp/fi/*.el lisp/local/*.el)

echo-fasls:
	echo ${fasls}
