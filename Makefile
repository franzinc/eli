# $Header: /repo/cvs.copy/eli/Makefile,v 1.30 1989/02/17 19:35:17 layer Exp $

cl = /usr/local/cl3.0
emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    rlogin.elc shell.elc keys.elc tcplisp.elc\
	    utils.elc ltags.elc clman.elc clman-oblist.elc\
	    ../tools/doc.elc

precompile = (set-case-mode :case-sensitive-lower)(require :process)(require :foreign)(require :cstructs)

.SUFFIXES:
.SUFFIXES: .el .elc .cl .fasl
.el.elc: 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

.cl.fasl:
	echo '$(precompile)(compile-file "$*.cl")' | $(cl) -qq -batch

default:	ipc.fasl emacs.fasl depend spec.out

depend: ${elc-files}

spec.out:	../doc/spec.n ${elc-files}
	$(emacs) -batch -q -l ../tools/doc.elc

tags:;	(cd ..; etags fi/*.el local/*.el)
