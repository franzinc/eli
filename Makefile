# $Header: /repo/cvs.copy/eli/Makefile,v 1.38 1989/08/07 16:40:18 layer Exp $

emacs = /usr/local/emacs

cl = /usr/local/cl

# if the following is null, then the .fasl files will be compiled into
# excl::*library-code-pathname*
cl_library = 

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    rlogin.elc shell.elc keys.elc tcplisp.elc\
	    utils.elc ltags.elc clman.elc\
	    ../../tools/doc.elc

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

all:	elcs fasls spec.out

fasls:
	./docompile.sh ipc ${cl}
	./docompile.sh emacs ${cl}

elcs: ${elcs}

tags:;	(cd ../..; etags lisp/fi/*.el lisp/local/*.el)

###############################################################################

spec.out:	../../doc/spec.n ${elcs}
	$(emacs) -batch -q -l ../../tools/doc.elc
