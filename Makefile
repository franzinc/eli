# $Header: /repo/cvs.copy/eli/Makefile,v 1.45 1990/11/29 14:12:40 layer Exp $

# for some system V machines:
SHELL = /bin/sh

emacs = emacs

cl = /usr/local/cl

# if the following is null, then the .fasl files will be compiled into
# excl::*library-code-pathname*
cl_library = 

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    su.elc telnet.elc rlogin.elc shell.elc keys.elc tcplisp.elc\
	    utils.elc ltags.elc clman.elc doc.elc

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

default:	elcs spec.out

all:	default fasls

fasls:	emacs.fasl ipc.fasl

emacs.fasl:
	./docompile.sh emacs ${cl} ${cl_library}

ipc.fasl:
	./docompile.sh ipc ${cl} ${cl_library}

elcs: ${elcs}

###############################################################################

spec.out:	spec.n ${elcs}
	$(emacs) -batch -q -l doc.elc


install:
	cp -rp spec.out *.cl dot.* *.sh *.el *.elc *.data $(DEST)
