# $Header: /repo/cvs.copy/eli/Makefile,v 1.47 1991/01/29 15:29:26 layer Exp $

# for some system V machines:
SHELL = /bin/sh

emacs = emacs

cl = /usr/local/cl

# if the following is null, then the .fasl files will be compiled into
# excl::*library-code-pathname*
cl_library = 

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    su.elc telnet.elc rlogin.elc shell.elc keys.elc tcplisp.elc\
	    query.elc utils.elc ltags.elc clman.elc doc.elc\
	    lep/lep-utils.elc lep/basic-lep.elc lep/lep.elc\
	    lep/dmode.elc lep/stream.elc lep/composer.elc

compile_time_env = -l cl -l bytecomp -l `pwd`/lep/lep-utils -l `pwd`/lep/basic-lep

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q $(compile_time_env) -f batch-byte-compile $*.el

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
