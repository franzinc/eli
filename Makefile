# $Header: /repo/cvs.copy/eli/Makefile,v 1.53 1991/03/13 15:03:50 layer Exp $

# for some system V machines:
SHELL = /bin/sh

emacs = emacs

cl = /usr/local/cl

# if the following is null, then the .fasl files will be compiled into
# excl::*library-code-pathname*
cl_library = 

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    su.elc telnet.elc rlogin.elc shell.elc keys.elc tcplisp.elc\
	    nonlep/ltags.elc nonlep/query.elc utils.elc clman.elc doc.elc\
	    lep/basic-lep.elc lep/lep.elc lep/lze.elc lep/db.elc\
	    lep/dmode.elc lep/stream.elc lep/composer.elc lep/changes.elc

compile_time_env = -l cl -l bytecomp -l `pwd`/lep/basic-lep

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q $(compile_time_env) -f batch-byte-compile $*.el

default:	elcs ug.out spec.out

all:	default fasls

fasls:	emacs.fasl ipc.fasl

emacs.fasl:
	./docompile.sh emacs ${cl} ${cl_library}

ipc.fasl:
	./docompile.sh ipc ${cl} ${cl_library}

elcs: ${elcs}

###############################################################################

spec.out:	spec.n ${elcs}
	$(emacs) -batch -q -l doc.elc -- spec.n spec.out

ug.out:	ug.n ${elcs}
	$(emacs) -batch -q -l doc.elc -- ug.n ug.out


install:
	cp -rp spec.out *.cl dot.* *.sh *.el *.elc *.data $(DEST)
	mkdir $(DEST)/lep
	mkdir $(DEST)/nonlep
	cp -p lep/*.el* $(DEST)/lep
	cp -p nonlep/*.el* $(DEST)/nonlep
