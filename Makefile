# $Header: /repo/cvs.copy/eli/Makefile,v 1.72 1991/12/11 16:06:36 layer Exp $

# for some system V machines:
SHELL = /bin/sh

emacs = emacs

elcs = modes.elc indent.elc subproc.elc sublisp.elc filec.elc ring.elc\
	su.elc telnet.elc rlogin.elc shell.elc keys.elc\
	utils.elc clman.elc Doc.elc\
	basic-lep.elc lep.elc lze.elc db.elc\
	stream.elc dmode.elc composer.elc changes.elc\
	leep0.elc leep.elc

compile_time_env = -l cl -l bytecomp -l `pwd`/utils -l `pwd`/basic-lep

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q $(compile_time_env) -f batch-byte-compile $*.el

default:	elcs tags

leep.elc:
	$(emacs) -nw -batch -q $(compile_time_env)\
		-l `pwd`/leep0.elc -f batch-byte-compile $*.el

all:	clean default docs # fasls

clean:
	rm -f *.elc

docs: UserGuide.doc RefMan.doc RefCard.doc

#cl = /usr/local/cl
#
# if the following is null, then the .fasl files will be compiled into
# excl::*library-code-pathname*
#cl_library = 
#
#fasls:	emacs.fasl ipc.fasl
#
#emacs.fasl:
#	./docompile.sh emacs ${cl} ${cl_library}
#
#ipc.fasl:
#	./docompile.sh ipc ${cl} ${cl_library}

elcs: ${elcs}

###############################################################################

RefMan.n:	UserGuide.n
	rm -f RefMan.n
	egrep '^%%' UserGuide.n > RefMan.n

RefCard.n:	UserGuide.n
	rm -f RefCard.n
	egrep '^%%' UserGuide.n | sed 's/%%/@@/' > RefCard.n

RefMan.doc:	RefMan.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- RefMan.n RefMan.doc

RefCard.doc:	RefCard.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- RefCard.n RefCard.doc

UserGuide.doc:	UserGuide.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- UserGuide.n UserGuide.doc


install:
	cp -rp *.doc *.cl *.sh *.el *.elc *.data $(DEST)

printer = lw2
enscript = enscript -P$(printer) -h

print:
	$(enscript) -f Courier10        UserGuide.doc
	$(enscript) -f Courier8         RefMan.doc
	$(enscript) -f Courier6 -B -2r  RefCard.doc

tags:	TAGS

TAGS:	${elcs}
	etags *.el
