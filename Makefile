# $Header: /repo/cvs.copy/eli/Makefile,v 1.80 1993/07/22 23:04:35 layer Exp $

# for some system V machines:
SHELL = /bin/sh

#the following works, but is not backward compatible--you can't load
#the .elc files compiled with lemacs into emacs 18.xxx.
# emacs = /beat/emacs/lemacs-19.6/src/xemacs
emacs = emacs

elcs = fi-modes.elc fi-indent.elc fi-subproc.elc fi-sublisp.elc fi-filec.elc\
	fi-lemacs.elc fi-emacs19.elc fi-ring.elc\
	fi-su.elc fi-telnet.elc fi-rlogin.elc fi-shell.elc fi-keys.elc\
	fi-utils.elc fi-clman.elc Doc.elc\
	fi-basic-lep.elc fi-lep.elc fi-lze.elc fi-db.elc\
	fi-stream.elc fi-dmode.elc fi-composer.elc fi-changes.elc\
	fi-leep0.elc fi-leep.elc

# cl.el instead of cl is needed because of a bug in emacs 18.59 (the
# cl.elc there is bogus and doesn't expand setf methods prooperly).

compile_time_env = -l cl.el -l bytecomp -l `pwd`/fi-utils -l `pwd`/fi-basic-lep

all:	elcs test.out docs tags

fi-leep.elc:
	$(emacs) -nw -batch -q $(compile_time_env)\
		-l `pwd`/fi-leep0.elc -f batch-byte-compile $*.el

.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc: 
	$(emacs) -nw -batch -q $(compile_time_env) -f batch-byte-compile $*.el

test.out:	$(elcs) fi-test.el
	$(emacs) -nw -batch -q -l fi-test.el
	@date > test.out

clean:
	rm -f *.elc *.doc

docs: UserGuide.doc RefMan.doc RefCard.doc

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

include Makefile.fi
