# $Id: Makefile,v 1.133 1997/12/11 00:55:31 layer Exp $
# This makefile requires GNU make.

include version.mak

# for some system V machines:
SHELL = /bin/sh

ifndef emacs
emacs = emacs
endif

ifdef emacs_pwd
pwd = $(emacs_pwd)
else
pwd = pwd
endif

has_xemacs = t

xemacs = xemacs

default:	fi-vers.el compile

all:	fi-vers.el compile test.out tags docs

compile:	fi-vers.el
	$(emacs) -nw -batch -q -l `$(pwd)`/fi-compile.el -kill
ifeq ($(has_xemacs),t)
	$(xemacs) -nw -batch -q -l `$(pwd)`/fi-xcompile.el -kill
endif

fi-vers.el: Makefile version.mak
	rm -f fi-vers.el
	echo ';; automatically generate file--do not edit.' > fi-vers.el
	echo '(defvar fi:emacs-lisp-interface-version)' >> fi-vers.el
	echo '(setq fi:emacs-lisp-interface-version "$(VERSION)")' >> fi-vers.el

docs: UserGuide.doc RefMan.doc RefCard.doc

RefMan.n:	UserGuide.n
	rm -f RefMan.n
	egrep '^%%' UserGuide.n > RefMan.n

RefCard.n:	UserGuide.n
	rm -f RefCard.n
	egrep '^%%' UserGuide.n | sed 's/%%/@@/' > RefCard.n

RefMan.doc:	RefMan.n $(elcs)
	$(emacs) -batch -q -l Doc.elc -- RefMan.n RefMan.doc

RefCard.doc:	RefCard.n $(elcs)
	$(emacs) -batch -q -l Doc.elc -- RefCard.n RefCard.doc

UserGuide.doc:	UserGuide.n $(elcs)
	$(emacs) -batch -q -l Doc.elc -- UserGuide.n UserGuide.doc

test.out:	$(elcs) fi-test.el
	$(emacs) -nw -batch -q -l fi-test.el
	@date > test.out

clean:	FORCE
	rm -f *.elc *.doc test.out

tags:	FORCE
	etags fi-*.el

FORCE:

include local.mak
