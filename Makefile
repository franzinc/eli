# $Id: Makefile,v 1.135 1998/01/12 22:22:47 layer Exp $
# This makefile requires GNU make.

include version.mak

# for some system V machines:
SHELL = /bin/sh

ifeq ($(OS),Windows_NT)
emacs = /d/emacs-19.34/bin/emacs.exe
pwd = $(shell ../bin/pwd)
else
has_xemacs = t
endif

ifndef emacs
emacs = emacs
endif

ifndef pwd
pwd = $(shell pwd)
endif

xemacs = xemacs

default:	fi-vers.el compile

all:	fi-vers.el compile test.out tags docs

compile:	fi-vers.el
	$(emacs) -nw -batch -q -l $(pwd)/fi-compile.el -kill
ifeq ($(has_xemacs),t)
	$(xemacs) -nw -batch -q -l $(pwd)/fi-xcompile.el -kill
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
	etags *.el

FORCE:

include local.mak
