# $Id: Makefile,v 1.138.4.1 1998/05/08 17:13:52 layer Exp $
# This makefile requires GNU make.

include version.mak

SHELL = /bin/sh

# $(OS) is from the environment on Windows NT
ifeq ($(OS),Windows_NT)
emacsdir = $(shell perl emacsdir.pl)
emacs = $(emacsdir)/bin/emacs.exe
# ../bin/pwd prints like c:/... instead of //c/... like the cygnus version.
pwd = $(shell ../bin/pwd)
else
xemacs = xemacs
emacs = emacs
pwd = $(shell pwd)
endif

default:	fi-vers.el compile

all:	fi-vers.el compile test.out tags docs

compile:	fi-vers.el
	$(emacs) -nw -batch -q -l $(pwd)/fi-compile.el -kill
ifdef xemacs
	$(xemacs) -nw -batch -q -l $(pwd)/fi-xcompile.el -kill
endif

fi-vers.el: Makefile version.mak
	rm -f fi-vers.el
	echo ';; automatically generate file--do not edit.' > fi-vers.el
	echo '(defvar fi:emacs-lisp-interface-version)' >> fi-vers.el
	echo '(setq fi:emacs-lisp-interface-version "$(VERSION)")' >> fi-vers.el
	echo '(defvar fi:compiled-with-version)' >> fi-vers.el
	echo '(setq fi:compiled-with-version (eval-when-compile (cons emacs-major-version emacs-minor-version)))' >> fi-vers.el

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
