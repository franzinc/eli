# $Id: Makefile,v 1.138.4.3 1998/06/24 23:06:53 layer Exp $
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

all:	fi-vers.el compile readme.htm

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

test.out:	$(elcs) fi-test.el
	$(emacs) -nw -batch -q -l fi-test.el
	@date > test.out

clean:	FORCE
	rm -f *.elc *.doc readme.htm test.out

tags:	FORCE
	etags *.el

FORCE:

include local.mak
