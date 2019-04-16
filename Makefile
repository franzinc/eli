# This makefile requires GNU make.

SHELL = bash

###############################################################################
# $(OS) is from the environment on Windows NT
ifeq ($(OS),Windows_NT)
emacsdir := $(shell perl emacsdir.pl)
emacs = $(emacsdir)/bin/emacs.exe
# ../bin/pwd prints like c:/... instead of /c/... like the cygnus version.
pwd = $(shell ../bin/pwd)
#############
else ### unix
emacs = emacs
pwd = $(shell pwd)
endif
###############################################################################

all default:
	"$(emacs)" -batch -q --eval '(setq fi--force-compile t)' \
		-l $(pwd)/fi-site-init.el -kill

copy_dist: FORCE
ifndef DISTDIR
	@echo DISTDIR is not defined; exit 1
endif
	mkdir -p $(DISTDIR)
	cp *.el *.elc $(DISTDIR)

clean:	FORCE
	rm -f *.elc *.doc

tags:	FORCE
	etags *.el

FORCE:

include local.mak
