# $Id: Makefile,v 1.131 1997/10/30 00:45:29 layer Exp $

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

default:	compile test.out docs

all:	compile test.out tags docs

compile:
	$(emacs) -nw -batch -q -l `$(pwd)`/fi-compile.el -kill
ifeq ($(has_xemacs),t)
	$(xemacs) -nw -batch -q -l `$(pwd)`/fi-xcompile.el -kill
endif

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

tags:	TAGS

TAGS:	$(elcs)
	etags fi-*.el

###############################################################################

release_files = ChangeLog fi-*.el fi-*.elc Makefile \
	examples/*.el # Doc.el *.doc 

echo_release_files:
	@echo $(release_files)

dist:	FORCE
	rm -fr eli-dist
	mkdir eli-dist
	cp -p $(release_files) eli-dist
	gtar zcf dist.tar.gz eli-dist

release_root = /net/vapor/scm/emacs-lib/Dists

fi-dist:	all
	chmod 644 fi-site-init.el ChangeLog
	rm -f version
	emacs -batch -l `$(pwd)`/fi-inc-vers > version 2>&1
	@echo new emacs-lisp interface version: `cat version`
	cvs commit -m"`cat version`" fi-site-init.el ChangeLog
	@if test -d "$(release_root)/fi-`cat version`"; then\
	  echo $(release_root)/fi-`cat version` exists; exit 1;\
	fi
#just store the major version number in UserGuide.n:
#	@if grep "Release $(version)" UserGuide.n > /dev/null; then\
#	  foo=;\
#	else\
#	  echo The version in fi-site-init.el and UserGuide.n do not agree;\
#	  exit 1;\
#	fi
	mkdir $(release_root)/fi-`cat version`
	tar cf - $(release_files) | \
	  (cd $(release_root)/fi-`cat version`; tar xf -)
	(version=`cat version`; cd $(release_root); \
	 tar cf - fi-$$version | \
	     gzip -9 > $(release_root)/eli-$$version.tar.gz)
	(version=`cat version`; cvs tag `echo fi_$$version | sed s/\\\\./_/g'`)

###############################################################################

hosts = beast beta tiger freezer sole sparky akbar fridge louie hefty \
	vapor biggie boys high baby

elib_root = /usr/fi/emacs-lib
to = $(elib_root)/fi
rdist = rdist

FILES_TO_RDIST = ChangeLog fi-*.el fi-*.elc \
		local-fi-*.el local-fi-*.elc Makefile 

rdist:
	rm -fr DIST
	$(rdist) -hwqc $(FILES_TO_RDIST) "`hostname`:`$(pwd)`/DIST"
	(cd DIST; $(rdist) -Rc . "{`echo $(hosts) | sed 's/ /,/g'`}:$(to)")
	rm -fr DIST

save_version:
	@if test -z "$(version)"; then echo version= is null; exit 1; fi
	@for host in $(hosts); do\
		echo rsh $$host -n mv $(to) $(elib_root)/fi-$(version);\
	done

FORCE:
