# $Id: local.mak,v 2.1 1997/12/11 00:28:30 layer Exp $

TGZFILE = eli-$(VERSION).tar.gz
DIR = eli-$(VERSION)

release_files = Makefile version.mak fi-*.el fi-*.elc *.doc \
	examples/emacs.el

echo_release_files:
	@echo $(release_files)

dist = eli-$(VERSION)

dist:	FORCE
	rm -fr $(dist)
	mkdir $(dist)
	sed -e 's/%%VERSION%%/$(VERSION)/g' \
	    -e 's/%%TGZFILE%%/$(TGZFILE)/g' \
	    -e 's/%%DIR%%/$(DIR)/g' \
		< relnotes.txt \
		> $(dist)/README
	cp -p $(release_files) $(dist)
	gtar zcf $(dist).tar.gz $(dist)

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
