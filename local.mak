# $Id: local.mak,v 2.2 1997/12/11 00:55:31 layer Exp $

TGZFILE = eli-$(VERSION).tar.gz
DIR = eli-$(VERSION)

release_files = Makefile version.mak Doc.el Doc.elc fi-*.el fi-*.elc *.doc \
	examples/emacs.el

echo_release_files:
	@echo $(release_files)

dist:	FORCE
	rm -fr eli-$(VERSION)
	mkdir eli-$(VERSION)
	sed -e 's/%%VERSION%%/$(VERSION)/g' \
	    -e 's/%%TGZFILE%%/$(TGZFILE)/g' \
	    -e 's/%%DIR%%/$(DIR)/g' \
		< relnotes.txt \
		> README-$(VERSION)
	cp -p $(release_files) eli-$(VERSION)
	echo '# intentionally empty' > eli-$(VERSION)/local.mak
	gtar zcf eli-$(VERSION).tar.gz eli-$(VERSION)

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
