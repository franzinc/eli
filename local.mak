# $Id: local.mak,v 2.6.20.2 1998/06/23 22:50:47 layer Exp $

TGZFILE = eli-$(VERSION).tar.gz
DISTDIR = eli-$(VERSION)
README_HTM = readme.htm
README_TXT = readme.txt

release_files = Makefile version.mak Doc.el Doc.elc fi-*.el fi-*.elc *.doc \
	examples/emacs.el

echo_release_files:
	@echo $(release_files)

dist:	FORCE
	@if test ! -d tmp; then mkdir tmp; fi
	rm -fr dists/$(DISTDIR)
	mkdir dists/$(DISTDIR)
	rm -fr tmp/$(DISTDIR)
	mkdir tmp/$(DISTDIR)
	cp -p $(release_files) tmp/$(DISTDIR)
	sed -e 's/__VERSION__/$(VERSION)/g' \
	    -e 's/__TGZFILE__/$(TGZFILE)/g' \
	    -e 's/__README_HTM__/$(README_HTM)/g' \
	    -e 's/__README_TXT__/$(README_TXT)/g' \
	    -e 's/__DISTDIR__/$(DISTDIR)/g' \
		< readme.htm \
		> tmp/$(DISTDIR)/$(README_HTM)
	echo '# intentionally empty' > tmp/$(DISTDIR)/local.mak
	gtar Czcf tmp dists/$(DISTDIR)/$(TGZFILE) $(DISTDIR)
	cp -p tmp/$(DISTDIR)/$(README_HTM) dists/$(DISTDIR)
	rm -fr tmp/$(DISTDIR)

###############################################################################

hosts = corba beast romeo beta tiger freezer sole sparky akbar louie hefty \
	vapor biggie boys high baby

elib_root = /usr/fi/emacs-lib
to = $(elib_root)/fi
rdist = rdist

FILES_TO_RDIST = ChangeLog fi-*.el fi-*.elc \
		local*.el local*.elc Makefile 

rdist:
	rm -fr DIST
	$(rdist) -hwqc $(FILES_TO_RDIST) "`hostname`:$(pwd)/DIST"
	(cd DIST; $(rdist) -Rc . "{`echo $(hosts) | sed 's/ /,/g'`}:$(to)")
	rm -fr DIST
