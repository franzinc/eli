# $Id: local.mak,v 2.6.20.11 1999/02/02 21:25:33 layer Exp $

TGZFILE = eli-$(VERSION).tar.gz
DISTDIR = eli-$(VERSION)
README_HTM = readme.htm
README_TXT = readme.txt

release_files = Makefile version.mak Doc0.el fi-*.el fi-*.elc \
	examples/emacs.el doc/eli.htm readme.htm

echo_release_files:
	@echo $(release_files)

dist:	FORCE
	@if test ! -d tmp; then mkdir tmp; fi
	rm -fr dists/$(DISTDIR)
	mkdir dists/$(DISTDIR)
	rm -fr tmp/$(DISTDIR)
	mkdir tmp/$(DISTDIR)
	tar cf - $(release_files) | (cd tmp/$(DISTDIR); tar xf -)
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

hosts = corba,heavy,news,beast,romeo,beta,tiger,freezer,sole,sparky,louie,hefty,killer,biggie,boys,high,baby

elib_root = /usr/fi/emacs-lib
to = $(elib_root)/fi
rdist = rdist

FILES_TO_RDIST = $(release_files) local*.el local*.elc 

rdist:	DIST
	(cd DIST; $(rdist) -Rc . "{$(hosts)}:$(to)")
	rm -fr DIST

DIST:	FORCE
	rm -fr DIST
	mkdir DIST
	cp /dev/null DIST/local.mak
	$(rdist) -hwqc $(FILES_TO_RDIST) "`hostname`:$(pwd)/DIST"
