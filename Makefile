# $Header: /repo/cvs.copy/eli/Makefile,v 1.87 1993/08/11 17:04:43 layer Exp $

# for some system V machines:
SHELL = /bin/sh

CC = gcc
CFLAGS = -O

emacs = emacs

elcs = fi-modes.elc fi-indent.elc fi-subproc.elc fi-sublisp.elc fi-filec.elc \
       fi-lemacs.elc fi-emacs19.elc fi-ring.elc \
       fi-su.elc fi-telnet.elc fi-rlogin.elc fi-shell.elc fi-keys.elc \
       fi-utils.elc fi-clman.elc Doc.elc \
       fi-basic-lep.elc fi-lep.elc fi-lze.elc fi-db.elc \
       fi-stream.elc fi-dmode.elc fi-composer.elc fi-changes.elc \
       fi-leep0.elc fi-leep.elc fi-leep-lemacs.elc makeman.elc

# use cl.el instead of cl because of a bug in emacs 18.59 (the
# cl.elc in the distribution is bogus and doesn't expand setf methods
# properly).

compile_time_env = -l cl.el -l bytecomp -l `pwd`/fi-utils -l `pwd`/fi-basic-lep

default:	elcs test.out clman

all:	elcs test.out docs tags

fi-leep.elc:
	$(emacs) -nw -batch -q $(compile_time_env) \
		-l `pwd`/fi-leep0.elc -f batch-byte-compile $*.el

fi-leep-lemacs.elc:
	$(emacs) -nw -batch -q $(compile_time_env) \
		-l `pwd`/fi-leep0.elc -f batch-byte-compile $*.el

.SUFFIXES:
.SUFFIXES: .el .elc

.el.elc: 
	$(emacs) -nw -batch -q $(compile_time_env) -f batch-byte-compile $*.el

docs: UserGuide.doc RefMan.doc RefCard.doc

elcs: ${elcs}

RefMan.n:	UserGuide.n
	rm -f RefMan.n
	egrep '^%%' UserGuide.n > RefMan.n

RefCard.n:	UserGuide.n
	rm -f RefCard.n
	egrep '^%%' UserGuide.n | sed 's/%%/@@/' > RefCard.n

RefMan.doc:	RefMan.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- RefMan.n RefMan.doc

RefCard.doc:	RefCard.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- RefCard.n RefCard.doc

UserGuide.doc:	UserGuide.n ${elcs}
	$(emacs) -batch -q -l Doc.elc -- UserGuide.n UserGuide.doc

test.out:	$(elcs) fi-test.el
	$(emacs) -nw -batch -q -l fi-test.el
	@date > test.out

clean_OS:
	rm -f *.o clman makeman

clean:	clean_OS
	rm -f fi-*.elc *.doc

tags:	TAGS

TAGS:	${elcs}
	etags fi-*.el

###############################################################################

all_clman:	clman clman.data small_manual small_manual/clman.data

small_manual:
	-mkdir small_manual
	-mkdir small_manual/manual

makeman.o: makeman.c clman.h
	$(CC) $(CFLAGS) -c makeman.c

makeman:	makeman.o clmanaux.o
	$(CC) $(CFLAGS) -o makeman makeman.o clmanaux.o

clman.oblist:	manual/OBLIST.el makeman.elc
	emacs -nw -batch -q -l `pwd`/manual/OBLIST.el -l `pwd`/makeman.elc \
		> clman.oblist

clman.data:	clman.oblist makeman
	makeman clman.data < clman.oblist

small_manual/manual/OBLIST.el:	manual/OBLIST.el
	egrep -v '"clim/' manual/OBLIST.el > small_manual/manual/OBLIST.el

small_manual/clman.oblist:	small_manual/manual/OBLIST.el makeman.elc
	emacs -nw -batch -q -l `pwd`/small_manual/manual/OBLIST.el \
		-l `pwd`/makeman.elc \
		> small_manual/clman.oblist

small_manual/clman.data:	small_manual/clman.oblist makeman
	makeman small_manual/clman.data < small_manual/clman.oblist

clean_clman:
	rm -fr *.o clman makeman clman.oblist clman.data small_manual

clmanaux.o: clmanaux.c clman.h
	$(CC) $(CFLAGS) -c clmanaux.c

clman.o: clman.c clman.h
	$(CC) $(CFLAGS) -c clman.c

clman:	clman.o clmanaux.o
	$(CC) $(CFLAGS) -o clman clman.o clmanaux.o

###############################################################################

release_root = /net/vapor/scm/emacs-lib/Dists

fi_release_directory = $(release_root)/fi-$(version)
fi_release_files = ChangeLog fi-*.el fi-*.elc Makefile *.doc examples/*.el
fi_release_gztar = $(release_root)/eli-$(version).tar.gz

fi-dist:	all
	@if test -z "$(version)"; then\
	  echo Make variable version is null; exit 1;\
	fi
	@if test -d "$(fi_release_directory)"; then\
	  echo $(fi_release_directory) exists; exit 1;\
	fi
	@if grep "Release $(version)" UserGuide.n > /dev/null; then\
	  foo=;\
	else\
	  echo The version in fi-site-init.el and UserGuide.n do not agree;\
	  exit 1;\
	fi
	mkdir $(fi_release_directory)
	tar cf - $(fi_release_files) | (cd $(fi_release_directory); tar xf -)
	(cd $(fi_release_directory);tar cf - .|gzip -9 > $(fi_release_gztar))

clman_version = 4.1

clman_files_common	  = Makefile clman.c clman.h clmanaux.c
clman_release_files       = $(clman_files_common) \
			    -C small_manual manual/OBLIST.el \
			    -C small_manual clman.data
clman_release_files_clim2 = $(clman_files_common) manual/OBLIST.el clman.data
clman_data_release	  = $(release_root)/clman.data
clman_release_gztar       = $(release_root)/clman-$(clman_version).tar.gz
clman_release_gztar_clim2 = $(release_root)/clman-$(clman_version)-clim2.tar.gz
clman_release_stats	  = $(release_root)/clman-$(clman_version).stats

# we make two clman dists, one with clim2 an one without

clman-dist-clean:	clean_clman clman-dist

clman-dist:	all_clman
	rm -f $(clman_release_gztar) $(clman_release_gztar_clim2)
	tar cf - $(clman_release_files) | gzip -9 > $(clman_release_gztar)
	tar cf - $(clman_release_files_clim2) | \
		gzip -9 > $(clman_release_gztar_clim2)
	rm -f $(clman_release_stats)
	echo Size of $(clman_release_gztar) uncompressed: \
		>> $(clman_release_stats)
	gzip -d < $(clman_release_gztar) | wc -c \
		>> $(clman_release_stats)
	echo Size of $(clman_release_gztar_clim2) uncompressed: \
		>> $(clman_release_stats)
	gzip -d < $(clman_release_gztar_clim2) | wc -c \
		>> $(clman_release_stats)
	cp -p clman.data $(clman_data_release)

###############################################################################

# removed: girls biggie fax
hosts = ox akbar clay hyper sole fridge
elib_root = /usr/fi/emacs-lib
to = $(elib_root)/fi

rdist = /usr/ucb/rdist

rdist: all
	rm -fr DIST
	${rdist} -qc Makefile ChangeLog *.doc fi-*.el fi-*.elc \
		clman.c clman.h clmanaux.c \
		"`hostname`:`pwd`/DIST"
	(cd DIST;ln -s /net/vapor/scm/emacs-lib/src/eli/clman.data clman.data)
	(cd DIST;ln -s /net/vapor/scm/emacs-lib/src/clman manual)
	(cd DIST; ${rdist} -Rc . "{`echo ${hosts} | sed 's/ /,/g'`}:$(to)")
	for h in ${hosts}; do \
		echo making clman on $$h; \
		rsh $$h "(cd ${to}; make CC=cc clman)"; \
	done
	rm -fr DIST

save_version:
	@if test -z "$(version)"; then echo version= is null; exit 1; fi
	@for host in ${hosts}; do\
		echo rsh $$host -n mv $(to) $(elib_root)/fi-$(version);\
	done
