# $Header: /repo/cvs.copy/eli/Makefile,v 1.25 1988/11/22 23:07:29 layer Exp $

cl = /usr/local/cl3.0
emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    tools/doc.elc rlogin.elc shell.elc keys.elc tcplisp.elc utils.elc\
	    ltags.elc

precompile = (set-case-mode :case-sensitive-lower)(require :process)(require :foreign)(require :cstructs)

.SUFFIXES:
.SUFFIXES: .el .elc .cl .fasl
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

.cl.fasl : 
	echo '$(precompile)(compile-file "$*.cl")' | $(cl) -qq -batch

all:	ipc.fasl emacs.fasl depend spec.out
	@rm -f stamp; date > stamp

depend: ${elc-files}

spec.out:	spec.n ${elc-files} tools/doc.el
	$(emacs) -batch -q -l tools/doc.elc

FASL = XXX
CL = XXX
HOST = XXX

$(FASL):	$(CL)
	rsh $(HOST) -n rm -f /tmp/$?
	rcp $? $(HOST):/tmp/$?
	echo '$(precompile)(compile-file "/tmp/$?" :output-file "/tmp/$@")' | rsh $(HOST) $(cl) -qq -batch
	rcp $(HOST):/tmp/$@ $@

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

tags:
	(cd ../..; etags lisp/fi/*.el lisp/local/*.el lisp/*.el src/*.[ch])
