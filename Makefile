.SUFFIXES:
.SUFFIXES: .el .elc
.el.elc : 
	$(emacs) -nw -batch -q -l bytecomp -f batch-byte-compile $*.el

emacs = /usr/local/emacs

elc-files = modes.elc subproc.elc sublisp.elc filec.elc ring.elc\
	    doc.elc rlogin.elc shell.elc

all:	depend spec.out

depend: ${elc-files}

spec.out:	spec.n ${elc-files} doc.el
	$(emacs) -batch -q -l doc.elc

pr = enscript -Plw -h -2r

print:; ${pr} Makefile *.el

e = /usr/tech/layer/emacs

tags:
	(cd ${e}; etags ${e}/lisp/fi/*.el ${e}/lisp/local/*.el)

backup:
	rdist -Rc . binky:emacs.save/fi

install:
	rdist -c spec.out *.el *.elc akbar:/usr/local/lib/emacs/lisp/fi
