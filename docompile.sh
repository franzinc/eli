#! /bin/sh -x
# $Header: /repo/cvs.copy/eli/Attic/docompile.sh,v 1.6 1990/08/31 23:48:46 layer Exp $

# usage: $0 file cl_binary [cl_library]

if test -z "$3"; then
	libdir="excl::*library-code-pathname*"
else
	libdir="\"$3/\""
fi

$2 -qq -batch << EOF
(set-case-mode :case-sensitive-lower)
(require :process)
(require :foreign)
(require :defctype)
(compile-file "$1.cl" :output-file (namestring
				     (merge-pathnames "$1.fasl" $libdir)))
EOF
