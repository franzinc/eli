#! /bin/sh -ex
# $Header: /repo/cvs.copy/eli/Attic/docompile.sh,v 1.5 1989/08/16 13:51:59 layer Rel $

# usage: $0 file cl_binary [cl_library]

if test -z "$3"; then
	libdir="excl::*library-code-pathname*"
else
	libdir="\"$3\""
fi

$2 -qq -batch << EOF
(set-case-mode :case-sensitive-lower)
(require :process)
(require :foreign)
(require :cstructs)
(compile-file "$1.cl" :output-file (namestring
				     (merge-pathnames "$1.fasl" $libdir)))
EOF
