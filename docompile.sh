#! /bin/sh -ex
# $Header: /repo/cvs.copy/eli/Attic/docompile.sh,v 1.4 1989/08/07 18:48:40 layer Exp $

# usage: $0 file cl_binary [cl_library]

libdir=${3-excl::*library-code-pathname*}

$2 -qq -batch << EOF
(set-case-mode :case-sensitive-lower)
(require :process)
(require :foreign)
(require :cstructs)
(compile-file "$1.cl" :output-file (namestring
				     (merge-pathnames "$1.fasl" $libdir)))
EOF
