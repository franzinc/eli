# $Id: version.mak,v 3.1 2004/01/16 19:27:49 layer Exp $

VERSION = $(shell grep Revision: ChangeLog | sed -e 's,.*Revision: \([0-9.]*\).*,\1,')
