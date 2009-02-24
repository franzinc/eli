# $Id: emacsdir.pl,v 3.1.42.1 2009/02/24 21:11:39 layer Exp $
#
# This script requires Cygwin Perl

use Win32::Registry;

Win32::Registry::RegOpenKeyEx(&HKEY_LOCAL_MACHINE,
			      'SOFTWARE\Franz Inc.\GNU Emacs Installer',
		0, &KEY_READ, $EmacsKey) ||
	die "Couldn't retrieve emacs_dir from Registry!!\n";

Win32::Registry::RegQueryValueEx($EmacsKey, 'Install_Dir', 0, $type, $emacsdir);

Win32::Registry::RegCloseKey($EmacsKey);

$emacsdir =~ y/A-Z/a-z/;
$emacsdir =~ s/\\/\//;

($drive, $dir) = $emacsdir =~ /([a-zA-Z]):(.*)/;
$emacsdir = "/cygdrive/$drive$dir";

print "$emacsdir";
