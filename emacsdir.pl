# $Id: emacsdir.pl,v 2.5 2002/07/09 22:15:31 layer Exp $
#
# This script requires Active Perl version 502
#  (\\beast\import\pc\perl-win32\APi502e.exe)

use Win32::Registry;

Win32::Registry::RegOpenKeyEx(&HKEY_LOCAL_MACHINE, 'SOFTWARE\GNU\Emacs',
		0, &KEY_READ, $EmacsKey) ||
	die "Couldn't retrieve emacs_dir from Registry!!\n";

Win32::Registry::RegQueryValueEx($EmacsKey, 'emacs_dir', 0, $type, $emacsdir);

Win32::Registry::RegCloseKey($EmacsKey);

$emacsdir =~ y/A-Z/a-z/;
$emacsdir =~ s/\\/\//;

($drive, $dir) = $emacsdir =~ /([a-zA-Z]):(.*)/;
$emacsdir = "/cygdrive/$drive$dir";

print "$emacsdir";
