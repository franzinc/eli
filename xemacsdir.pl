# $Id: xemacsdir.pl,v 3.0 2003/12/15 22:52:58 layer Exp $
#
# This script requires Active Perl version 502 or later
#  (\\beast\import\pc\perl-win32\APi502e.exe)

use Win32::Registry;

Win32::Registry::RegOpenKeyEx(&HKEY_LOCAL_MACHINE, 'SOFTWARE\GNU\XEmacs',
		0, &KEY_ALL_ACCESS, $EmacsKey) ||
	die "Couldn't retrieve emacs_dir from Registry!!\n";

Win32::Registry::RegQueryValueEx($EmacsKey, 'emacs_dir', 0, $type, $emacsdir);

Win32::Registry::RegCloseKey($EmacsKey);

$emacsdir =~ y/A-Z/a-z/;
$emacsdir =~ s/\\/\//g;

($drive, $dir) = $emacsdir =~ /([a-zA-Z]):(.*)/;
# Upcase drive letter for Interix:
$drive = uc $drive;
$emacsdir = "//$drive$dir";

print "$emacsdir";
