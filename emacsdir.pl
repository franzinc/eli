# $Id: emacsdir.pl,v 2.1.10.2.2.1 2000/08/29 22:15:43 layer Exp $
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
# Upcase drive letter for Interix:
$drive = uc $drive;
$emacsdir = "//$drive$dir";

print "$emacsdir";
