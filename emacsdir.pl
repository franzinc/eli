#
# This script requires Cygwin Perl

use Win32::Registry;

Win32::Registry::RegOpenKeyEx(
    &HKEY_LOCAL_MACHINE,
    'SOFTWARE\Franz Inc.\GNU Emacs Installer',
    0, &KEY_READ, $EmacsKey) ||
# 64-bit Windows
    Win32::Registry::RegOpenKeyEx(
	&HKEY_LOCAL_MACHINE,
	'SOFTWARE\Wow6432Node\Franz Inc.\GNU Emacs Installer',
	0, &KEY_READ, $EmacsKey) ||
    die "Couldn't retrieve emacs_dir from Registry!!\n";

Win32::Registry::RegQueryValueEx($EmacsKey, 'Install_Dir', 0, $type, $emacsdir);

Win32::Registry::RegCloseKey($EmacsKey);

$emacsdir =~ y/A-Z/a-z/;
$emacsdir =~ s/\\/\//;

($drive, $dir) = $emacsdir =~ /([a-zA-Z]):(.*)/;
$emacsdir = "/cygdrive/$drive$dir";

print "$emacsdir";
