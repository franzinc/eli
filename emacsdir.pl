# $Id: emacsdir.pl,v 2.1 1998/03/18 20:28:53 layer Exp $
require 'NT.ph';

Win32::RegOpenKeyEx(&HKEY_LOCAL_MACHINE, 'SOFTWARE\GNU\Emacs',
		&NULL, &KEY_ALL_ACCESS, $EmacsKey) ||
	die "Couldn't retrieve emacs_dir from Registry!!\n";

Win32::RegQueryValueEx($EmacsKey, 'emacs_dir', &NULL, $type, $emacsdir);

Win32::RegCloseKey($EmacsKey);

$emacsdir =~ y/A-Z/a-z/;
$emacsdir =~ s/\\/\//;
$emacsdir =~ s/([a-zA-Z]):(.*)/\/\/\1\2/;

print "$emacsdir";
