@rem $Id: make.bat,v 2.3 1996/09/04 00:03:05 layer Exp $
@echo off

if exist c:\emacs-19.34\bin\runemacs.exe goto set1
if exist d:\emacs-19.34\bin\runemacs.exe goto set2

goto cannotfind

:set1
set emacs_dir=c:\emacs-19.34
goto donmake

:set2
set emacs_dir=d:\emacs-19.34
goto donmake

:donmake
nmake -nologo -f makefile.win32 emacs_dir=%emacs_dir% %1 %2 %3 %4 %5 %6 %7 %8 %9
goto exit

:cannotfind
echo Cannot find emacs directory.
goto exit

:exit
