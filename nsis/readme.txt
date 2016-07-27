
This is the Easy Emacs Installer.

How to build:

- Download and Install NSIS v 2.01 from http://nsis.sourceforge.net
- Download the Emacs version of choice from
  http://ftp.gnu.org/pub/gnu/emacs/windows/

  For the current version I downloaded emacs-21.3-fullbin-i386.tar.gz

- Create a directory and copy install.nsi into it.
- Extract emacs into this directory. This should create an emacs-<version>/
  subdirectory.
- If you have downloaded a different version of emacs, modify the variables
  in install.nsi that make explicit reference to Emacs version:

  VERBOSE_PROD, emacs_dir, and OutFile

- Launch the NSIS Compilation utility. NSIS.exe is a quick-launch app.
  You can launch the compilation utility or launch makensisw.exe directly.

  You can load a script via the menu or by dragging install.nsi into
  the window. It says you need to right-click and choose some option
  to start the compile, but for me it started once the drag operation
  completed.

- wait a while. A file by the name of OutFile should be created in 
  the same directory when complete.



NOTES:

- The Easy Installer installs Emacs
- Runs addpm.exe to setup start menu shortcuts for Emacs.
- Adds an uninstall shortcut to the Emacs folder (see below)
- Checks for an ACL installation via the registry. If found
  will check for a .emacs file (see below) and ask to append/create
  to one, sample code for starting the ELI automatically.

NOTES FOR EMACS UNINSTALLATION.

There is no emacs uninstaller. However, found relevant info at

  http://roso.epfl.ch/mbi/biogeme/EMACS-README.TXT

There may be a more informative source.

* Uninstalling Emacs

  If you should need to uninstall Emacs, simply delete all the files and
  subdirectories from the directory where it was unpacked (Emacs does
  not install or update any files in system directories or anywhere
  else).  If you ran the addpm.exe program to create the registry
  entries and the Start menu icon, then you can remove the registry
  entries using regedit.  All of the settings are written under the
  Software\GNU\Emacs key in HKEY_LOCAL_MACHINE, or if you didn't have
  administrator privileges, the same key in HKEY_CURRENT_USER.  Just
  delete the Software\GNU\Emacs key.

  The Start menu entry can be removed by right-clicking on the Task bar
  and selecting Properties, then using the Remove option on the Start
  Menu Programs page.  (If you installed under an account with
  administrator privileges, then you need to click the Advanced button
  and look for the Gnu Emacs menu item under All Users.)


NOTES FOR HOW EMACS FINDS THE .EMACS FILE.

Found at: http://www.gnu.org/software/emacs/windows/faq3.html

We do not check the registry for HOME locations.

Where do I put my .emacs, (or _emacs), file?

Your startup file can be named either _emacs or .emacs, and should be
placed in your "home" directory. If you have both files in your home
directory, Emacs will load the .emacs file and ignore _emacs.

Your home directory is where the HOME configuration variable tells
Emacs it is. As with the other Emacs configuration variables, HOME can
be set in a number of ways:

    * Environment variable: Emacs will always first check to see if
      you have an environment variable named HOME set and use that
      value as your home directory. Under Windows 95, you can set the
      HOME environment variable in your autoexec.bat file (you will
      need to reboot); under NT, you can set the HOME environment
      variable in the System panel of the Control Panel. (Note that if
      you set HOME as an environment variable in the System panel it
      will only take effect in new processes created after you've
      closed the System panel; if it still does not appear to be
      working, then you might try rebooting.)
    * Registry: If Emacs doesn't find an environment variable named
      HOME, it will check the registry for a value named HOME and use
      that value as your home directory. Emacs first checks the key
      "HKEY_CURRENT_USER\\SOFTWARE\\GNU\\Emacs", and then the key
      "HKEY_LOCAL_MACHINE\\SOFTWARE\\GNU\\Emacs" (although the use of
      the latter key is discouraged since HOME should be set on a
      per-user basis, not on a per-machine basis). Note that the value
      of HOME is not set by the addpm.exe program that initializes the
      registry with the other Emacs configuration variables.

If Emacs cannot find HOME set in any of the above locations, it will
assume your HOME directory is "C:/".

The directory specified by HOME is also the directory substituted for
tildes (~) in file names used in Emacs (so, by definition, you can
always load your startup file in Emacs by doing C-x C-f "~/.emacs").
