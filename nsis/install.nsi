; $Id: install.nsi,v 1.1.2.1 2004/10/07 21:15:06 layer Exp $

!define REGKEY "Software\Franz Inc.\Allegro Emacs Easy Installer"
!define VERBOSE_PROD "Gnu Emacs 21.3"
!define SHORT_PROD "Gnu Emacs"

SetCompressor bzip2

Name "${VERBOSE_PROD}"

; The installer program that will be created
OutFile "instemacs213.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\${VERBOSE_PROD}"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "${REGKEY}" "Install_Dir"

LicenseData binary-license.txt

;--------------------------------

; Pages

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "${VERBOSE_PROD}"

  SectionIn RO
  
  ; Set output path to the installation directory.
  SetOutPath "$INSTDIR"

  File /r "emacs-21.3\*"
  File "binary-license.txt"

  ; maybe use this for .emacs file?
  ;IfFileExists "$INSTDIR\nfs.cfg" +2
	; File /oname=nfs.cfg nfs.cfg.default

  ; Write the installation path into the registry
  WriteRegStr HKLM "${REGKEY}" "Install_Dir" "$INSTDIR"

!define UNINSTMAIN "Software\Microsoft\Windows\CurrentVersion\Uninstall"
!define UNINSTKEY "${UNINSTMAIN}\${SHORT_PROD}"
  
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "${UNINSTKEY}" "DisplayName" "${VERBOSE_PROD}"
  WriteRegStr HKLM "${UNINSTKEY}" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoModify" 1
  WriteRegDWORD HKLM "${UNINSTKEY}" "NoRepair" 1
  WriteUninstaller "uninstall.exe"

  ExecWait $INSTDIR\bin\addpm.exe

SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"

!define SMDIR "$SMPROGRAMS\${SHORT_PROD}"

  CreateDirectory "${SMDIR}"
  CreateShortCut "${SMDIR}\Uninstall.lnk" "$INSTDIR\uninstall.exe"
SectionEnd

Section "Run Emacs after install"

  MessageBox MB_YESNO|MB_ICONQUESTION|MB_TOPMOST \
	"Would you like to start Emacs now?" \
	IDNO norun

  Exec '"$INSTDIR\bin\runemacs.exe"'

norun:

SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  ; Remove registry keys
  DeleteRegKey HKLM "${UNINSTKEY}"
  DeleteRegKey HKLM "${REGKEY}"

  ; have to manually remove emacs registry key
  DeleteRegKey HKLM "Software\GNU\Emacs"

  ; Remove files and uninstaller
  Rmdir /r $INSTDIR

  ; Remove directories used
  RMDir /r "${SMDIR}"
  RMDir /rebootok "$INSTDIR"

  SetShellVarContext all
  Rmdir /r "$SMPROGRAMS\Gnu Emacs"

SectionEnd
