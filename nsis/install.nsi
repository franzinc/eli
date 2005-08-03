; $Id: install.nsi,v 1.2 2005/08/03 05:08:34 layer Exp $

!define REGKEY "Software\GNU\Gnu Emacs Easy Installer"
!define ACLREGKEY "Software\Franz Inc.\Allegro Common Lisp"
!define VERBOSE_PROD "Gnu Emacs 21.3"
!define SHORT_PROD "Gnu Emacs"
!define emacs_dir "emacs-21.3"

!include "StrFunc.nsh"
; !include "ReplaceSubStr.nsh"

# Declare used Functions from StrFunc
${StrRep}

SetCompressor lzma

Name "${VERBOSE_PROD}"

; The installer program that will be created
OutFile "emacs213.exe"

; The default installation directory
InstallDir "$PROGRAMFILES\${VERBOSE_PROD}"

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "${REGKEY}" "Install_Dir"

;--------------------------------

; Pages

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

  File /r "${emacs_dir}\*"

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
  var homevar
  var test
  var edited_eli_p
  var eli_init_file
  var eli_handle
  var eli_text
  var allegro_dir

Section "Allegro Emacs-Lisp Interface setup"

  ;; check for Allegro installation first.
  ReadRegStr $allegro_dir HKLM "${ACLREGKEY}" "InstallationDirectory"
  StrLen $test $allegro_dir
  IntCmp $test 0 end_eli_section

  ; Check for existing .emacs file.
  ; if HOME is set, look for .emacs there, else look in 'C:\'

  ReadEnvStr $homevar "HOME"

  ; MessageBox MB_OK|MB_ICONSTOP "HOME dir is '$homevar'"

  StrLen $test $homevar
  IntCmp $test 0 eli_default
  ; need to check for trailing slash here. annoying.
  StrCpy $eli_init_file "$homevar\.emacs"
  goto check_for_eli

eli_default:
  StrCpy $eli_init_file "C:\.emacs"

check_for_eli:
  ;; prompt for modify/create .emacs file.
  IfFileExists "$homevar\.emacs" prompt_to_append prompt_to_create

prompt_to_append:
  MessageBox MB_YESNO|MB_ICONQUESTION \
	"Would you like us to modify $eli_init_file to include sample \
	code for loading the Allegro Emacs-Lisp interface?" \
	/SD IDNO \
	IDNO end_eli_section

  ; append to eli file.
  FileOpen $eli_handle $eli_init_file "a"
  FileSeek $eli_handle 0 END
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  goto write_to_eli

prompt_to_create:
  MessageBox MB_YESNO|MB_ICONQUESTION \
	"Would you like to create $eli_init_file with sample code \
	for loading the Allegro Emacs-Lisp interface?" \
	/SD IDNO \
	IDNO end_eli_section

  ; create eli file.
  FileOpen $eli_handle $eli_init_file "w"

write_to_eli:
  IntOp $edited_eli_p 1 + 0
  FileWrite $eli_handle \
	"; This is sample code for starting and specifying defaults to the"
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  FileWrite $eli_handle \
	"; Emacs-Lisp interface. Uncomment this code if you want the ELI"
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  FileWrite $eli_handle \
	"; to load automatically when you start emacs."
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  ${StrRep} $eli_text '; (push "$allegro_dir/eli" load-path)' '\' '/'
  FileWrite $eli_handle $eli_text
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  FileWrite $eli_handle '; (load "fi-site-init.el")'
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  FileWrite $eli_handle ';'
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  ${StrRep} $eli_text \
	'; (setq fi:common-lisp-image-name "$allegro_dir/mlisp.exe")' \
	'\' '/'
  FileWrite $eli_handle $eli_text
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  ${StrRep} $eli_text \
	'; (setq fi:common-lisp-image-file "$allegro_dir/mlisp.dxl")' \
	'\' '/'
  FileWrite $eli_handle $eli_text
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  ${StrRep} $eli_text '; (setq fi:common-lisp-directory "$allegro_dir")' \
	'\' '/'
  FileWrite $eli_handle $eli_text
  FileWriteByte $eli_handle "13"
  FileWriteByte $eli_handle "10"
  FileClose $eli_handle

end_eli_section:

SectionEnd

Section "Start Menu Shortcuts"

  SetShellVarContext all
!define SMDIR "$SMPROGRAMS\${SHORT_PROD}"

  CreateDirectory "${SMDIR}"
  CreateShortCut "${SMDIR}\Uninstall.lnk" "$INSTDIR\uninstall.exe"
SectionEnd

Section "Run Emacs after install"

  MessageBox MB_YESNO|MB_ICONQUESTION|MB_TOPMOST \
	"Would you like to start Emacs now?" \
	/SD IDNO \
	IDNO norun

  IntCmp $edited_eli_p 0 open_without_initfile
  Exec '"$INSTDIR\bin\runemacs.exe" "$eli_init_file"'
  goto norun
open_without_initfile:
  Exec '"$INSTDIR\bin\runemacs.exe "'

norun:

SectionEnd

;--------------------------------

; Uninstaller

Section "Uninstall"
  
  SetShellVarContext all

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

  Rmdir /r "$SMPROGRAMS\Gnu Emacs"

SectionEnd
