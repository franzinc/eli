;; See the file LICENSE for the full license governing this code.

;; All variable definitions for ELI.  Moved here to minimize 
;; warnings during the byte-compile process.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-basic-lep.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi::trace-lep-filter nil)	; for debugging

(defvar eli--session nil)
(defvar eli--connection nil)
(defvar eli--process nil)

;; Why are these necessary????
(defvar resize-mini-windows)
(defvar max-mini-window-height)

(defvar fi::show-some-text-1-first-time t)

(defvar fi::*show-some-text-buffer-name* "*CL-temp*")

(defvar fi::*connection* nil)

(defvar fi:use-emacs-mule-lisp-listeners t
  "*Flag which determines whether to set a buffer's Lisp listener's
*terminal-io* stream-external-format to :emacs-mule.  This flag should
normally be set to true.  Setting this flag to nil restores the
buffer-coding-systems setting behavior to be as it was in the Allegro CL 6.2
release.")

(defvar fi::default-lisp-connection-coding-system 'utf-8)

(defvar fi::debug-subprocess-filter nil)
(defvar fi::debug-subprocess-filter-output nil)

(defvar fi:lisp-evalserver-number-reads 200
  "*The number of times the Lisp eval server tries to read from the
lisp-evalserver process before giving up.  Without this feature Emacs would
hang if Lisp got into an infinite loop while printing.  If the size of the
values returned to Emacs is large, then the value of this variable should
be increased.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-emacs21.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi::connection-open-composer-loaded nil)

(defvar fi::connection-open-composer-loaded-cached nil)

(defvar fi::composer-connection-open nil)
(defvar fi::composer-running nil)

(defvar fi::composer-cached-connection nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-filec.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:shell-token-pattern "[ \t\n()<>&|;=]"
  "*The regular expression used by file name completion to mark path name
boundaries.")

(defvar fi::shell-completions-window nil
  "If non-nil, completion window requires cleaning up.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-indent.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:lisp-electric-semicolon nil
  "*If non-nil, semicolons that begin comments are indented as they are
inserted into the buffer.")

(make-variable-buffer-local 'fi:lisp-electric-semicolon)

(defvar fi:lisp-comment-indent-specification '(40 t nil 0)
  "*Specification list for indentations of semicolon comments.
The nth element of the list specifies the indentation for a comment beginning
with n semicolons (e.g. the first element of the list is the indentation for
comments beginning with one semicolon).  Each element of the list may be one
of `t' (indent comment just like an s-expression), `nil' (don't change the
indentation of the comment, i.e. leave the semicolon where it is), a non-
negative integer (specifying the absolute column to which the comment is to
be indented), or a negative integer (specifying a negative offset for the
comment relative to the current column).

NOTE: if the buffer local variable comment-column is changed, then the
first element of fi:lisp-comment-indent-specification is changed to contain
the value of comment-column.")

(make-variable-buffer-local 'fi:lisp-comment-indent-specification)

(defvar fi:lisp-body-indent 2
  "*The indentation of a list continued on another line.")

(make-variable-buffer-local 'fi:lisp-body-indent)

(defvar fi:lisp-indent-offset nil
  "*If non-nil, then indent by a constant amount of the column in which the
sexp starts.")

(make-variable-buffer-local 'fi:lisp-indent-offset)

(defvar fi:lisp-indent-hook-property 'fi:lisp-indent-hook
  "*The indicator for the lisp-indentation hook property of a symbol.
This variable is buffer-local.")

(make-variable-buffer-local 'fi:lisp-indent-hook-property)

(defvar fi:lisp-tag-indentation 1
  "*Indentation of tags relative to containing list.
This variable is used by the function `fi:lisp-indent-tagbody' to indent tags
that occur within special forms whose symbols have a 'fi:lisp-indent-hook
property of 'tag or 'tagbody.  The indentation is relative to the
indentation of the parenthesis enclosing the special form.")

(make-variable-buffer-local 'fi:lisp-tag-indentation)

(defvar fi:lisp-tag-body-indentation 2
  "*Indentation of non-tagged lines relative to containing list.
This variable is used by the function `fi:lisp-indent-tagbody' to indent normal
lines (lines without tags) that occur within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'tag or 'tagbody.  The indentation is
relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the body of tags will be indented as a block at the same
indentation as the first s-expression following the tag.  In this case, the
s-expressions before the first tag are indented as an undistinguished
form.")

(make-variable-buffer-local 'fi:lisp-tag-body-indentation)

(defvar fi:lisp-tag-indentation-hook nil
  "*Name of function to apply to return indentation of tag.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the tag,
the last parse state, and the indent point) to return the appropriate
indentation for tags occurring within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'tag or 'tagbody.  The indentation
returned is absolute.")

(make-variable-buffer-local 'fi:lisp-tag-indentation-hook)

(defvar fi:lisp-keyword-indentation 1
  "*Indentation of keywords relative to containing list.
This variable is used by the function `fi:lisp-indent-keyword-list' to indent
keywords that occur within special forms whose symbols have a
'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The
indentation is relative to the indentation of the parenthesis enclosing the
special form.")

(make-variable-buffer-local 'fi:lisp-keyword-indentation)

(defvar fi:lisp-keyword-argument-indentation t
  "*Indentation of keyword argument lines relative to containing list.
This variable is used by the function `fi:lisp-indent-keyword-list' to indent
keyword-argument lines that occur within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The indentation
is relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the argument(s) of keywords will be indented as a block
at the same indentation as the first s-expression following the tag.  See
the documentation for the function `fi:lisp-indent-keyword-list'.")

(make-variable-buffer-local 'fi:lisp-keyword-argument-indentation)

(defvar fi:lisp-keyword-indentation-hook nil
  "*Name of function to apply to return indentation of a keyword.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the keyword,
the last parse state, and the indent point) to return the appropriate
indentation for keywords occurring within special forms whose symbols have
a 'fi:lisp-indent-hook property of 'keyword or 'keyword-list.  The inden-
tation returned is absolute.")

(make-variable-buffer-local 'fi:lisp-keyword-indentation-hook)

(defvar fi:lisp-maximum-indent-struct-depth 3
  "*Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is NIL, no backtracking will occur and lists whose `car'
is a symbol with a 'fi:lisp-indent-hook property of 'label, 'labels, 'flet,
'macrolet, 'defun, or a list may not be indented properly.  In addition,
quoted lists will not be treated specially.  If this variable is T, there
is no limit placed on backtracking.  A numeric value specifies the maximum
depth to backtrack.  A reasonable value is 3.")

(make-variable-buffer-local 'fi:lisp-maximum-indent-struct-depth)

(defvar fi:indent-methods-case-sensitive t
  "*If non-nil, the code that is being edited is for a case-sensitive dialect
of Lisp.  This variable is buffer-local.  If a Lisp is case-insensitive,
indentation specifications should be placed on the Emacs Lisp symbol that
corresponds to the lowercase name of the function, macro, or special
form.")

(make-variable-buffer-local 'fi:indent-methods-case-sensitive)

(defvar fi:lisp-package t
  "*This variable may be NIL, T, or a symbol or string.
If the value is NIL, a package qualifier is ignored when getting the
indentation specification for a symbol.  If the value is T, the package
qualifier is not ignored.  If this variable is any other symbol or a string,
it names the package to be used for all unqualified symbols.  When this
variable is not NIL, the qualified symbol is first checked for an indentation
specification, then the unqualified symbol is checked.  This variable is
buffer-local.")

(make-variable-buffer-local 'fi:lisp-package)


(defconst fi:lisp-indent-hook 'fi:lisp-indent-hook
  "Function funcalled to calculate the indentation at a specific point.
Called with two arguments, indent-point and `state'.")

(make-variable-buffer-local 'fi:lisp-indent-hook)

(defvar fi::comment-indent-hook-values '(0 nil))

(make-variable-buffer-local 'fi::comment-indent-hook-values)

(defvar fi::lisp-most-recent-parse-result nil
  "Most recent parse result: point at parse end and parse state.
A list that is the `cons' of the point at which the most recent
parse ended and the parse state from `fi::parse-partial-sexp'.")

(make-variable-buffer-local 'fi::lisp-most-recent-parse-result)

(defvar fi::calculate-lisp-indent-state-temp nil
  "Used as the last argument to fi::parse-partial-sexp so we do as little
consing as is possible.")

(make-variable-buffer-local 'fi::calculate-lisp-indent-state-temp)

(defvar fi::lisp-indent-state-temp nil
  "Used as the last argument to fi::parse-partial-sexp so we can do as
little consing as possible.")

(make-variable-buffer-local 'fi::lisp-indent-state-temp)

(defvar fi::lisp-doing-electric-semicolon nil)

(defconst fi::semi-char 59)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-keys.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:subprocess-super-key-map nil
  "Used by fi:subprocess-superkey as the place where super key bindings are
kept.  Buffer local.")

(make-variable-buffer-local 'fi:subprocess-super-key-map)

(defvar fi:superkey-shadow-universal-argument t
  "*If non-nil, then make C-u a superkey in subprocess modes, otherwise
leave it alone.")

(defvar fi:find-tag-lock t
  "*If non-nil, then the first time find-tag or find-tag-other-window are
executed a buffer will be displayed explaining the method for finding
Lisp definitions.")

(defvar fi:check-unbalanced-parentheses-when-saving t
  "*If non-nil, for the Lisp editing modes (Common Lisp, Emacs Lisp, and
Franz Lisp) check for unbalanced parentheses before writing the file.
If the value is T, then ask whether or not the file should be written ``as
is'' if there are too many or few parens--answering no leaves the point at
the place of error.  If the value is 'warn, then a warning is issued and
the file is written.")

(defvar fi:define-global-lisp-mode-bindings t
  "*This variable is obsolete.  Use fi:legacy-keybindings instead.")

(defvar fi:legacy-keybindings t
  "*If non-nil then define the global, legacy keybindings, which in some
cases are in violation of the Elisp major mode conventions outlined in the
Emacs Lisp Manual.  For compatibility reasons the value of this variable is
`t' by default.")

(defvar fi:menu-bar-single-item t
  "*If non-nil then put a single item onto the menu-bar.  Otherwise, the
sub-menus in the single menu are put onto the menu-bar.  This variable is
ignored in all but XEmacs and Emacs 21 and later.")

(defvar fi:arglist-on-space t
  "*If non-nil, then bind SPC to a function that retrieves arglist
information and displays it according to the value of the variable
fi:auto-arglist-pop-up-style.")

(defvar fi:use-web-documentation t
  "*If non-nil, then fi:lisp-function-documentation is replaced with
fi:manual for looking up documentation in the Allegro CL documentation
set.")

(defvar fi::.defkey-marker. (cons 'not-a 'keymap))

(defvar fi:lisp-mode-auto-indent t
  "*If non-nil, then the command bound to \\r, fi:lisp-mode-newline, will
indent, newline and indent.  It does this by funcalling the value bound to
indent-line-function.")


(defvar fi:raw-mode nil
  "*If non-nil, then the inferior lisp process gets characters as they are
typed, not when a complete expressions has been entered.  This means that
no input editing of expressions can occur.  The intention of this feature
is that it be used by programs written in Common Lisp that need to read
characters without newlines after them.  See the example in the Allegro
User Guide for more information.")

(defvar fi:raw-mode-echo t
  "*If non-nil, then echo characters in the inferior lisp buffer when
fi:raw-mode is non-nil.")

(defvar fi::wc-stack nil)
(defconst fi::wc-stack-max 15)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-lze.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi::show-compilation-status nil)
(make-variable-buffer-local 'fi::show-compilation-status)

(defvar fi::mode-line-note-for-compile " COMPILING")
(defvar fi::mode-line-note-for-eval " EVALUATING")

(defvar fi:lisp-evals-always-compile t
  "*This variable controls whether or not the fi:lisp-eval-or-compile-*
functions will compile or evaluate their forms.  If non-nil, then
compilation is the default, otherwise evaluation is the default.
The non-default functionality can be selected by using a prefix argument.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-modes.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:inferior-common-lisp-mode-map nil
  "The inferior-common-lisp major-mode keymap.")
(defvar fi:inferior-common-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-common-lisp mode.")

(defvar fi:inferior-franz-lisp-mode-map nil
  "The inferior-franz-lisp major-mode keymap.")
(defvar fi:inferior-franz-lisp-mode-super-key-map nil
  "Used for super-key processing in inferior-franz-lisp mode.")

(defvar fi:lisp-listener-mode-map nil
  "The tcp-lisp major-mode keymap.")
(defvar fi:lisp-listener-mode-super-key-map nil
  "Used for super-key processing in tcp-lisp mode.")

(defvar fi:common-lisp-mode-map nil
  "Major mode map used when editing Common Lisp source.")
(defvar fi:franz-lisp-mode-map nil
  "Major mode map used when editing Franz Lisp source.")
(defvar fi:emacs-lisp-mode-map nil
  "Major mode map used when editing GNU Emacs Lisp source.")

(defvar fi:lisp-mode-syntax-table nil
  "The value of which is the syntax table for all Lisp modes, except Emacs
Lisp mode.")
(defvar fi:emacs-lisp-mode-syntax-table nil
  "The value of which is the syntax table for Emacs Lisp mode.")

(defvar fi:common-lisp-file-types '(".cl" ".lisp" ".lsp")
  "*A list of the file types which are automatically put in
fi:common-lisp-mode.  NOTE: the value of this variable is only used at
interface load time.  Setting after the interface is loaded will have no
effect.")

(defvar fi:lisp-do-indentation t
  "*When non-nil, do FI-style indentation in Lisp modes.")

(defvar fi:auto-fill nil
  "*When non-nil, and fi:lisp-do-indentation is non-nil, turn on auto-fill
mode in Lisp editing modes.")

(defvar fi:subprocess-mode nil
  "Non-nil when buffer has a subprocess.")

(defvar fi:in-package-regexp nil
  "*If non-nil, the regular expression that describes the IN-PACKAGE form,
for purposes of tracking package changes in a subprocess Lisp buffer.  The
value of this is taken from fi:default-in-package-regexp in Lisp subprocess
buffers, but is nil elsewhere.")
(make-variable-buffer-local 'fi:in-package-regexp)

(defvar fi::multiple-in-packages nil
  ;; non-nil if there are multiple ones in the current buffer
  )
(make-variable-buffer-local 'fi::multiple-in-packages)

(defvar fi:default-in-package-regexp
  "(\\(cl:\\|common-lisp:\\)?in-package\\>\\|:pa\\>\\|:pac\\>\\|:pack\\>\\|:packa\\>\\|:packag\\>\\|:package\\>"
  "*The regular expression matching the Lisp expression to change the
current package.  The two things this must match are the IN-PACKAGE macro
form and all the possible instances of the :package top-level command.
If nil, no automatic package tracking will be done.")

(defvar fi::menubar-initialization nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-ring.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:default-input-ring-max 50
  "*The default maximum length to which an input ring is allowed to grow.")

(defvar fi::input-ring nil
  "A list of previous input to a subprocess.")

(make-variable-buffer-local 'fi::input-ring)

(defvar fi::input-ring-max fi:default-input-ring-max
  "Maximum length of input ring before oldest elements are thrown away.")

(make-variable-buffer-local 'fi::input-ring-max)

(defvar fi::input-ring-yank-pointer nil
  "The tail of the input ring whose car is the last thing yanked.")

(make-variable-buffer-local 'fi::input-ring-yank-pointer)

(defvar fi::last-input-search-string ""
  "Last input search string in each fi::subprocess buffer.")

(make-variable-buffer-local 'fi::last-input-search-string)

(defvar fi::last-command-was-successful-search nil
  "Switch to indicate that last command was a successful input re-search.")

(make-variable-buffer-local 'fi::last-command-was-successful-search)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-shell.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:shell-mode-map nil
  "The shell major-mode keymap.")

(defvar fi:shell-mode-super-key-map nil
  "Used for super-key processing in shell mode.")

(defvar fi:shell-image-name
    (if (on-ms-windows)
	(format "%s/system32/cmd.exe" (getenv "WINDIR"))
      "csh")
  "*Default Shell image to invoke from (fi:shell).  If the value
is a string then it names the image file or image path that
`fi:shell' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:shell-image-arguments
    (if (on-ms-windows)
	'("/q")
      '("-i"))
  "*Default Shell image arguments when invoked from (fi:shell).")

(defvar fi:shell-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in shell mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:shell-mode-use-history nil
  "*If non-nil when fi:shell-mode is first entered, setup a binding that
causes ! to do history processing and substitute the values from the
history list into the current command line.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; fi-subproc.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:shell-cd-regexp ":?cd"
  "*The regular expression matching the C shell `cd' command and the
Common Lisp :cd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-cd-regexp)

(defvar fi:shell-popd-regexp ":?popd"
  "*The regular expression matching the C shell `popd' command and the
Common Lisp :popd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-popd-regexp)

(defvar fi:shell-pushd-regexp ":?pushd"
  "*The regular expression matching the C shell `pushd' command and the
Common Lisp :pushd top-level command.   If nil, no tracking of directory
changes will be done.")
(make-variable-buffer-local 'fi:shell-pushd-regexp)

(defvar fi:subprocess-continuously-show-output-in-visible-buffer t
  "*If t, output from a subprocess to a visible buffer is continuously
shown.  If a subprocess buffer is visible and the window point is beyond
the process output marker, output to that buffer from its associated
process will be continuously visible.  If the window point is before the
process output marker, the window is not updated.  This is a buffer-local
symbol.")
(make-variable-buffer-local
 'fi:subprocess-continuously-show-output-in-visible-buffer)

(defvar fi:subprocess-enable-superkeys nil
  "*If t, certain keys become `superkeys' in subprocess buffers--this
should be set before starting any subprocesses.  The superkeys are C-a,
C-d, C-o,C-u, C-w, C-z, and C-\\, which will behave as they would in the
current local keymap when typed at the end of a subprocess buffer.  If
typed elsewhere, these keys have their normal global binding.  This is a
buffer-local symbol.  Use setq-default to set the default value for this
symbol.")
(make-variable-buffer-local 'fi:subprocess-enable-superkeys)

(defvar fi:new-screen-for-common-lisp-buffer nil
  "*If non-nil, then starting Common Lisp will cause emacs to create a new
screen for the *common-lisp* buffer, if this version of emacs is capable of
creating separate screens.  If you redefine fi:display-buffer-function this
variable will be ignored.")

(defvar fi:display-buffer-function
    'fi::switch-to-buffer-new-screen
  "*If non-nil, then the value should be a function taking one argument,
a buffer, which is used to display a buffer when a subprocess is created.")

(defvar fi:subprocess-env-vars
    '(("EMACS" . "t")
      ("TERM" . "emacs")
;;;; This is the *wrong* thing to do, since it will override the DISPLAY
;;;; set by `ssh'.
      ;;("DISPLAY" . (or (getenv "DISPLAY") (format "%s:0.0" (system-name))))
      ("TERMCAP" . (format "emacs:co#%d:tc=unknown:" (frame-width))))
  "*An alist containing the environment variables to pass to newly created
subprocesses.")
(when (on-ms-windows)
  (setq fi:subprocess-env-vars
    (append fi:subprocess-env-vars
	    '(("SHELL")))))

(defvar fi:user-env-vars nil
  ;; concept courtesy of John M. Adams
  "*An alist containing environment variable/value pairs to add to the
environment of the Lisp started with fi:common-lisp.")

(defvar fi:pop-to-sublisp-buffer-after-lisp-eval nil
  "*If non-nil, then go to the Lisp subprocess buffer after sending
expressions to Lisp (via the functions which eval or compile the region, a
form or the entire buffer).")

(defvar fi:package nil
  "A buffer-local variable whose value is automatically set for editing
modes and will be nil or a string which names a package in the Lisp
world--ie, in a Lisp subprocess running as an inferior of Emacs in some
buffer.  It is used when expressions are sent from an Emacs buffer to a
Lisp process so that the symbols are read into the correct Lisp package.")

(make-variable-buffer-local 'fi:package)

(defvar fi:readtable nil
  "A buffer-local variable whose value is automatically set for editing
modes and will be nil or a string which names a readtable in the Lisp
world--ie, in a Lisp subprocess running as an inferior of Emacs in some
buffer.  It is used when expressions are sent from an Emacs buffer to a
Lisp process so that the expressions are read using the correct
readtable.")

(make-variable-buffer-local 'fi:readtable)

(defvar fi::started-via-file nil
  "If non-nil, then ELI started via fi:start-interface-via-file.")

 
;;;;
;;; Common Lisp Variables and Constants
;;;;

(defvar fi:emacs-to-lisp-transaction-directory
    nil
  "*The directory in which files for Emacs/Lisp communication are stored.
When using Lisp and Emacs on different machines, this directory must be
accessible on both machine with the same pathname (via the wonders of NFS).")

(defvar fi:echo-evals-from-buffer-in-listener-p nil
  "*If non-nil, functions which eval a region, form, or an entire buffer will
echo evaluated forms and results directly in the initial Lisp listener buffer.
If nil, the results of evalation will be printed in the minibuffer if they
fit, or otherwise in a popup buffer.")

(defvar fi:start-lisp-interface-arguments
    'fi::start-lisp-interface
  "*This value of this variable determines whether or not the emacs-lisp
interface is started automatically when fi:common-lisp is used to run
Common Lisp images.   If non-nil, then a the value of this variable should
be a function of one argument that returns command line argument sufficient
to start the emacs-lisp interface.  The argument is a boolean that
determines whether or not background streams are used (see
fi:use-background-streams).")

(defvar fi:use-background-streams t
  "*If non-nil, then the default function bound to
fi:start-lisp-interface-arguments will cause background streams to be
initialized in ACL (see the function excl:use-background-streams).  Roughly
speaking, background streams cause processes that do output, but which are
not associated with any particular stream, to do it in a unique listener in
an emacs buffer.  This allows MP:PROCESS-RUN-FUNCTION in the Lisp
environment to be more useful.")

(defvar fi:start-lisp-interface-hook nil
  "*A function or a list of functions to call when we get the rendezvous
info from Lisp in the Lisp subprocess buffer.  This is used to
automatically startup the Emacs-Lisp hidden communication via a socket.
fi:start-lisp-interface-arguments initiates the connection with Lisp from
the Emacs side, Lisp then starts up the daemon which listens for
connections and prints a string to the subprocess buffer (which is not
displayed by the process filter for the Common Lisp subprocess), at which
time the hooks are run.")

(defvar fi:common-lisp-buffer-name "*common-lisp*"
  "*Default buffer name used by fi:common-lisp.  This variable is set by
fi:common-lisp when a new buffer name is used.")

(defvar fi:common-lisp-directory nil
  "*Default directory in which the process started by fi:common-lisp uses.")

(defvar fi:common-lisp-image-name "alisp"
  "*Default Common Lisp executable image used by fi:common-lisp.  The value
is a string that names the executable image fi:common-lisp invokes.")

(defvar fi:common-lisp-image-file nil
  "*Default Common Lisp heap image used by fi:common-lisp.  If this
variable is nil, and the corresponding argument to fi:common-lisp is not
given, then a default heap image is loaded.")

(defvar fi:common-lisp-image-arguments
    (if (on-ms-windows) '("+B" "+cn") nil)
  "*Default Common Lisp image arguments when invoked from `fi:common-lisp',
which must be a list of strings.  Each element of the list is one command
line argument.")

(defvar fi:common-lisp-host "localhost"
  "*The host on which fi:common-lisp starts the Common Lisp
subprocess.  The default is the host on which emacs is running.")

(defvar fi:common-lisp-prompt-pattern
    "^\\(\\[[0-9]+i?c?\\] \\|\\[step\\] \\)?\\(<[-A-Za-z.]* ?[0-9]*?>\\|[-A-Za-z.0-9]+([0-9]+):\\) "
  "*The regular expression which matches the Common Lisp prompt.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be a prompt.")

;;;;
;;; Franz Lisp Variables and Constants
;;;;

(defvar fi:franz-lisp-buffer-name "*franz-lisp*"
  "*Default buffer name used by fi:franz-lisp.")

(defvar fi:franz-lisp-image-name "lisp"
  "*Default Franz Lisp image to invoke from `fi:franz-lisp'.  If the value
is a string then it names the image file or image path that
`fi:franz-lisp' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:franz-lisp-image-arguments nil
  "*Default Franz Lisp image arguments when invoked from `fi:franz-lisp'.")

(defvar fi:franz-lisp-host nil
  "*The host on which fi:franz-lisp starts the Franz Lisp
subprocess.  The default is the host on which emacs is running.")

(defvar fi:franz-lisp-process-name nil)

(defvar fi:franz-lisp-prompt-pattern
  "^[-=]> +\\|^c{[0-9]+} +"
  "*The regular expression which matches the Franz Lisp prompt, used in
Inferior Franz Lisp mode.  Anything from beginning of line up to the end
of what this pattern matches is deemed to be a prompt.")

;;;;;;;;;;;;;;;;;;;;;; general subprocess internal variables

(defvar fi::last-input-start nil
  "Marker for start of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")
(make-variable-buffer-local 'fi::last-input-start)

(defvar fi::last-input-end nil
  "Marker for end of last input in fi:shell-mode or fi:inferior-lisp-mode
buffer.")
(make-variable-buffer-local 'fi::last-input-end)

(defvar fi::shell-directory-stack nil
  "List of directories saved by pushd in this buffer's shell.")
(make-variable-buffer-local 'fi::shell-directory-stack)

(defvar fi::prompt-pattern)
(make-variable-buffer-local 'fi::prompt-pattern)

(defvar fi::process-is-local t)
(make-variable-buffer-local 'fi::process-is-local)

(defvar fi:franz-lisp-directory nil)

(defconst fi:subprocess-max-buffer-lines nil
  "*If non-nil, keep buffers created by fi:common-lisp, et al to a maximum
of this number of lines when inserting new output.")

;;;;;;;;;;;;;;;;;;;;;; lisp mode specific internal variables

(defvar fi::process-name nil
  "Name of inferior lisp process.")
(make-variable-buffer-local 'fi::process-name)

(defvar fi::common-lisp-first-time t)

(defvar fi::franz-lisp-first-time t)

;;;;;;;;;;;;;;;;;;;;;; common lisp mode specific internal variables

(defvar fi::common-lisp-backdoor-main-process-name nil
  "The name of the Common Lisp process to which we have a backdoor
connection.")

(defvar fi::lisp-version nil
  "The version of the remote lisp.")

(defvar fi::listener-protocol ':listener)

;;;; the rest are buffer local:

(defvar fi::lisp-host nil
"Host that is running the lisp in this buffer.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-host)

(defvar fi::lisp-port nil
"Port to use in getting new listeners from remote lisp.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-port)

(defvar fi::lisp-password nil
"Password to use in getting new listeners from remote lisp.  Buffer local.")
(make-variable-buffer-local 'fi::lisp-password)

(defvar fi::lisp-is-remote nil
  "Non-nil if the lisp process tied to the current buffer is on another
machine, which implies that it was started via an `rsh'.  This variable is
buffer local.")
(make-variable-buffer-local 'fi::lisp-is-remote)

(defvar fi::setup-for-mule nil)
(make-variable-buffer-local 'fi::setup-for-mule)

(defvar fi::common-lisp-connection-type nil
  "When creating an inferior cl process, the value to bind
process-connection-type (q.v.).")

(defvar fi:common-lisp-subprocess-timeout 30
  "*This variable only has an effect on Windows.  The value is the timeout,
in seconds, that Emacs will wait for Lisp to startup, when no connection
can be made in fi:common-lisp.")

(defvar fi::common-lisp-compatibility-mode-timeout 1)

(defvar fi:common-lisp-subprocess-wait-forever nil
  "*This variable only has an effect on Windows.  If the value is non-nil,
then wait forever for the Lisp to startup.  Use with caution.  The
keyboard-quit function will interrupt the waiting, however.")

(defvar minibuffer-confirm-incomplete)

(defvar fi:eli-compatibility-mode t
  "*If non-nil, then check for a Lisp running on port 9666, which is the
port used by ACL 6.2 and before.  Set this variable to `nil' if you have
problems starting Lisp from Emacs and only use the Emacs and Lisp portions
from the same Lisp distribution.")

(defvar fi::cl-process-name "common-lisp")

(defvar fi::tcp-listener-table nil)
;; Start counting at 2 because initial lisp listener is assigned 1.
(defvar fi::tcp-listener-generation 2)

;; cached & discombobulated value of CDPATH env variable
(defvar fi::cdpath nil)

(defvar fi::rsh-command nil)
(defvar fi::rsh-args '("-Y"))
