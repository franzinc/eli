;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and stored only in accordance with the terms of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure by the Government are subject to
;; restrictions of Restricted Rights for Commercial Software developed
;; at private expense as specified in DOD FAR 52.227-7013 (c) (1) (ii).
;;
;; This file may be distributed without further permission from
;; Franz Inc. as long as
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this package, please feel
;; free to contact Franz Inc. at
;;
;;	Franz Inc.
;;	Attn: Emacs Group Manager
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-sublisp.el,v 1.22 1988/04/27 10:29:51 layer Exp $

;; Interaction with a Lisp subprocess

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Description
;;;
;;;  The code in this file implements the interactively and user callable
;;;  function while in lisp mode, some of which need a socket connection to
;;;  the lisp to be able to work.
;;;
;;;  The commands which need the socket connection are conceptually
;;;  commands that need information from the lisp environment to be able
;;;  work.  Emacs uses a dedicated "backdoor" lisp listener connected by a
;;;  socket to avoid having to type at the listener in the regular inferior
;;;  lisp buffer. The strategy is only to send rather short known forms to
;;;  the backdoor listener, and to have it read all large and
;;;  possibly-erroneous forms out of a temp file. This reduces the
;;;  possibility of the listener getting hung up because someone has
;;;  strange lisp syntax in his buffer, or broken reader macros, or broken
;;;  regular macros.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables
;;;

(defvar fi:package nil
  "A buffer-local variable whose value should either be nil or a string
which names a package in the Lisp world.  It is used when expressions are
sent from Emacs buffers to Lisp so that the expressions are read in the
proper Lisp package.")

(defvar fi:unix-domain t
  "If non-nil, then `fi:unix-domain-socket' specifies the name of the
socket file.  It is recommended that this interface be used, and not
internet ports, because when internet ports are used only one process on a
machine may use this interface (it is a global resource).  When using UNIX
domain sockets, communication is done through a socket file in the user's
home directory.  But, if you really want to use internet ports, here are
the steps to take:

1. Set this variable to nil.
2. Add the following line to /etc/services:
	excl		1123/tcp
3. Make sure `fi:local-host-name' is in /etc/hosts and points to the local
or loopback host.
4. On the Common Lisp side, put the following in you .clinit.cl file:
	(setq ipc:*inet-port* 1123) 	; the number from /etc/services
	(setq ipc:*unix-domain* nil)

The problem with this, is that people can then use `telnet' to get a
listener on your lisp!")

(defvar fi:unix-domain-socket "~/.excl_to_emacs"
  "The name of the socket file that lisp and emacs use to communicate.
This is used when fi:unix-domain is non-nil.")

(defvar fi:local-host-name "localhost"
  "On BSD 4.2 (on SUN) the name of 127.1--usually localhost or loopback.
This is only used when fi:unix-domain is nil.")

(defvar fi:excl-service-name "excl"
  "The service name from /etc/services (`tcp' type).  This is only used
when fi:unix-domain is nil.")

(defvar fi:source-info-not-found-hook 'find-tag
  "The value of this variable is funcalled when source information is not
present in Lisp for a symbol.  The function is given one argument, the name
for which source is desired (a string).  The null string means use the word
at the point as the search word.  This allows the GNU Emacs tags facility
to be used when the information is not present in Lisp.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Visible/Interactively Called Functions
;;;

(defun fi:inferior-lisp-newline ()
  "Bound to newline in an inferior lisp buffer.  At the end of the buffer
it inserts a newline and performs automatic
indentation.  Whole expressions are sent to Lisp (and not each piece after
each newline is typed).  This allows previously types lines to be edited
before Lisp sees the input.  Typed anywhere else in the buffer, this
functions causes the current line, minus the prompt, to be sent to the Lisp
process, or listener when using TCP/IP-style communication."
  (interactive)
  (if (eobp)
      (let ((start (marker-position
		    (process-mark (get-buffer-process (current-buffer)))))
	    (have-list nil))
	(save-excursion
	  (goto-char start)
	  (if (looking-at "(") (setq have-list t)))
	(if have-list
	    (let ((send-sexp t))
	      (goto-char start)
	      (condition-case nil
		  (forward-sexp 1)
		(error (setq send-sexp nil)))
	      (end-of-buffer)
	      (if send-sexp
		  (fi:subprocess-send-input)
		;; not a complete sexp, so newline and indent
		(progn
		  (newline)
		  (funcall indent-line-function)
		  )))
	  ;; a non-list s-exp, so just send it off...
	  (fi:subprocess-send-input)))
    (fi:subprocess-send-input)))

(defun fi:inferior-lisp-send-sexp-input (arg)
  "Send s-expression(s) to the Lisp subprocess."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'sexp))

(defun fi:inferior-lisp-send-list-input (arg)
  "Send list(s) to the Lisp subprocess."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'lists))

(defun fi:lisp-eval-last-sexp (compile-file-p)
  "Send the sexp before the point to the Lisp subprocess.
If a Lisp subprocess has not been started, then one is started.  With a
prefix argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-send start (point) compile-file-p)))

(defun fi:lisp-eval-defun (compile-file-p)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess.  A `top-level'form is one that starts in column 1.  If a Lisp
subprocess has not been started, then one is started.  With a prefix
argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (fi::eval-send start end compile-file-p)))

(defun fi:lisp-eval-region (compile-file-p)
  "Send the text in the region to the Lisp subprocess, one expression at a
time if there is more than one complete expression.  If a Lisp subprocess
has not been started, then one is started.  With a prefix argument, the
source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-send (min (point) (mark))
		 (max (point) (mark))
		 compile-file-p))

(defun fi:lisp-eval-current-buffer (compile-file-p)
  "Send the entire buffer to the Lisp subprocess.  If a Lisp subprocess has
not been started, then one is started.  With a prefix argument, the
source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-send (point-min) (point-max) compile-file-p))

(defun fi:set-associated-sublisp (buffer-name)
  "On a per-buffer basis, this function can be used to set the Lisp process
associated with a Lisp source file.  The buffer name is interactively read
and must be the name of an existing buffer. This process is used when
sending expressions to Lisp for evaluation."
  (interactive "bBuffer name containing a Lisp process: ")
  (let ((process (get-buffer-process (get-buffer buffer-name))))
    (if process
 	(setq fi::sublisp-name (process-name process))
      (error "No process associated with buffer %s" buffer-name))))

;;;
;;; Interactive functions requiring the socket to lisp
;;;

(defun fi:lisp-arglist (&optional symbol)
  "Print the arglist (using excl:arglist) for a symbol, which is read from
the minibuffer.  The word around the point is used as the default."
  (interactive)
  (setq symbol (fi::add-package-info
		(fi::get-cl-symbol nil t (interactive-p) "Function")))
  (process-send-string
   (fi::background-sublisp-process)
   (format
    "(progn
      (format t  \"~:[()~;~:*~{~a~^ ~}~]\"
       (cond
	((macro-function '%s) '(\"%s is a macro\"))
	((special-form-p '%s) '(\"%s is a special form\"))
	((not (fboundp '%s)) '(\"%s has no function binding\"))
	(t (excl::arglist '%s))))
      (values))\n"
    symbol symbol symbol symbol symbol symbol symbol)))

(defun fi:lisp-describe (&optional symbol)
  "Describe a symbol, which is read from the minibuffer.  The word around
the point is used as the default."
  (interactive)
  (setq symbol (fi::add-package-info
		(fi::get-cl-symbol
		 nil nil (interactive-p) "Describe symbol")))
  (process-send-string
   (fi::background-sublisp-process)
   (format "(progn (lisp:describe '%s) (values))\n" symbol)))

(defun fi:lisp-function-documentation (&optional symbol)
  "Print the function documentation for a symbol, which is read from the
minibuffer.  The word around the point is used as the default." 
  (interactive)
  (setq symbol (fi::add-package-info
		(fi::get-cl-symbol
		 nil nil (interactive-p) "Function documentation for symbol")))
  (process-send-string
   (fi::background-sublisp-process)
   (format "(princ (lisp:documentation '%s 'lisp:function))\n" symbol)))

(defun fi:lisp-find-tag (&optional symbol)
  "Find the Common Lisp source for a symbol, using the characters around
the point as the default tag."
  (interactive)
  (setq symbol (fi::get-cl-symbol nil nil (interactive-p) "Source for symbol"))
  (let ((s (fi::add-package-info symbol)))
    (condition-case ()
	(process-send-string
	 (fi::background-sublisp-process)
	 (format "(format t \"\2~s\" (cons '%s (source-file '%s t)))\n"
		 s s))
      (error ;; the backdoor-lisp-listener is not listening...
       (setq fi::tag-state nil)
       (fi::lisp-find-etag (symbol-name symbol))))))

(defun fi:lisp-tags-loop-continue ()
  "Find the next occurrence of the tag last used by fi:lisp-find-tag."
  (interactive)
  (if (and fi::tag-state (cdr fi::tag-state))
      (let ((symbol (car fi::tag-state))
	    (info (cdr (cdr fi::tag-state))))
	(cond (info
	       (setq fi::tag-state (cons symbol info))
	       (fi::find-source-from-lisp-info symbol info t))
	      (t (setq fi::tag-state (cons symbol nil))
		 (if (y-or-n-p
		      "no more source info from lisp, use tags file? ")
		     (progn
		       ;; the following is a crock, but it works
		       (setq last-tag (symbol-name symbol))
		       (find-tag nil t))))))
    (if fi::tag-state
	(setq last-tag (symbol-name (car fi::tag-state))))
    (find-tag nil t)))

(defun fi:tcp-lisp-send-eof ()
  "Do a db:debug-pop on the TCP listener."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-kill-process ()
  "Kill a tcp-lisp process via the backdoor lisp listener: a
mp:process-kill is sent to the lisp."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-kill (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-interrupt-process ()
  "Interrupt the tcp-lisp process via a mp:process-interrupt spoken to the
backdoor lisp listener."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-interrupt
      (mp::process-name-to-process \"%s\")
      #'break \"interrupt from emacs\")\n"
   (buffer-name (current-buffer))))

(defvar fi::lisp-macroexpand-command
  "(progn
    (errorset
     (let ((*print-pretty* t)(excl::*print-nickname* t)(*package* %s))
       (with-open-file (*standard-input* \"%s\")
	 (lisp:prin1 (%s (lisp:read)))))
     t)
    (values))\n")

(defun fi::lisp-macroexpand-common (handler)
  (save-excursion
    (skip-chars-forward " \t")
    (if (not (looking-at "("))
      (up-list -1))
    (let ((filename (format "/tmp/emlisp%d"
			    (process-id
			     (get-process fi::freshest-common-sublisp-name))))
	  (start (point)))
      (forward-sexp)
      (write-region start (point) filename nil 'nomessage)
      (fi::background-sublisp-process)
      (process-send-string
       fi::background-sublisp-process
       (format fi::lisp-macroexpand-command
	       (if (and (boundp 'fi:package) fi:package)
		 (format "(or (find-package :%s) (make-package :%s))"
			 fi:package fi:package)
		 "*package*")
	       filename
	       handler)))))

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point."
  (interactive)
  (fi::lisp-macroexpand-common "lisp:macroexpand"))

(defun fi:lisp-walk (arg)
  "Print the full macroexpansion the form at the point.
With a prefix argument, macroexpand the code as the compiler would."
  (interactive "P")
  (fi::lisp-macroexpand-common
   (if arg "excl::compiler-walk" "excl::walk")))

(defun fi:backdoor-eval (string &rest args)
  "Evaluate apply format to STRING and ARGS and evaluate this in Common
Lisp at the other end of our socket."
  (process-send-string
   (fi::background-sublisp-process)
   (format "(progn (format t \"\1\") %s)\n"
	   (apply 'format string args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internals
;;;

(defun fi::add-package-info (symbol)
  (let ((p (get symbol 'fi:package)))
    (cond (p (format "%s::%s" p symbol))
	  ((and (boundp 'fi:package) fi:package)
	   (format "%s::%s" fi:package symbol))
	  (t (symbol-name symbol)))))

(defun fi::remove-package-info (symbol)
  (let (p s)
    (cond ((string-match ":?:" symbol nil)
	   (setq p (substring symbol 0 (match-beginning 0)))
	   (setq s (intern (substring symbol (match-end 0))))
	   (put s 'fi:package p))
	  (t
	   (setq s (intern symbol))
	   (if (and (boundp 'fi:package) fi:package)
	       (put s 'fi:package fi:package))))
    s))

(defun fi::get-cl-symbol (symbol up-p interactive-p interactive)
  ;; Ask the user for a CL symbol to be investigated.
  ;; If INTERACTIVE, then that prompt is used.
  ;; If UP-P, then the default is the symbol at the car of the form at the
  ;; cursor.  Otherwise, it is the symbol at the cursor.
  (let ((default 
	    (condition-case ()
		(if up-p
		    (save-excursion
		      (buffer-substring (progn (up-list -1)
					       (forward-char 1)
					       (point))
					(progn (forward-sexp 1) (point))))
		  (save-excursion
		    (buffer-substring (progn (forward-char 1)
					     (backward-sexp 1)
					     (skip-chars-forward "#'")
					     (point))
				      (progn (forward-sexp 1) (point)))))
	      (error ""))))
    (if interactive-p
	(setq symbol
	  (read-string
	   (if (equal default "")
	       (format "%s: " interactive)
	     (format "%s (default %s): " interactive default)))))
    (if (or (null symbol) (equal symbol ""))
	(setq symbol default))
    (fi::remove-package-info symbol)))

(defvar fi::background-sublisp-process nil
  "Process connected to sublist socket for fi:lisp-arglist and friends.")

(defvar fi::background-sublisp-form
  "(progn
 (setf (getf (mp:process-property-list mp:*current-process*)
             ':no-interrupts)
       t)
 (loop
  (princ \"\n\")
  (errorset (eval (read)) t)))\n"
 "The program executed by the backdoor lisp listener.")

(defun fi::background-sublisp-process (&optional nomake)
  (if (or (null fi::background-sublisp-process)
	  (not (eq (process-status fi::background-sublisp-process) 'open)))
      (if nomake
	  (setq fi::background-sublisp-process nil)
	(progn
	  (message "trying to start a backdoor lisp listener")
	  (and fi::background-sublisp-process
	       (delete-process fi::background-sublisp-process))
	  (setq fi::background-sublisp-process
	    (if fi:unix-domain
		(open-network-stream
		 "sublisp-back" nil
		 (expand-file-name fi:unix-domain-socket)
		 0)
	      (open-network-stream "sublisp-back" nil fi:local-host-name
				   fi:excl-service-name)))
	  (setq fi::sublisp-returns-state nil)
	  (process-send-string		; first send the process name
	   fi::background-sublisp-process
	   (format "\"%s\"" "GNU Listener"))
	  (process-send-string fi::background-sublisp-process
			       fi::background-sublisp-form)
	  (set-process-filter fi::background-sublisp-process
			      'fi::sublisp-backdoor-filter))))
  fi::background-sublisp-process)

;; This is the filter for the back door lisp process.
;; It collects output until it sees a ctl-A\n, then prints the preceding
;; collected text.  If the text fits on one line, it is printed to the message
;; area.  Otherwise it goes to a temporary pop up buffer.

(defvar fi::sublisp-returns "")
(defvar fi::sublisp-returns-state nil)

(defun fi::sublisp-backdoor-filter (proc string)
  ;; This collects everything returned until a ^A\n prompt is seen,
  ;; then displays it.  The first time is special cased to throw away
  ;; the initial prompt without display.  Someday we should use the state
  ;; variable for detecting screwups and coordinating reset.
  ;; The \n is part of the prompt so that a subsequent prettyprint isn't
  ;; confused about the starting column.
  ;;
  ;; If the input back from lisp starting with the following has special
  ;; meaning:
  ;;  \1  throw away the results (see fi:backdoor-eval)
  ;;  \2  the result is source-file info (see fi::find-source-from-lisp-info)
  ;;
  (setq fi::sublisp-returns (concat fi::sublisp-returns string))
  (let ((len (length fi::sublisp-returns)))
    (if (and (= 10 (aref fi::sublisp-returns (- len 1))); newline
	     (= 1 (aref fi::sublisp-returns (- len 2))))
	(if (eq fi::sublisp-returns-state nil); ignore the startup response
	    (setq fi::sublisp-returns-state t
		  fi::sublisp-returns "")
	  (progn (setq fi::sublisp-returns
		   (substring fi::sublisp-returns
			      (progn (string-match "\n*" fi::sublisp-returns)
				     (match-end 0))
			      -2))
		 (let ((first-char (elt fi::sublisp-returns 0)))
		   (cond
		     ((= first-char 1)
		      ;; throw away the result
		      (setq fi::sublisp-returns ""))
		     ((= first-char 2)	; display source-file
		      (let* ((temp (get-buffer-create " *lisp temp*"))
			     (form (substring fi::sublisp-returns 1)))
			;; first remove all #p's
			(save-excursion
			  (set-buffer temp)
			  (erase-buffer)
			  (princ form temp)
			  (beginning-of-buffer)
			  (replace-string "#p" "")
			  (setq form (car (read-from-string (buffer-string))))
			  (kill-buffer temp))
			(setq fi::sublisp-returns "")
			(setq fi::tag-state form)
			(if (cdr form)
			    (fi::find-source-from-lisp-info
			     (car form) (cdr form))
			  (fi::lisp-find-etag
			   (symbol-name
			    (fi::remove-package-info
			     (symbol-name (car form))))))))
		     (t  
		      (if (or (> (length fi::sublisp-returns) 78)
			      ;; should be mbuf width
			      (string-match "\n" fi::sublisp-returns nil))
			  (with-output-to-temp-buffer "*Help*"
			    (princ fi::sublisp-returns))
			(message fi::sublisp-returns))
		      (setq fi::sublisp-returns "")))))))))

(defvar fi::tag-state nil
  "The last source info requested from cl.  This is used to implement
tags-loop-continue for lisp.")

(defun fi::lisp-find-etag (string)
  (if fi:source-info-not-found-hook
      (funcall fi:source-info-not-found-hook string)
    (message "The source location of `%s' is unknown." string)))

(defun fi::find-source-from-lisp-info (xname info &optional same-window)
  ;; info is an alist of (type . filename)
  (setq info (car info))
  (let* ((type (car info))
	 (file (cdr info))
	 (name (symbol-name xname))
	 (search-form nil))
    (if (not (file-exists-p file))
	(error "Can't file source file: `%s'" file))
    (if same-window
	(find-file file)
      (find-file-other-window file))
    (beginning-of-buffer)
    (cond ((eq ':function type) (setq search-form "un"))
	  ((eq ':macro type) (setq search-form "macro"))
	  (t (setq search-form "\\(\\w\\)*")))
    (let ((start (string-match "::?" name))
	  (end (match-end 0)))
      (if start
	  (setq name
	    (format "\\(%s\\)*[:]*%s"
		    (substring name 0 start)
		    (substring name end))))
      (if (re-search-forward
	   (format "^(def%s %s" search-form name)
	   nil t)
	  (beginning-of-line)
	(message "couldn't find form in file")))))

(defun fi:inferior-lisp-send-input (arg type)
  "Send ARG, which is an s-expression, to the Lisp subprocess. TYPE
must be either 'sexps or 'lists, specifying whether lists or
s-expressions should be parsed (internally, either `(scan-sexps)' or
`(scan-lists)' is used). If at the end of buffer, everything typed since
the last output from the Lisp subprocess is collected and sent to the Lisp
subprocess.  With an argument, only the specified number of s-expressions
or lists from the end of the buffer are sent. If in the middle of the
buffer, the current s-expression(s) or list(s) is(are) copied to the end of
the buffer and then sent. An argument specifies the number of s-expressions
or lists to be sent. If s-expressions are being parsed,the cursor
follows a closing parenthesis, the preceding s-expression(s) is(are)
processed.  If the cursor is at an opening parenthesis, the following
s-expression(s) is(are) processed.  If the cursor is at a closing
parenthesis, the preceding s-expression(s) is(are) processed.  Otherwise,
the enclosing s-expression(s) is(are) processed.  If lists are being
parsed, the enclosing list is processed."
  (if (and (eobp) (null arg))
      (progn
	(move-marker fi::last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(funcall indent-line-function)
	(move-marker fi::last-input-end (point)))

    ;; we are in the middle of the buffer somewhere and need to collect
    ;; and s-exp to re-send

    ;; we grab everything from the end of the current line back to the end
    ;; of the last prompt
    
    (let ((exp-to-resend "")
	  (start-resend (point))
	  (end-resend (point)))
      (if (null arg) (setq arg 1))
      (if (equal type 'sexp)
	  (setq exp-to-resend
	    (buffer-substring
	     (setq start-resend
	       (save-excursion
		 (cond
		   ((= (preceding-char) ?\))
		    (scan-sexps (point) (- arg)))
		   ((= (following-char) ?\() (point))
		   ((= (following-char) ?\))
		    (forward-char 1) (scan-sexps (point) (- arg)))
		   (t (scan-sexps (point) (- arg))))))
	     (setq end-resend
	       (save-excursion
		 (cond
		   ((= (preceding-char) ?\)) (point))
		   ((= (following-char) ?\() (scan-sexps (point) arg))
		   ((= (following-char) ?\)) (forward-char 1) (point))
		   (t (scan-sexps (point) arg)))))))
	(setq exp-to-resend
	  (buffer-substring
	   (setq start-resend (scan-lists (point) (- arg) 1))
	   (setq end-resend (scan-lists (point) arg 1)))))
      (if (eobp)
	  (progn
	    (insert "\n")
	    (funcall indent-line-function)
	    (move-marker fi::last-input-start start-resend)
	    (move-marker fi::last-input-end (point-max)))
	(progn
	  (goto-char (point-max))
	  (move-marker fi::last-input-start (point))
	  (insert exp-to-resend)
	  (if (not (bolp)) (insert "\n"))
	  (move-marker fi::last-input-end (point))))))
  (let ((process (get-buffer-process (current-buffer))))
    (fi::send-region-split process fi::last-input-start fi::last-input-end
			   fi:subprocess-map-nl-to-cr)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi::eval-send (start end compile-file-p)
  "Send the text from START to END over to the sublisp, in the
correct fi:package, of course."
  (fi::sublisp-select)
  (let* ((stuff (buffer-substring start end))
	 (sublisp-process (get-process fi::sublisp-name)))
    (fi::send-string-load
     sublisp-process stuff fi:subprocess-map-nl-to-cr compile-file-p)
    (fi::send-string-split sublisp-process "\n" fi:subprocess-map-nl-to-cr)
    (switch-to-buffer-other-window (process-buffer sublisp-process))
    (goto-char (point-max))))

(defun fi::sublisp-select ()
  "Find a sublisp for eval commands to send code to.  Result stored in
the variable fi::sublisp-name.  If fi::sublisp-name is set, and there is an
associated process buffer, thats that. If fi::sublisp-name is nil, or if
there is no process buffer with that name, then try for
freshest-<franz,common>-sublisp-name, which should contain the name of the
most recently started sublisp.  If neither of these exist, runs the command
franz-lisp or common-lisp, depending on the major mode of the buffer."
  ;; see if sublisp is named yet.  if its not, name it intelligently.
  (cond (fi::sublisp-name t)
	((eql major-mode 'fi:franz-lisp-mode)
	 (if fi::freshest-franz-sublisp-name
	     (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
	   (setq fi::sublisp-name "franz-lisp")))
	((eql major-mode 'fi:common-lisp-mode)
	 (if fi::freshest-common-sublisp-name
	     (setq fi::sublisp-name fi::freshest-common-sublisp-name)
	   (setq fi::sublisp-name "common-lisp")))
	(t (error "Cant start a subprocess for Major mode %s." major-mode)))
  ;; start-up the sublisp process if necessary and possible
  (cond ((get-process fi::sublisp-name) t)
	((eql major-mode 'fi:franz-lisp-mode)
	 (if (and fi::freshest-franz-sublisp-name 
		  (get-process fi::freshest-franz-sublisp-name))
	     (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
	   (setq fi::sublisp-name (prog1
				      (fi:franz-lisp)
				    (switch-to-buffer nil)
				    (sleep-for 5)))))
	((eql major-mode 'fi:common-lisp-mode)
	 (if (and fi::freshest-common-sublisp-name 
		  (get-process fi::freshest-common-sublisp-name))
	     (setq fi::sublisp-name fi::freshest-common-sublisp-name)
	   (setq fi::sublisp-name (prog1
				      (fi:common-lisp)
				    (switch-to-buffer nil)
				    (sleep-for 1)))))
	(t (error "Can't start a subprocess for sublisp-name %s."
		  fi::sublisp-name))))

(defun fi::send-string-load (process text nl-to-cr compile-file-p)
  (if (null fi::emacs-to-lisp-transaction-file)
      (let ()
	(setq fi::emacs-to-lisp-transaction-file
	  (let* ((filename (buffer-file-name (current-buffer))))
	    (format "/tmp/,%s" (file-name-nondirectory filename))))
	(setq fi::emacs-to-lisp-package
	  (if fi:package
	      (format "(in-package :%s)\n" fi:package)
	    nil))
	(setq fi::emacs-to-lisp-transaction-buf
	  (let ((name (file-name-nondirectory
		       fi::emacs-to-lisp-transaction-file)))
	    (or (get-buffer name)
		(create-file-buffer name))))
	(let ((file fi::emacs-to-lisp-transaction-file))
	  (save-window-excursion
	    (pop-to-buffer fi::emacs-to-lisp-transaction-buf)
	    (set 'fi::remove-file-on-kill-emacs file)
	    (set 'fi::remove-file-on-kill-emacs file)))))
  (save-window-excursion
    (let ((file fi::emacs-to-lisp-transaction-file)
	  (pkg fi::emacs-to-lisp-package))
      (pop-to-buffer fi::emacs-to-lisp-transaction-buf)
      (erase-buffer)
      (if pkg (insert pkg))
      (insert text)
      (newline)
      (write-region (point-min) (point-max) file)
      (bury-buffer)))
  (let ((load-string
	 (if compile-file-p
	     (format
	      "(let ((*record-source-files* nil))
 		 (excl::compile-file-if-needed \"%s\")
		 (load \"%s.fasl\"))"
	      fi::emacs-to-lisp-transaction-file
	      (fi::file-name-sans-type fi::emacs-to-lisp-transaction-file))
	   (format "(let ((*record-source-files* nil)) (load \"%s\"))"
		   fi::emacs-to-lisp-transaction-file))))
    (fi::send-string-split process load-string nl-to-cr)))

(defun fi::file-name-sans-type (name)
  "Return FILENAME sans file extension or type."
  (substring name 0
 	     (or (string-match "\\.cl$" name)
 		 (string-match "\\.lisp$" name)
 		 (string-match "\\.l$" name)
 		 (length name))))
