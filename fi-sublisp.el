;; If you have any comments or questions on this interface, please feel
;; free to contact Franz Inc. at
;;	Franz Inc.
;;	Attn: Kevin Layer
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;;	(415) 548-3600
;; or
;;	emacs-info@franz.com
;;	uunet!franz!emacs-info

;; This file has its (distant) roots in lisp/shell.el, so:
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;;
;; This file is derived from part of GNU Emacs.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; $Header: /repo/cvs.copy/eli/fi-sublisp.el,v 1.54 1991/03/12 18:30:57 layer Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Visibles
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:emacs-to-lisp-transaction-directory "/tmp"
  "*The directory in which files for Emacs/Lisp communication are stored.
When using Lisp and Emacs on different machines, this directory should be
accessible on both machine with the same pathname (via the wonders of NFS).
The users home directory is often a good place to store these temporary
files, since it is usually accessible.")

(defvar fi:pop-to-sublisp-buffer-after-lisp-eval t
  "*If non-nil, then go to the Lisp subprocess buffer after sending
expressions to Lisp (via fi:lisp-eval-* function).")

(defvar fi:package nil
  "A buffer-local variable whose value should either be nil or a string
which names a package in the Lisp world (ie, in a Lisp subprocess running
as an inferior of Emacs in some buffer).  It is used when expressions are
sent from an Emacs buffer to a Lisp process so that the symbols are read
into the correct Lisp package.")

(make-variable-buffer-local 'fi:package)

(defvar fi:echo-evals-from-buffer-in-listener-p nil
  "*If non-nil, forms evalutated directly in fi:common-lisp-mode by the
fi:lisp-eval-* functions will be echoed by Common Lisp.")

(defun fi:set-associated-sublisp (buffer-name)
  "When evaluated in a Lisp source buffer causes further `eval'
commands (those which send expressions from Emacs to Lisp) to use
BUFFER-NAME as the buffer which contains a Lisp subprocess.  If evaluated
when not in a Lisp source buffer, then the process type is read from the
minibuffer (\"common-lisp\" or \"franz-lisp\").  The buffer name is
interactively read and must be the name of an existing buffer.  New buffers
with the same mode as the current buffer will also use BUFFER-NAME for
future `eval' commands."
  (interactive "bBuffer name containing a Lisp process: ")
  (let* ((process (get-buffer-process (get-buffer buffer-name)))
	 (mode (or (and (memq major-mode '(fi:common-lisp-mode
					   fi:franz-lisp-mode))
			major-mode)
		   (let* ((alist '(("common-lisp" . fi:common-lisp-mode)
				   ("franz-lisp" . fi:franz-lisp-mode)))
			  (type (completing-read "Lisp type: "
						 alist nil t "common-lisp")))
		     (cdr (assoc type alist))))))
    (if process
	(let ((buffers (buffer-list))
	      (proc-name (process-name process)))
	  (cond ((eq mode 'fi:common-lisp-mode)
		 (setq fi:common-lisp-process-name proc-name))
		((eq mode 'fi:franz-lisp-mode)
		 (setq fi:franz-lisp-process-name proc-name)))
	  (while buffers
	    (if (eq mode (fi::symbol-value-in-buffer 'major-mode
						     (car buffers)))
		(fi::set-in-buffer 'fi::process-name proc-name
				   (car buffers)))
	    (setq buffers (cdr buffers))))
      (error "There is no process associated with buffer %s!"
	     buffer-name))))

;;;;
;;; Internals
;;;;

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
    ;;
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
		   ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
		   ((= (following-char) ?\() (point))
		   ((= (following-char) ?\))
		    (forward-char 1) (scan-sexps (point) (- arg)))
		   ((not (memq (char-syntax (preceding-char)) '(?w ?_)))
		    (point))
		   (t (scan-sexps (point) (- arg))))))
	     (setq end-resend
	       (save-excursion
		 (cond
		   ((= (preceding-char) ?\)) (point))
		   ((= (following-char) ?\() (scan-sexps (point) arg))
		   ((= (following-char) ?\)) (forward-char 1) (point))
		   ((not (memq (char-syntax (following-char)) '(?w ?_)))
		    (point))
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
    (send-region process fi::last-input-start fi::last-input-end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi::eval-send (start end compile-file-p)
  "Send the text from START to END over to the sublisp, in the
correct fi:package, of course."
  (fi::sublisp-select)
  (let* ((stuff (buffer-substring start end))
	 (sublisp-process (get-process fi::process-name)))
    (fi::send-string-load sublisp-process stuff compile-file-p)
    (send-string sublisp-process "\n")
    (if fi:pop-to-sublisp-buffer-after-lisp-eval
	(progn
	  (select-window
	   (display-buffer (process-buffer sublisp-process)))
	  (goto-char (point-max))))))

(defun fi::eval-string-send (string &optional compile-file-p)
  "Send STRING to the sublisp, in the correct package, of course."
  (fi::sublisp-select)
  (let ((sublisp-process (get-process fi::process-name)))
    (fi::send-string-load sublisp-process string compile-file-p)
    (send-string sublisp-process "\n")
    (if fi:pop-to-sublisp-buffer-after-lisp-eval
	(progn
	  (select-window
	   (display-buffer (process-buffer sublisp-process)))
	  (goto-char (point-max))))))

(defun fi::sublisp-select ()
  "Find a sublisp for eval commands to send code to.  Result stored in
the variable fi::process-name.  If fi::process-name is set, and there is an
associated process buffer, thats that. If fi::process-name is nil, or if
there is no process buffer with that name, then try for
freshest-<franz,common>-sublisp-name, which should contain the name of the
most recently started sublisp.  If neither of these exist, runs the command
franz-lisp or common-lisp, depending on the major mode of the buffer."
  ;; see if sublisp is named yet.  if its not, name it intelligently.
  (cond (fi::process-name)
	((or (eq major-mode 'fi:inferior-common-lisp-mode)
	     (eq major-mode 'fi:lisp-listener-mode))
	 (setq fi::process-name fi:common-lisp-process-name))
	((eq major-mode 'fi:inferior-franz-lisp-mode)
	 (setq fi::process-name fi:franz-lisp-process-name))
	((eq major-mode 'fi:franz-lisp-mode)
	 (if fi:franz-lisp-process-name
	     (setq fi::process-name fi:franz-lisp-process-name)))
	((eq major-mode 'fi:common-lisp-mode)
	 (if fi:common-lisp-process-name
	     (setq fi::process-name fi:common-lisp-process-name)))
	(t (error "Cant start a subprocess for Major mode %s." major-mode)))
  ;; start-up the sublisp process if necessary and possible
  (cond ((and fi::process-name
	      (let ((p (get-process fi::process-name)))
		(fi:process-running-p p))))
	((eq major-mode 'fi:franz-lisp-mode)
	 (if (and fi:franz-lisp-process-name 
		  (get-process fi:franz-lisp-process-name))
	     (setq fi::process-name fi:franz-lisp-process-name)
	   (setq fi::process-name (save-excursion (fi:franz-lisp)))))
	((eq major-mode 'fi:common-lisp-mode)
	 (if (and fi:common-lisp-process-name 
		  (get-process fi:common-lisp-process-name))
	     (setq fi::process-name fi:common-lisp-process-name)
	   (setq fi::process-name (save-excursion (fi:common-lisp)))))
	(t (error "Can't start a subprocess for sublisp-name %s."
		  fi::process-name)))
  (if (processp fi::process-name)
      (setq fi::process-name (process-name fi::process-name)))
  nil)

(make-variable-buffer-local 'fi::emacs-to-lisp-transaction-file)
(make-variable-buffer-local 'fi::emacs-to-lisp-package)
(make-variable-buffer-local 'fi::emacs-to-lisp-transaction-buf)

(setq-default fi::emacs-to-lisp-transaction-file nil)
(setq-default fi::emacs-to-lisp-package nil)
(setq-default fi::emacs-to-lisp-transaction-buf nil)

(defun fi::send-string-load (process text compile-file-p)
  (let (pkg (source-file (buffer-file-name)))
    (if (or (null fi::emacs-to-lisp-transaction-file)
	    (null fi::emacs-to-lisp-transaction-buf))
	(progn
	  (or fi::emacs-to-lisp-transaction-file
	      (setq fi::emacs-to-lisp-transaction-file
		(format "%s/%s.cl"
			fi:emacs-to-lisp-transaction-directory
			(make-temp-name "EtoL"))))
	  (setq fi::emacs-to-lisp-package
	    (if fi:package
		(format "(in-package :%s)\n" fi:package)
	      nil))
	  (setq fi::emacs-to-lisp-transaction-buf
	    (get-buffer-create
	     (format " %s" (file-name-nondirectory
			    (or buffer-file-name
				(buffer-name (current-buffer)))))))
	  (let ((file fi::emacs-to-lisp-transaction-file))
	    (save-window-excursion
	      (set-buffer fi::emacs-to-lisp-transaction-buf)
	      (set 'fi::remove-file-on-kill-emacs file)
	      (set 'fi::remove-file-on-kill-emacs file)))))
    (setq pkg fi::emacs-to-lisp-package)
    (save-window-excursion
      (let ((file fi::emacs-to-lisp-transaction-file))
	(set-buffer fi::emacs-to-lisp-transaction-buf)
	(erase-buffer)
	(if (and pkg (not fi:echo-evals-from-buffer-in-listener-p))
	    (insert pkg))
	(insert (format "#+(version>= 4 1) (setq excl::*partial-source-file-p* t excl::*source-pathname* #P\"%s\")
" source-file))
	(insert text)
	(write-region (point-min) (point-max) file nil 'nomessage)
	(bury-buffer)))
    (let ((load-string
	   (if compile-file-p
	       (format
		"(let (#-(version>= 4 1) (excl::*record-source-files* nil)
		       (*package* *package*))
		   %s
 		   (excl::compile-file \"%s\")
		   (load (format nil \"%s.~a\" sys::*fasl-default-type*) :verbose nil)
                   (values))"
		(if pkg pkg "")
		fi::emacs-to-lisp-transaction-file
		(fi::file-name-sans-type fi::emacs-to-lisp-transaction-file))
	     (if fi:echo-evals-from-buffer-in-listener-p
		 (format "(with-open-file (istm \"%s\")
			 (let (#-(version>= 4 1) (excl::*record-source-files* nil)
			       (*package* *package*)
			       (stm (make-echo-stream istm *terminal-io*)))
			   %s
			   (princ \";; eval from emacs: \") (fresh-line)
			   (load stm :verbose nil :print t)
			   (values)))"
			 fi::emacs-to-lisp-transaction-file
			 (if pkg pkg ""))
	       (format "(let (#-(version>= 4 1) (excl::*record-source-files* nil)
			      (*package* *package*))
			   %s
			   (princ \";; Loading forms from buffer \\\"%s\\\".\") (fresh-line)
			   (load \"%s\" :verbose nil)
                           (values))"
		       (if pkg pkg "")
		       (buffer-name)
		       fi::emacs-to-lisp-transaction-file)))))
      (send-string process load-string))))



(defun fi:remove-all-temporary-lisp-transaction-files ()
  "This function will clean up all the files created for Emacs-Lisp
communication.  See the variable fi:emacs-to-lisp-transaction-directory for
the location of the files.  A good place to call this function is in the
kill-emacs-hook."
  (let ((buffers (buffer-list))
	  file)
      (while buffers
	(setq file (fi::symbol-value-in-buffer
		    'fi::remove-file-on-kill-emacs (car buffers)))
	(if (and file (file-exists-p file)) (delete-file file))
	(setq buffers (cdr buffers)))))

(make-variable-buffer-local 'fi::remove-file-on-kill-emacs)
