;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are the property 
;; of Franz, Inc.  
;;
;; This file (or any derivation of it) may be distributed without 
;; further permission from Franz Inc. as long as:
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, other than a tape
;;	  fee, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this interface, please feel
;; free to contact Franz Inc. at
;;	Franz Inc.
;;	Attn: Kevin Layer
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;;	(415) 548-3600
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

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

;; $Header: /repo/cvs.copy/eli/fi-sublisp.el,v 1.32 1988/07/15 18:33:15 layer Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Visibles
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi:pop-to-sublisp-buffer-after-lisp-eval t
  "*If non-nil, then after sending expressions to a Lisp process do pop to
the buffer which contains the Lisp.")

(defvar fi:package nil
  "A buffer-local variable whose value should either be nil or a string
which names a package in the Lisp world (ie, in a Lisp subprocess running
as an inferior of Emacs in some buffer).  It is used when expressions are
sent from an Emacs buffer to a Lisp process so that the symbols are read
into the correct Lisp package.")

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
		 (setq fi::freshest-common-sublisp-name proc-name))
		((eq mode 'fi:franz-lisp-mode)
		 (setq fi::freshest-franz-sublisp-name proc-name)))
	  (while buffers
	    (if (eq mode (fi::symbol-value-in-buffer 'major-mode
						     (car buffers)))
		(fi::set-in-buffer 'fi::sublisp-name proc-name
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
    (if fi:pop-to-sublisp-buffer-after-lisp-eval
	(progn
	  (switch-to-buffer-other-window (process-buffer sublisp-process))
	  (goto-char (point-max))))))

(defun fi::eval-string-send (string compile-file-p &optional always-pop-to-p)
  "Send STRING to the sublisp, in the correct package, of course."
  (fi::sublisp-select)
  (let ((sublisp-process (get-process fi::sublisp-name)))
    (fi::send-string-load
     sublisp-process string fi:subprocess-map-nl-to-cr compile-file-p)
    (fi::send-string-split sublisp-process "\n" fi:subprocess-map-nl-to-cr)
    (if (or always-pop-to-p fi:pop-to-sublisp-buffer-after-lisp-eval)
	(progn
	  (switch-to-buffer-other-window (process-buffer sublisp-process))
	  (goto-char (point-max))))))

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
	((eq major-mode 'fi:inferior-common-lisp-mode)
	 (setq fi::sublisp-name fi::freshest-common-sublisp-name))
	((eq major-mode 'fi:inferior-franz-lisp-mode)
	 (setq fi::sublisp-name fi::freshest-franz-sublisp-name))
	((eq major-mode 'fi:franz-lisp-mode)
	 (if fi::freshest-franz-sublisp-name
	     (setq fi::sublisp-name fi::freshest-franz-sublisp-name)
	   (setq fi::sublisp-name "franz-lisp")))
	((eq major-mode 'fi:common-lisp-mode)
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
	    (format "/tmp/%s,%s"
		    (user-login-name)
		    (if filename (file-name-nondirectory filename)
		      "noname"))))
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

(defun fi:remove-all-temporary-lisp-transaction-files ()
  "This function will clean up all the files created in /tmp for Emacs/Lisp
communication.  The files are named /tmp/<USER-LOGIN-NAME>,<BUFFER-NAME>."
  (let ((buffers (buffer-list))
	  file)
      (while buffers
	(setq file (fi::symbol-value-in-buffer
		    'fi::remove-file-on-kill-emacs (car buffers)))
	(if (and file (file-exists-p file)) (delete-file file))
	(setq buffers (cdr buffers)))))

(make-variable-buffer-local 'fi::remove-file-on-kill-emacs)
