;;
;; copyright (C) 1991 Franz Inc, Berkeley, Ca.
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
;;	emacs-info@franz.com
;;	uunet!franz!emacs-info
;;
;; $Header: /repo/cvs.copy/eli/fi-db.el,v 1.10 1991/03/13 10:37:30 layer Exp $
;;

(defconst lep:current-frame-regexp "^ ->")
(defconst lep:ok-frame-regexp "^   (")
(defconst lep:scan-stack-mode-map nil)
(defconst lep:debugger-help
    "Debugger commands:

  C-cC-c :continue
  C-cC-p :pop
  C-cC-r :reset
  .      make frame under the point the current frame
  D      disassemble the function
  R      restart function (give prefix to specify different form)
  a      toggle visibility of all frames (by default a subset are visible)
  d      next line
  e      edit source corresponding to function
  g      revert stack from Lisp
  h      Causes this help text to become visible
  l      display the lexical variables
  p      pretty print
  q      switch back to \"%s\" buffer
  r      return a value
  u      previous line             

Type SPACE to hide this help summary.

")
(defconst lep::help-displayed t)
(defconst lep:show-all-frames nil)

(defvar lep::process-name nil)
(make-variable-buffer-local 'lep::process-name)

(defvar lep::debugger-from-buffer nil)
(make-variable-buffer-local 'lep::debugger-from-buffer)

(defvar lep::db-saved-window-configuration nil)

(defun fi:scan-stack (&optional all)
  "Debug a Common Lisp process, which is read, with completion, from the
minibuffer.   The \"Initial Lisp Listener\" is the default process.  The
debugging occurs on a stack scan, created by :zoom on the Common Lisp
process. With a prefix argument, do a \":zoom :all t\"."
  (interactive "P")
  (setq lep::db-saved-window-configuration (current-window-configuration))
  (let ((process-name (lep::buffer-process))
	(from-buffer (when fi:subprocess-mode
		       (current-buffer))))
    (make-request (lep::zoom-session :process-name process-name
				     :all (or lep:show-all-frames all))
		  ;; Normal continuation
		  ((from-buffer process-name) (stack)
		   (let* ((name "*acl debugger*")
			  (big-name (format "%s<%s>" name process-name))
			  (buffer-name
			   (cond ((get-buffer name) name)
				 ((get-buffer big-name) big-name)
				 (t name))))
		     (pop-to-buffer buffer-name)
		     (if buffer-read-only (toggle-read-only))
		     (erase-buffer)
		     (insert (format lep:debugger-help
				     (buffer-name
				      (or from-buffer
					  lep::debugger-from-buffer))))
		     (unless lep::help-displayed
		       (lep:ss-hide-help-text))
		     (insert stack)
		     (beginning-of-buffer)
		     (re-search-forward lep:current-frame-regexp)
		     (beginning-of-line)
		     (lep::scan-stack-mode from-buffer process-name)))
		  ;; Error continuation
		  (() (error)
		   (message "Cannot zoom on stack: %s" error)))))

(defun lep::scan-stack-mode (from-buffer process-name)
  "Major mode for debugging a Common Lisp process.
The keymap for this mode is bound to fi:scan-stack-mode-map
\\{fi:scan-stack-mode-map}
Entry to this mode runs the lep:scan-stack-mode-hook hook."
  (interactive)
  (let ((saved-from-buffer
	 ;; KILL-ALL-LOCAL-VARIABLES will kill lep::debugger-from-buffer
	 lep::debugger-from-buffer))
    (kill-all-local-variables)
    (setq lep::debugger-from-buffer (or from-buffer
					saved-from-buffer)))
  (setq lep::process-name process-name)
  (setq major-mode 'let:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null lep:scan-stack-mode-map)
      (let ((map (make-keymap))
	    (ccmap (make-sparse-keymap)))
	(define-key ccmap "\C-c"	'lep:ss-continue)
	(define-key ccmap "\C-p"	'lep:ss-pop)
	(define-key ccmap "\C-r"	'lep:ss-reset)
	(define-key map "\C-c"	ccmap)
	
	(define-key map " "	'lep:ss-hide-help-text)
	(define-key map "."	'lep:ss-set-current)
	(define-key map "D"	'lep:ss-disassemble)
	(define-key map "R"	'lep:ss-restart)
	(define-key map "d"	'next-line)
	(define-key map "a"	'lep:ss-toggle-all)
	(define-key map "e"	'lep:ss-edit)
	(define-key map "g"	'lep:ss-revert-stack)
	(define-key map "h"	'lep:ss-unhide-help-text)
	(define-key map "l"	'lep:ss-locals)
	(define-key map "p"	'lep:ss-pprint)
	(define-key map "q"	'lep:ss-quit)
	(define-key map "r"	'lep:ss-return)
	(define-key map "u"	'previous-line)
	(setq lep:scan-stack-mode-map map)))
  (use-local-map lep:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'lep:scan-stack-mode-hook))

(defun lep:ss-reset ()
  "Do a :reset on the process being debugged.  This causes the process
being debugged to throw out to the outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (lep::do-tpl-command-on-process t nil "reset"))

(defun lep:ss-continue ()
  "Do a :continue on the process being debugged.  This causes the process
being debugged to continue from a continuable error, taking the default
restart (restart number 0)."
  (interactive)
  (lep::do-tpl-command-on-process t nil "continue"))

(defun lep:ss-pop ()
  "Do a :pop on the process being debugged.  This causes the process being
debugged to pop out to the next outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (lep::do-tpl-command-on-process t nil "pop"))

(defun lep:ss-return ()
  "Do a :return on the process being debugged.  This causes the process
being debugged to return a value from the current frame, as if the error
never occured.  The form to evaluate to obtain the return value for the
current frame is read from the minibuffer and evaluated in the Common Lisp
environment.  The debugger buffer is buried and the window configuration as
it was before this mode was entered is restored."
  (interactive)
  (lep::do-tpl-command-on-process
   t
   t
   "return"
   (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): " "nil"))))

(defun lep:ss-restart (new-form)
  "Do a :restart on the process being debugged.  This causes the process
being debugged to restart the execution of the function associated with the
current frame.  With a prefix argument, a form to evaluate to obtain the
function and arguments is read from the minibuffer and evaluated in the
Common Lisp environment.  The default function and arguments are the ones
in the current frame.   The debugger buffer is buried and the window
configuration as it was before this mode was entered is restored."
  (interactive "P")
  (lep::do-tpl-command-on-process
   t
   t
   "restart"
   (when new-form
     (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): ")))))

(defun lep:ss-edit ()
  "Find the source file associated with the function in the current frame
and pop up a buffer with that definition visible."
  (interactive)
  (lep::do-tpl-command-on-process nil t "edit"))

(defun lep:ss-revert-stack ()
  "Cause the stack in the debugger buffer to be synchronized with the
actual stack in the Common Lisp environment.  This is useful when commands
are typed in the *common-lisp* buffer which change the state of the process
being debugged."
  (interactive)
  (fi:scan-stack))

(defun lep:ss-toggle-all ()
  "Toggle showing all frames in the currently debugged process stack.  By
default, there are certain types of frames hidden because they offer no
additional information."
  (interactive)
  (setq lep:show-all-frames (not lep:show-all-frames))
  (fi:scan-stack))

(defun lep:ss-set-current ()
  "Make the frame to which the point lies the current frame for future
operations.  It is not necessary to use this command, usually, since most
commands make the frame to which the point lies the current frame before
performing their assigned action."
  (interactive)
  (let ((offset (lep::offset-from-current-frame)))
    (if offset
	(progn
	  (if (> offset 0)
	      (lep::do-tpl-command-on-process
	       nil nil "dn" offset ':zoom nil)
	    (lep::do-tpl-command-on-process
	     nil nil "up" (- offset) ':zoom nil))
	  (lep::make-current offset)))))

(defun lep:ss-quit ()
  "Quit debugging the Common Lisp process.  The debugger buffer is buried
and the window configuration as it was before this mode was entered is
restored."
  (interactive)
  (set-window-configuration lep::db-saved-window-configuration)
  (end-of-buffer))

(defun lep:ss-disassemble ()
  "Disassemble the function associated with the current frame, putting the
disassembly into a help buffer and positioning the point on the instruction
that will next be executed if the current error can be continued."
  (interactive)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request
     (lep::disassemble-session :process-name process-name :offset offset)
     ((offset) (text pc)
      (when offset (lep::make-current offset))
      (fi::show-some-text-1 text nil 'lep::disassemble-hook pc))
     (() (error)
      (message "Cannot dissassemble: %s" error)))))

(defun lep::disassemble-hook (pc)
  (when pc
    (when (re-search-forward (format "^[ \t]*%s:" pc) nil t)
      (beginning-of-line)
      (insert ">"))))

(defun lep:ss-locals ()
  "Find the local variables to the function associated with the current
frame, and display them in a help buffer.  See the Allegro CL compiler
switch compiler:save-local-names-switch for information on accessing local
variables in the debugger."
  (interactive)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request
     (lep::local-session :process-name process-name
			 :offset offset)
     ((offset) (text)
      (when offset (lep::make-current offset))
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot find locals: %s" error)))))

(defun lep:ss-pprint ()
  "Pretty print the current frame, function and arguments, into a help
buffer."
  (interactive)
  (let ((process-name (lep::buffer-process))
	(offset (lep::offset-from-current-frame)))
    (make-request
     (lep::pprint-frame-session :process-name process-name
				:offset offset)
     ((offset) (text)
      (when offset (lep::make-current offset))
;;;; figure out how to find the package
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot pprint: %s" error)))))

(defun lep:ss-hide-help-text ()
  "Hide the help text at the beginning of the debugger buffer."
  (interactive)
  (save-excursion
    (widen)
    (beginning-of-buffer)
    (or (re-search-forward "^Evaluation stack:$" nil t)
	(end-of-buffer))
    (beginning-of-line)
    (narrow-to-region (point) (point-max))
    (setq lep::help-displayed nil)))

(defun lep:ss-unhide-help-text ()
  "Unhide the help text at the beginning of the debugger buffer."
  (interactive)
  (save-excursion
    (widen))
  (recenter)
  (setq lep::help-displayed t))

;;;
;;; internals
;;;

(defun lep::do-tpl-command-on-process (done set-current-frame
				       command &rest args)
  (let ((process-name (lep::buffer-process))
	(offset (when set-current-frame
		  (lep::offset-from-current-frame))))
    (make-request (lep::tpl-command-session
		   :process-name process-name
		   :command command
		   :args args
		   :done done
		   :offset offset)
		  ;; Normal continuation
		  ((offset) (done)
		   (if done
		       (lep:ss-quit)
		     (when offset (lep::make-current offset))))
		  ;; Error continuation
		  ((process-name) (error)
		   (message "Lisp error: %s" error)
		   (beep)
		   (sit-for 2)
		   (if (y-or-n-p
			(format "Revert stack from process \"%s\"? "
				process-name))
		       (fi:scan-stack))))))

(defun lep::offset-from-current-frame ()
  (beginning-of-line)
  (if (looking-at lep:current-frame-regexp)
      nil
    (if (not (looking-at lep:ok-frame-regexp))
	(error "Not on a frame."))
    (let* ((down t)
	   (start (point))
	   (end
	    (save-excursion
	      (or (and (re-search-forward lep:current-frame-regexp nil t)
		       (progn (setq down nil) t))
		  (re-search-backward lep:current-frame-regexp nil t)
		  (error "Can't find current frame indicator."))
	      (beginning-of-line)
	      (point)))
	   (lines (count-lines start end)))
      (if down
	  lines
	(- lines)))))

(defun lep::make-current (offset)
  (toggle-read-only)
  (delete-char 3)
  (insert " ->")
  (beginning-of-line)
  (save-excursion
    (forward-line (- offset))
    (unwind-protect
	(progn
	  (if (not (looking-at lep:current-frame-regexp))
	      (error "Not on current frame."))
	  (replace-match "   "))
      (toggle-read-only))))

(defun lep::buffer-process ()
  (cond
   (lep::process-name)
   (t
    (let* ((processes
	    (cdr (car (lep::eval-in-lisp 'lep::list-all-processes-session))))
	   (completions
	    (mapcar (function (lambda (x) (list x))) processes)))
      (completing-read "Process to debug: "
		       completions
		       nil
		       t
		       "Initial Lisp Listener")))))
