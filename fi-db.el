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
;; $Header: /repo/cvs.copy/eli/fi-db.el,v 1.17 1991/05/28 16:18:43 layer Exp $
;;

(defconst fi::ss-help
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

(defconst fi:scan-stack-mode-map nil)
(defconst fi:scan-stack-mode-display-help t)

;;;;;

(defconst fi::ss-current-frame-regexp "^ ->")
(defconst fi::ss-ok-frame-regexp "^   (")
(defconst fi::ss-show-all-frames nil)

(defvar fi::ss-process-name nil)
(make-variable-buffer-local 'fi::ss-process-name)

(defvar fi::ss-debugger-from-buffer nil)
(make-variable-buffer-local 'fi::ss-debugger-from-buffer)

(defvar fi::ss-saved-window-configuration nil)

(defun fi:scan-stack (&optional all)
  "Debug a Common Lisp process, which is read, with completion, from the
minibuffer.   The \"Initial Lisp Listener\" is the default process.  The
debugging occurs on a stack scan, created by :zoom on the Common Lisp
process. With argument ALL, do a \":zoom :all t\"."
  (interactive "P")
  (setq fi::ss-saved-window-configuration (current-window-configuration))
  (let ((process-name (fi::buffer-process))
	(from-buffer (when fi:subprocess-mode
		       (current-buffer))))
    (make-request (lep::zoom-session :process-name process-name
				     :all (or fi::ss-show-all-frames all))
		  ;; Normal continuation
		  ((from-buffer process-name) (stack)
		   (let ((buffer-name (format "*debugger:%s*"
					      (fi::make-pretty-process-name
					       process-name))))
		     (pop-to-buffer buffer-name)
		     (if buffer-read-only (toggle-read-only))
		     (erase-buffer)
		     (insert (format fi::ss-help
				     (buffer-name
				      (or from-buffer
					  fi::ss-debugger-from-buffer))))
		     (when (null fi:scan-stack-mode-display-help)
		       (fi:ss-hide-help-text))
		     (insert stack)
		     (beginning-of-buffer)
		     (re-search-forward fi::ss-current-frame-regexp)
		     (beginning-of-line)
		     (fi:scan-stack-mode from-buffer process-name)))
		  ;; Error continuation
		  (() (error)
		   (message "Cannot zoom on stack: %s" error)))))

(defun fi::make-pretty-process-name (process-name)
  ;; spaces to hyphens, and remove *'s
  (let ((s process-name)
	(i 0)
	(max (length process-name))
	(res "")
	(c nil))
    (while (< i max)
      (setq c (aref s i))
      (cond ((= ?* c))
	    ((= ?  c)
	     (setq res (concat res "-")))
	    (t (setq res (concat res (char-to-string c)))))
      (setq i (+ i 1)))
    res))

(defun fi:scan-stack-mode (&optional from-buffer process-name)
  "Major mode for debugging a Common Lisp process.
The keymap for this mode is bound to fi:scan-stack-mode-map
\\{fi:scan-stack-mode-map}
Entry to this mode runs the fi:scan-stack-mode-hook hook."
  (let ((saved-from-buffer
	 ;; KILL-ALL-LOCAL-VARIABLES will kill fi::ss-debugger-from-buffer
	 fi::ss-debugger-from-buffer))
    (kill-all-local-variables)
    (setq fi::ss-debugger-from-buffer (or from-buffer
					saved-from-buffer)))
  (setq fi::ss-process-name process-name)
  (setq major-mode 'let:scan-stack-mode)
  (setq mode-name "Scan stack mode")
  (if (null fi:scan-stack-mode-map)
      (let ((map (make-keymap))
	    (ccmap (make-keymap)))
	(define-key ccmap "\C-c"	'fi:ss-continue)
	(define-key ccmap "\C-p"	'fi:ss-pop)
	(define-key ccmap "\C-r"	'fi:ss-reset)
	(define-key map "\C-c"	ccmap)
	
	(define-key map " "	'fi:ss-hide-help-text)
	(define-key map "."	'fi:ss-set-current)
	(define-key map "D"	'fi:ss-disassemble)
	(define-key map "R"	'fi:ss-restart)
	(define-key map "d"	'next-line)
	(define-key map "a"	'fi:ss-toggle-all)
	(define-key map "e"	'fi:ss-edit)
	(define-key map "g"	'fi:ss-revert-stack)
	(define-key map "h"	'fi:ss-unhide-help-text)
	(define-key map "l"	'fi:ss-locals)
	(define-key map "p"	'fi:ss-pprint)
	(define-key map "q"	'fi:ss-quit)
	(define-key map "r"	'fi:ss-return)
	(define-key map "u"	'previous-line)
	(setq fi:scan-stack-mode-map map)))
  (use-local-map fi:scan-stack-mode-map)
  (if (not buffer-read-only) (toggle-read-only))
  (setq truncate-lines t)
  (run-hooks 'fi:scan-stack-mode-hook))

(defun fi:ss-reset ()
  "Do a :reset on the process being debugged.  This causes the process
being debugged to throw out to the outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (fi::do-tpl-command-on-process t nil "reset"))

(defun fi:ss-continue ()
  "Do a :continue on the process being debugged.  This causes the process
being debugged to continue from a continuable error, taking the default
restart (restart number 0)."
  (interactive)
  (fi::do-tpl-command-on-process t nil "continue"))

(defun fi:ss-pop ()
  "Do a :pop on the process being debugged.  This causes the process being
debugged to pop out to the next outer most read-eval-print loop, and
causes the debugger buffer to be buried and the window configuration as it
was before this mode was entered to be restored."
  (interactive)
  (fi::do-tpl-command-on-process t nil "pop"))

(defun fi:ss-return ()
  "Do a :return on the process being debugged.  This causes the process
being debugged to return a value from the current frame, as if the error
never occured.  The form to evaluate to obtain the return value for the
current frame is read from the minibuffer and evaluated in the Common Lisp
environment.  The debugger buffer is buried and the window configuration as
it was before this mode was entered is restored."
  (interactive)
  (fi::do-tpl-command-on-process
   t
   t
   "return"
   (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): " "nil"))))

(defun fi:ss-restart (new-form)
  "Do a :restart on the process being debugged.  This causes the process
being debugged to restart the execution of the function associated with the
current frame.  With argument NEW-FORM, a form to evaluate to obtain the
function and arguments to be restarted is read from the minibuffer and
evaluated in the Common Lisp environment.  The default function and
arguments are the ones in the current frame.   The debugger buffer is
buried and the window configuration as it was before this mode was entered
is restored."
  (interactive "P")
  (fi::do-tpl-command-on-process
   t
   t
   "restart"
   (when new-form
     (list 'read-from-string
	   (read-string "Form (evaluated in the Lisp environment): ")))))

(defun fi:ss-edit ()
  "Find the source file associated with the function in the current frame
and pop up a buffer with that definition visible."
  (interactive)
  (fi::do-tpl-command-on-process nil t "edit"))

(defun fi:ss-revert-stack ()
  "Cause the stack in the debugger buffer to be synchronized with the
actual stack in the Common Lisp environment.  This is useful when commands
are typed in the *common-lisp* buffer which change the state of the process
being debugged."
  (interactive)
  (fi:scan-stack))

(defun fi:ss-toggle-all ()
  "Toggle showing all frames in the currently debugged process stack.  By
default, there are certain types of frames hidden because they offer no
additional information."
  (interactive)
  (setq fi::ss-show-all-frames (not fi::ss-show-all-frames))
  (fi:scan-stack))

(defun fi:ss-set-current ()
  "Make the frame to which the point lies the current frame for future
operations.  It is not necessary to use this command, usually, since most
commands make the frame to which the point lies the current frame before
performing their assigned action."
  (interactive)
  (let ((offset (fi::offset-from-current-frame)))
    (if offset
	(progn
	  (if (> offset 0)
	      (fi::do-tpl-command-on-process
	       nil nil "dn" offset ':zoom nil)
	    (fi::do-tpl-command-on-process
	     nil nil "up" (- offset) ':zoom nil))
	  (fi::make-stack-frame-current offset)))))

(defun fi:ss-quit ()
  "Quit debugging the Common Lisp process.  The debugger buffer is buried
and the window configuration as it was before this mode was entered is
restored."
  (interactive)
  (set-window-configuration fi::ss-saved-window-configuration)
  (end-of-buffer))

(defun fi:ss-disassemble ()
  "Disassemble the function associated with the current frame, putting the
disassembly into a help buffer and positioning the point on the instruction
that will next be executed if the current error can be continued."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (make-request
     (lep::disassemble-session :process-name process-name :offset offset)
     ((offset) (text pc)
      (when offset (fi::make-stack-frame-current offset))
      (fi::show-some-text-1 text nil 'fi::disassemble-hook pc))
     (() (error)
      (message "Cannot dissassemble: %s" error)))))

(defun fi::disassemble-hook (pc)
  (when pc
    (when (re-search-forward (format "^[ \t]*%s:" pc) nil t)
      (beginning-of-line)
      (insert ">"))))

(defun fi:ss-locals ()
  "Find the local variables to the function associated with the current
frame, and display them in a help buffer.  See the Allegro CL compiler
switch compiler:save-local-names-switch for information on accessing local
variables in the debugger."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (make-request
     (lep::local-session :process-name process-name
			 :offset offset)
     ((offset) (text)
      (when offset (fi::make-stack-frame-current offset))
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot find locals: %s" error)))))

(defun fi:ss-pprint ()
  "Pretty print the current frame, function and arguments, into a help
buffer."
  (interactive)
  (let ((process-name (fi::buffer-process))
	(offset (fi::offset-from-current-frame)))
    (make-request
     (lep::pprint-frame-session :process-name process-name
				:offset offset)
     ((offset) (text)
      (when offset (fi::make-stack-frame-current offset))
;;;; figure out how to find the package
      (fi::show-some-text-1 text nil))
     (() (error)
      (message "Cannot pprint: %s" error)))))

(defun fi:ss-hide-help-text ()
  "Hide the help text at the beginning of the debugger buffer."
  (interactive)
  (save-excursion
    (widen)
    (beginning-of-buffer)
    (or (re-search-forward "^Evaluation stack:$" nil t)
	(end-of-buffer))
    (beginning-of-line)
    (narrow-to-region (point) (point-max))
    (setq fi:scan-stack-mode-display-help nil)))

(defun fi:ss-unhide-help-text ()
  "Unhide the help text at the beginning of the debugger buffer."
  (interactive)
  (save-excursion
    (widen))
  (recenter)
  (setq fi:scan-stack-mode-display-help t))

;;;
;;; internals
;;;

(defun fi::do-tpl-command-on-process (done set-current-frame command
				      &rest args)
  (let ((process-name (fi::buffer-process))
	(offset (when set-current-frame
		  (fi::offset-from-current-frame))))
    (make-request (lep::tpl-command-session
		   :process-name process-name
		   :command command
		   :args args
		   :done done
		   :offset offset)
		  ;; Normal continuation
		  ((offset) (done)
		   (if done
		       (fi:ss-quit)
		     (when offset (fi::make-stack-frame-current offset))))
		  ;; Error continuation
		  ((process-name) (error)
		   (message "Lisp error: %s" error)
		   (beep)
		   (sit-for 2)
		   (if (y-or-n-p
			(format "Revert stack from process \"%s\"? "
				process-name))
		       (fi:scan-stack))))))

(defun fi::offset-from-current-frame ()
  (beginning-of-line)
  (if (looking-at fi::ss-current-frame-regexp)
      nil
    (if (not (looking-at fi::ss-ok-frame-regexp))
	(error "Not on a frame."))
    (let* ((down t)
	   (start (point))
	   (end
	    (save-excursion
	      (or (and (re-search-forward fi::ss-current-frame-regexp nil t)
		       (progn (setq down nil) t))
		  (re-search-backward fi::ss-current-frame-regexp nil t)
		  (error "Can't find current frame indicator."))
	      (beginning-of-line)
	      (point)))
	   (lines (count-lines start end)))
      (if down
	  lines
	(- lines)))))

(defun fi::make-stack-frame-current (offset)
  (toggle-read-only)
  (delete-char 3)
  (insert " ->")
  (beginning-of-line)
  (save-excursion
    (forward-line (- offset))
    (unwind-protect
	(progn
	  (if (not (looking-at fi::ss-current-frame-regexp))
	      (error "Not on current frame."))
	  (replace-match "   "))
      (toggle-read-only))))

(defun fi::buffer-process ()
  (cond
   (fi::ss-process-name)
   (t (fi::read-lisp-process-name "Process to debug: "))))

(defun fi::read-lisp-process-name (prompt)
  (let* ((processes
	  (cdr (car (lep::eval-session-in-lisp
		     'lep::list-all-processes-session))))
	 (completions
	  (mapcar (function (lambda (x) (list x))) processes)))
    (completing-read prompt completions nil t
		     "Initial Lisp Listener")))
