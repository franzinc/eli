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
;; $Header: /repo/cvs.copy/eli/fi-leep.el,v 1.2 1991/08/22 21:30:01 layer Exp $

;; The epoch side of presentations in a lisp-listener window.

;;(defvar fi::normal-button-style nil)
;;(defvar fi::normal-style nil)
(defvar fi::highlighted-button-style nil)
(defvar fi::highlighted-style nil)

(defun fi::initialize-for-presenting-listeners ()
  ;;(when fi::normal-button-style
  ;;  (release-attribute fi::normal-button-style))
  ;;(setq fi::normal-button-style (reserve-attribute))
  (when fi::highlighted-button-style
    (release-attribute fi::highlighted-button-style))
  (setq fi::highlighted-button-style (reserve-attribute))
  ;;(setq fi::normal-style      (epoch::make-style))
  (setq fi::highlighted-style (epoch::make-style))
  ;;(epoch::set-style-foreground fi::normal-style      (epoch::foreground))
  ;;(epoch::set-style-background fi::normal-style      (epoch::background))
  (epoch::set-style-foreground fi::highlighted-style (epoch::foreground))
  (epoch::set-style-background fi::highlighted-style (epoch::background))
  (epoch::set-style-underline  fi::highlighted-style (epoch::foreground))
  ;;(epoch::set-attribute-style fi::normal-button-style      fi::normal-style)
  (epoch::set-attribute-style fi::highlighted-button-style fi::highlighted-style)
  )

(when (boundp 'epoch::version)
  (fi::initialize-for-presenting-listeners))

(defun composer::setup-buffer-for-presentations (buffer)
  (make-local-variable 'highlighted-presentation)
  (setq highlighted-presentation nil)
  (make-local-variable 'window-stream-presentation)
  (setq window-stream-presentation (make-presentation :start 0 :end 8388607))
  (set-buffer buffer)
  (make-local-variable 'presentation-stack)
  (setq presentation-stack (list window-stream-presentation))
  (make-local-variable 'incomplete-input)
  (setq incomplete-input nil)
  (make-local-variable 'highlight-button)
  (setq highlight-button (epoch::make-button))
  (make-local-variable 'read-only-button)
  (setq read-only-button (epoch::make-button))
  (epoch::move-button read-only-button 1 (point-max))
  (epoch::set-button-read-only read-only-button t)
  (set-button-attribute highlight-button fi::highlighted-button-style)
  (fi:setup-epoch-gesture-bindings))

(defvar fi:default-epoch-gesture-binding-list
    (and (boundp 'epoch::version)
	 (list (list 'fi:epoch-gesture-describe   mouse-left   (+ mouse-shift))
	       (list 'fi:epoch-gesture-inspect    mouse-left   (+ mouse-control))
	       (list 'fi:epoch-gesture-edit       mouse-middle 0)
	       (list 'fi:epoch-gesture-select     mouse-middle (+ mouse-shift))
	       (list 'fi:epoch-gesture-menu       mouse-right  0)))
  "*The mapping of mouse clicks onto logical gestures.
Each entry is a list of length 3: The command to send the gesture,
the numeric epoch mouse code, and the epoch numeric shifts.
The function should be defined in this way:
  (defun fi:epoch-gesture-select (x)
    (fi::interrupt-process-for-gesture ':select))")

(defun fi:setup-epoch-gesture-bindings ()
  (dolist (e fi:default-epoch-gesture-binding-list)
    (local-set-mouse (second e) (third e) (first e))))

(defun fi:epoch-gesture-select (x)
  (fi::interrupt-process-for-gesture ':select))

(defun fi:epoch-gesture-inspect (x)
  (fi::interrupt-process-for-gesture ':inspect))

(defun fi:epoch-gesture-edit (x)
  (fi::interrupt-process-for-gesture ':edit))

(defun fi:epoch-gesture-menu (x)
  (fi::interrupt-process-for-gesture ':menu))

(defun fi:epoch-gesture-describe (x)
  (fi::interrupt-process-for-gesture ':describe))

(defvar composer::init-presentations
    "(progn
      (princ \";; Converting *terminal-io* for presentations...\n\")
      (force-output)
      (setq excl::*command-table*
	    (excl::find-command-table 'lep::listener-command-table))
      (lep::mogrify-terminal-io)
      (force-output)
      (values))\n")

(defun composer::make-presenting-listener (new-screen-p)
  (when (and new-screen-p (fboundp 'create-screen))
    (let ((screen (create-screen "*listener*" epoch::screen-properties)))
      (epoch::map-screen screen)
      (epoch::select-screen screen)
      screen))
  (let* ((proc (fi:open-lisp-listener -1 nil
				      (function
				       (lambda (proc)
					 ;; proc is ignored
					 composer::init-presentations))))
	 (buffer (process-buffer proc)))
    (composer::setup-buffer-for-presentations buffer)
    (set-process-filter proc 'fi::leep-subprocess-filter)
    proc))

;; The presentation-stack local variable is a stack of presentations opened
;; but not yet closed.

;; This defstruct is moved to file leep0.el because the cruftly compiler
;; doesn't understand a defstruct in the same file.

;(defstruct presentation
;  start
;  end
;  data
;  subpresentation-vector)

(defun fi::insert-string (string start end)
  (do ((p start (1+ p)))
      ((eq p end))
    (insert-char (aref string p) 1)))

(defun fi::leep-subprocess-filter (process output &optional stay cruft)
  "Filter output to buffer including presentations."
  (let ((inhibit-quit t))
    (if cruft
	(setq output (fi::substitute-chars-in-string '((?\r)) output)))
    (when incomplete-input
      (setq output (concat incomplete-input output))
      (setq incomplete-input nil))
    (let* ((old-buffer (current-buffer))
	   (buffer (process-buffer process))
	   (in-buffer (eq buffer old-buffer))
	   (window-of-buffer (get-buffer-window buffer))
	   (no-window (or (null window-of-buffer)
			  (not (windowp window-of-buffer))))
	   (xmarker (process-mark process))
	   (marker (if (marker-position xmarker)
		       xmarker
		     (set-marker (make-marker) 0 buffer)))
	   (marker-point (marker-position marker))
	   (output-length (length output))
	   old-point
	   point-not-before-marker
	   new-point)
      ;; The three symbols below are not bound above because `(window-point)'
      ;;   for the selected window does not always return the same thing as the
      ;;   function `(point)' in that window!  [Version 18 is supposed to fix
      ;;   this bug.]
      ;; Note that there is no function that returns all of the windows that
      ;;   are currently displaying a buffer.  Because of this, not all windows
      ;;   will be updated properly by this filter function.  What should be
      ;;   done is to loop through all windows displaying the buffer and do
      ;;   `(set-window-point)' in each.
      (if (not in-buffer)
	  (progn
	    (set-buffer buffer)
	    (setq old-point
	      (if no-window
		  (point)
		(window-point window-of-buffer))))
	(setq old-point (point)))
      (setq point-not-before-marker (>= old-point marker-point))
      (setq new-point (if point-not-before-marker
			  (+ old-point output-length)
			old-point))
      (save-excursion
	;; Go to point of last output by fi::make-process and insert new
	;;   output there, preserving position of the marker.
	(goto-char marker-point)
	;; The code below works around what appears to be a display bug
	;;   in GNU Emacs 17.  If `(insert-before-markers)' is used when
	;;   the process marker (process-mark), window-start point
	;;   (window-start), and window point (point) are all coincident,
	;;   the window display `sticks' on the topmost line.  We use
	;;   `(insert-string)' followed by `(set-marker)' to avoid this
	;;   problem.  This also happens to be the way
	;;   `handle_process_output()' deals with this in `process.c'.

	;; Presentation escape sequences are:
	;;  &&		- escape a single &
	;;  &<		- start a presentation
	;;  &ddd>	- end presentation number ddd (arbitrary decimal int)

	(do ((pnt 0)
	     (len (length output))
	     index)
	    ((or (eq pnt len)
		 (null (setq index (string-match "&" output pnt))))
	     (when (< pnt len)
	       (fi::insert-string output pnt len)
	       (set-marker marker (point))))
	  (unless (eq pnt index)
	    (fi::insert-string output pnt index)
	    (set-marker marker (point)))
	  (setq index (+ index 1))
	  (cond ((eq index len)
		 (setq incomplete-input "&"
			 pnt len))
		((eq (aref output index) ?&)
		 (insert-char ?& 1)
		 (set-marker marker (point))
		 (setq pnt (+ index 1)))
		((eq (aref output index) ?<)
		 (let* ((pres (make-presentation :start (point) :end 0) :data 0)
			(parent (car presentation-stack))
			(subs (presentation-subpresentation-vector parent))
			(window-stream-presentation nil)) ;flag stream busy
		   (if subs
		       (let ((len (length subs))
			     (next (aref subs 0)))
			 (when (= next len)
			   (let ((new (make-vector (+ len len) nil)))
			     (setf (presentation-subpresentation-vector parent) new)
			     (dotimes (i next)
			       (aset new i (aref subs i)))
			     (setq subs new)))
			 (aset subs next pres)
			 (aset subs 0 (+ next 1)))
		     (setf (presentation-subpresentation-vector parent)
		       (vector 2 pres nil nil)))
		   (push pres presentation-stack))
		 (setq pnt (+ index 1)))
		((eq index (string-match "\\([0-9]\\)+>" output index))
		 (setq pnt (match-end 0))
		 (let ((pres (pop presentation-stack))
		       (window-stream-presentation nil)) ;flag stream busy
		   (setf (presentation-end pres) (point))
		   (setf (presentation-data pres)
		     (car (read-from-string output (match-beginning 1) (match-end 1))))
		   (let ((p (point)))
		     (set-marker marker p)
		     (epoch::move-button read-only-button 1 p))))
		((> (- len pnt) 10)	;broken protocol!!!
		 (fi::insert-string output pnt len)
		 (set-marker marker (point))
		 (setq pnt len))
		(t (setq incomplete-input (substring output (- index 1) len)
			 pnt len)))))

      (if (not in-buffer)
	  (if (and fi:subprocess-continuously-show-output-in-visible-buffer
		   point-not-before-marker)
	      ;; Keep window's notion of `point' in a constant relationship to
	      ;;   the process output marker.
	      (if no-window
		  (goto-char new-point)
		(set-window-point window-of-buffer new-point))
	    (if no-window
		t ;; Still there.
	      (set-window-point window-of-buffer old-point)))
	(goto-char new-point))
      (cond
       (in-buffer nil)
       (stay old-buffer)
       (t (set-buffer old-buffer))))))

;; I shouldn't redefine this -- rather I should push my own handler.

(defun motion::handler (type value scr)
  (if (null mouse-down-marker) (set-mouse-marker))
  (if (and (boundp 'mouse::downp) mouse::downp)
      (progn (when (and (boundp 'highlighted-presentation)
			highlighted-presentation)
	       (set-highlight-button-to-presentation nil)
	       (setq highlighted-button nil))
	     (mouse-sweep-update))
    ;; The existence of the buffer variable serves as a flag that
    ;; mouse events are interesting.
    (when (boundp 'highlighted-presentation)
      (let ((epoch::event-handler-abort nil)
	    (coords (coords-at-mouse))
	    presentation)
	(when coords
	  (save-excursion
	    (set-buffer (cadr coords))
	    (setq presentation
	      (presentation-at-point (car coords) window-stream-presentation))
	    (unless (eq presentation highlighted-presentation)
	      (set-highlight-button-to-presentation
	       (setq highlighted-presentation presentation))
	      (epoch::redisplay-screen))))))))

(defun coords-at-mouse ()
  (let* (x y pos
	 (w (selected-window))
	 (w-edges (window-edges w))
	 (left (car w-edges))
	 (top (elt w-edges 1))
	 ;;(right (- (elt w-edges 2) (+ 2 left)))
	 ;;(bottom (- (elt w-edges 3) (+ 2 top)))
	 )
    (setq pos (query-mouse))
    ;;convert to window relative co-ordinates
    (setq x (- (car pos) left))
    (setq y (- (elt pos 1) top))
    ;;(setq x (max 0 (min right x)))
    ;;(setq y (max 0 (min bottom y)))
    (epoch::coords-to-point (+ x left) (+ y top))))

;(defun presentation-at-point (point window-stream-presentation)
;  (when point				;sometimes is nil
;    (do ((p window-stream-presentation)
;	 (winner nil))
;	((null p) (unless (eq winner window-stream-presentation) winner))
;      (let ((start (presentation-start p)))
;	(if (<= start point)
;	    (if (<= (presentation-end p) point)
;		(setq p (presentation-next p))
;	      (setq winner p)
;	      (setq p (presentation-first p)))
;	  (setq p nil))))))

;; This assumes that the presentations in the subpresentation-vector
;; do not have overlapping extents.

(defun presentation-at-point (point p)
  (when (and point			;sometimes is nil
	     p)				;nil flags that window is being written
    (do ((winner nil)
	 subs)
	((or (null p)
	     (null (setq subs (presentation-subpresentation-vector p))))
	 winner)
      (setq p nil)
      (let* ((low 1)
	     (hih (aref subs 0)))
	(do (pres n nn)
	    ((or p
		 (eq n (setq nn (/ (+ low hih) 2)))))
	  (setq n nn)
	  (setq pres (aref subs n))
	  (cond ((<  point (presentation-start pres)) (setq hih n))
		((>= point (presentation-end   pres)) (setq low n))
		(t (setq p pres)
		   (setq winner pres))))))))

(defun set-highlight-button-to-presentation (presentation)
  (if presentation
      (epoch::move-button highlight-button
			  (presentation-start presentation)
			  (presentation-end   presentation))
    (epoch::move-button highlight-button 1 1)))

;(defun fi::interrupt-process-for-click (gesture shifts)
;  (let ((coords (coords-at-mouse)))
;    (when coords
;      (save-excursion
;	(set-buffer (cadr coords))
;	(fi:eval-in-lisp
;	 (format
;	  "(mp:process-interrupt
;		(mp::process-name-to-process \"%s\")
;		#'composer::epoch-click %s %s %s)\n"
;	  (buffer-name (current-buffer))
;	  (let ((pres (presentation-at-point (car coords) window-stream-presentation)))
;	    (when pres (presentation-data pres)))
;	  gesture shifts))))))
;
;(defun fi:epoch-click-left (x)
;  (fi::interrupt-process-for-click 0 0))
;
;(defun fi:epoch-click-middle (x)
;  (fi::interrupt-process-for-click 1 0))
;
;(defun fi:epoch-click-right (x)
;  (fi::interrupt-process-for-click 2 0))

;;;;;;;;;;  The new mechanism.

(defun fi::interrupt-process-for-gesture (gesture)
  (let ((coords (coords-at-mouse)))
    (when coords
      (save-excursion
	(set-buffer (cadr coords))
	(fi:eval-in-lisp
	 (format
	  "(mp:process-interrupt
		(mp::process-name-to-process \"%s\")
		#'composer::epoch-gesture %s %s)\n"
	  (buffer-name (current-buffer))
	  (let ((pres (presentation-at-point (car coords) window-stream-presentation)))
	    (when pres (presentation-data pres)))
	  gesture))))))
