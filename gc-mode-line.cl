;;; gc-mode-line.cl	-[Fri Mar 17 20:59:47 1995 by smh]-

;;; This and the accompanying gc-mode-line.c file implement `Run Bars' in the
;;; Emacs mode line for Common Lisp buffers, both interaction and source buffers.

;;; This file may be used with Allegro CL version 4.2 and the Franz Emacs-Lisp
;;; interface version 2.0.12 or later.  In versions after 4.2 this code will be
;;; built into the lisp image.

;;; To incorporate this code, it is best to rebuild your lisp image:
;;;   (1) Place the two files in the .../build subdirectory of the lisp
;;;       distribution.
;;;   (2) Compile the C source with this Unix command:
;;;          cc -c gc-mode-line.c
;;;       This creates the file .../build/gc-mode-line.o .
;;;   (3) Compile the Lisp source by starting your existing lisp and issuing
;;;       the lisp command:
;;;          :cf gc-mode-line.cl
;;;       This creates the file .../build/gc-mode-line.fasl .
;;;   (4) In the top-level lisp distribution directory, rebuild lisp by issuing
;;;       the install_lisp shell command(s) with which you originally built
;;;       your lisp image(s), suffixing the two files to the command, e.g.:
;;;         build/install_lisp gc-mode-line.o gc-mode-line.fasl

;;; If you want to test the code without rebuilding, you can manually load
;;; gc-mode-line.o and gc-mode-line.fasl into an existing lisp image.
;;; The mode line code is invoked automatically, if present, by the emacs-lisp
;;; interface at lisp startup time, but if you load the code into an image
;;; after startup by evaluating this Emacs command, followed by a RETURN:
;;;    M-x fi:show-run-status

(in-package :excl)

#+(version>= 4 3)
(eval-when (compile load eval)
  (error "This file should not be used with versions after ACL 4.2."))

(unless (fboundp 'run_bar_hook)
  ;;(load (merge-pathnames "gc-mode-line.o" *load-pathname*))
  (ff:defforeign-list '((run_bar_hook)
			#+never (mode_line_run)
			#+never (mode_line_wait))))

(defun excl::run-status-process (&optional (stream *terminal-io*))
  (flet ((mode-line-caller (arg1 arg2)
	   #'(lambda ()
	       (run_bar_hook arg1 arg2))))
    (setf (mp::process-interruptable-p mp:*current-process*) nil)
    (let ((fd (stream-output-fn stream))
	  (run  (mode-line-caller 2 0))
	  (wait (mode-line-caller 3 0)))
      (unwind-protect
	  (progn (run_bar_hook 1 fd)
		 (pushnew run  mp::*scheduler-wakeup-hooks*)
		 (pushnew wait mp::*scheduler-sleep-hooks*)
		 (mp:process-disable mp:*current-process*))
	;; Use remove to avoid interrupt hazard.
	(setf mp::*scheduler-sleep-hooks*  (remove wait mp::*scheduler-sleep-hooks*))
	(setf mp::*scheduler-wakeup-hooks* (remove run  mp::*scheduler-wakeup-hooks*))
	(run_bar_hook 1 0)))))
