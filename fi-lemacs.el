;; lemacs specific hacks for the Franz Inc. emacs-lisp interface
;;
;; Copyright (c) 1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; $Header: /repo/cvs.copy/eli/Attic/fi-lemacs.el,v 2.14 1993/09/23 18:03:46 layer Exp $

(defconst fi:allegro-file-menu
    '("ACLFile"
      ["Run/Restart Common Lisp, new window" fi:menu-common-lisp-new-screen
       (fi::connection-not-open)]
      ["Run/Restart Common Lisp" fi:menu-common-lisp (fi::connection-not-open)]
      ["Create Listener, new window" fi:menu-open-lisp-listener-new-screen
       (fi::connection-open)]
      ["Create Listener" fi:open-lisp-listener (fi::connection-open)]
      "----"
      ["Compile region or form"
       fi:lisp-compile-active-region-or-defun (fi::connection-open)]
      ("Compile other"
       ["region" fi:lisp-compile-region (fi::connection-open)]
       ["last s-exp" fi:lisp-compile-last-sexp (fi::connection-open)]
       ["buffer" fi:lisp-compile-current-buffer (fi::connection-open)])
      "----"
      ("Changed definitions"
       ["List all changed definitions" fi:list-changed-definitions
	(fi::connection-open)]
       ["List buffer changed definitions"
	fi:list-buffer-changed-definitions
	(fi::connection-open)]
       ["Compile all changed definitions" fi:compile-changed-definitions
	(fi::connection-open)]
       ["Compile buffer changed definitions"
	fi:compile-buffer-changed-definitions
	(fi::connection-open)]
       ["Eval all changed definitions" fi:eval-changed-definitions
	(fi::connection-open)]
       ["Eval buffer changed definitions"
	fi:eval-buffer-changed-definitions
	(fi::connection-open)]
       ["Copy all changed definitions" fi:copy-changed-definitions
	(fi::connection-open)]
       ["Copy buffer changed definitions" fi:copy-buffer-changed-definitions
	(fi::connection-open)]
       ["Compare source files" fi:compare-source-files (fi::connection-open)]
       )
      ["Compile file" fi:compile-file (fi::connection-open)]
      ["Load file" fi:load-file (fi::connection-open)]
      "----"
      ["List buffer definitions" fi:list-buffer-definitions
       (fi::connection-open)]
      "----"
      ["Exit Allegro CL" fi:exit-lisp (fi::connection-open)]
      ))

(defconst fi:allegro-edit-menu
    '("ACLEdit"
      ["Find definition" fi:lisp-find-definition (fi::connection-open)]
      ["Find definition other window" fi:lisp-find-definition-other-window
       (fi::connection-open)]
      ["Find next definition" fi:lisp-find-next-definition
       (fi::connection-open)]
      "----"
      ["Center defun" fi:center-defun t]
      ["Extract list" fi:extract-list t]
      ["Close all parens" fi:super-paren t]
      ["Comment region" fi:comment-region t]
      ["Uncomment region" fi:uncomment-region t]
      ))

(defconst fi:allegro-help-menu
    '("ACLHelp"
      ["Arglist" fi:lisp-arglist (fi::connection-open)]
      ["Describe" fi:describe-symbol (fi::connection-open)]
      ["Apropos" fi:lisp-apropos (fi::connection-open)]
      ["Function Documentation" fi:lisp-function-documentation
       (fi::connection-open)]
      "----"
      ["CL reference manual" fi:clman t]
      ))

(defconst fi:allegro-debug-menu
    '("ACLDebug"
      ["Toggle trace" fi:toggle-trace-definition (fi::connection-open)]
      ["Debug process" fi:scan-stack (fi::connection-open)]
      ["Macroexpand" fi:lisp-macroexpand (fi::connection-open)]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively
       (fi::connection-open)]
      ["List undefined functions" fi:list-undefined-functions
       (fi::connection-open)]
      ["List unused functions" fi:list-unused-functions (fi::connection-open)]
      ["Kill definition" fi:kill-definition (fi::connection-open)]
      "----"
      ["List generic function methods" fi:list-generic-function-methods
       (fi::connection-open)]
      ["Edit generic function methods" fi:edit-generic-function-methods
       (fi::connection-open)]
      ("Cross reference"
       ["List calls to" fi:list-who-calls (fi::connection-open)]
       ["List callers of" fi:list-who-is-called-by (fi::connection-open)]
       ["Edit calls to" fi:edit-who-calls (fi::connection-open)]
       ["Edit callers of" fi:edit-who-is-called-by (fi::connection-open)])
      ))

(defconst fi:composer-menu
    '("Composer"
      ["Start Composer" fi:start-composer
       (fi::connection-open-composer-loaded-and-stopped)]
      ["Start Composer w/mouse line" fi:start-composer-mouse-line
       (fi::connection-open-composer-loaded-and-stopped)]
      "----"
      ("CLOS"
       ["Inspect class" fi:inspect-class (fi::composer-connection-open)]
       ["Inspect generic function" fi:inspect-function
	(fi::composer-connection-open)]
       ["Show class subclasses" fi:show-subclasses
	(fi::composer-connection-open)]
       ["Show class superclasses" fi:show-superclasses
	(fi::composer-connection-open)]
       )
      ("Xref"
       ["Show calls to" fi:show-calls-to (fi::composer-connection-open)]
       ["Show calls from" fi:show-calls-from (fi::composer-connection-open)]
       ["Show calls to and from" fi:show-calls-to-and-from
	(fi::composer-connection-open)]
       ["Discard info" fi:discard-xref-info (fi::connection-open)]
       )
      ("Profiler"
       ["Start time profiler" fi:composer-start-time-profiler
	(fi::composer-connection-open)]
       ["Start space profiler" fi:composer-start-space-profiler
	(fi::composer-connection-open)]
       ["Stop profiler" fi:composer-stop-profiler
	(fi::composer-connection-open)]
       ["Display time" fi:composer-display-time-profiler
	(fi::composer-connection-open)]
       ["Display space" fi:composer-display-space-profiler
	(fi::composer-connection-open)]
       ["Options" fi:composer-profiler-options (fi::composer-connection-open)]
       )
      ("Other"
       ["Inspect" fi:inspect-value (fi::composer-connection-open)]
       ["Presenting Listener" composer::make-presenting-listener
	(fi::composer-connection-open)]
       ["Processes" fi:composer-process-browser (fi::composer-connection-open)]
       ["Systems" fi:composer-defsys-browser (fi::composer-connection-open)]
       ["Reinitialize resources" fi:composer-reinitialize-resources
	(fi::composer-connection-open)]
       ["Options" fi:composer-other-options (fi::composer-connection-open)]
       )
      ("Help"
       ["Help" fi:composer-help (fi::composer-connection-open)]
       ["Current pointer gesture bindings" fi:composer-help-gesture-bindings
	(fi::composer-connection-open)])
      "----"
      ["Exit Composer/Common Windows" fi:composer-exit
       (fi::composer-connection-open)]
      ))

(defun fi::connection-open ()
  (fi::lep-open-connection-p))

(defun fi::connection-not-open ()
  (not (fi::lep-open-connection-p)))

(defun fi::connection-once-open ()
  (and (not (fi::lep-open-connection-p))
       (not fi::common-lisp-first-time)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar fi::connection-open-composer-loaded nil)
(defvar fi::composer-connection-open nil)
(defvar fi::composer-running nil)

(defvar fi::composer-cached-connection nil)

(defun fi::connection-open-composer-loaded ()
  (when (not (eq fi::*connection* fi::composer-cached-connection))
    ;; the lisp was (possibly) restarted
    (setq fi::connection-open-composer-loaded nil)
    (setq fi::composer-running nil))
  (and (fi::lep-open-connection-p)
       (or (when (null fi::connection-open-composer-loaded)
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp "(when (find-package :wt) t)"))
		 (setq fi::connection-open-composer-loaded 'yes)
	       (setq fi::connection-open-composer-loaded 'no))
	     (setq fi::composer-cached-connection fi::*connection*)
	     nil)
	   (eq fi::connection-open-composer-loaded 'yes))))

(defun fi::connection-open-composer-loaded-and-stopped ()
  (and (fi::connection-open-composer-loaded)
       (or (unless fi::composer-running
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp
		    "(when (and (find-package :wt)
                                (wt::common-windows-initialized-p)
				(wt::connected-to-server-p))
                       t)"))
		 (setq fi::composer-running 'yes)
	       (setq fi::composer-running 'no))
	     nil)
	   (eq fi::composer-running 'no))))

(defun fi::composer-connection-open ()
  (and (fi::connection-open-composer-loaded)
       (or (unless fi::composer-connection-open
	     (if (let ((fi:package nil))
		   (fi:eval-in-lisp
		    "wt::(and (connected-to-epoch-p)
 			      (common-windows-initialized-p)
			      (connected-to-server-p))"))
		 (setq fi::composer-connection-open 'yes)
	       (setq fi::composer-connection-open 'no))
	     nil)
	   (eq fi::composer-connection-open 'yes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::install-menubar (menu-bar)
  (set-menubar (delete (assoc (car menu-bar) current-menubar)
		       (copy-sequence current-menubar)))
  (add-menu nil (car menu-bar) (cdr menu-bar)))

(push '(progn
	(fi::install-menubar fi:allegro-file-menu)
	(fi::install-menubar fi:allegro-edit-menu)
	(fi::install-menubar fi:allegro-debug-menu)
	(fi::install-menubar fi:allegro-help-menu)
	(when fi:composer-menu
	  (fi::install-menubar fi:composer-menu)))
      fi::initialization-forms)

(defun fi:menu-common-lisp ()
  (interactive)
  (let ((fi:new-screen-for-common-lisp-buffer nil))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-common-lisp-new-screen ()
  (interactive)
  (let ((get-screen-for-buffer-default-screen-name 'common-lisp)
	(fi:new-screen-for-common-lisp-buffer t))
    (call-interactively 'fi:common-lisp)))

(defun fi:menu-open-lisp-listener-new-screen ()
  (interactive)
  (let ((get-screen-for-buffer-default-screen-name 'lisp-listener)
	(fi:new-screen-for-common-lisp-buffer t))
    (call-interactively 'fi:open-lisp-listener)))

(defun fi:lisp-compile-active-region-or-defun ()
  (interactive)
  (if (and zmacs-regions (mark-marker))
      ;; region is active
      (fi:lisp-compile-region)
    (fi:lisp-compile-defun)))

(defun fi:exit-lisp ()
  (interactive)
  (message "Exiting Allegro CL...")
  (fi:eval-in-lisp
   "(mp:process-interrupt
      (mp:process-name-to-process \"Initial Lisp Listener\")
      #'excl:exit 0)")
  (let ((cl-buffer (get-buffer fi:common-lisp-buffer-name)))
    (when (and cl-buffer (get-buffer-window cl-buffer))
      (set-buffer cl-buffer)
      (if (eq fi::emacs-type 'lemacs19)
	  (let* ((screen (selected-screen)))
	    (if (and (string= (symbol-name (get major-mode 'screen-name))
			      (screen-name screen))
		     (one-window-p)
;;;; delete-screen causes lemacs to die with a segv:
		     nil
		     )
		(delete-screen screen)
	      (bury-buffer)))
	(bury-buffer))))
  (message "Exiting Allegro CL...done."))

(defun fi:start-composer ()
  (interactive)
  (message "Starting Allegro Composer...")
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line nil)nil)")
  (message "Starting Allegro Composer...done.")
  (setq fi::composer-running 'yes)
  (setq fi::composer-connection-open 'yes))

(defun fi:start-composer-mouse-line ()
  (interactive)
  (message "Starting Allegro Composer...")
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line t)nil)")
  (message "Starting Allegro Composer...done.")
  (setq fi::composer-running 'yes)
  (setq fi::composer-connection-open 'yes))

(defun fi:discard-xref-info ()
  (interactive)
  (message "Discarding cross reference info...")
  (fi:eval-in-lisp "(progn(xref:discard-all-xref-info)nil)")
  (message "Discarding cross reference info...done."))
  
(defun fi:composer-other-options ()
  (interactive)
  (message "Creating Composer options dialog...")
  (fi:eval-in-lisp "(progn(wt::set-options-command t)nil)")
  (message "Creating Composer options dialog...done."))

(defun fi:composer-help ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::print-startup-help)nil)"))

(defun fi:composer-help-gesture-bindings ()
  (interactive)
  (fi:eval-in-lisp "(progn(wt::composer-report-gestures-command t)nil)"))

(defun fi:composer-exit ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer:stop-composer :kill-cw t)nil)")
  (setq fi::composer-running 'no)
  (setq fi::composer-connection-open 'no))

(defun fi:composer-process-browser ()
  (interactive)
  (message "Starting process browser...")
  (fi:eval-in-lisp "(progn(composer::process-browser)nil)")
  (message "Starting process browser...done."))

(defun fi:composer-reinitialize-resources ()
  (interactive)
  (message "Reinitializing resources...")
  (fi:eval-in-lisp "(progn(composer::init-resource-database)nil)")
  (message "Reinitializing resources...done."))

(defun fi:composer-defsys-browser ()
  (interactive)
  (message "Starting defsystem browser...")
  (fi:eval-in-lisp "(progn(composer::defsys-browser)nil)")
  (message "Starting defsystem browser...done."))

(defun fi:composer-start-time-profiler ()
  (interactive)
  (message "Starting time profiler...")
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :time)nil)")
  (message "Starting time profiler...done."))

(defun fi:composer-start-space-profiler ()
  (interactive)
  (message "Starting space profiler...")
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :space)nil)")
  (message "Starting space profiler...done."))

(defun fi:composer-display-time-profiler ()
  (interactive)
  (message "Displaying time profile graph...")
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :time)nil)")
  (message "Displaying time profile graph...done."))

(defun fi:composer-display-space-profiler ()
  (interactive)
  (message "Displaying space profile graph...")
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :space)nil)")
  (message "Displaying space profile graph...done."))

(defun fi:composer-stop-profiler ()
  (interactive)
  (message "Stopping the profiler...")
  (fi:eval-in-lisp "(progn(composer::stop-profiler-command-1)nil)")
  (message "Stopping the profiler...done."))

(defun fi:composer-profiler-options ()
  (interactive)
  (message "Creating profiler options dialog...")
  (fi:eval-in-lisp
   "(progn(wt::run-motif-application 'wt::make-profiler-options)nil)")
  (message "Creating profiler options dialog...done."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fi:common-lisp-mode-popup-menu
    '("common lisp mode popup menu"
      ["Compile region or form"
       fi:lisp-compile-active-region-or-defun
       (fi::connection-open)]
      ["Compile and load file" fi:menu-compile-and-load-file
       (fi::connection-open)]
      "----"
      ["Find definition" fi:menu-lisp-find-definition (fi::connection-open)]
      ["Find next definition" fi:lisp-find-next-definition
       (fi::connection-open)]
      ["Arglist" fi:menu-lisp-arglist (fi::connection-open)]
      ["Toggle trace" fi:menu-toggle-trace-definition (fi::connection-open)]
      ["Macroexpand" fi:lisp-macroexpand (fi::connection-open)]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively
       (fi::connection-open)]
      ))

(defun fi:menu-lisp-find-definition ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:lisp-find-definition)))

(defun fi:menu-lisp-arglist ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:lisp-arglist)))

(defun fi:menu-toggle-trace-definition ()
  (interactive)
  (let ((fi::use-symbol-at-point t))
    (call-interactively 'fi:toggle-trace-definition)))

(defun fi:menu-compile-and-load-file ()
  (interactive)
  (when (buffer-file-name)
    (fi:compile-file (buffer-file-name))))

(defun fi:menu-load-file ()
  (interactive)
  (when (buffer-file-name)
    (fi:load-file (buffer-file-name))))

(defun fi:common-lisp-mode-popup-menu (e)
  (interactive "@e")
  (mouse-set-point e)
  (popup-menu fi:common-lisp-mode-popup-menu))

(defconst fi:inferior-common-lisp-mode-popup-menu
    '("inferior common lisp mode popup menu"
      ["Zoom" fi:debug-menu-zoom (fi::connection-open)]
      ["Down frame" fi:debug-menu-down-frame (fi::connection-open)]
      ["Up frame" fi:debug-menu-up-frame (fi::connection-open)]
      ["Edit frame" fi:debug-menu-edit-frame (fi::connection-open)]
      ["Locals for frame" fi:debug-menu-locals (fi::connection-open)]
      "----"
      ["Continue" fi:debug-menu-continue (fi::connection-open)]
      ["Restart" fi:debug-menu-restart (fi::connection-open)]
      ["Pop" fi:debug-menu-pop (fi::connection-open)]
      ["Reset" fi:debug-menu-reset (fi::connection-open)]
      "----"
      ["List processes" fi:debug-menu-processes (fi::connection-open)]
      ))

(defun fi:debug-menu-reset ()
  (interactive)
  (insert ":reset")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-pop ()
  (interactive)
  (insert ":pop")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-restart ()
  (interactive)
  (insert ":restart")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-continue ()
  (interactive)
  (insert ":continue")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-zoom ()
  (interactive)
  (insert ":zoom")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-down-frame ()
  (interactive)
  (insert ":dn")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-up-frame ()
  (interactive)
  (insert ":up")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-edit-frame ()
  (interactive)
  (insert ":edit")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-locals ()
  (interactive)
  (insert ":local")
  (fi:inferior-lisp-newline))

(defun fi:debug-menu-processes ()
  (interactive)
  (insert ":processes")
  (fi:inferior-lisp-newline))

(defun fi:inferior-common-lisp-mode-popup-menu ()
  (interactive "@")
  (goto-char (point-max))
  (popup-menu fi:inferior-common-lisp-mode-popup-menu))

(defun fi::install-mode-menus ()
  (let ((menu-bar
	 (cond
	  ((eq major-mode 'fi:common-lisp-mode)
	   (define-key fi:common-lisp-mode-map 'button3
	     'fi:common-lisp-mode-popup-menu))
	  ((eq major-mode 'fi:inferior-common-lisp-mode)
	   (define-key fi:inferior-common-lisp-mode-map 'button3
	     'fi:inferior-common-lisp-mode-popup-menu)))))))

(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::install-mode-menus)
(add-hook 'fi:common-lisp-mode-hook 'fi::install-mode-menus)
