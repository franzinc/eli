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
;; $Header: /repo/cvs.copy/eli/Attic/fi-lemacs.el,v 2.6 1993/08/12 23:45:06 layer Exp $

(defconst fi:allegro-file-menu
    '("ACLFile"
      ["Run/Restart Common Lisp" fi:common-lisp fi::connection-not-open]
      ["New Listener" fi:open-lisp-listener fi::connection-open]
      "----"
      ("Compile"
       ["top-level form" fi:lisp-compile-defun fi::connection-open]
       ["region" fi:lisp-compile-region fi::connection-open]
       ["last s-exp" fi:lisp-compile-last-sexp fi::connection-open]
       ["buffer" fi:lisp-compile-current-buffer fi::connection-open])
      ("Evaluate"
       ["top-level form" fi:lisp-eval-defun fi::connection-open]
       ["region" fi:lisp-eval-region fi::connection-open]
       ["last s-exp" fi:lisp-eval-last-sexp fi::connection-open]
       ["buffer" fi:lisp-eval-current-buffer fi::connection-open])      
      ["Compile file" fi:compile-file fi::connection-open]
      ["Load file" fi:compile-file fi::connection-open]
      "----"
      ["List buffer definitions" fi:list-buffer-definitions
       fi::connection-open]
      "----"
      ["Exit Allegro CL" fi:exit-lisp fi::connection-open]
      ))

(defconst fi:allegro-edit-menu
    '("ACLEdit"
      ["Find definition" fi:lisp-find-definition fi::connection-open]
      ["Find definition other window" fi:lisp-find-definition-other-window
       fi::connection-open]
      ["Find next definition" fi:lisp-find-next-definition fi::connection-open]
      "----"
      ["Center defun" fi:center-defun t]
      ["Extract list" fi:extract-list t]
      ["Close all parens" fi:super-paren t]
      ["Comment region" fi:comment-region t]
      ["Uncomment region" fi:uncomment-region t]
      ))

(defconst fi:allegro-help-menu
    '("ACLHelp"
      ["Arglist" fi:lisp-arglist fi::connection-open]
      ["Describe" fi:describe-symbol fi::connection-open]
      ["Apropos" fi:lisp-apropos fi::connection-open]
      ["Function Documentation" fi:lisp-function-documentation
       fi::connection-open]
      "----"
      ["CL reference manual" fi:clman t]
      ))

(defconst fi:allegro-debug-menu
    '("ACLDebug"
      ["Toggle trace" fi:toggle-trace-definition fi::connection-open]
      ["Debug process" fi:scan-stack fi::connection-open]
      ["Macroexpand" fi:lisp-macroexpand fi::connection-open]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively
       fi::connection-open]
      ["List undefined functions" fi:list-undefined-functions
       fi::connection-open]
      ["List unused functions" fi:list-unused-functions fi::connection-open]
      ["Kill definition" fi:kill-definition fi::connection-open]
      "----"
      ["List generic function methods" fi:list-generic-function-methods
       fi::connection-open]
      ["Edit generic function methods" fi:edit-generic-function-methods
       fi::connection-open]
      "----"
      ("Changed definitions"
       ["List all changed definitions" fi:list-changed-definitions
	fi::connection-open]
       ["List buffer changed definitions"
	fi:list-buffer-changed-definitions
	fi::connection-open]
       ["Compile all changed definitions" fi:compile-changed-definitions
	fi::connection-open]
       ["Compile buffer changed definitions"
	fi:compile-buffer-changed-definitions
	fi::connection-open]
       ["Eval all changed definitions" fi:eval-changed-definitions
	fi::connection-open]
       ["Eval buffer changed definitions"
	fi:eval-buffer-changed-definitions
	fi::connection-open]
       ["Copy all changed definitions" fi:copy-changed-definitions
	fi::connection-open]
       ["Copy buffer changed definitions" fi:copy-buffer-changed-definitions
	fi::connection-open]
       ["Compare source files" fi:compare-source-files fi::connection-open]
       )
      ("Cross reference"
       ["List calls to" fi:list-who-calls fi::connection-open]
       ["List callers of" fi:list-who-is-called-by fi::connection-open]
       ["Edit calls to" fi:edit-who-calls fi::connection-open]
       ["Edit callers of" fi:edit-who-is-called-by fi::connection-open])
      ))

(defconst fi:composer-menu
    '("Composer"
      ["Start Composer" fi:start-composer fi::connection-open]
      ["Start Composer w/mouse line" fi:start-composer-mouse-line
       fi::connection-open]
      "----"
      ("CLOS"
       ["Inspect class" fi:inspect-class fi::connection-open]
       ["Inspect generic function" fi:inspect-function fi::connection-open]
       ["Show class subclasses" fi:show-subclasses fi::connection-open]
       ["Show class superclasses" fi:show-superclasses fi::connection-open]
       )
      ("Xref"
       ["Show calls to" fi:show-calls-to fi::connection-open]
       ["Show calls from" fi:show-calls-from fi::connection-open]
       ["Show calls to and from" fi:xxx fi::connection-open]
       ["Discard info" fi:xxx fi::connection-open]
       )
      ("Profiler"
       ["Start time profiler" fi:composer-start-time-profiler
	fi::connection-open]
       ["Start space profiler" fi:composer-start-space-profiler
	fi::connection-open]
       ["Stop profiler" fi:composer-stop-profiler fi::connection-open]
       ["Display time" fi:composer-display-time-profiler
	fi::connection-open]
       ["Display space" fi:composer-display-space-profiler
	fi::connection-open]
       ["Options" fi:composer-profiler-options fi::connection-open]
       )
      ("Other"
       ["Inspect" fi:inspect-value fi::connection-open]
       ["Presenting Listener" composer::make-presenting-listener
	fi::connection-open]
       ["Processes" fi:composer-process-browser fi::connection-open]
       ["Systems" fi:composer-defsys-browser fi::connection-open]
       ["Reinitialize resources" fi:composer-reinitialize-resources
	fi::connection-open]
       ["Options" fi:composer-other-options fi::connection-open]
       )
      ("Help"
       ["Help" fi:composer-help fi::connection-open]
       ["Current pointer gesture bindings" fi:composer-help-gesture-bindings
	fi::connection-open])
      "----"
      ["Exit Composer/Common Windows" fi:composer-exit
       fi::connection-open]
      ))

(defun fi::connection-open ()
  (fi::lep-open-connection-p))

(defun fi::connection-not-open ()
  (not (fi::lep-open-connection-p)))

(defun fi::connection-once-open ()
  (and (not (fi::lep-open-connection-p))
       (not fi::common-lisp-first-time)))

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

(defun fi:exit-lisp ()
  (interactive)
  (fi:eval-in-lisp "(exit 0 :quiet t)"))

(defun fi:start-composer ()
  (interactive)
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line nil)nil)"))

(defun fi:start-composer-mouse-line ()
  (interactive)
  (fi:eval-in-lisp "(progn(wt::start-composer :mouse-line t)nil)"))
  
(defun fi:composer-other-options ()
  (interactive)
  (fi:eval-in-lisp
   "(progn(wt::set-options-command t)nil)"))

(defun fi:composer-help ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::print-startup-help)nil)"))

(defun fi:composer-help-gesture-bindings ()
  (interactive)
  (fi:eval-in-lisp "(progn(wt::composer-report-gestures-command t)nil)"))

(defun fi:composer-exit ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer:stop-composer :kill-cw t)nil)"))

(defun fi:composer-process-browser ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::process-browser)nil)"))

(defun fi:composer-reinitialize-resources ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::init-resource-database)nil)"))

(defun fi:composer-defsys-browser ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::defsys-browser)nil)"))

(defun fi:composer-start-time-profiler ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :time)nil)"))

(defun fi:composer-start-space-profiler ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::start-profiler-command-1 :space)nil)"))

(defun fi:composer-display-time-profiler ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :time)nil)"))

(defun fi:composer-display-space-profiler ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::display-profile-command-1 :space)nil)"))

(defun fi:composer-stop-profiler ()
  (interactive)
  (fi:eval-in-lisp "(progn(composer::stop-profiler-command-1)nil)"))

(defun fi:composer-profiler-options ()
  (interactive)
  (fi:eval-in-lisp
   "(progn(wt::run-motif-application 'wt::make-profiler-options)nil)"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst fi:common-lisp-mode-popup-menu
    '("common lisp mode popup menu"
      ["Find definition" fi:menu-lisp-find-definition fi::connection-open]
      ["Find next definition" fi:lisp-find-next-definition fi::connection-open]
      ["Arglist" fi:menu-lisp-arglist fi::connection-open]
      ["Toggle trace" fi:menu-toggle-trace-definition fi::connection-open]
      ["Macroexpand" fi:lisp-macroexpand fi::connection-open]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively
       fi::connection-open]
      "----"
      ["Compile and load file" fi:menu-compile-and-load-file
       fi::connection-open]
      ["Load file" fi:menu-load-file fi::connection-open]
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
      ["Zoom" fi:debug-menu-zoom fi::connection-open]
      ["Down frame" fi:debug-menu-down-frame fi::connection-open]
      ["Up frame" fi:debug-menu-up-frame fi::connection-open]
      ["Edit frame" fi:debug-menu-edit-frame fi::connection-open]
      ["Locals for frame" fi:debug-menu-locals fi::connection-open]
      "----"
      ["Continue" fi:debug-menu-continue fi::connection-open]
      ["Restart" fi:debug-menu-restart fi::connection-open]
      ["Pop" fi:debug-menu-pop fi::connection-open]
      ["Reset" fi:debug-menu-reset fi::connection-open]
      "----"
      ["List processes" fi:debug-menu-processes fi::connection-open]
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
