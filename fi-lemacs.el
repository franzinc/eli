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
;; $Header: /repo/cvs.copy/eli/Attic/fi-lemacs.el,v 2.2 1993/07/15 00:02:05 layer Exp $

;; does lemacs have dialogs?

(defvar fi:allegro-menubar-name "Allegro")
(defvar fi:composer-menubar-name "Allegro Composer")

(defconst fi:allegro-global-menu
    (list
     fi:allegro-menubar-name
     ["Run Common Lisp" fi:common-lisp t]
     ["New Listener" fi:open-lisp-listener t]
     ["Restart Common Lisp" fi:restart-common-lisp t]))

(defconst fi:common-lisp-mode-menu
    (append
     fi:allegro-global-menu
     (list
      "----"
      (list
       "Compile"
       ["top-level form" fi:lisp-compile-defun t]
       ["region" fi:lisp-compile-region t]
       ["last s-exp" fi:lisp-compile-last-sexp t]
       ["buffer" fi:lisp-compile-current-buffer t])
      (list
       "Evaluate"
       ["top-level form" fi:lisp-eval-defun t]
       ["region" fi:lisp-eval-region t]
       ["last s-exp" fi:lisp-eval-last-sexp t]
       ["buffer" fi:lisp-eval-current-buffer t])
      (list
       "On fspec at point"
       ["Find definition" fi:lisp-find-definition t]
       ["Find definition other window" fi:lisp-find-definition-other-window t]
       ["Arglist" fi:lisp-arglist t]
       ["Describe" fi:describe-symbol t]
       ["Apropos" fi:lisp-apropos t]
       ["Function Documentation" fi:lisp-function-documentation t]
       ["Who calls" fi:lisp-who-calls t]
       ["Toggle trace" fi:toggle-trace-definition t]
       ["Kill definition" fi:kill-definition t])
      "----"
      ["Find next definition" fi:lisp-find-next-definition t]
      ["Macroexpand" fi:lisp-macroexpand t]
      ["Recursive macroexpand" fi:lisp-macroexpand-recursively t]
      ["Comment region" fi:comment-region t]
      ["Uncomment region" fi:uncomment-region t]
      ["Delete *CL-temp* window" fi:lisp-delete-pop-up-window t]
      )))

(defconst fi:inferior-common-lisp-mode-menu
    (append
     fi:allegro-global-menu
     (list
      "----"
      ["Interrupt listener" fi:interrupt-listener t]
      ["Interrupt process" fi:interrupt-xxxx t]
      "----"
      ["Previous input" fi:pop-input t]
      ["Next input" fi:push-input t]
      ["Search previous input" fi:re-search-backward-input t]
      ["Search next input" fi:re-search-forward-input t]
      "----"
      ["Debug current process" fi:scan-stack t]
      ["Debug process" fi:scan-stack t]
      )))

(defconst fi:composer-global-menu
    (list
     fi:composer-menubar-name
     ;; all the mouse line stuff should be duplicated here
     ;; when fi:common-lisp starts up it should automatically cause this
     ;; menubar to be added globally
     ["Profiler options" fi:scan-stack t]
     ))

(defconst fi:clim-menu
    '("CLIM"
      ))

(defun fi:common-lisp-mode-menu (e)
  (interactive "@e")
  (mouse-set-point e)
  (fi::sensitize-allegro-menus-hook)
  (popup-menu fi:common-lisp-mode-menu))
  
(defun fi:inferior-common-lisp-mode-menu (e)
  (interactive "@e")
  (mouse-set-point e)
  (fi::sensitize-allegro-menus-hook)
  (popup-menu fi:inferior-common-lisp-mode-menu))
  
(defun fi::install-menubar (menu-bar)
  (set-menubar (delete (assoc (car menu-bar) current-menubar)
		       (copy-sequence current-menubar)))
  (add-menu nil (car menu-bar) (cdr menu-bar)))

(defun fi::install-mode-menus ()
  (fi::install-menubar fi:allegro-global-menu)
  (let ((menu-bar
	 (cond ((eq major-mode 'fi:common-lisp-mode)
		(define-key fi:common-lisp-mode-map 'button3
		  'fi:common-lisp-mode-menu)
		fi:common-lisp-mode-menu)
	       ((eq major-mode 'fi:inferior-common-lisp-mode)
		(define-key fi:inferior-common-lisp-mode-map 'button3
		  'fi:inferior-common-lisp-mode-menu)
		fi:inferior-common-lisp-mode-menu))))
    (when menu-bar
      (set-buffer-menubar (delete (assoc (car menu-bar) current-menubar)
				  (copy-sequence current-menubar)))
      (add-menu nil (car menu-bar) (cdr menu-bar)))))

(add-hook 'fi:common-lisp-mode-hook 'fi::install-mode-menus)
(add-hook 'fi:inferior-common-lisp-mode-hook 'fi::install-mode-menus)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::install-mouse-tracker ()
  (require 'mode-motion)
  (setq mode-motion-hook 'mode-motion-highlight-sexp))

(add-hook 'fi:common-lisp-mode-hook 'fi::install-mouse-tracker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::sensitize-allegro-menus-hook ()
  (let* ((allegro-menu
	  (cdr (car (find-menu-item current-menubar
				    (list fi:allegro-menubar-name)))))
	 (run-common-lisp
	  (car (find-menu-item allegro-menu '("Run Common Lisp"))))
	 (new-listener
	  (car (find-menu-item allegro-menu '("New Listener"))))
	 (restart-common-lisp
	  (car (find-menu-item allegro-menu '("Restart Common Lisp"))))
	 (on-fspec-at-point
	  (car (find-menu-item allegro-menu '("On fspec at point"))))

	 (find-next
	  (car (find-menu-item allegro-menu '("Find next definition"))))
	 (macroexpand
	  (car (find-menu-item allegro-menu '("Macroexpand"))))
	 (recursive-macroexpand
	  (car (find-menu-item allegro-menu '("Recursive macroexpand"))))
	 
	 (compile-menu
	  (cdr (car (find-menu-item allegro-menu '("Compile")))))
	 (eval-menu
	  (cdr (car (find-menu-item allegro-menu '("Evaluate")))))

	 (changes nil)
	 (connection-open (fi::lep-open-connection-p)))
    
    (when on-fspec-at-point
      (setq changes (fi::deactivate-menu on-fspec-at-point connection-open)))

    (when run-common-lisp
      ;; when lisp is running, we make sure the menu item is deactivated.
      ;; when lisp is not running, we make sure the menu item is activated.
      (cond ((and connection-open (aref run-common-lisp 2))
	     (aset run-common-lisp 2 nil)
	     (setq changes t))
	    ((and (not connection-open) (not (aref run-common-lisp 2)))
	     (aset run-common-lisp 2 t)
	     (setq changes t))))
    
    (when new-listener
      ;; when lisp is running, we make sure the menu item is activated.
      ;; when lisp is not running, we make sure the menu item is deactivated.
      (cond ((and connection-open (not (aref new-listener 2)))
	     (aset new-listener 2 t)
	     (setq changes t))
	    ((and (not connection-open) (aref new-listener 2))
	     (aset new-listener 2 nil)
	     (setq changes t))))
    
    (when restart-common-lisp
      ;; when lisp is running, we make sure the menu item is deactivated.
      ;; when lisp is not running, we make sure the menu item is activated.
      (cond (fi::common-lisp-first-time
	     (when (aref restart-common-lisp 2)
	       (aset restart-common-lisp 2 nil)
	       (setq changes t)))
	    ((and connection-open (aref restart-common-lisp 2))
	     (aset restart-common-lisp 2 nil)
	     (setq changes t))
	    ((and (not connection-open) (not (aref restart-common-lisp 2)))
	     (aset restart-common-lisp 2 t)
	     (setq changes t))))

    (when find-next
      (cond ((and connection-open (not (aref find-next 2)))
	     (aset find-next 2 t)
	     (aset macroexpand 2 t)
	     (aset recursive-macroexpand 2 t)
	     (setq changes t))
	    ((and (not connection-open) (aref find-next 2))
	     (aset find-next 2 nil)
	     (aset macroexpand 2 nil)
	     (aset recursive-macroexpand 2 nil)
	     (setq changes t))))
    
    (when compile-menu
      (let ((compile-defun
	     (car (find-menu-item compile-menu '("top-level form"))))
	    (compile-region
	     (car (find-menu-item compile-menu '("region"))))
	    (compile-last-sexp
	     (car (find-menu-item compile-menu '("last s-exp"))))
	    (compile-buffer
	     (car (find-menu-item compile-menu '("buffer")))))
	(cond ((and connection-open (not (aref compile-defun 2)))
	       (aset compile-defun 2 t)
	       (aset compile-region 2 t)
	       (aset compile-last-sexp 2 t)
	       (aset compile-buffer 2 t)
	       (setq changes t))
	      ((and (not connection-open) (aref compile-defun 2))
	       (aset compile-defun 2 nil)
	       (aset compile-region 2 nil)
	       (aset compile-last-sexp 2 nil)
	       (aset compile-buffer 2 nil)
	       (setq changes t)))))
    
    (when eval-menu
      (let ((eval-defun
	     (car (find-menu-item eval-menu '("top-level form"))))
	    (eval-region
	     (car (find-menu-item eval-menu '("region"))))
	    (eval-last-sexp
	     (car (find-menu-item eval-menu '("last s-exp"))))
	    (eval-buffer
	     (car (find-menu-item eval-menu '("buffer")))))
	(cond ((and connection-open (not (aref eval-defun 2)))
	       (aset eval-defun 2 t)
	       (aset eval-region 2 t)
	       (aset eval-last-sexp 2 t)
	       (aset eval-buffer 2 t)
	       (setq changes t))
	      ((and (not connection-open) (aref eval-defun 2))
	       (aset eval-defun 2 nil)
	       (aset eval-region 2 nil)
	       (aset eval-last-sexp 2 nil)
	       (aset eval-buffer 2 nil)
	       (setq changes t)))))
    
    ;; if we made any changes, return nil
    ;; otherwise return t to indicate that we haven't done anything.
    (not changes)))

(defun fi::deactivate-menu (menu &optional activate)
  (let ((items (cdr menu))
	(changes nil))
    (while items
      (when (arrayp (car items))
	(unless (eq activate (aref (car items) 2))
	  (aset (car items) 2 activate)
	  (setq changes t)))
      (setq items (cdr items)))
    changes))

(add-hook 'activate-menubar-hook 'fi::sensitize-allegro-menus-hook)
