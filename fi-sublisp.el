;;; subprocess-lisp.el
;;;   functions to send lisp source code to the sublisp process
;;;
;;; $Header: /repo/cvs.copy/eli/fi-sublisp.el,v 1.7 1988/02/21 21:26:55 layer Exp $

(defun inferior-lisp-newline ()
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
		  (subprocess-send-input)
		;; not a complete sexp, so newline and indent
		(progn
		  (newline)
		  (lisp-indent-line))))
	  ;; a non-list s-exp, so just send it off...
	  (subprocess-send-input)))
    (subprocess-send-input)))

(defun inferior-lisp-send-sexp-input (arg)
  "Send s-expression(s) to the Lisp subprocess."
  (interactive "P")
  (inferior-lisp-send-input arg 'sexp))

(defun inferior-lisp-send-list-input (arg)
  "Send list(s) to the Lisp subprocess."
  (interactive "P")
  (inferior-lisp-send-input arg 'lists))

(defun inferior-lisp-send-input (arg type)
  "Send s-expression(s) or list(s) to the Lisp subprocess.
The second parameter must be either 'sexps or 'lists, specifying
  whether lists or s-expressions should be parsed (internally,
  either `(scan-sexps)' or `(scan-lists)' is used).
If at the end of buffer, everything typed since the last output
  from the Lisp subprocess is collected and sent to the Lisp
  subprocess.  With an argument, only the specified number of
  s-expressions or lists from the end of the buffer are sent.
If in the middle of the buffer, the current s-expression(s) or
  list(s) is(are) copied to the end of the buffer and then sent.
  An argument specifies the number of s-expressions or lists to
  be sent.
If s-expressions are being parsed:
  If the cursor follows a closing parenthesis, the preceding
  s-expression(s) is(are) processed.  If the cursor is at an
  opening parenthesis, the following s-expression(s) is(are)
  processed.  If the cursor is at a closing parenthesis, the
  preceding s-expression(s) is(are) processed.  Otherwise, the
  enclosing s-expression(s) is(are) processed.
If lists are being parsed:
  The enclosing list is processed."
  (if (and (eobp) (null arg))
      (progn
	(move-marker last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(lisp-indent-line)
	(move-marker last-input-end (point)))

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
	    (lisp-indent-line)
	    (move-marker last-input-start start-resend)
	    (move-marker last-input-end (point-max)))
	(progn
	  (goto-char (point-max))
	  (move-marker last-input-start (point))
	  (insert exp-to-resend)
	  (if (not (bolp)) (insert "\n"))
	  (move-marker last-input-end (point))))))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region-split process last-input-start last-input-end
		       subprocess-map-nl-to-cr)
    (input-ring-save last-input-start (1- last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:eval-last-sexp (compile-file-p)
  "Send sexp before point to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive "P")
  (let* ((stab (syntax-table))
	 (start  (unwind-protect
		     (save-excursion
		       (set-syntax-table fi:lisp-mode-syntax-table)
		       (forward-sexp -1)
		       (point))
		   (set-syntax-table stab))))
    (fi:eval-send start (point) compile-file-p)))

(defun fi:eval-defun (compile-file-p)
  "Send the current `defun' to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (fi:eval-send start end compile-file-p)))

(defun fi:eval-region (compile-file-p)
  "Send the region to the Lisp subprocess sublisp-name an sexp at a time.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive "P")
  (fi:eval-send (min (point) (mark))
		(max (point) (mark))
		compile-file-p))

(defun fi:eval-current-buffer (compile-file-p)
  "Send the entire current buffer to the Lisp subprocess sublisp-name.
If sublisp-name is nil, startup an appropriate sublisp, based on
the major-mode of the buffer."
  (interactive "P")
  (fi:eval-send (point-min) (point-max) compile-file-p))

(defun fi:eval-send (start end compile-file-p)
  "Send the text from START to END over to the sublisp, in the
correct package, of course."
  (fi:sublisp-select)
  (let* ((stuff (buffer-substring start end))
	 (sublisp-process (get-process sublisp-name)))
    (send-string-load
     sublisp-process stuff subprocess-map-nl-to-cr compile-file-p)
    (send-string-split sublisp-process "\n" subprocess-map-nl-to-cr)
    (switch-to-buffer-other-window (process-buffer sublisp-process))
    ;;(input-ring-save-string stuff)
    (goto-char (point-max))))
  
(defun set-associated-sublisp (buffer-name)
  "Set the sublisp associated with a franz lisp or common lisp source file."
  (interactive "sSublisp name: ")
  (if (get-process buffer-name)
      (setq sublisp-name buffer-name)
    (error "No such process buffer.")))

(defun fi:sublisp-select ()
  "Find a sublisp for eval commands to send code to.  Result stored in
the variable sublisp-name.  
If sublisp-name is set, and there is an associated process buffer, thats that.
If sublisp-name is nil, or if there is no process buffer with that name, then
try for freshest-<franz,common>-sublisp-name, which should contain the name of
the most recently started sublisp.  If neither of these exist, runs the command
franz-lisp or common-lisp, depending on the major mode of the buffer."
  (interactive)
  ;;; see if sublisp is named yet.  if its not, name it intelligently.
  (cond (sublisp-name t)
	((eql major-mode 'franz-lisp-mode)
	 (if freshest-franz-sublisp-name
	     (setq sublisp-name freshest-franz-sublisp-name)
	   (setq sublisp-name "franz-lisp")))
	((eql major-mode 'common-lisp-mode)
	 (if freshest-common-sublisp-name
	     (setq sublisp-name freshest-common-sublisp-name)
	   (setq sublisp-name "common-lisp")))
	(t (error "Cant start a subprocess for Major mode %s." major-mode)))
  ;; start-up the sublisp process if necessary and possible
  (cond ((get-process sublisp-name) t)
	((eql major-mode 'franz-lisp-mode)
	 (if (and freshest-franz-sublisp-name 
		  (get-process freshest-franz-sublisp-name))
	     (setq sublisp-name freshest-franz-sublisp-name)
	   (setq sublisp-name (prog1
				  (run-franz-lisp nil)
				(switch-to-buffer nil)
				(sleep-for 5)))))
	((eql major-mode 'common-lisp-mode)
	 (if (and freshest-common-sublisp-name 
		  (get-process freshest-common-sublisp-name))
	     (setq sublisp-name freshest-common-sublisp-name)
	   (setq sublisp-name (prog1
				  (run-common-lisp nil)
				(switch-to-buffer nil)
				(sleep-for 1)))))
	(t (error
	    "Cant start a subprocess for sublisp-name %s." sublisp-name))))

(defun send-string-load (process text nl-to-cr compile-file-p)
  (if (null emacs-to-lisp-transaction-file)
      (let ()
	(setq emacs-to-lisp-transaction-file
	  (let* ((filename (buffer-file-name (current-buffer))))
	    (format "/tmp/,%s" (file-name-nondirectory filename))))
	(setq emacs-to-lisp-package (if package
					(format "(in-package :%s)\n" package)
				      nil))
	(setq emacs-to-lisp-transaction-buf
	  (let ((name (file-name-nondirectory
		       emacs-to-lisp-transaction-file)))
	    (or (get-buffer name)
		(create-file-buffer name))))
	(let ((file emacs-to-lisp-transaction-file))
	  (save-window-excursion
	    (pop-to-buffer emacs-to-lisp-transaction-buf)
	    (set 'remove-file-on-kill-emacs file)
	    (set 'remove-file-on-kill-emacs file)))))
  (save-window-excursion
    (let ((file emacs-to-lisp-transaction-file)
	  (pkg emacs-to-lisp-package))
      (pop-to-buffer emacs-to-lisp-transaction-buf)
      (erase-buffer)
      (if pkg (insert pkg))
      (insert text)
      (newline)
      (write-region (point-min) (point-max) file)
      (bury-buffer)))
  (let ((load-string
	 (if compile-file-p
	     (format ":cl \"%s\"" emacs-to-lisp-transaction-file)
	   (format ":ld \"%s\"" emacs-to-lisp-transaction-file))))
    (send-string-split process load-string nl-to-cr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; TCP/backdoor lisp listener
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The backdoor lisp listener functions.

;; Certain commands are typically invoked while editing lisp text, e.g.
;; arglist or macroexpand.  These a sent to the inferior lisp to be evaluated
;; in that lisp world.  Gnu uses a dedicated "backdoor" lisp listener connected
;; by a socket to avoid having to type at the listener in the regular inferior
;; lisp window.
;; The strategy is only to send rather short known forms to the backdoor
;; listener, and to have it read all large and possibly-erroneous forms out
;; of a temp file.  (See lisp-send-defun and sublisp-macroexpand.)
;;  This reduces the possibility of the listener getting hung up
;; because someone has strange lisp syntax in his buffer, or broken
;; reader macros, or broken regular macros, ...

(defvar unix-domain t)
(defvar unix-domain-socket (expand-file-name "~/.excl_to_emacs"))

(defvar local-host-name "localhost")
(defvar excl-service-name "excl")

(defun run-common-lisp-tcp (&optional host)
  "Make a new buffer connected to the Lisp Listener Daemon on the given host,
which defaults to the local host.  A fresh buffer is made for each invocation.
Does not yet have provision for signals (e.g. ^C)."
  (interactive)
  (let ((buffer (generate-new-buffer "*tcplisp*"))
	(host (or host local-host-name))
	proc)
    (save-excursion
      (set-buffer buffer)
      (if unix-domain
	  (setq proc (open-network-stream
		      (buffer-name buffer) buffer unix-domain-socket 0))
	(setq proc (open-network-stream (buffer-name buffer) buffer
					local-host-name
					excl-service-name)))
      (goto-char (point-max))
      (set-marker (process-mark proc) (point)))
    (switch-to-buffer buffer)
    (fi:inferior-lisp-mode common-lisp-prompt-pattern)))

(defvar background-sublisp-process nil
  "Process connected to sublist socket for sublisp-arglist and friends.")

(defun background-sublisp-process (&optional nomake)
  (if (or (null background-sublisp-process)
	  (not (eq (process-status background-sublisp-process) 'open)))
    (if nomake
      (setq background-sublisp-process nil)
      (progn
	(message "starting a backdoor lisp listener")
	(and background-sublisp-process
	     (delete-process background-sublisp-process))
	(setq background-sublisp-process
	  (if unix-domain
	      (open-network-stream "sublisp-back" nil unix-domain-socket 0)
	    (open-network-stream "sublisp-back" nil local-host-name
				 excl-service-name)))
	(setq sublisp-returns-state nil)
	(process-send-string background-sublisp-process
			     background-sublisp-form)
	(set-process-filter background-sublisp-process
			    'sublisp-backdoor-filter))))
  background-sublisp-process)

(defvar background-sublisp-form
 "(progn
 (setf (mp:process-name mp:*current-process*) \"Gnu Listener\")
 (loop
  (princ \"\n\")
  (errorset (eval (read)) t)))\n"
 "The program executed by the backdoor lisp listener.")


;; This is the filter for the back door lisp process.
;; It collects output until it sees a ctl-A\n, then prints the preceding
;; collected text.  If the text fits on one line, it is printed to the message
;; area.  Otherwise it goes to a temporary pop up buffer.

(defvar sublisp-returns "")
(defvar sublisp-returns-state nil)

(defun sublisp-backdoor-filter (proc string)
  ;; This collects everything returned until a ^A\n prompt is seen,
  ;; then displays it.  The first time is special cased to throw away
  ;; the initial prompt without display.  Someday we should use the state
  ;; variable for detecting screwups and coordinating reset.
  ;; The \n is part of the prompt so that a subsequent prettyprint isn't
  ;; confused about the starting column.
  (setq sublisp-returns (concat sublisp-returns string))
  (let ((len (length sublisp-returns)))
    (if (and (= 10 (aref sublisp-returns (- len 1))) ; newline
	     (= 1 (aref sublisp-returns (- len 2))))
      (if (eq sublisp-returns-state nil) ; ignore the startup response
	(setq sublisp-returns-state t
	      sublisp-returns "")
	(progn (setq sublisp-returns
		 (substring sublisp-returns
			    (progn (string-match "\n*" sublisp-returns)
				   (match-end 0))
			    -2))
	       (let ((first-char (elt sublisp-returns 0)))
		 (cond
		   ((= first-char 2)	; display source-file
		    (let* ((temp (get-buffer-create " *lisp temp*"))
			   (form (substring sublisp-returns 1)))
		      ;; first remove all #p's
		      (save-excursion
			(set-buffer temp)
			(erase-buffer)
			(princ form temp)
			(beginning-of-buffer)
			(replace-string "#p" "")
			(setq form (car (read-from-string (buffer-string))))
			(kill-buffer temp))
		      (setq sublisp-returns "")
		      (if (cdr form)
			  (find-source-from-info (car form) (cdr form))
			(message "The source location of `%s' is unknown."
				 (car form)))))
		   (t  
		    (if (or (> (length sublisp-returns) 78)
			    ;; should be mbuf width
			    (string-match "\n" sublisp-returns nil))
			(with-output-to-temp-buffer "*Help*"
			  (princ sublisp-returns))
		      (message sublisp-returns))
		    (setq sublisp-returns "")))))))))

(defun find-source-from-info (xname info)
  ;; info is an alist of (type . filename)
  (if (= 1 (length info))
      (setq info (car info))
    (let ((completion-ignore-case t)
	  (string-info (mapcar '(lambda (x)
				 (rplaca x (symbol-name (car x)))
				 x)
			       (copy-alist info))))
      (setq info
	(assoc (intern-soft
		(completing-read (format "Find `%s' of what type? " xname)
				 string-info
				 nil t nil))
	       info))))
  (let* ((type (car info))
	 (file (cdr info))
	 (name (symbol-name xname))
	 (search-form nil))
    (if (not (file-exists-p file))
	(error "can't file source file `%s'" file))
    (find-file-other-window file)
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

(defun get-cl-symbol (symbol up-p &optional interactive)
  ;; Ask the user for a CL symbol to be investigated.
  ;; If INTERACTIVE, then that prompt is used.
  ;; If UP-P, then the default is the symbol at the car of the form at the
  ;; cursor.  Otherwise, it is the symbol at the cursor.
  (if interactive (setq symbol (read-string interactive)))
  (if (or (null symbol) (equal symbol ""))
    (setq symbol
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
			    (progn (forward-sexp 1) (point)))))))
  (if (and (boundp 'package)
	   package
	   (not (string-match ":" symbol nil)))
    (setq symbol (format "%s::%s" package symbol)))
  (if interactive (list symbol) symbol))

(defun sublisp-arglist (&optional symbol)
  "Asks the sublisp to run arglist on a symbol."
  (interactive (get-cl-symbol nil t "Function: "))
  (setq symbol (get-cl-symbol symbol t))
  (process-send-string
   (background-sublisp-process)
   (format
    "(progn
      (format t  \"~:[()~;~:*~{~a~^ ~}~]\"
       (cond
	((special-form-p '%s) '(\"%s is a special form\"))
	((macro-function '%s) '(\"%s is a macro\"))
	((not (fboundp '%s)) '(\"%s has no function binding\"))
	(t (excl::arglist '%s))))
      (values))\n"
    symbol symbol symbol symbol symbol symbol symbol)))

(defun sublisp-describe (&optional symbol)
  "Asks the sublisp to describe a symbol."
  (interactive (get-cl-symbol nil nil "Describe symbol: "))
  (setq symbol (get-cl-symbol symbol nil))
  (process-send-string
   (background-sublisp-process)
   (format "(progn (lisp:describe '%s) (values))\n" symbol)))

(defun sublisp-function-documentation (&optional symbol)
  "Asks the sublisp for function documentation of symbol."
  (interactive (get-cl-symbol nil nil "Function documentation for symbol: "))
  (setq symbol (get-cl-symbol symbol nil))
  (process-send-string
   (background-sublisp-process)
   (format "(princ (lisp:documentation '%s 'lisp:function))\n" symbol)))

(defun sublisp-display-source (&optional symbol)
  "Find the common lisp source for SYMBOL."
  (interactive (get-cl-symbol nil nil "Source for symbol: "))
  (setq symbol (get-cl-symbol symbol nil))
  (process-send-string
   (background-sublisp-process)
   (format "(format t \"\2~s\" (cons '%s (source-file '%s t)))\n"
	   symbol symbol)))

(defvar sublisp-macroexpand-command
 "(progn
    (errorset
     (let ((*print-pretty* t)(excl::*print-nickname* t)(*package* %s))
       (with-open-file (*standard-input* \"%s\")
	 (lisp:prin1 (lisp:macroexpand (lisp:read)))))
     t)
    (values))\n")

(defun sublisp-macroexpand ()
  "Macroexpand the form at the cursor using the backdoor Lisp process."
  (interactive)
  (save-excursion
    (skip-chars-forward " \t")
    (if (not (looking-at "("))
      (up-list -1))
    (let ((filename (format "/tmp/emlisp%d"
		      (process-id (get-process
				    freshest-common-sublisp-name))))
	  (start (point)))
      (forward-sexp)
      (write-region start (point) filename nil 'nomessage)
      (background-sublisp-process)
      (process-send-string
       background-sublisp-process
       (format sublisp-macroexpand-command
	       (if (and (boundp 'package) package)
		 (format "(or (find-package :%s) (make-package :%s))"
			 package package)
		 "*package*")
	       filename)))))
