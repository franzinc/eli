;;; subprocess-lisp.el
;;;   functions to send lisp source code to the sublisp process
;;;
;;; $Header: /repo/cvs.copy/eli/fi-sublisp.el,v 1.3 1987/09/20 14:43:51 layer Exp $

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
		       (set-syntax-table lisp-mode-syntax-table)
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
      (setq emacs-to-lisp-transaction-file (format
					    "%s.cl"
					    (make-temp-name
					     (format "/tmp/%s"
						     (user-login-name))))
	    emacs-to-lisp-package (if package
				      (format "(in-package :%s)\n" package)
				    nil)
	    emacs-to-lisp-transaction-buf (create-file-buffer
					   emacs-to-lisp-transaction-file)))
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
	     (format ":cl %s" emacs-to-lisp-transaction-file)
	   (format ":ld %s" emacs-to-lisp-transaction-file))))
    (send-string-split process load-string nl-to-cr)))
