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

;; $Header: /repo/cvs.copy/eli/fi-keys.el,v 1.18 1988/11/21 21:15:52 layer Exp $

;;;;
;;; Key defs
;;;;

(defun fi::subprocess-mode-super-keys (map mode)
  "Setup keys in MAP as a subprocess super-key map.  MODE is either
shell, rlogin, sub-lisp or tcp-lisp."
  (define-key map "\C-a" 'fi:subprocess-beginning-of-line)
  (define-key map "\C-k" 'fi:subprocess-kill-output)
  (define-key map "\C-l" 'fi:list-input-ring)
  (define-key map "\C-m" 'fi:subprocess-input-region)
  (define-key map "\C-n" 'fi:push-input)
  (define-key map "\C-o" 'fi:subprocess-send-flush)
  (define-key map "\C-p" 'fi:pop-input)
  (define-key map "\C-r" 'fi:re-search-backward-input)
  (define-key map "\C-s" 'fi:re-search-forward-input)
  (define-key map "\C-u" 'fi:subprocess-kill-input)
  (define-key map "\C-v" 'fi:subprocess-show-output)
  (define-key map "\C-w" 'fi:subprocess-backward-kill-word)

  (cond
    ((memq mode '(sub-lisp shell))
     (if (eq mode 'shell)
	 (define-key map "\C-z"	'fi:subprocess-suspend))
     (define-key map "\C-c"	'fi:subprocess-interrupt)
     (define-key map "\C-d"	'fi:subprocess-send-eof)
     (define-key map "\C-\\"	'fi:subprocess-quit))
    ((eq mode 'tcp-lisp)
     (define-key map "\C-c"	'fi:tcp-lisp-interrupt-process)
     (define-key map "\C-d"	'fi:tcp-lisp-send-eof)
     (define-key map "\C-\\"	'fi:tcp-lisp-kill-process)))
  map)

(defun fi::subprocess-mode-commands (map supermap mode)
  "Define subprocess mode commands on MAP, using SUPERMAP as the supermap.
MODE is either sub-lisp, tcp-lisp, shell or rlogin."
  (define-key map "\C-m" 'fi:subprocess-send-input)
  (if fi:subprocess-enable-superkeys
      (progn
	(define-key map "\C-a"  'fi:subprocess-superkey)
	;; \C-c points to supermap
	(define-key map "\C-d"  'fi:subprocess-superkey)
	(define-key map "\C-o"  'fi:subprocess-superkey)
	(define-key map "\C-u"  'fi:subprocess-superkey)
	(define-key map "\C-w"  'fi:subprocess-superkey)
	(define-key map "\C-z"  'fi:subprocess-superkey)
	(define-key map "\C-\\" 'fi:subprocess-superkey)))
  (if supermap (define-key map "\C-c" supermap))
  map)

(defun fi::lisp-mode-commands (map supermap mode)
  (define-key map "\e" (make-sparse-keymap))
  (define-key map "\C-x" (make-sparse-keymap))

  (if supermap (define-key map "\C-c" supermap))
  
  (define-key map "\t"		'lisp-indent-line)
  (define-key map "\e\C-q"	'indent-sexp)
  (define-key map "\C-?"	'backward-delete-char-untabify)
  
  (cond
    ((memq mode '(sub-lisp tcp-lisp))
     (define-key map "\r"	'fi:inferior-lisp-newline)
     (define-key map "\e\r"	'fi:inferior-lisp-input-sexp)
     (define-key map "\C-x\r"	'fi:inferior-lisp-input-list))
    (t (define-key map "\r"	'fi:lisp-reindent-newline-indent)))

  (cond
    ((memq major-mode '(fi:common-lisp-mode fi:inferior-common-lisp-mode
			fi:tcp-common-lisp-mode))
     (define-key map "\e."	'fi:lisp-find-tag)
     (define-key map "\e,"	'fi:lisp-tags-loop-continue)
     (define-key map "\e\t"	'fi:lisp-complete-symbol)
     (define-key map "\eA"	'fi:lisp-arglist)
     (define-key map "\eC"	'fi:lisp-who-calls)
     (define-key map "\eD"	'fi:lisp-describe)
     (define-key map "\eF"	'fi:lisp-function-documentation)
     (define-key map "\eM"	'fi:lisp-macroexpand)
     (define-key map "\eW"	'fi:lisp-walk)))
  (cond
    ((eq major-mode 'fi:emacs-lisp-mode)
     (define-key map "\e\C-x"	'eval-defun))
    ((memq major-mode '(fi:common-lisp-mode fi:franz-lisp-mode
			fi:lisp-mode))
     (define-key map "\e\C-x"	'fi:lisp-eval-defun)
     (define-key map "\C-c\C-b"	'fi:lisp-eval-current-buffer)
     (define-key map "\C-c\C-s" 'fi:lisp-eval-last-sexp)
     (define-key map "\C-c\C-r"	'fi:lisp-eval-region)))
  map)

(defun fi::tcp-common-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'tcp-lisp)
			  supermap
			  'tcp-lisp))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'sub-lisp)
			  supermap
			  'sub-lisp))

;;;;;;;;;;;;;;;;;;;;; inferior lisp mode related functions

(defun fi:lisp-reindent-newline-indent ()
  "Indent the current line, insert a newline and indent to the proper
column."
  (interactive)
  (save-excursion (funcall indent-line-function))
  (newline)
  (funcall indent-line-function))

(defun fi:inferior-lisp-newline ()
  "Bound to RET in an inferior Lisp buffer.  At the end of the buffer it
inserts a newline and performs automatic indentation.  Whole expressions
are sent to Lisp (not each piece after each newline is typed).  This allows
previously typed lines to be edited before Lisp is sent the input.  Typed
anywhere else in the buffer, this functions causes the input previously
typed (around the point) to be copied to the end of the subprocess buffer
and send to Lisp."
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
		  (fi:subprocess-send-input)
		;; not a complete sexp, so newline and indent
		(progn
		  (newline)
		  (funcall indent-line-function))))
	  ;; a non-list s-exp, so just send it off...
	  (fi:subprocess-send-input)))
    ;;NOT AT THE END OF THE BUFFER!
    ;; find the user's input contained around the cursor and send that to
    ;; the inferior lisp
    (let ((start-of-last-prompt
	   (save-excursion
	     (or (and (re-search-backward subprocess-prompt-pattern nil t)
		      (point))
		 (point-max))))
	  start end)
      (if (or (and (bolp) (looking-at "("))
	      (re-search-backward "^(" start-of-last-prompt t)
	      (prog1 (re-search-backward subprocess-prompt-pattern nil t)
		(goto-char (match-end 0))))
	  (progn
	    (setq start (point))
	    (let* ((eol (save-excursion (end-of-line) (point)))
		   (state (save-excursion (parse-partial-sexp start eol)))
		   (depth (car state)))
	      (if (zerop depth)
		  (setq end eol)
		(setq end
		  (condition-case ()
		      (save-excursion
			(if (< depth 0)
			    (up-list (- depth))
			  (goto-char eol)
			  (up-list depth))
			(point))
		    (error nil))))

	      (if (or (null end) (= end (point-max)))
		  (progn
		    (goto-char (point-max))
		    (fi:inferior-lisp-newline))
		(fi:subprocess-input-region start end))))
	(error "couldn't find start of input")))))
    
(defun fi:subprocess-input-region (start end)
  "Send the region defined by the point and mark to the Lisp subprocess."
  (interactive "r")
  (let* ((process (get-buffer-process (current-buffer)))
	 (string (buffer-substring start end)))
    (goto-char (point-max))
    (setq start (point))
    (move-marker fi::last-input-start (point))
    (insert string)
    (if (not (bolp)) (insert "\n"))
    (setq end (point))
    (move-marker fi::last-input-end (point))
    (fi::send-region-split process start end fi:subprocess-map-nl-to-cr)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:inferior-lisp-input-sexp (&optional arg)
  "Send the sexp on which the point resides to the Lisp subprocess.  With a
numeric prefix argument, send that many sexps."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'sexp))

(defun fi:inferior-lisp-input-list (&optional arg)
  "Send the list before the point to the Lisp subprocess.  With a numeric
prefix argument, send that many lists."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'lists))

(defun fi:lisp-eval-last-sexp (compile-file-p)
  "Send the sexp before the point to the Lisp subprocess associated with
this buffer.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-send start (point) compile-file-p)))

(defun fi:lisp-eval-defun (compile-file-p)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess associated with this buffer.  A `top-level' form is one that
starts in column 1.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (fi::eval-send start end compile-file-p)))

(defun fi:lisp-eval-region (compile-file-p)
  "Send the text in the region to the Lisp subprocess associated with this
buffer, one expression at a time if there is more than one complete
expression.  If a Lisp subprocess has not been started, then one is
started.  With a prefix argument, the source sent to the subprocess is
compiled."
  (interactive "P")
  (fi::eval-send (min (point) (mark))
		 (max (point) (mark))
		 compile-file-p))

(defun fi:lisp-eval-current-buffer (compile-file-p)
  "Send the entire buffer to the Lisp subprocess associated with this
buffer.  If a Lisp subprocess has not been started, then one is started.
With a prefix argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-send (point-min) (point-max) compile-file-p))


;;;;;;;;;;;;;;;;;;;;; TCP lisp mode related functions

(defun fi::get-default-symbol (prompt &optional up-p)
  (let* ((symbol-at-point
	  (condition-case ()
	      (save-excursion
		(if up-p
		    (progn
		      (if (= (following-char) ?\() (forward-char 1))
		      (if (= (preceding-char) ?\)) (forward-char -1))
		      (up-list -1)
		      (forward-char 1)))
		(while (looking-at "\\sw\\|\\s_")
		  (forward-char 1))
		(if (re-search-backward "\\sw\\|\\s_" nil t)
		    (progn (forward-char 1)
			   (buffer-substring
			    (point)
			    (progn (forward-sexp -1)
				   (while (looking-at "\\s'")
				     (forward-char 1))
				   (point))))
		  nil))
	    (error nil)))
	 (read-symbol
	  (read-string
	   (if symbol-at-point
	       (format "%s: (default %s) " prompt symbol-at-point)
	     (format "%s: " prompt))))
	 (symbol (if (string= read-symbol "")
		     symbol-at-point
		   read-symbol))
	 (colonp (string-match ":?:" symbol nil)))
    (if (and (not colonp) fi:package)
	(setq symbol (format "%s::%s" fi:package symbol)))
    (list symbol)))

(defun fi:lisp-find-tag (tag &optional next)
  "Find the Common Lisp source for a symbol, using the characters around
the point as the default tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Lisp locate source")))
  (fi::lisp-find-tag-common tag next nil))

(defun fi:lisp-find-tag-other-window (tag &optional next)
  "Find the Common Lisp source for a symbol, using the characters around
the point as the default tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Lisp locate source other window")))
  (fi::lisp-find-tag-common tag next t))

(defun fi:lisp-tags-loop-continue ()
  "Find the next occurrence of the tag last used by fi:lisp-find-tag."
  (interactive)
  (fi:lisp-tags-loop-continue-common))

(defun fi:lisp-arglist (symbol)
  "Print the arglist (using excl:arglist) for a symbol, which is read from
the minibuffer.  The word around the point is used as the default."
  (interactive (fi::get-default-symbol "Function" t))
  (let ((string
	 (format "(progn
                    (format t  \"~:[()~;~:*~{~a~^ ~}~]\"
                    (cond
                     ((macro-function '%s) '(\"%s is a macro\"))
                     ((special-form-p '%s) '(\"%s is a special form\"))
                     ((not (fboundp '%s)) '(\"%s has no function binding\"))
                     (t (excl::arglist '%s))))
                    (values))\n"
		 symbol symbol symbol symbol symbol symbol symbol)))
    (if (fi::background-sublisp-process)
	(process-send-string fi::backdoor-process string)
      (fi::eval-string-send string nil t))))

(defun fi:lisp-describe (symbol)
  "Describe a symbol, which is read from the minibuffer.  The word around
the point is used as the default."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (let ((string (format "(progn (lisp:describe '%s) (values))\n" symbol)))
    (if (fi::background-sublisp-process)
	(process-send-string fi::backdoor-process string)
      (fi::eval-string-send string nil t))))

(defun fi:lisp-function-documentation (symbol)
  "Print the function documentation for a symbol, which is read from the
minibuffer.  The word around the point is used as the default." 
  (interactive
   (fi::get-default-symbol "Function documentation for symbol"))
  (let ((string (format "(princ (lisp:documentation '%s 'lisp:function))\n"
			symbol)))
    (if (fi::background-sublisp-process)
	(process-send-string fi::backdoor-process string)
      (fi::eval-string-send string nil t))))

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point."
  (interactive)
  (fi::lisp-macroexpand-common "lisp:macroexpand" "macroexpand"))

(defun fi:lisp-walk (arg)
  "Print the full macroexpansion the form at the point.
With a prefix argument, macroexpand the code as the compiler would."
  (interactive "P")
  (fi::lisp-macroexpand-common
   (if arg "excl::compiler-walk" "excl::walk")
   "walk"))

(defun fi::lisp-macroexpand-common (handler type)
  (let* ((start (condition-case ()
		    (fi::find-other-end-of-list)
		  (error nil)))
	 (filename (format "/tmp/%s,mexp" (user-login-name)))
	 (string
	  (format fi::lisp-macroexpand-command
		  (if (and (boundp 'fi:package) fi:package)
		      (format "(or (find-package :%s) (make-package :%s))"
			      fi:package fi:package)
		    "*package*")
		  filename
		  handler)))
    (if start
	(write-region start (point) filename nil 'nomessage)
      (let ((form (read-string (format "form to %s: " type)))
	    (obuf (current-buffer))
	    (tbuf (get-buffer-create "*cl-macroexpand-temp*")))
	(set-buffer tbuf)
	(erase-buffer)
	(insert form)
	(write-region (point-min) (point-max) filename nil 'nomessage)
	(set-buffer obuf)))
    (if (fi::background-sublisp-process)
	(process-send-string fi::backdoor-process string)
      (fi::eval-string-send string nil t))))

(defun fi:lisp-who-calls (&optional symbol)
  "Print all the callers of a function.  The default symbol name is taken
from the sexp around the point."
  (interactive (fi::get-default-symbol "Find references to symbol"))
  ;; Since this takes a while, tell the user that it has started.
  (message "finding callers of %s..." symbol)
  (let ((string (format
		 "(progn (excl::who-references '%s) (values))\n" symbol)))
    (if (fi::background-sublisp-process)
	(process-send-string fi::backdoor-process string)
      (fi::eval-string-send string nil t))))

(defun fi:lisp-complete-symbol ()
  "Perform completion on the Common Lisp symbol preceding the point.  That
symbol is compared to symbols that exist in the Common Lisp, to which there
is a TCP/IP connection (see fi:eval-in-lisp).  If the symbol starts just
after an open-parenthesis, then only symbols (in the Common Lisp) with
function defintions are considered.  Otherwise all symbols are considered."
  (interactive)
  (let* ((end (point))
	 package real-beg
	 (beg (save-excursion
		(backward-sexp 1)
		(while (= (char-syntax (following-char)) ?\')
		  (forward-char 1))
		(setq real-beg (point))
		(let ((opoint (point)))
		  (if (re-search-forward ":?:" end t)
		      (setq package
			(concat
			 ":" (buffer-substring opoint (match-beginning 0))))))
		(point)))
	 (pattern (buffer-substring beg end))
	 (functions-only (if (eq (char-after (1- real-beg)) ?\() t nil))
	 (completions
	  (progn
	    ;; first, go into that package
	    (if (null (fi:eval-in-lisp "(packagep (in-package :%s))"
				       (or fi:package "user")))
		(error "subprocess is in unknown package: %s" fi:package))
	    ;; then evaluate our expr
	    (fi:eval-in-lisp "(excl::list-all-completions \"%s\" %s %s)"
			     pattern package functions-only)))
	 (alist
	  (if (consp completions)
	      (apply 'list
		     (mapcar
		      (function
		       (lambda (x)
			(let* ((whole-name (symbol-name x))
			       (name (progn
				       (string-match "^\\(.*::?\\)?\\(.*\\)$"
						     whole-name)
				       (substring whole-name
						  (match-beginning 2)
						  (match-end 2)))))
			  (cons name whole-name))))
		      completions))))
	 (completion (if alist (try-completion pattern alist))))
    (cond ((eq completion t))
	  ((null completion)
	   (message "Can't find completion for \"%s\"" pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (let ((new (cdr (assoc completion alist))))
	     (if new
		 (progn
		   (delete-region real-beg end)
		   (insert new))
	       (delete-region beg end)
	       (insert completion))))
	  (t
	   (message "Making completion list...")
	   (with-output-to-temp-buffer "*Help*"
	     (display-completion-list
	      (all-completions pattern alist)))
	   (message "Making completion list...done")))))

(defun fi:tcp-lisp-send-eof ()
  "Simulate an EOF on the tcp-lisp process via a db:debug-pop spoken to the
backdoor Common Lisp listener."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-kill-process ()
  "Kill a tcp-lisp process via a mp:process-kill spoken to the backdoor
Common Lisp listener."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-kill (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-interrupt-process ()
  "Interrupt the tcp-lisp process via a mp:process-interrupt spoken to the
backdoor Common Lisp listener."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-interrupt
      (mp::process-name-to-process \"%s\")
      #'break \"interrupt from emacs\")\n"
   (buffer-name (current-buffer))))


;;;;;;;;;;;;;;;;;;;;; general subprocess related functions

(defun fi:subprocess-superkey (&optional special-binding)
  "This function implements superkeys in subprocess buffers.
A superkey is treated specially when at the end of a subprocess buffer,
but has its normal, global, binding when used elsewhere in the buffer.
At the end of the buffer the key has SPECIAL-BINDING.  If SPECIAL-BINDING
is not given, the key takes its binding from the
fi:subprocess-super-key-map keymap."
  (interactive)
  (if (eobp)
      (if special-binding
	  (call-interactively special-binding)
	(fi::subprocess-reprocess-keys fi:subprocess-super-key-map))
    (fi::subprocess-reprocess-keys global-map)))

(defun fi::subprocess-reprocess-keys (&optional map key)
  "Reprocess KEY or the last key sequence (which may be incomplete) in MAP.
This is used to reprocess a key sequence as if it were seen in another
context, e.g. to process global bindings of keys from a subprocess
buffer (in fi:shell-mode or fi:inferior-lisp-mode) when some keys are hit
other than at the end of the buffer."
  (if (null map) (setq map global-map))
  (let* ((last-key (if key
		       (if (integerp key)
			   (char-to-string key)
			 key)
		     (this-command-keys)))
	 (last-binding (lookup-key map last-key)))
    (while (keymapp last-binding)
      (setq last-binding
	(lookup-key last-binding
		    (setq last-key (char-to-string (read-char))))))
    (if (commandp last-binding)
	(call-interactively last-binding)
      (ding))))

(defun fi:subprocess-beginning-of-line (arg)
  "Moves point to beginning of line, just like (beginning-of-line),
except that if the pattern at the beginning of the line matches the
current subprocess prompt pattern, this function skips over it."
  (interactive "P")
  (beginning-of-line arg)
  (if (looking-at subprocess-prompt-pattern)
      (re-search-forward subprocess-prompt-pattern nil t)))

(defun fi:subprocess-backward-kill-word (words)
  "Kill previous word in current subprocess input line.  This function
takes care not to delete past most recent subprocess output."
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (marker-position (process-mark (get-buffer-process (current-buffer))))
     (point))
    (backward-kill-word words)))

(defun fi:subprocess-send-input ()
  "Send input to the subprocess.  At end of buffer, sends all text after
last output as input to the subshell, including a newline inserted at the
end. Not at end, copies current line to the end of the buffer and sends it,
after first attempting to discard any prompt at the beginning of the line
by matching the regexp that is the value of subprocess-prompt-pattern if
possible.  This regexp should start with \"^\"."
  (interactive)
  (if fi::shell-completions-window (fi::shell-completion-cleanup))
  (end-of-line)
  (if (eobp)
      (progn
	(move-marker fi::last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(move-marker fi::last-input-end (point)))
    (let ((max (point)))
      (beginning-of-line)
      (re-search-forward subprocess-prompt-pattern max t))
    (let ((copy (buffer-substring (point)
				  (progn (forward-line 1) (point)))))
      (goto-char (point-max))
      (move-marker fi::last-input-start (point))
      (insert copy)
      (move-marker fi::last-input-end (point))))
  (fi::subprocess-watch-for-special-commands)
  (let ((process (get-buffer-process (current-buffer))))
    (fi::send-region-split process fi::last-input-start fi::last-input-end
			   fi:subprocess-map-nl-to-cr)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:subprocess-send-eof ()
  "Send an end of file to the subprocess."
  (interactive)
  (process-send-eof))

(defun fi:subprocess-kill-output ()
  "Kill all output from the subprocess since the last input."
  (interactive)
  (goto-char (point-max))
  (kill-region fi::last-input-end (point))
  (insert "[output flushed]\n")
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))

(defun fi:subprocess-send-flush ()
  "Send the `flush output' character (^O) to subprocess."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:subprocess-show-output ()
  "Display the start of this batch of shell output at top of window.
Also move the point there."
  (interactive)
  (set-window-start (selected-window) fi::last-input-end)
  (goto-char fi::last-input-end))

(defun fi:subprocess-interrupt ()
  "Interrupt the current subprocess."
  (interactive)
  (interrupt-process nil t))

(defun fi:subprocess-kill ()
  "Send a `kill' (SIGKILL) signal to the current subprocess."
  (interactive)
  (kill-process nil t))

(defun fi:subprocess-quit ()
  "Send a quit signal to the subprocess."
  (interactive)
  (quit-process nil t))

(defun fi:subprocess-suspend ()
  "Suspend, with a SIGSTOP, the current subprocess."
  (interactive)
  (stop-process nil t))

(defun fi:subprocess-kill-input ()
  "Kill all input since the last output by the subprocess."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))
