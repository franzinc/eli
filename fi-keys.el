;;
;; copyright (C) 1987, 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-keys.el,v 1.50 1991/04/20 23:24:28 layer Exp $

(defvar fi:subprocess-super-key-map nil
  "Used by fi:subprocess-superkey as the place where super key bindings are
kept.  Buffer local.")

(make-variable-buffer-local 'fi:subprocess-super-key-map)


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
  (define-key map "\C-y" 'fi:pop-input)	; for compatibility with shell-mode
  (cond ((memq mode '(sub-lisp tcp-lisp))
	 (define-key map "." 'fi:lisp-sync-current-working-directory))
	(t
	 (define-key map "." 'fi:shell-sync-current-working-directory)))

  (cond
   ((eq mode 'rlogin)
    (define-key map "\C-z"	'fi:rlogin-send-stop)
    (define-key map "\C-c"	'fi:rlogin-send-interrupt)
    (define-key map "\C-d"	'fi:rlogin-send-eof)
    (define-key map "\C-\\"	'fi:rlogin-send-quit))
   ((memq mode '(sub-lisp shell))
    (if (eq mode 'shell)
	(define-key map "\C-z"	'fi:subprocess-suspend))
    (define-key map "\C-c"	'fi:subprocess-interrupt)
    (if fi::lisp-is-remote
	(define-key map "\C-d"	'fi:remote-lisp-send-eof)
      (define-key map "\C-d"	'fi:subprocess-send-eof))
    (define-key map "\C-\\"	'fi:subprocess-quit))
   ((eq mode 'tcp-lisp)
    (define-key map "\C-c"	'fi:tcp-lisp-listener-interrupt-process)
    (define-key map "\C-d"	'fi:tcp-lisp-listener-send-eof)
    (define-key map "\C-\\"	'fi:tcp-lisp-listener-kill-process)))
  
  (if (memq mode '(sub-lisp tcp-lisp))
      (progn
	(define-key map "s" 'fi:scan-stack)))

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
  (define-key map "\e" (make-keymap))
  (define-key map "\C-x" (make-keymap))

  (if supermap
      (define-key map "\C-c" supermap)
    ;; editing mode
    (let ((c-map (make-keymap)))
      (define-key map "\C-c" c-map)
      (define-key c-map "-"	'fi:log-functional-change)))
  
  (define-key map "\C-c%"	'fi:extract-list)
  (define-key map "\C-c;"	'fi:comment-region)
  (define-key map "\C-c\C-e"	'fi:end-of-defun)
  (define-key map "\C-c]"	'fi:super-paren)
  
  (if fi:lisp-do-indentation
      (progn
	(define-key map "\t"		'fi:lisp-indent-line)
	(define-key map "\e\C-q"	'fi:indent-sexp))
    (progn
      (define-key map "\t"		'lisp-indent-line)
      (define-key map "\e\C-q"		'indent-sexp)))

  (define-key map "\C-?"	'backward-delete-char-untabify)

  (cond
    ((memq mode '(sub-lisp tcp-lisp))
     (define-key map "\r"	'fi:inferior-lisp-newline)
     (define-key map "\e\r"	'fi:inferior-lisp-input-sexp)
     (define-key map "\C-x\r"	'fi:inferior-lisp-input-list))
    (t (define-key map "\r"	'fi:lisp-mode-newline)))

  (cond
    ((memq major-mode '(fi:common-lisp-mode fi:inferior-common-lisp-mode
			fi:lisp-listener-mode))
     (define-key map "\C-c?"	'fi:lisp-apropos)
     (define-key map "\e."	'fi:lisp-find-tag)
     (define-key map "\e,"	'fi:lisp-tags-loop-continue)
     (define-key map "\e\t"	'fi:lisp-complete-symbol)
     (define-key map "\eA"	'fi:lisp-arglist)
     (define-key map "\eC"	'fi:list-who-calls)
     (define-key map "\eD"	'fi:describe-symbol)
     (define-key map "\eF"	'fi:lisp-function-documentation)
     (define-key map "\eM"	'fi:lisp-macroexpand)
     (define-key map "\eT"	'fi:toggle-trace-definition)
     (define-key map "\eW"	'fi:lisp-macroexpand-recursively)
     (let ((4-map (make-keymap)))
       (define-key map "\C-x4" 4-map)
       (define-key 4-map "." 	'fi:lisp-find-tag-other-window))))
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

(defun fi::lisp-listener-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'tcp-lisp)
			  supermap
			  'tcp-lisp))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'sub-lisp)
			  supermap
			  'sub-lisp))

;;;;;;;;;;;;;;;;;;;;; inferior lisp mode related functions

(defun fi:lisp-mode-newline ()
  "Insert a newline at the point.  Users wishing to have newline do
indentation, can redefine this function to indent, newline and indent."
  (interactive)
  (newline))

(defun fi:inferior-lisp-newline ()
  "When at the end of the buffer, insert a newline into a Lisp subprocess
buffer, and if a complete form has been entered, send the input to the Lisp
subprocess.  This allows partially complete, multi-line expressions to be
edited before Lisp sees them.

If not at the end of the buffer, this function does its best to find a
complete form around the point, copy it to the end of the buffer, and send
it to the Lisp subprocess."
  (interactive)
  (if (eobp)
      (let ((start (marker-position
		    (process-mark (get-buffer-process (current-buffer)))))
	    (send-that-sexp t))
	(goto-char start)
	(while (and (not (eobp))
		    (condition-case ()
			(progn (forward-sexp 1) t)
		      (error (setq send-that-sexp nil))))
	  (while (looking-at ")")
	    ;; Can either signal an error or delete them silently.  Hmm,
	    ;; for now we'll signal the error:
	    ;;(delete-char 1)
	    (error "too many )'s")
	    ))
	(end-of-buffer)
	(if send-that-sexp
	    (fi:subprocess-send-input)
	  (progn
	    (newline)
	    (funcall indent-line-function))))

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
  "Send the region defined by the point and mark to the Lisp subprocess.
When called from a program give START and END buffer positions."
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
    (send-region process start end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:inferior-lisp-input-sexp (&optional arg)
  "Send the s-expression after the point to the Lisp subprocess.
With prefix arg, ARG, send that many s-expressions."
  (interactive "p")
  (fi:inferior-lisp-send-input arg 'sexp))

(defun fi:inferior-lisp-input-list (&optional arg)
  "Send the list before the point to the Lisp subprocess.
With prefix arg, ARG, send that many lists."
  (interactive "p")
  (fi:inferior-lisp-send-input arg 'lists))

(defun fi:inferior-lisp-send-input (arg type)
  "Send ARG, which is an s-expression, to the Lisp subprocess. TYPE
must be either 'sexps or 'lists, specifying whether lists or
s-expressions should be parsed (internally, either `(scan-sexps)' or
`(scan-lists)' is used). If at the end of buffer, everything typed since
the last output from the Lisp subprocess is collected and sent to the Lisp
subprocess.  With an argument, only the specified number of s-expressions
or lists from the end of the buffer are sent. If in the middle of the
buffer, the current s-expression(s) or list(s) is(are) copied to the end of
the buffer and then sent. An argument specifies the number of s-expressions
or lists to be sent. If s-expressions are being parsed,the cursor
follows a closing parenthesis, the preceding s-expression(s) is(are)
processed.  If the cursor is at an opening parenthesis, the following
s-expression(s) is(are) processed.  If the cursor is at a closing
parenthesis, the preceding s-expression(s) is(are) processed.  Otherwise,
the enclosing s-expression(s) is(are) processed.  If lists are being
parsed, the enclosing list is processed."
  (if (and (eobp) (null arg))
      (progn
	(move-marker fi::last-input-start
		     (process-mark (get-buffer-process (current-buffer))))
	(insert "\n")
	(funcall indent-line-function)
	(move-marker fi::last-input-end (point)))

    ;; we are in the middle of the buffer somewhere and need to collect
    ;; and s-exp to re-send
    ;; we grab everything from the end of the current line back to the end
    ;; of the last prompt
    ;;
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
		   ((= (preceding-char) ?\)) (scan-sexps (point) (- arg)))
		   ((= (following-char) ?\() (point))
		   ((= (following-char) ?\))
		    (forward-char 1) (scan-sexps (point) (- arg)))
		   ((not (memq (char-syntax (preceding-char)) '(?w ?_)))
		    (point))
		   (t (scan-sexps (point) (- arg))))))
	     (setq end-resend
	       (save-excursion
		 (cond
		   ((= (preceding-char) ?\)) (point))
		   ((= (following-char) ?\() (scan-sexps (point) arg))
		   ((= (following-char) ?\)) (forward-char 1) (point))
		   ((not (memq (char-syntax (following-char)) '(?w ?_)))
		    (point))
		   (t (scan-sexps (point) arg)))))))
	(setq exp-to-resend
	  (buffer-substring
	   (setq start-resend (scan-lists (point) (- arg) 1))
	   (setq end-resend (scan-lists (point) arg 1)))))
      (if (eobp)
	  (progn
	    (insert "\n")
	    (funcall indent-line-function)
	    (move-marker fi::last-input-start start-resend)
	    (move-marker fi::last-input-end (point-max)))
	(progn
	  (goto-char (point-max))
	  (move-marker fi::last-input-start (point))
	  (insert exp-to-resend)
	  (if (not (bolp)) (insert "\n"))
	  (move-marker fi::last-input-end (point))))))
  (let ((process (get-buffer-process (current-buffer))))
    (send-region process fi::last-input-start fi::last-input-end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

;;;;;;;;;;;;;;;;;;;;; TCP lisp mode related functions

(defun fi::get-symbol-at-point (&optional up-p packagify)
    (let ((symbol (condition-case ()
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
		    (error nil))))
      (if (and fi:package packagify (not (string-match ":?:" symbol nil)))
	  (format "%s::%s" fi:package symbol)
	symbol)))

;; This is a pretty bad hack but it appears that within completing-read
;; fi:package has the wrong value so we bind this variable to get around
;; the problem.
(defvar fi::original-package nil)

(defun fi::get-default-symbol (prompt &optional up-p use-package)
  (let* ((symbol-at-point
	  (fi::get-symbol-at-point up-p))
	 (read-symbol
	  (let ((fi::original-package fi:package))
	  (completing-read
	   (if symbol-at-point
	       (format "%s: (default %s) " prompt symbol-at-point)
	     (format "%s: " prompt))
	   'fi::minibuffer-complete
	   )))
	 (symbol (if (string= read-symbol "")
		     symbol-at-point
		   read-symbol))
	 (colonp (string-match ":?:" symbol nil)))
    (if (and (not colonp) use-package fi:package)
	(setq symbol (format "%s::%s" fi:package symbol)))
    (list symbol)))

(defun fi::minibuffer-complete (pattern predicate what)
  (let ((fi:package fi::original-package))
    (let (package deletion)
      (if (string-match ":?:" pattern)
	  (setq package
	    (concat
	     ":" (substring pattern 0 (match-beginning 0)))
	    deletion (substring pattern 0 (match-end 0))
	    pattern
	    (substring pattern (match-end 0))))
      (let* ((alist
	      (fi::lisp-complete-1 pattern package nil))
	     (completion (and alist (try-completion pattern alist))))
	(ecase what
	  ((nil)
	   (if (eq completion t)
	       t
	     (concat deletion completion)))
	  ((t)
	   (mapcar (function cdr) alist))
	  (lambda (not (not alist))))))))
	      
      


(defun fi:remote-lisp-send-eof ()
  "Simulate sending an EOF to a Lisp subprocess that was started on a
remote machine (with respect to the machine on which emacs is running).
The remote process was most likely started with `rsh', and sending an EOF
to a remote process started in this way closes down the pipe.  The fake EOF
is done by doing a debugger:debug-pop on the \"Initial Lisp Listener\"
process via the backdoor."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"Initial Lisp Listener\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-listener-send-eof ()
  "Simulate sending an EOF on the Lisp Listener pseudo-process.  It is not
a real process because it is a network connection to the Common Lisp
UNIX process, and EOF has no meaning (out-of-band data is not handled in
either Emacs or Common Lisp, at this time).  The fake EOF is simulated by
doing a debugger:debug-pop on the Common Lisp process tied to the Lisp
Listener buffer via the backdoor."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-listener-kill-process ()
  "Simulate sending a SIGQUIT to the Lisp Listener pseudo-process,
meaning kill the Common Lisp thread associated with the Lisp Listener
buffer.  It is not a real process because it is a network connection to the
Common Lisp UNIX process, and since there is no tty control there is no
character which is interpreted as `quit'.  The fake `quit' is simulated by
doing a mp:process-kill on the Common Lisp process tied to the Lisp
Listener buffer via the backdoor."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-kill (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-listener-interrupt-process ()
  "Simulate sending a SIGINT to the Lisp Listener pseudo-process,
meaning interrupt the Common Lisp thread associated with the Lisp Listener
buffer.  It is not a real process because it is a network connection to the
Common Lisp UNIX process, and since there is no tty control there is no
character which is interpreted as `interrupt'.  The fake `interrupt' is
simulated by doing a mp:process-interrupt on the Common Lisp process tied
to the Lisp Listener buffer via the backdoor."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-interrupt
      (mp::process-name-to-process \"%s\")
      #'break \"interrupt from emacs\")\n"
   (buffer-name (current-buffer))))


;;;;;;;;;;;;;;;;;;;;; general subprocess related functions

(defun fi:subprocess-superkey ()
  "This function implements superkeys in subprocess buffers.  Any key which
is bound to this function is, by definition, a superkey.  A superkey is
treated specially when at the end of a subprocess buffer, but has its
normal, global, binding when used elsewhere in the buffer.
The key takes its binding from the fi:subprocess-super-key-map keymap,
which is a buffer local variable."
  (interactive)
  (if (eobp)
      (fi::subprocess-reprocess-keys fi:subprocess-super-key-map)
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
  "Moves point to beginning of line, just like
	(beginning-of-line arg),
except that if the pattern at the beginning of the line matches the
current subprocess prompt pattern, this function skips over it.
With argument ARG non nil or 1, move forward ARG - 1 lines first."
  (interactive "p")
  (beginning-of-line arg)
  (if (looking-at subprocess-prompt-pattern)
      (re-search-forward subprocess-prompt-pattern nil t)))

(defun fi:subprocess-backward-kill-word (arg)
  "Kill previous word in current subprocess input line.  This function
takes care not to delete past the beginning of the the current input.
With argument ARG, kill that many words."
  (interactive "p")
  (save-restriction
    (narrow-to-region
     (marker-position (process-mark (get-buffer-process (current-buffer))))
     (point))
    (backward-kill-word arg)))

(defun fi:subprocess-send-input ()
  "Send input to the subprocess.  At end of buffer, send all text after
last output as input to the subshell, including a newline inserted at the
end.  When not at end, copy current line to the end of the buffer and
send it,after first attempting to discard any prompt at the beginning of
the line by matching the regexp that is the value of
the buffer-local subprocess-prompt-pattern, which is initialized by each
subprocess mode."
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
    (send-region process fi::last-input-start fi::last-input-end)
    (fi::input-ring-save fi::last-input-start (1- fi::last-input-end))
    (set-marker (process-mark process) (point))))

(defun fi:subprocess-send-eof ()
  "Send an EOF (end of file) to the current subprocess."
  (interactive)
  (process-send-eof))

(defun fi:rlogin-send-eof ()
  "Send an EOF to the process running as in the remote shell in the current
subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-d"))

(defun fi:subprocess-kill-output ()
  "Kill all output from the subprocess since the last input.  This is a
convenient way to delete the output from the last command."
  (interactive)
  (goto-char (point-max))
  (kill-region fi::last-input-end (point))
  (insert "[output flushed]\n")
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))

(defun fi:subprocess-send-flush ()
  "Send the `flush output' character (^O) to current subprocess."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:subprocess-show-output ()
  "Display the start of this batch of shell output at top of window.
Also move the point there."
  (interactive)
  (set-window-start (selected-window) fi::last-input-end)
  (goto-char fi::last-input-end))

(defun fi:subprocess-interrupt ()
  "Send a kill (SIGINT) signal to the current subprocess."
  (interactive)
  (interrupt-process nil t))

(defun fi:rlogin-send-interrupt ()
  "Send an interrupt (SIGINT) to the process running as in the current
subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-c"))

(defun fi:subprocess-kill ()
  "Send a kill (SIGKILL) signal to the current subprocess."
  (interactive)
  (kill-process nil t))

(defun fi:subprocess-quit ()
  "Send a quit (SIGQUIT) signal to the current subprocess."
  (interactive)
  (quit-process nil t))

(defun fi:rlogin-send-quit ()
  "Send a quit (SIGQUIT) to the process running as in the current
subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-\\"))

(defun fi:subprocess-suspend ()
  "Suspend the current subprocess."
  (interactive)
  (stop-process nil t))

(defun fi:rlogin-send-stop ()
  "Send a stop (SIGSTOP) signal to the process running as in the current
subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun fi:subprocess-kill-input ()
  "Kill all input since the last output by the current subprocess."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

(defun fi:lisp-sync-current-working-directory ()
  "Sychronize the current working directory in Emacs and Lisp, by making
Lisp conform to the value of default-directory."
  (interactive)
  (send-string
   (get-buffer-process (current-buffer))
   (format "(excl::chdir \"%s\")\n" default-directory)))

(defun fi:shell-sync-current-working-directory ()
  "Sychronize the current working directory in Emacs and the shell, by
making the shell conform to the value of default-directory."
  (interactive)
  (send-string
   (get-buffer-process (current-buffer))
   (format "cd %s\n" default-directory)))

(defun fi:log-functional-change ()
  "Indicate that a function has changed by putting in a descriptive message
at the head of the function."
  (interactive)
  (let* ((case-fold-search t)
	 (bof
	  (cond ((string-match "lisp" mode-name)
		 'fi:beginning-of-defun)
		((string= mode-name "C")
		 '(lambda () (re-search-backward "^{" nil t)))
		(t (error "can't handle this mode: %s" mode-name)))))
    (if (not (funcall bof)) (error "no function to annotate")))
  (end-of-line)
  (newline)
  (funcall indent-line-function)
  (insert-string (concat (format "%s- " (fi::log-comment-start))
			 (current-time-string) " by " 
			 (user-login-name) " - "))
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (insert (format "%s\n" comment-end))
    (forward-line -1)
    (funcall indent-line-function)))

(defun fi::log-comment-start ()
  (if (and (boundp 'fi:lisp-comment-indent-specification)
	   fi:lisp-comment-indent-specification)
      (let ((list fi:lisp-comment-indent-specification)
	    (done nil)
	    (res ""))
	(while (and list (not done))
	  (if (eq t (car list)) (setq done t))
	  (setq res (concat res comment-start))
	  (setq list (cdr list)))
	res)
    comment-start))

(defun fi:beginning-of-defun (&optional arg)
  "Move the point to the start of the current top-level form.
With argument ARG, do it that many times.  For when called from a program,
return t, unless beginning of the buffer is reached before finding the
target."
  (interactive "p")
  (or (looking-at "^\\s(")
      (if fi:subprocess-mode
	  (goto-char (process-mark (get-buffer-process (current-buffer))))
	(progn
	  (and arg (< arg 0) (forward-char 1))
	  (and (re-search-backward "^\\s(" nil 'move (or arg 1))
	       (progn (beginning-of-line) t))))))

(defun fi:end-of-defun ()
  "Put the point at the end of the current top-level form."
  (interactive)
  (if fi:subprocess-mode
      (goto-char (point-max))
    (when (fi:beginning-of-defun 1)
      (forward-sexp 1))))

(defun fi:super-paren ()
  "Insert a sufficient number of parenthesis to complete the enclosing
form.  If there are too many parens delete them.  The form is also indented."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (save-excursion (fi:beginning-of-defun) (point)))
    (let (p)
      (while (progn (setq p (point))
		    (fi:beginning-of-defun)
		    (condition-case nil (progn (forward-sexp 1) nil)
		      (error t)))
	(goto-char p)
	(insert ")"))
      (unless (eq p (point)) (delete-region p (point)))))
  (fi:beginning-of-defun)
  (indent-sexp)
  (forward-sexp 1))

(defun fi:find-unbalanced-parenthesis ()
  "Verify that parentheses in the current Lisp source buffer are balanced.
If they are not, position the point at the first syntax error found."
  (interactive)
  (let ((started-here (point)))
    (goto-char (point-min))
    (while (re-search-forward "^(" nil t)
      (backward-char 1)
      (forward-sexp 1)
      (if (looking-at ")") (error "Extra `)'")))
    (goto-char started-here)
    (message "All parentheses appear to be balanced.")))

(defun fi:fill-paragraph (arg)
  "Properly fill paragraphs of Lisp comments by inserting the appropriate
semicolons at the beginning of lines.  With prefix argument, ARG, justify
the paragraph as well."
  (interactive "P")
  (save-excursion
    (beginning-of-line 0)
    (if (re-search-forward "\\(^[ \t]*[;]+ \\)" nil t)
	(let ((fill-prefix (buffer-substring (match-beginning 1)
					     (match-end 1))))
	  (fill-paragraph arg))
      (fill-paragraph arg))))

(defun fi:extract-list (arg)
  "Take the list after the point and and remove the surrounding list.  With
argument ARG do it that many times." 
  (interactive "p")
  (let ((string (progn
		  (mark-sexp 1)
		  (buffer-substring (point) (mark)))))
    (backward-up-list (or arg 1))
    (mark-sexp 1)
    (delete-region (point) (mark))
    (insert string)
    (backward-sexp 1)))

(defun fi:comment-region (start end &optional arg)
  "Comment all lines in the current region.  With prefix arg, uncomment the
region (it should have been commented with this function).
When calling from a program, arguments are START and END, both buffer
positions, and ARG."
  (interactive "r\nP")
  (save-excursion
    (let ((start (progn (goto-char (max start (point-min)))
			(skip-chars-forward " \t\n") ;skip blank lines
			(beginning-of-line)
			(point)))
	  (end (progn (goto-char (min end (point-max)))
		      (if (or (bolp)
			      (looking-at "[ \t]*$"))
			  (skip-chars-backward " \t\n")) ;skip blank lines
		      (end-of-line)
		      (point))))
      (goto-char end)
      (if (null arg)
	  ;;Comment Region
	  (if (string-equal comment-end "")
	      ;;When no comment-end exists, put a comment-start
	      ;;at the start of each line.
	      (while (and (>= (point) start)
			  (progn (beginning-of-line)
				 (insert-string comment-start)
				 (= (forward-line -1) 0))))
	    ;;When comment-end exists, put comment marks only at the
	    ;;beginning and end of the region, and put a comment-start after
	    ;;each comment-end in the region.
	    (insert-string comment-end)
	    (goto-char end)
	    (while (search-backward comment-end start 'move)
	      (replace-match (concat comment-end comment-start))
	      (goto-char (match-beginning 0)))
	    (insert-string comment-start))
	;;Uncomment Region
	(if (string-equal comment-end "")
	    (while (and (>= (point) start)
			(progn (beginning-of-line)
			       (if (looking-at (regexp-quote comment-start))
				   (replace-match ""))
			       (= (forward-line -1) 0))))
	  (backward-char (length comment-end))
	  (if (looking-at (regexp-quote comment-end))
	      (replace-match ""))
	  (while (re-search-backward (regexp-quote
				      (concat comment-end comment-start))
				     start 'move)
	    (replace-match comment-end))
	  (if (looking-at (regexp-quote comment-start))
	      (replace-match "")))))))
