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

;; $Header: /repo/cvs.copy/eli/fi-keys.el,v 1.40 1991/01/31 22:05:30 layer Exp $

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

  (if supermap
      (define-key map "\C-c" supermap)
    ;; editing mode
    (let ((c-map (make-sparse-keymap)))
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
			fi:tcp-common-lisp-mode))
     (define-key map "\e."	'fi:lisp-find-tag)
     (define-key map "\e,"	'fi:lisp-tags-loop-continue)
     (define-key map "\e\t"	'fi:lisp-complete-symbol)
     (define-key map "\eA"	'fi:lisp-arglist)
     (define-key map "\eC"	'fi:lisp-who-calls)
     (define-key map "\eD"	'fi:lisp-describe)
     (define-key map "\eF"	'fi:lisp-function-documentation)
     (define-key map "\eM"	'fi:lisp-macroexpand)
     (define-key map "\eT"	'fi:toggle-trace-definition)
     (define-key map "\eW"	'fi:lisp-walk)
     (let ((4-map (make-sparse-keymap)))
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

(defun fi::tcp-common-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'tcp-lisp)
			  supermap
			  'tcp-lisp))

(defun fi::inferior-lisp-mode-commands (map supermap)
  (fi::lisp-mode-commands (fi::subprocess-mode-commands map supermap 'sub-lisp)
			  supermap
			  'sub-lisp))

;;;;;;;;;;;;;;;;;;;;; inferior lisp mode related functions

(defun fi:lisp-mode-newline ()
  "Function bound to C-m.  The default version just inserts a newline."
  (interactive)
  (newline))

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
	    (send-that-sexp t))
	(save-excursion
	  (goto-char start)
	  (while (and (not (eobp))
		      (condition-case ()
			  (progn (forward-sexp 1) t)
			(error (setq send-that-sexp nil))))
	    (while (looking-at ")")
	      (delete-char 1))))
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

(defun fi::get-default-symbol (prompt &optional up-p use-package)
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
    (if (and (not colonp) use-package fi:package)
	(setq symbol (format "%s::%s" fi:package symbol)))
    (list symbol)))

(defun fi:remote-lisp-send-eof ()
  "Simulate an EOF on the inferior lisp process via a db:debug-pop spoken
to the backdoor Common Lisp listener."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"Initial Lisp Listener\"))\n"
   (buffer-name (current-buffer))))

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

(defun fi:rlogin-send-eof ()
  "Send eof to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-d"))

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

(defun fi:rlogin-send-interrupt ()
  "Send interrupt to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-c"))

(defun fi:subprocess-kill ()
  "Send a `kill' (SIGKILL) signal to the current subprocess."
  (interactive)
  (kill-process nil t))

(defun fi:subprocess-quit ()
  "Send a quit signal to the subprocess."
  (interactive)
  (quit-process nil t))

(defun fi:rlogin-send-quit ()
  "Send quit to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-\\"))

(defun fi:subprocess-suspend ()
  "Suspend, with a SIGSTOP, the current subprocess."
  (interactive)
  (stop-process nil t))

(defun fi:rlogin-send-stop ()
  "Send stop to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun fi:subprocess-kill-input ()
  "Kill all input since the last output by the subprocess."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))

(defun fi:lisp-sync-current-working-directory ()
  "Make the subprocess Lisp have the same current working directory as
Emacs."
  (interactive)
  (send-string
   (get-buffer-process (current-buffer))
   (format "(excl::chdir \"%s\")\n" default-directory)))

(defun fi:shell-sync-current-working-directory ()
  (interactive)
  (send-string
   (get-buffer-process (current-buffer))
   (format "cd %s\n" default-directory)))

(defun fi:log-functional-change ()
  "Indicate that a function has changed by putting in a description message
at the head of the function."
  (interactive)
  (let* ((case-fold-search t)
	 (bof
	  (cond ((string-match "lisp" mode-name)
		 'beginning-of-defun)
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

(defun fi:beginning-of-defun (&optional n)
  "Move the point to the start of the current top-level form.  With a
prefix argument, do it this many times.  Returns t, unless beginning of
buffer is hit."
  (interactive "p")
  (if fi:subprocess-mode
      (goto-char (process-mark (get-buffer-process (current-buffer))))
    (progn
      (and n (< n 0) (forward-char 1))
      (and (re-search-backward "^\\s(" nil 'move (or n 1))
	   (progn (beginning-of-line) t)))))

(defun fi:end-of-defun ()
  (interactive)
  (if fi:subprocess-mode
      (goto-char (point-max))
    (when (or (looking-at "^\\s(") (beginning-of-defun 1))
      (forward-sexp 1))))

(defun fi:super-paren ()
  "Insert a sufficient number of parenthesis to complete the enclosing
form.  If there are too many parens delete them.  The form is indent, too."
  (interactive)
  (save-restriction
    (narrow-to-region (point) (save-excursion (fi:beginning-of-defun) (point)))
    (let (p)
      (while (progn (setq p (point))
		    (beginning-of-defun)
		    (condition-case nil (progn (forward-sexp 1) nil)
		      (error t)))
	(goto-char p)
	(insert ")"))
      (unless (eq p (point)) (delete-region p (point)))))
  (fi:beginning-of-defun)
  (indent-sexp)
  (forward-sexp 1))

(defun fi:find-unbalanced-parenthesis ()
  "Verifies that parentheses in the current Lisp buffer are balanced."
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
semicolons at the beginning of lines.  Prefix argument means justify
paragraph as well."
  (interactive "P")
  (save-excursion
    (beginning-of-line 0)
    (if (re-search-forward "\\(^[ \t]*[;]+ \\)" nil t)
	(let ((fill-prefix (buffer-substring (match-beginning 1)
					     (match-end 1))))
	  (fill-paragraph arg))
      (fill-paragraph arg))))

(defun fi:extract-list (n)
  "Take the s-expression to which the cursor points and remove the outer N,
defaults to 1, s-expressions."
  (interactive "p")
  (let ((string (progn
		  (mark-sexp 1)
		  (buffer-substring (point) (mark)))))
    (backward-up-list (or n 1))
    (mark-sexp 1)
    (delete-region (point) (mark))
    (insert string)
    (backward-sexp 1)))

(defun fi:comment-region (start end &optional arg)
  "Comment out all lines in the current region.  With arg, 
un-comments region that was commented by comment region.
When calling from a program, the arguments are the START
and END of the region, and the optional ARG."
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
