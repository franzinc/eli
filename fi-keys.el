;;
;; copyright (C) 1987, 1988 Franz Inc, Berkeley, Ca.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and stored only in accordance with the terms of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure by the Government are subject to
;; restrictions of Restricted Rights for Commercial Software developed
;; at private expense as specified in DOD FAR 52.227-7013 (c) (1) (ii).
;;
;; This file may be distributed without further permission from
;; Franz Inc. as long as
;;
;;	* it is not part of a product for sale,
;;	* no charge is made for the distribution, and
;;	* all copyright notices and this notice are preserved.
;;
;; If you have any comments or questions on this package, please feel
;; free to contact Franz Inc. at
;;
;;	Franz Inc.
;;	Attn: Emacs Group Manager
;;	1995 University Ave
;;	Suite 275
;;	Berkeley, CA 94704
;; or
;;	emacs-info%franz.uucp@Berkeley.EDU
;;	ucbvax!franz!emacs-info

;; $Header: /repo/cvs.copy/eli/fi-keys.el,v 1.4 1988/05/12 10:41:31 layer Exp $

;;;;
;;; Key defs
;;;;

(defun fi::subprocess-mode-super-keys (map mode)
  "Setup keys in MAP as a subprocess super-key map.  MODE is either
shell, rlogin, sub-lisp or tcp-lisp."
  (define-key map "\C-a" 'fi:subprocess-beginning-of-line)
  (define-key map "\C-k" 'fi:subprocess-kill-output)
  (define-key map "\C-l" 'fi:list-input-ring)
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
  (if (not (eq 'rlogin mode))
      (define-key map "\C-i" 'fi:shell-file-name-completion))
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
  
  (define-key map "\e\C-q"	'indent-sexp)
  (define-key map "\C-?"	'backward-delete-char-untabify)
  
  (cond
    ((memq mode '(sub-lisp tcp-lisp))
     (define-key map "\r"	'fi:inferior-lisp-newline)
     (define-key map "\e\r"	'fi:inferior-lisp-send-sexp-input)
     (define-key map "\C-x\r"	'fi:inferior-lisp-send-list-input))
    (t (define-key map "\r"	'fi:lisp-reindent-newline-indent)))

  (cond
    ((memq major-mode '(fi:common-lisp-mode fi:inferior-common-lisp-mode
			fi:tcp-common-lisp-mode))
     (define-key map "\e."	'fi:lisp-find-tag)
     (define-key map "\e,"	'fi:lisp-tags-loop-continue)
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
  "Bound to newline in an inferior lisp buffer.  At the end of the buffer
it inserts a newline and performs automatic
indentation.  Whole expressions are sent to Lisp (and not each piece after
each newline is typed).  This allows previously types lines to be edited
before Lisp sees the input.  Typed anywhere else in the buffer, this
functions causes the current line, minus the prompt, to be sent to the Lisp
process, or listener when using TCP/IP-style communication."
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
		  (funcall indent-line-function)
		  )))
	  ;; a non-list s-exp, so just send it off...
	  (fi:subprocess-send-input)))
    (fi:subprocess-send-input)))

(defun fi:inferior-lisp-send-sexp-input (arg)
  "Send s-expression(s) to the Lisp subprocess."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'sexp))

(defun fi:inferior-lisp-send-list-input (arg)
  "Send list(s) to the Lisp subprocess."
  (interactive "P")
  (fi:inferior-lisp-send-input arg 'lists))

(defun fi:lisp-eval-last-sexp (compile-file-p)
  "Send the sexp before the point to the Lisp subprocess.
If a Lisp subprocess has not been started, then one is started.  With a
prefix argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (let ((start (save-excursion
		 (forward-sexp -1)
		 (point))))
    (fi::eval-send start (point) compile-file-p)))

(defun fi:lisp-eval-defun (compile-file-p)
  "Send the current top-level (or nearest previous) form to the Lisp
subprocess.  A `top-level'form is one that starts in column 1.  If a Lisp
subprocess has not been started, then one is started.  With a prefix
argument, the source sent to the subprocess is compiled."
  (interactive "P")
  (let* ((end (save-excursion (end-of-defun) (point)))
	 (start (save-excursion
		  (beginning-of-defun)
		  (point))))
    (fi::eval-send start end compile-file-p)))

(defun fi:lisp-eval-region (compile-file-p)
  "Send the text in the region to the Lisp subprocess, one expression at a
time if there is more than one complete expression.  If a Lisp subprocess
has not been started, then one is started.  With a prefix argument, the
source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-send (min (point) (mark))
		 (max (point) (mark))
		 compile-file-p))

(defun fi:lisp-eval-current-buffer (compile-file-p)
  "Send the entire buffer to the Lisp subprocess.  If a Lisp subprocess has
not been started, then one is started.  With a prefix argument, the
source sent to the subprocess is compiled."
  (interactive "P")
  (fi::eval-send (point-min) (point-max) compile-file-p))


;;;;;;;;;;;;;;;;;;;;; TCP lisp mode related functions

(defun fi::get-default-symbol (prompt &optional up-p)
  (let* ((sdefault
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
	      nil)))
	 (spec (read-string
		(if sdefault
		    (format "%s: (default %s) " prompt sdefault)
		  (format "%s: " prompt)))))
    (list 
     (fi::add-package-info
      (if (equal spec "")
	  sdefault
	spec)))))

(defun fi:lisp-arglist (symbol)
  "Print the arglist (using excl:arglist) for a symbol, which is read from
the minibuffer.  The word around the point is used as the default."
  (interactive (fi::get-default-symbol "Function" t))
  (process-send-string
   (fi::background-sublisp-process)
   (format
    "(progn
      (format t  \"~:[()~;~:*~{~a~^ ~}~]\"
       (cond
	((macro-function '%s) '(\"%s is a macro\"))
	((special-form-p '%s) '(\"%s is a special form\"))
	((not (fboundp '%s)) '(\"%s has no function binding\"))
	(t (excl::arglist '%s))))
      (values))\n"
    symbol symbol symbol symbol symbol symbol symbol)))

(defun fi:lisp-describe (symbol)
  "Describe a symbol, which is read from the minibuffer.  The word around
the point is used as the default."
  (interactive (fi::get-default-symbol "Describe symbol"))
  (process-send-string
   (fi::background-sublisp-process)
   (format "(progn (lisp:describe '%s) (values))\n" symbol)))

(defun fi:lisp-function-documentation (symbol)
  "Print the function documentation for a symbol, which is read from the
minibuffer.  The word around the point is used as the default." 
  (interactive
   (fi::get-default-symbol "Function documentation for symbol"))
  (process-send-string
   (fi::background-sublisp-process)
   (format "(princ (lisp:documentation '%s 'lisp:function))\n" symbol)))

(defun fi:lisp-who-calls (&optional symbol)
  "Asks the sublisp which functions reference a symbol."
  (interactive (fi::get-default-symbol "Find references to symbol"))
  ;; Since this takes a while, tell the user that it has started.
  (message "searching...")		; Find some way to flush when done...
  (process-send-string
   (fi::background-sublisp-process)
   (format "(progn (excl::who-references '%s) (values))\n" symbol)))

(defun fi:lisp-macroexpand ()
  "Print the macroexpansion of the form at the point."
  (interactive)
  (fi::lisp-macroexpand-common "lisp:macroexpand"))

(defun fi:lisp-walk (arg)
  "Print the full macroexpansion the form at the point.
With a prefix argument, macroexpand the code as the compiler would."
  (interactive "P")
  (fi::lisp-macroexpand-common
   (if arg "excl::compiler-walk" "excl::walk")))

(defun fi:lisp-find-tag (symbol &optional next)
  "Find the Common Lisp source for a symbol, using the characters around
the point as the default tag."
  (interactive (if current-prefix-arg
		   '(nil t)
		 (fi::get-default-symbol "Source for symbol")))
  (if next
      (fi:lisp-tags-loop-continue)
    (let ((s (fi::add-package-info symbol)))
      (condition-case ()
	  (process-send-string
	   (fi::background-sublisp-process)
	   (format "(format t \"\2~s\" (cons '%s (source-file '%s t)))\n"
		   s s))
	(error;; the backdoor-lisp-listener is not listening...
	 (setq fi::tag-state nil)
	 (fi::lisp-find-etag (symbol-name symbol)))))))

(defun fi:lisp-tags-loop-continue ()
  "Find the next occurrence of the tag last used by fi:lisp-find-tag."
  (interactive)
  (if (and fi::tag-state (cdr fi::tag-state))
      (let ((symbol (car fi::tag-state))
	    (info (cdr (cdr fi::tag-state))))
	(cond (info
	       (setq fi::tag-state (cons symbol info))
	       (fi::find-source-from-lisp-info symbol info t))
	      (t (setq fi::tag-state (cons symbol nil))
		 (if (y-or-n-p
		      "no more source info from lisp, use tags file? ")
		     (progn
		       ;; the following is a crock, but it works
		       (setq last-tag (symbol-name symbol))
		       (find-tag nil t))))))
    (if fi::tag-state
	(setq last-tag (symbol-name (car fi::tag-state))))
    (find-tag nil t)))

(defun fi:tcp-lisp-send-eof ()
  "Do a db:debug-pop on the TCP listener."
  (interactive)
  (fi:backdoor-eval 
   "(db:debug-pop (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-kill-process ()
  "Kill a tcp-lisp process via the backdoor lisp listener: a
mp:process-kill is sent to the lisp."
  (interactive)
  (fi:backdoor-eval 
   "(mp:process-kill (mp::process-name-to-process \"%s\"))\n"
   (buffer-name (current-buffer))))

(defun fi:tcp-lisp-interrupt-process ()
  "Interrupt the tcp-lisp process via a mp:process-interrupt spoken to the
backdoor lisp listener."
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
  "Move to beginning of line, skipping over initial prompt.
Moves point to beginning of line, just like (beginning-of-line),
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
  (fi::subprocess-hack-directory)
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
  "Kill all output from the subprocess since last input."
  (interactive)
  (goto-char (point-max))
  (kill-region fi::last-input-end (point))
  (insert "[output flushed]\n")
  (set-marker (process-mark (get-buffer-process (current-buffer))) (point)))

(defun fi:subprocess-send-flush ()
  "Send `flush output' character (^O) to subprocess."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-o"))

(defun fi:subprocess-show-output ()
  "Display start of this batch of shell output at top of window.
Also move the point there."
  (interactive)
  (set-window-start (selected-window) fi::last-input-end)
  (goto-char fi::last-input-end))

(defun fi:subprocess-interrupt ()
  "Interrupt the subprocess."
  (interactive)
  (interrupt-process nil t))

(defun fi:subprocess-kill ()
  "Send a `kill' signal to the subprocess in the current buffer."
  (interactive)
  (kill-process nil t))

(defun fi:subprocess-quit ()
  "Send a quit signal to the subprocess."
  (interactive)
  (quit-process nil t))

(defun fi:subprocess-suspend ()
  "Send a ^Z to the subprocess."
  (interactive)
  (stop-process nil t))

(defun fi:subprocess-kill-input ()
  "Kill all input since last stuff output by the subprocess."
  (interactive)
  (kill-region (process-mark (get-buffer-process (current-buffer)))
	       (point)))
