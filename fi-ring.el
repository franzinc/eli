;;; subprocess-ring.el
;;;   subprocess input ring for subprocess modes
;;;
;;; $Header: /repo/cvs.copy/eli/fi-ring.el,v 1.2 1988/02/19 12:16:58 layer Exp $

;;; This code is very similar to the kill-ring implementation
;;; and implements the subprocess input ring.  Each subprocess buffer
;;; has its own input ring.

(defvar default-input-ring-max 50
  "Default maximum length of input rings.")

(defvar input-ring nil
  "List of previous input to subprocess.")

(defvar input-ring-max default-input-ring-max
  "Maximum length of input ring before oldest elements are thrown away.")

(defvar input-ring-yank-pointer nil
  "The tail of the input ring whose car is the last thing yanked.")

(defvar last-input-search-string ""
  "Last input search string in each subprocess buffer.")

(defvar last-command-was-successful-search nil
  "Switch to indicate that last command was a successful input re-search.")

(defun input-append (string before-p)
  (setq last-command-was-successful-search nil)
  (setcar input-ring
	  (if before-p
	      (concat string (car input-ring))
	      (concat (car input-ring) string))))

(defun input-region (beg end)
  "Delete text between point and mark and save in input ring.
The command yank-input can retrieve it from there.

This is the primitive for programs to kill text into the input ring.
Supply two arguments, character numbers indicating the stretch of text
  to be killed.
If the previous command was also a kill command,
  the text killed this time appends to the text killed last time
  to make one entry in the subprocess input ring."
  (interactive "*r")
  (setq last-command-was-successful-search nil)
  (input-ring-save beg end)
  (delete-region beg end))

(defun input-ring-save-string (string)
  "Save a string on the subprocess input ring."
  (setq input-ring (cons string input-ring))
  (if (> (length input-ring) input-ring-max)
      (setcdr (nthcdr (1- input-ring-max) input-ring) nil))
  (setq input-ring-yank-pointer input-ring))

(defun input-ring-save (beg end)
  "Save the region on the subprocess input ring but don't kill it."
  (interactive "r")
  (setq last-command-was-successful-search nil)
  (if (eq last-command 'input-region)
      (input-append (buffer-substring beg end) (< end beg))
    (setq input-ring (cons (buffer-substring beg end) input-ring))
    (if (> (length input-ring) input-ring-max)
	(setcdr (nthcdr (1- input-ring-max) input-ring) nil)))
  (setq this-command 'input-region)
  (setq input-ring-yank-pointer input-ring))

(defun append-next-input ()
  "Append next kill to input ring.
Causes the following command, if kill into input ring, to append to previous
kill into input ring."
  (interactive)
  (setq last-command-was-successful-search nil)
  (setq this-command 'input-region))

(defun rotate-yank-input-pointer (arg)
  "Rotate the yanking point in the subprocess input ring."
  (interactive "p")
  (setq last-command-was-successful-search nil)
  (let ((ring-length (length input-ring))
	(yank-ring-length (length input-ring-yank-pointer)))
    (cond
     ((zerop ring-length)
      (error "Subprocess input ring is empty."))
     ((< arg 0)
      (setq arg (- ring-length (% (- arg) ring-length)))
      (setq input-ring-yank-pointer
	    (nthcdr (% (+ arg (- ring-length yank-ring-length)) ring-length)
		    input-ring)))
     (t
      (setq input-ring-yank-pointer
	    (nthcdr (% (+ arg (- ring-length yank-ring-length)) ring-length)
		    input-ring))))))

(defun yank-input-pop (arg)
  "Replace just-yanked text from input ring with previous text.
This command is allowed only immediately after a yank-input or
a yank-input-pop.  At such a time, the region contains a stretch of
reinserted previous subprocess input.  Function yank-input-pop deletes
that text and inserts in its place a different stretch of previous
input text.

With no argument, the previous input is inserted.
With argument n, the n'th previous input is inserted.
If n is negative, this is a more recent input.

The sequence of inputs wraps around, so that after the oldest one comes
the newest one."
  (interactive "*p")
  (setq last-command-was-successful-search nil)
  (if (not (memq last-command '(yank-input
				re-search-backward-input
				re-search-forward-input)))
      (error "Previous command was not a yank of input."))
  (setq this-command 'yank-input)
  (let ((before (< (point) (mark))))
    (delete-region (point) (mark))
    (rotate-yank-input-pointer arg)
    (set-mark (point))
    (insert (car input-ring-yank-pointer))
    (if before (exchange-point-and-mark))))

(defun pop-input (arg)
  "Yank text from input ring.
Cycle through input ring with each
successive invocation.  Just like yank-input-pop except that it
does not have to be called only after yank-input."
  (interactive "*p")
  (setq last-command-was-successful-search nil)
  (if (not (memq last-command '(yank-input
				re-search-backward-input
				re-search-forward-input)))
      (progn
	(yank-input arg)
	(setq this-command 'yank-input))
      (progn
	(setq this-command 'yank-input)
	(let ((before (< (point) (mark))))
	     (delete-region (point) (mark))
	     (rotate-yank-input-pointer arg)
	     (set-mark (point))
	     (insert (car input-ring-yank-pointer))
	     (if before (exchange-point-and-mark))))))

(defun yank-input-push (arg)
  "Replace just-yanked text from input ring with following text on input ring.
This command is allowed only immediately after a yank-input or a
yank-input-push.  This function is identical to yank-input-pop
except that it goes through the input ring in the opposite direction."
  (interactive "*p")
  (setq last-command-was-successful-search nil)
  (if (not (memq last-command '(yank-input
				re-search-backward-input
				re-search-forward-input)))
      (error "Previous command was not a yank of input."))
  (setq this-command 'yank-input)
  (let ((before (< (point) (mark))))
    (delete-region (point) (mark))
    (rotate-yank-input-pointer (- arg))
    (set-mark (point))
    (insert (car input-ring-yank-pointer))
    (if before (exchange-point-and-mark))))

(defun push-input (arg)
  "Yank text from input ring.
Cycle through input ring in reverse
order with each successive invocation.  Just like pop-input except
that it goes through the ring in the other direction."
  (interactive "*p")
  (setq last-command-was-successful-search nil)
  (if (not (memq last-command '(yank-input
				re-search-backward-input
				re-search-forward-input)))
      (progn
	(yank-input (- (1- arg)))
	(setq this-command 'yank-input))
      (progn
	(setq this-command 'yank-input)
	(let ((before (< (point) (mark))))
	     (delete-region (point) (mark))
	     (rotate-yank-input-pointer (- arg))
	     (set-mark (point))
	     (insert (car input-ring-yank-pointer))
	     (if before (exchange-point-and-mark))))))

(defun yank-input (&optional arg)
  "Reinsert the last subprocess input text.
More precisely, reinsert the input text most recently killed OR yanked.
With just C-U as argument, same but put point in front (and mark at end).
With argument n, reinsert the nth most recent input text.
See also the command yank-input-pop."
  (interactive "*P")
  (setq last-command-was-successful-search nil)
  (rotate-yank-input-pointer (if (listp arg) 0
				 (if (eq arg '-) -1
				     (1- arg))))
  (set-mark (point))
  (insert (car input-ring-yank-pointer))
  (if (consp arg)
      (exchange-point-and-mark)))

(defun list-input-ring (arg &optional reflect)
  "Display contents of input ring, starting at arg.
The list is displayed in reverse order if the optional second
parameter is non-nil."
  (interactive "p")
  (let* ((input-ring-for-list input-ring)
	 (input-ring-max-for-list input-ring-max)
	 (input-ring-yank-pointer-for-list input-ring-yank-pointer)
	 (ring-length (length input-ring))
	 (yank-ring-length (length input-ring-yank-pointer))
	 (loops ring-length)
	 nth
	 first
	 count)
	(if (zerop ring-length) (error "Input ring is empty."))
 	;; We rely on (error) to exit from this function. [HW]
	(if reflect
	  (if (= arg 1)
	    (setq arg -1)
	    (setq arg (1- arg))))
	(cond
	 ((< arg 0)
	  (setq arg (- ring-length (% (- arg) ring-length)))
	  (setq count (1+ arg))
	  (setq nth (% (+ arg (- ring-length yank-ring-length)) ring-length)))
	 ((= arg 0)
	  (setq count 1)
	  (setq nth (% (+ arg (- ring-length yank-ring-length)) ring-length)))
	 (t
	  (setq count arg)
	  (setq arg (1- arg))
	  (setq nth (% (+ arg (- ring-length yank-ring-length)) ring-length))))
	(setq first nth)
	(with-output-to-temp-buffer
	  "*Input Ring*"
	  (save-excursion
	    (set-buffer standard-output)
	    (let ((lastcdr (nthcdr nth input-ring-for-list)))
		 ; GNU Emacs really needs better looping constructs. [HW]
		 (while
		   (not (cond
			 ((= loops 0)
			  t)
			 ((and (= nth (1- ring-length)) (not reflect))
			  (setq nth 0)
			  nil)
			 ((and (= nth 0) reflect)
			  (setq nth (1- ring-length))
			  nil)
			 (t
			  (setq nth (if reflect (1- nth) (1+ nth)))
			  nil)))
		   (insert (int-to-string count) " " (car lastcdr) "\n")
		   (setq lastcdr (nthcdr nth input-ring-for-list))
		   (setq count (if reflect (1- count) (1+ count)))
		   (setq loops (1- loops))
		   (cond
		    ((> count ring-length)
		     (setq count 1))
		    ((< count 1)
		     (setq count ring-length)))))))))

(defun re-search-input-ring (regexp direction)
  "Look for input text that contains string regexp.
Set input-ring-yank-pointer to text."
  (let* ((ring-length (length input-ring))
	 (yank-ring-length (length input-ring-yank-pointer))
	 (nth (- ring-length yank-ring-length))
	 (loops ring-length)
	 (return-value nil)
	 (lastcdr (nthcdr nth input-ring)))
    (if (zerop ring-length) (error "Input ring is empty."))
    ;; We rely on (error) to exit from this function. [HW]
    (while
      (not
       (cond
	((= loops 0)
	 t)
	((string-match regexp (car lastcdr) nil)
	 (setq input-ring-yank-pointer lastcdr)
	 (setq return-value t))
	((and (= nth (1- ring-length)) (>= direction 0))
	 (setq nth 0)
	 nil)
	((and (= nth 0) (< direction 0))
	 (setq nth (1- ring-length))
	 nil)
	(t
	 (setq nth (if (< direction 0) (1- nth) (1+ nth)))
	 nil)))
      (setq lastcdr (nthcdr nth input-ring))
      (setq loops (1- loops)))
    (if return-value (setq last-input-search-string regexp))
    return-value))

(defun re-search-backward-input (arg regexp)
  "Search in input ring for text that contains regexp and yank."
  (interactive "*p\nsRE search input backward: ")
  (if (string= regexp "") (setq regexp last-input-search-string))
  (if last-command-was-successful-search
      (rotate-yank-input-pointer 1))
  (setq last-command-was-successful-search nil)
  (if (let ((found t))
	   (while (and (> arg 0) found)
		  (setq found (re-search-input-ring regexp 1))
		  (setq arg (1- arg))
		  (if (and (> arg 0) found)
		      (rotate-yank-input-pointer 1)))
	   found)
      (progn
	(yank-input-at-pointer)
	(setq this-command 're-search-backward-input)
	(setq last-command-was-successful-search t))
      (message "Matching string not found in input ring.")))

(defun re-search-forward-input (arg regexp)
  "Search in input ring for text that contains regexp and yank."
  (interactive "*p\nsRE search input forward: ")
  (if last-command-was-successful-search
      (rotate-yank-input-pointer -1))
  (setq last-command-was-successful-search nil)
  (if (string= regexp "") (setq regexp last-input-search-string))
  (if (let ((found t))
	   (while (and (> arg 0) found)
		  (setq found (re-search-input-ring regexp -1))
		  (setq arg (1- arg))
		  (if (and (> arg 0) found)
		      (rotate-yank-input-pointer -1)))
	   found)
      (progn
	(yank-input-at-pointer)
	(setq this-command 're-search-backward-input)
	(setq last-command-was-successful-search t))
      (message "Matching string not found in input ring.")))

(defun yank-input-at-pointer ()
  "Yank input at current input ring pointer.
Used internally by re-search-backward-input and re-search-forward-input."
  ; This business of last-command does not work here since the
  ; `last command' was self-insert-command because of the prompt
  ; for a regular expression by (re-search-forward-input) and
  ; (re-search-backward-input).
  (delete-region (process-mark (get-buffer-process (current-buffer))) (point))
  (set-mark (point))
  (insert (car input-ring-yank-pointer)))
