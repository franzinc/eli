;;
;; copyright (C) 1987-1989 Franz Inc, Berkeley, Ca.
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

;; This file has its (distant) roots in lisp/lisp-mode.el, so:
;;
;; Copyright (C) 1985, 1986, 1987 Free Software Foundation, Inc.
;;
;; This file is derived from part of GNU Emacs.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.
;;
;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

;; $Header: /repo/cvs.copy/eli/fi-indent.el,v 1.3 1989/03/21 23:13:44 layer Exp $

(defvar lisp-electric-semicolon nil
  "*If `t', semicolons that begin comments are indented as they are typed.")

(defvar lisp-comment-indent-specification (list comment-column t nil 0)
  "*Specification list for indentations of semicolon comments.
The nth element of the list specifies the indentation for a comment beginning
with n semicolons (e.g. the first element of the list is the indentation for
comments beginning with one semicolon).  Each element of the list may be one
of `t' (indent comment just like an s-expression), `nil' (don't change the
indentation of the comment, i.e. leave the semicolon where it is), a non-
negative integer (specifying the absolute column to which the comment is to
be indented), or a negative integer (specifying a negative offset for the
comment relative to the current column).")
 
(defvar lisp-body-indent 2 "")
 
(defvar lisp-indent-offset nil "")

(defconst lisp-indent-hook 'lisp-indent-hook "")

(defvar lisp-indent-hook-property 'lisp-indent-hook
  "The indicator for the lisp-indentation hook property of a symbol.
This variable is buffer-local.")

(defvar lisp-tag-indentation 1
  "*Indentation of tags relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent tags
that occur within special forms whose symbols have a 'lisp-indent-hook
property of 'tag or 'tagbody.  The indentation is relative to the
indentation of the parenthesis enclosing the special form.")
 
(defvar lisp-tag-body-indentation 2
  "*Indentation of non-tagged lines relative to containing list.
This variable is used by the function `lisp-indent-tagbody' to indent normal
lines (lines without tags) that occur within special forms whose symbols have
a 'lisp-indent-hook property of 'tag or 'tagbody.  The indentation is
relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the body of tags will be indented as a block at the same
indentation as the first s-expression following the tag.  In this case, the
s-expressions before the first tag are indented as an undistinguished form.")
 
(defvar lisp-tag-indentation-hook nil
  "*Name of function to apply to return indentation of tag.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the tag,
the last parse state, and the indent point) to return the appropriate
indentation for tags occurring within special forms whose symbols have
a 'lisp-indent-hook property of 'tag or 'tagbody.  The indentation
returned is absolute.")
 
(defvar lisp-keyword-indentation 1
  "*Indentation of keywords relative to containing list.
This variable is used by the function `lisp-indent-keyword-list' to indent
keywords that occur within special forms whose symbols have a 'lisp-indent-hook
property of 'keyword or 'keyword-list.  The indentation is relative to the
indentation of the parenthesis enclosing the special form.")

(defvar lisp-keyword-argument-indentation t
  "*Indentation of keyword argument lines relative to containing list.
This variable is used by the function `lisp-indent-keyword-list' to indent
keyword-argument lines that occur within special forms whose symbols have
a 'lisp-indent-hook property of 'keyword or 'keyword-list.  The indentation
is relative to the indentation of the parenthesis enclosing the special form.
If the value is T, the argument(s) of keywords will be indented as a block
at the same indentation as the first s-expression following the tag.  See
the documentation for the function `lisp-indent-keyword-list'.")
 
(defvar lisp-keyword-indentation-hook nil
  "*Name of function to apply to return indentation of a keyword.
This variable may be bound to the name of a function to be applied (to
three arguments: the character position of the beginning of the keyword,
the last parse state, and the indent point) to return the appropriate
indentation for keywords occurring within special forms whose symbols have
a 'lisp-indent-hook property of 'keyword or 'keyword-list.  The inden-
tation returned is absolute.")
 
(defvar lisp-maximum-indent-struct-depth 3
  "*Maximum depth to backtrack out from a sublist for structured indentation.
If this variable is NIL, no backtracking will occur and lists whose `car'
is a symbol with a 'lisp-indent-hook property of 'label, 'labels, 'flet,
'macrolet, 'defun, or a list may not be indented properly.  In addition,
quoted lists will not be treated specially.  If this variable is T, there
is no limit placed on backtracking.  A numeric value specifies the maximum
depth to backtrack.  A reasonable value is 3.")

(defvar lisp-case-sensitive t
  "If non-NIL, the code that is being edited is for a case-sensitive dialect
of Lisp.  This variable is buffer-local.  If a Lisp is case-insensitive,
indentation specifications should be placed on the Emacs Lisp symbol that
corresponds to the lowercase name of the function, macro, or special form.")

(defvar lisp-package t
  "This variable may be NIL, T, or a symbol or string.
If the value is NIL, a package qualifier is ignored when getting the
indentation specification for a symbol.  If the value is T, the package
qualifier is not ignored.  If this variable is any other symbol or a string,
it names the package to be used for all unqualified symbols.  When this
variable is not NIL, the qualified symbol is first checked for an indentation
specification, then the unqualified symbol is checked.  This variable is
buffer-local.")

(defvar lisp-multiline-string-indentation nil
  "If non-NIL, a Lisp string that is continued over a line will be indented.
Each continuation line of a multiline string is indented relative to the
position of the initial string delimiter by a number of characters that is
the value of this variable.")

(defvar lisp-multiline-comment-indentation nil
  "If non-NIL, the second and subsequent lines of a multiline Lisp comment
will be indented relative to the first character of the initial two-character
sequence that begins the comment by a number of characters that is the value
of this variable.  It is not possible to distinguish a continued comment
from a continued quoted symbol if the same character is used as the quoted
symbol delimiter and as the second character of the begin-comment sequence 
and as the first character of the end-comment sequence.  The character
sequence that begins and ends a nestable comment is assumed to be two
characters.  GNU Emacs does not recognize nested comments.")

(defvar lisp-string-delimiters nil
  "Buffer-local variable that is a list of characters that delimit strings.")

(defvar lisp-comment-delimiters nil
  "Buffer-local variable that is a list of characters that can be used as the
second character of a two-character sequence to begin a nested comment and as
the first character of a two-character sequence to end a nested comment.")

(defvar lisp-comment-second-delimiters nil
  "Buffer-local variable that is a list of characters that can be used as the
first character of a two-character sequence to begin a nested comment and as
the second character of a two-character sequence to end a nested comment.")

(mapcar 'make-variable-buffer-local
	'(lisp-electric-semicolon
	  lisp-comment-indent-specification
	  lisp-body-indent
	  lisp-indent-offset
	  lisp-indent-hook
	  lisp-indent-hook-property
	  lisp-case-sensitive
	  lisp-package
	  lisp-multiline-string-indentation
	  lisp-multiline-comment-indentation
	  lisp-string-delimiters
	  lisp-comment-delimiters
	  lisp-comment-second-delimiters
	  lisp-tag-indentation
	  lisp-tag-body-indentation
	  lisp-tag-indentation-hook
	  lisp-keyword-indentation
	  lisp-keyword-argument-indentation
	  lisp-keyword-indentation-hook
	  lisp-maximum-indent-struct-depth))
 
(defun lisp-comment-indent (&optional addr)
  (let* ((begin (if addr addr (point)))
	 (comment-spec (or lisp-comment-indent-specification
			   (list comment-column t nil 0)))
	 (spec-length (length comment-spec))
	 spec
	 count)
    (save-excursion
      (goto-char begin)
      (skip-chars-forward ";")
      (setq count (- (point) begin)))
    ;; The following expression is from Cesar Quiroz (quiroz@cs.rochester.edu)
    ;;   for solving a problem with indenting comments when in auto-fill mode.
    ;;   Refer to the change log for details.  [Harry Weeks, November 1987.]
    (cond ((and (= count 0) (not (string= comment-start ";")))
           (let ((len (length comment-start)))
             (while (and (< count len)
                         (eql (elt comment-start count) ?\;)) 
               (setq count (+ count 1))))))
    (setq spec
	  (if (> count spec-length)
	      (nth (1- spec-length) comment-spec)
	    (nth (1- count) comment-spec)))
    (car
     (setq comment-indent-hook-values
	   (cond
	    ((eq spec nil) (list (current-column) nil))
	    ((eq spec t) (let ((tem (calculate-lisp-indent)))
			   (list (if (listp tem) (car tem) tem) nil)))
	    ((and (integerp spec) (>= spec 0)) (list spec t))
	    ((integerp spec) (- (current-column) (list spec nil)))
	    (t (error "Bad comment indentation specification for count %d."
		      count)))))))
 
(defun lisp-semicolon ()
  "Lisp semicolon hook."
  (interactive)
  (insert ";")
  (if lisp-electric-semicolon
      (save-excursion
	(skip-chars-backward ";")
	(indent-lisp-semicolon))))
 
(defun indent-lisp-semicolon (&optional at last-state)
  "Indent Lisp semicolon at point.
The optional parameters specify the point at which the last partial
s-expression parse (using `parse-partial-sexp') terminated and the
status of that parse."
  (save-excursion
    (let ((new-point (point))
	  (old-point (if at at (point)))
	  (parse-state (if last-state last-state '(0 0 0 nil nil nil 0))))
      (if (and last-state (< old-point (point)))
	  (setq parse-state
		(parse-partial-sexp
		 old-point (point) nil nil last-state last-state))
	(progn
	  ;; Find beginning of the top-level list.
	  (save-excursion
	    (if (beginning-of-defun)
		(setq old-point (point))))
	  ;; Find beginning of outermost enclosing list if we could not
	  ;;   find the beginning of a top-level list.  We do this second
	  ;;   because `scan-lists' is much more expensive than applying
	  ;;   `beginning-of-defun'.
	  (if (= old-point (point))
	      (while (setq new-point
			   (condition-case nil
			       (scan-lists old-point -1 1)
			     (error nil)))
		(setq old-point new-point)))
	  (if (= old-point (point))
	      (setq old-point 1))
	  (setq parse-state
		(parse-partial-sexp
		 old-point (point) nil nil nil parse-state))))
      (if (not (or (nth 3 parse-state)
		   (nth 4 parse-state)
		   (nth 5 parse-state)))
	  (if (and (boundp 'comment-indent-hook) comment-indent-hook)
	      (let ((to-column (funcall comment-indent-hook (point))))
		;; Function `indent-to' only inserts characters.
		(delete-horizontal-space)
		(if (and (not (or (= (preceding-char) ?\ )
				  (= (preceding-char) ?\t)
				  (= (preceding-char) ?\n)
				  (= (preceding-char) ?\f)))
			 (or (not (car (cdr comment-indent-hook-values)))
			     (>= (current-column) to-column)))
		    ;; Insert space if not rigid comment or if rigid comment
		    ;;   and we are at or beyond the comment column.
		    (if (not (bobp)) (just-one-space)))
		(indent-to to-column 0)))
	(if (nth 4 parse-state)
	    (let (comment-at)
	      (beginning-of-line)
	      (if (setq comment-at (find-line-comment))
		  (if (and (boundp 'comment-indent-hook) comment-indent-hook)
		      (let (to-column)
			(goto-char comment-at)
			(setq to-column
			      (funcall comment-indent-hook (point)))
			;; Function `indent-to' only inserts characters.
			(delete-horizontal-space)
			(if (and (= (preceding-char) ?\))
				 (or (not
				      (car (cdr comment-indent-hook-values)))
				     (>= (current-column) to-column)))
			    ;; Insert space if not rigid comment or if rigid
			    ;;   comment and we are at or beyond the comment
			    ;;   column.
			    (if (not (bobp)) (just-one-space)))
			(indent-to to-column 0)))
		(error "sexp parse anomaly: no comment where expected"))))))))
 
(defvar lisp-most-recent-parse-result '(0 0 0 0 nil nil nil 0)
  "Most recent parse result: point at parse end and parse state.
A list that is the `cons' of the point at which the most recent
parse ended and the parse state from `parse-partial-sexp'.")

(defun lisp-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indent (calculate-lisp-indent)) shift-amt beg end
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (if (looking-at "[ \t]*;;;")
 	;; Don't alter indentation of a ;;; comment line.
	nil
      (if (listp indent) (setq indent (car indent)))
      (setq shift-amt (- indent (current-column)))
      (if (zerop shift-amt)
	  nil
	(delete-region beg (point))
	(indent-to indent))
      (if (setq comment-at (find-line-comment))
	  (save-excursion
	    (goto-char comment-at)
	    (indent-lisp-semicolon (car lisp-most-recent-parse-result)
				   (cdr lisp-most-recent-parse-result))))
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos)))
      ;; If desired, shift remaining lines of expression the same amount.
      (and whole-exp (not (zerop shift-amt))
	   (save-excursion
	     (goto-char beg)
	     (forward-sexp 1)
	     (setq end (point))
	     (goto-char beg)
	     (forward-line 1)
	     (setq beg (point))
	     (> end beg))
	   (indent-code-rigidly beg end shift-amt)))))
 
(defvar calculate-lisp-indent-state-temp '(0 0 0 nil nil nil 0)
  "Used as the last argument to parse-partial-sexp so we do as little
consing as is possible.")

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
Can instead return a list, whose car is the column to indent to.
This means that following lines at the same level of indentation
should not necessarily be indented the same way.
The second element of the list is the buffer position
of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
	  (state calculate-lisp-indent-state-temp)
	  paren-depth desired-indent (retry t)
	  last-sexp containing-sexp)
      (if parse-start
	  (goto-char parse-start)
	(beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
	(setq state (parse-partial-sexp
		     (point) indent-point 0 nil nil state)))

      (rplaca lisp-most-recent-parse-result (point))
      (rplacd lisp-most-recent-parse-result (copy-sequence state))
      
      ;; Find innermost containing sexp
      (while (and retry (setq paren-depth (car state)) (> paren-depth 0))
	(setq retry nil)
	(setq last-sexp (nth 2 state))
	(setq containing-sexp (car (cdr state)))
 	;; Position following last unclosed open.
	(goto-char (1+ containing-sexp))
 	;; Is there a complete sexp since then?
	(if (and last-sexp (> last-sexp (point)))
 	    ;; Yes, but is there a containing sexp after that?
	    (let ((peek (parse-partial-sexp last-sexp indent-point 0)))
	      (if (setq retry (car (cdr peek)))
		  (setq state peek))))
	(rplaca lisp-most-recent-parse-result (point))
	(rplacd lisp-most-recent-parse-result (copy-sequence state))
	(if (not retry)
 	    ;; Innermost containing sexp found
	    (progn
	      (goto-char (1+ containing-sexp))
	      (if (not last-sexp)
 		  ;; indent-point immediately follows open paren.
 		  ;; Don't call hook.
		  (setq desired-indent (current-column))
 		;; Move to first sexp after containing open paren
		(parse-partial-sexp (point) last-sexp 0 t nil
				    ;; the result goes here:
				    (cdr lisp-most-recent-parse-result))
		(rplaca lisp-most-recent-parse-result (point))
		(cond
		 ((looking-at "\\s(")
 		  ;; Looking at a list.  Don't call hook.
		  (if (not (> (save-excursion (forward-line 1) (point))
			      last-sexp))
		      (progn (goto-char last-sexp)
			     (beginning-of-line)
			     (parse-partial-sexp
			      (point) last-sexp 0 t nil
			      ;; the result goes here:
			      (cdr lisp-most-recent-parse-result))
			     (rplaca lisp-most-recent-parse-result (point))))
 		  ;; Indent under the list or under the first sexp on the
 		  ;; same line as last-sexp.  Note that first thing on that
 		  ;; line has to be complete sexp since we are inside the
 		  ;; innermost containing sexp.
		  (backward-prefix-chars)
		  (setq desired-indent (current-column)))
		 ((> (save-excursion (forward-line 1) (point))
		     last-sexp)
 		  ;; Last sexp is on same line as containing sexp.
 		  ;; It's almost certainly a function call.
		  (parse-partial-sexp (point) last-sexp 0 t nil
				      ;; the result goes here:
				      (cdr lisp-most-recent-parse-result))
		  (rplaca lisp-most-recent-parse-result (point))
		  (if (/= (point) last-sexp)
 		      ;; Indent beneath first argument or, if only one sexp
 		      ;; on line, indent beneath that.
		      (progn (forward-sexp 1)
			     (parse-partial-sexp
			      (point) last-sexp 0 t nil
			      ;; the result goes here:
			      (cdr lisp-most-recent-parse-result))
			     (rplaca lisp-most-recent-parse-result (point))))
		  (backward-prefix-chars))
		 (t
 		  ;; Indent beneath first sexp on same line as last-sexp.
 		  ;; Again, it's almost certainly a function call.
		  (goto-char last-sexp)
		  (beginning-of-line)
		  (parse-partial-sexp (point) last-sexp 0 t nil
				      ;; the result goes here:
				      (cdr lisp-most-recent-parse-result))
		  (rplaca lisp-most-recent-parse-result (point))
		  (backward-prefix-chars)))))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overriden by lisp-indent-offset
      ;; or if the desired indentation has already been computed.
      (cond ((nth 3 state)
	     ;; Inside a string (or nestable comment such as `#| ... |#'
	     ;;   or a quoted symbol such as `| ... |').
	     (if (or (and lisp-multiline-string-indentation
			  (memq (nth 3 state) lisp-string-delimiters))
		     (and lisp-multiline-comment-indentation
			  (memq (nth 3 state)
				lisp-comment-delimiters)))
		 (let* ((beginning (find-start-of-string-or-comment
				    state indent-point containing-sexp))
			(commentp (memq (nth 3 state)
					lisp-comment-delimiters))
			(offset
			 (if commentp
			     (if (integerp lisp-multiline-comment-indentation)
				 lisp-multiline-comment-indentation
			       0)
			   (if (integerp lisp-multiline-string-indentation)
			       lisp-multiline-string-indentation
			     1))))
		   (if beginning
		       (goto-char (+ beginning offset))
		     (goto-char (+ containing-sexp 1)))
		   (setq desired-indent (current-column)))
	       (progn
		 (goto-char indent-point)
		 (skip-chars-forward " \t")
		 (setq desired-indent (current-column)))))
	    ((and (integerp lisp-indent-offset) containing-sexp)
 	     ;; Indent by constant offset
	     (goto-char containing-sexp)
	     (setq desired-indent (+ lisp-indent-offset (current-column))))
	    ((not (or desired-indent
		      (and (boundp 'lisp-indent-hook)
			   lisp-indent-hook
			   (not retry)
			   (setq desired-indent
				 (funcall lisp-indent-hook
					  indent-point state)))))
 	     ;; Use default indentation if not computed yet
	     (setq desired-indent (current-column))))
      desired-indent)))

(defvar escape-syntax-characters nil
  "A per-buffer variable which is setup in the mode specific code in
modes.el.  It is a list of escape characters in the current syntax table.")

(defun find-start-of-string-or-comment (state indent-point containing-sexp)
  (let* ((char (nth 3 state))
	 (commentp (memq char lisp-comment-delimiters))
	 (stop (max (or (nth 2 state)
			containing-sexp
			(if commentp 2 1))
		    (if commentp 2 1)))
	 (depth 1)
	 (point (1- indent-point))
	 (done nil))
    ;; This algorithm is not foolproof.  It will only handle `reasonable'
    ;;   situations.
    ;; If the `string' delimiter is one of the characters in
    ;;   `lisp-comment-delimiters', it is assumed that we are within a
    ;;   multiline comment, not a quoted symbol name that spans multiple
    ;;   lines.
    ;; We're assuming a two-character sequence begins and ends a nestable
    ;;   comment.  Although this algorithm purports to handle nested
    ;;   comments, GNU Emacs cannot parse them correctly.
    (while (and (>= point stop) (null done))
      (if (and (= (char-after point) char)
	       (not (memq (char-after (- point 1)) escape-syntax-characters)))
	  (if commentp
	      (progn (cond ((and (memq (char-after (- point 1))
				       lisp-comment-second-delimiters)
				 (not (memq (char-after
					     (- point 2))
					    escape-syntax-characters)))
			    (setq depth (1- depth)))
			   ((memq (char-after (+ point 1))
				  lisp-comment-second-delimiters)
			    (setq depth (1+ depth))))
		     (if (= depth 0) (setq done (- point 1))))
	    (setq done point)))
      (setq point (1- point)))
    done))

(defun find-syntax-chars (syntax)
  "Return a list of characters with the specified syntax in the current
syntax table."
  (let ((i 0)
	(list nil))
    (while (< i 128)
      (if (= (char-syntax i) syntax)
	  (setq list (cons i list)))
      (setq i (1+ i)))
    list))

(defvar lisp-indent-state-temp '(nil nil nil nil nil nil nil)
  "Used as the last argument to parse-partial-sexp so we can do as little
consing as possible.")

(defun lisp-indent-hook (indent-point state)
  (let ((normal-indent (current-column))
	(calculated-indent nil))
    (save-excursion
      (goto-char (1+ (car (cdr state))))
      (re-search-forward "\\sw\\|\\s_")
      (if (/= (point) (car (cdr state)))
	  (let ((function (buffer-substring (progn (forward-char -1) (point))
					    (progn (forward-sexp 1) (point))))
		(count 1))
	    (parse-partial-sexp (point) indent-point 1 t nil
				lisp-indent-state-temp)
	    (while (and (condition-case nil
			    (progn
			      (forward-sexp 1)
			      (parse-partial-sexp
			       (point) indent-point 1 t nil
			       lisp-indent-state-temp))
			  (error nil))
			(< (point) indent-point))
	      (setq count (1+ count)))
	    (setq calculated-indent
		  (lisp-invoke-method
		   (nth 1 state)
		   (lisp-get-method function)
		   0 count state indent-point))))
      (if (and (null calculated-indent)
	       lisp-maximum-indent-struct-depth)
	  (let ((depth 0)
	        (maximum-depth
		 (if (integerp lisp-maximum-indent-struct-depth)
		     lisp-maximum-indent-struct-depth
		   99999))
		last-start function sexp-beginning)
	    (goto-char (car (cdr state)))
	    (while (and (null calculated-indent)
			(< depth maximum-depth)
			(condition-case nil
			    (progn
			      (setq last-start (point))
			      (backward-up-list 1)
			      t)
			  (error nil)))
	      (save-excursion
		(setq depth (1+ depth))
		(setq sexp-beginning (point))
		(forward-char 1)
		(re-search-forward "\\sw\\|\\s_")
		(if (< (point) last-start)
		    (let ((count 1))
		      (setq function (buffer-substring
				      (progn (forward-char -1) (point))
				      (progn (forward-sexp 1) (point))))
		      (parse-partial-sexp (point) indent-point 1 t nil
					  lisp-indent-state-temp)
		      (while (and (condition-case nil
				      (progn
					(forward-sexp 1)
					(parse-partial-sexp
					 (point) indent-point 1 t nil
					 lisp-indent-state-temp))
				    (error nil))
				  (< (point) indent-point))
			(setq count (1+ count)))
		      (setq calculated-indent
			    (lisp-invoke-method
			     sexp-beginning
			     (lisp-get-method function)
			     depth count state
			     indent-point))))))))
      calculated-indent)))

(defun lisp-get-method (name)
  (let ((method
	 (let ((last-colon (lisp-find-char ?: name t))
	       (first-colon (lisp-find-char ?: name)))
	   (if last-colon
	       (cond
		((null lisp-package)
		 (lisp-get-method-aux (substring name (1+ last-colon))))
		(t
		 (or (lisp-get-method-aux name)
		     (if (= first-colon (1- last-colon))
			 (lisp-get-method-aux
			  (concat (substring name 0 first-colon) ":"
				  (substring name (1+ last-colon))))
		       nil)
		     (lisp-get-method-aux (substring name (1+ last-colon))))))
	     (cond
	      ((and lisp-package (not (eq lisp-package t)))
	       (or (lisp-get-method-aux (concat lisp-package "::" name))
		   (lisp-get-method-aux (concat lisp-package ":" name))
		   (lisp-get-method-aux name)))
	      (t
	       (lisp-get-method-aux name)))))))
    (if (eq method t)
	nil
      method)))

(defun lisp-get-method-aux (name)
  (or (and (boundp 'lisp-indent-hook-property)
	   lisp-indent-hook-property
	   (get (intern-soft (if lisp-case-sensitive
				 name
			       (downcase name)))
		lisp-indent-hook-property))
      (get (intern-soft (if lisp-case-sensitive
			    name
			  (downcase name)))
	   'lisp-indent-hook)))

(defun lisp-find-char (char string &optional from-end)
  (let* ((l (length string))
	 (i (if from-end (1- l) 0))
	 (e (if from-end -1 l))
	 (d (if from-end -1 1))
	 (n nil))
    (while (and (not n) (not (= i e)))
      (if (= char (elt string i))
	  (setq n i)
	(setq i (+ i d))))
    n))

(defun lisp-invoke-method (form-start method depth count state indent-point)
  (cond ((and form-start
	      (or (eq (char-after (- form-start 1)) ?\#) ;; Vectors.
		  (and (eq (char-after (- form-start 1)) ?\') ;; Quoted lists.
		       (not (eq (char-after (- form-start 2)) ?\#)))))
	 (lisp-indent-quoted-list depth count state indent-point))
	((integerp method)
	 (lisp-indent-specform method depth count state indent-point))
        ((consp method)
	 (cond ((eq (car method) 'recursive)
		(lisp-invoke-method
		 form-start (nth 1 method) 0 count state indent-point))
	       ((eq (car method) 'if)
		(lisp-invoke-method
		 form-start
		 (apply 'lisp-if-indent
			depth count state indent-point
			(cdr method))
		 depth count state indent-point))
	       ((and (symbolp (car method)) (fboundp (car method)))
		(apply (car method)
		       depth count state indent-point
		       (cdr method)))
	       (t (lisp-indent-struct method depth count state indent-point))))
	((eq method 'quote)
	 (lisp-indent-quoted-list depth count state indent-point))
	((memq method '(tag tagbody))
	 (lisp-indent-tagbody depth count state indent-point))
	((memq method '(keyword keyword-list))
	 (lisp-indent-keyword-list
	  depth count state indent-point t nil t))
	((memq method '(lambda lambda-list))
	 (lisp-indent-keyword-list depth count state indent-point t nil t
				   nil nil nil "&optional" "&rest" "&key"
				   "&allow-other-keys" "&aux" "&body"
				   "&whole" "&env"))
	((and method (symbolp method))
	 ;; this is how one form shadows the indenation of another, though
	 ;; I suppose that this could loop infinitely!
	 (lisp-invoke-method form-start
			     (lisp-get-method (symbol-name method))
			     depth count state indent-point))
	((fboundp method)
	 (funcall method depth count state indent-point))
	(method (error "can't handle method %s" method))))

(defun lisp-indent-struct (methods depth count state indent-point)
  "Function to indent Lisp special forms that have substructure.
The METHODS is a list of triples, (Depth Count Method), where Depth and
Count specify when special treatment is required of a sublist in a form,
and Method is the method of indentation to apply to such a sublist.  If
Depth or Count is t or nil, Method is used for any Depth or Count.  The
Count may be a list of two elements, in which case it specifies an inclusive
range of subexpressions, either bound of which may be t nor nil.  The
Method will be applied to the form itself if the Depth is zero.  The Method
is interpreted identically to the 'lisp-indent-hook property of symbols and
thus may be recursive (that is, itself a list of triples).  If the Method
is recursive, it is applied if DEPTH is greater or equal to Depth; this
implies that triples should be ordered greatest depth first in lists.  In
recursive applications of this function to a Method, DEPTH is set to the
depth of the current sublist relative to sublist associated with Method.
The METHODS list should be the value of the 'lisp-indent-hook property of
a symbol that is a Lisp special form having substructure.  The DEPTH is
the depth of the current sublist relative to the form.  The COUNT argument
is the number of the current s-expression in the form.

As an example, giving LABELS the following property

  (put 'labels 'lisp-indent-hook '((2 1 ((1 1 lambda-list) (0 t 1))) (0 t 1)))

will have the effect that s-expressions of the LABELS form itself are
indented with 1 distinguished form using `lisp-indent-specform' (this is
specified by '(0 t 1)).  In addition, all sublists of the first s-expression,
which is a list, of the LABELS special form (denoted by '(2 1 ...)) will be
treated just like a LAMBDA (whose method is '((1 1 lambda-list) (0 t 1)))."
  (let ((calculated-indent nil)
	method)
    (while (and (null calculated-indent)
		(setq method (car methods)))
      (setq methods (cdr methods))
      (let ((method-depth (car method))
	    (method-count (car (cdr method)))
	    (method-method (car (cdr (cdr method)))))
	(if (and (or (equal depth method-depth)
		     (memq method-depth '(t nil))
		     (and (consp method-method)
			  (not (and (symbolp (car method-method))
				    (fboundp (car method-method))))
			  (> depth method-depth)))
		 (if (consp method-count)
		     (and (or (memq (car method-count) '(t nil))
			      (>= count (car method-count)))
			  (or (memq (car (cdr method-count)) '(t nil))
			      (<= count (car (cdr method-count)))))
		   (or (equal count method-count)
		       (memq method-count '(t nil)))))
	    (let ((inner-depth (if (integerp method-depth)
				   (- depth method-depth)
				 99999))
		  inner-count
		  last-start
		  (depth 0)
		  (count 0))
	      (goto-char (car (cdr state)))
	      (while (and (< depth inner-depth)
			  (condition-case nil
			      (progn
				(setq last-start (point))
				(backward-up-list 1)
				t)
			    (error nil)))
		(setq depth (1+ depth)))
	      (condition-case nil
		  (progn
		    (forward-char 1)
		    (parse-partial-sexp (point) indent-point 1 t nil
					lisp-indent-state-temp)
		    (while (and (condition-case nil
				    (progn
				      (forward-sexp 1)
				      (parse-partial-sexp
				       (point) indent-point 1 t nil
				       lisp-indent-state-temp))
				  (error nil))
				(< (point) indent-point))
		      (setq count (1+ count))))
		(error nil))
	      (setq inner-count count)
	      (setq calculated-indent
		    (lisp-invoke-method
		     nil method-method inner-depth inner-count
		     state indent-point))))))
    (cond
     ((consp calculated-indent)
      calculated-indent)
     (calculated-indent
      (list calculated-indent (nth 1 state)))
     (t
      nil))))
 
(defun lisp-indent-quoted-list (depth count state indent-point)
  "Function for indenting quoted lists."
  (goto-char (1+ (car (cdr state))))
  (current-column))
 
(defun lisp-indent-keyword-list (depth count state indent-point
				 quotedp keyword-arg-pairs-p
				 &optional keyword-count
				 	   special-keyword-count
					   special-count
					   ignore-after-count
				 &rest keywords)
  "Function for indenting a form with keywords.
This function is useful for indenting lambda lists and special forms that
have keywords.  The argument QUOTEDP indicates that the form is a quoted
list (such as a lambda list) if it is non-NIL.

KEYWORD-ARG-PAIRS-P if non-NIL indicates that keywords and their arguments
come in pairs, i.e. there is a single s-expression associated with each
keyword.  If this is NIL, all s-expressions that follow a keyword, up to
the next keyword, are considered as the `arguments' to the keyword.  (Note
that the first s-expression following the keyword is always treated as the
argument to the keyword even if it is also a keyword.)

Optional argument KEYWORD-COUNT specifies the number of keyword that are
recognized as such.  Only the specified number of keywords (and their
associated argument s-expressions) will be indented distinctly: any further
keywords are treated simply as atomic expressions in the body of the form.
If this is T, all keywords found in the form are recognized.

Optional argument SPECIAL-KEYWORD-COUNT specifies the number of
distinguished keywords.  Distinguished keywords are indented twice the
value of `lisp-body-indent', just as distinguished forms are indented by
`lisp-indent-specform'.  If the argument to a distinguished keyword does
not appear on the same line as the keyword, the argument is indented thrice
the value of `lisp-body-indent'.  If KEYWORD-ARG-PAIRS-P is NIL, variable
`lisp-keyword-argument-indentation' determines the indentation of any
second and subsequent arguments to distinguished keywords.  If
`lisp-keyword-argument-indentation' is not T, arguments are indented thrice
the value of `lisp-body-indent'.  If SPECIAL-KEYWORD-COUNT is T, all
keywords and their arguments are treated as distinguished.  The keywords
encountered in the form are counted in parallel to satisfy both
SPECIAL-KEYWORD-COUNT and KEYWORD-COUNT.

Optional argument SPECIAL-COUNT specifies the number of distinguished
s-expressions in the form.  Any potential keywords and their arguments that
appear as distinguished s-expressions of the form are not counted toward
SPECIAL-KEYWORD-COUNT or KEYWORD-COUNT.

Optional argument IGNORE-AFTER-COUNT specifies the number of initial
non-keyword s-expressions in the form (after satisfying SPECIAL-COUNT)
after which keywords will no longer be recognized.  If this is T, no
keywords will be recognized after the first non-keyword, non-special
s-expression encountered.  (Note that specifying both KEYWORD-ARG-PAIRS-P
to be NIL and IGNORE-AFTER-COUNT to be T means only that no keywords will
be recognized if the first s-expression following any distinguished
s-expressions is not a keyword.)

Any further arguments to this function constitute the specific keywords to
be recognized.  If no keywords are explicitly specified, all keywords
(atoms beginning with a colon) are recognized."
  (if (> depth 0)
      nil
    (save-excursion
      (if (eq keyword-count t) (setq keyword-count 99999))
      (if (eq special-keyword-count t) (setq special-keyword-count 99999))
      (goto-char indent-point)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (let* ((keyword-at (point))
	     (keyword
	      (condition-case nil
		  (save-excursion
		    (buffer-substring
		     keyword-at (progn (forward-sexp 1) (point))))
		(error nil)))
	     (special-keys nil)
	     (keywords (if keywords
			   (mapcar '(lambda (key)
				     (if (consp key)
					 (progn
					   (setq special-keys
					     (cons key special-keys))
					   (car key))
				       key))
				   keywords)))
	     (is-keyword
	      (or (and (null keywords)
		       (and (not (eobp)) (eq (following-char) ?:)))
		  (and keywords
		       keyword
		       (lisp-find-keyword keyword keywords))))
	     (sexp-at (nth 1 state))
	     (sexp-column (save-excursion
			    (goto-char sexp-at)
			    (current-column))))
	(list
	 (let* ((keyword-info (lisp-scan-sexp-for-keywords
			       special-count ignore-after-count
			       (not quotedp) keyword-arg-pairs-p
			       keywords state indent-point))
		(last-keyword-began (nth 0 keyword-info))
		(last-keyword-arg-began (nth 1 keyword-info))
		(last-sexp-began (nth 2 keyword-info))
		(keywords-found (nth 3 keyword-info))
		(nonkeywords-found (nth 4 keyword-info))
		(special-indent (if (integerp special-count)
				    (lisp-indent-specform
				     special-count
				     depth count state indent-point)
				  nil)))
	   (if (consp special-indent)
	       (car special-indent)
	     (cond
	      ((and is-keyword
		    (integerp nonkeywords-found)
		    keyword-count
		    (< keywords-found keyword-count))
	       (cond
		((and special-keys
		      (lisp-find-special-keyword-indent keyword special-keys)))
		((and special-keyword-count
		      (< keywords-found special-keyword-count))
		 (+ sexp-column (* 2 lisp-body-indent)))
		((and (boundp 'lisp-keyword-indentation-hook)
		      lisp-keyword-indentation-hook)
		 (funcall lisp-keyword-indentation-hook
			  keyword-at state indent-point))
		(t
		 (+ sexp-column lisp-keyword-indentation))))
	      ((and special-keyword-count
		    (<= keywords-found special-keyword-count)
		    (integerp nonkeywords-found)
		    last-keyword-began
		    (or (null last-keyword-arg-began)
			(and (not (eq lisp-keyword-argument-indentation t))
			     (not keyword-arg-pairs-p))))
	       (+ sexp-column (* 3 lisp-body-indent)))
	      ((or (null last-keyword-began)
		   (and keyword-arg-pairs-p last-keyword-arg-began)
		   (null keyword-count)
		   (> keywords-found keyword-count)
		   (not (integerp nonkeywords-found)))
	       (if quotedp
		   (+ sexp-column 1)
		 (+ sexp-column lisp-body-indent)))
	      ((integerp lisp-keyword-argument-indentation)
	       (+ sexp-column lisp-keyword-argument-indentation))
	      ((eq lisp-keyword-argument-indentation t)
	       (condition-case nil
		   (progn (if last-keyword-arg-began
			      (goto-char last-keyword-arg-began)
			    (backward-sexp 1))
			  (current-column))
		 (error (+ sexp-column 1))))
	      (t
	       (+ sexp-column lisp-body-indent)))))
	 sexp-at)))))

(defun lisp-find-special-keyword-indent (keyword alist)
  (let ((keys alist)
	(key nil)
	(matched nil)
	(test-function
	 (if lisp-case-sensitive
	     'string-equal
	   'string-equal-nocase)))
    (while (and keys (not matched))
      (setq key (car keys))
      (if (funcall test-function (car key) keyword)
	  (setq matched (cdr key)))
      (setq keys (cdr keys)))
    matched))

(defun lisp-find-keyword (keyword keywords)
  (let ((keys keywords)
	(matched nil)
	(test-function
	 (if lisp-case-sensitive
	     'string-equal
	   'string-equal-nocase)))
    (while (and keys (not matched))
      (if (funcall test-function
		   (if (stringp (car keys))
		       (car keys)
		     (cdr (car keys)))
		   keyword)
	  (setq matched t))
      (setq keys (cdr keys)))
    matched))

(defun lisp-scan-sexp-for-keywords (special-count ignore-after-count
				    ignore-car keyword-arg-pairs-p
				    keywords state indent-point)
  "Scan an s-expression for keywords.
Returns a list of five elements: point where last keyword starts (or NIL
if no keyword was found), point where last keyword's argument starts (or
NIL if no keyword was found or if the last s-expression parsed was a keyword),
point where last s-expression parsed starts (or NIL if the current form
contains no s-expressions), count (possibly zero) of keywords found, and
the number (possibly zero) of non-keyword s-expressions (excluding keyword
arguments) that were found or T if IGNORE-AFTER-COUNT was non-NIL and keyword 
scanning was curtailed because of it.  A keyword and its argument constitute 
a keyword pair.  The argument to a keyword, even if it is itself a keyword, 
is not counted as a keyword.  The argument to a keyword is not counted as a
non-keyword s-expression."
  (let ((containing-form-start (car (cdr state)))
	(form-count 0)
	(nonkeyword-count 0)
	(ignore-keywords nil)
	(count 0)
	(last-keyword-start nil)
	(last-keyword-arg-start nil)
	(last-sexp-start nil)
	(last-was-keyword nil)
	end-sexp)
    (if (null special-count) (setq special-count 0))
    (if (eq special-count t) (setq special-count 99999))
    (goto-char containing-form-start)
    (forward-char 1)
    (if ignore-car
	(progn (forward-sexp 1)
	       (parse-partial-sexp (point) indent-point 1 t nil
				   lisp-indent-state-temp)))
    (while (and
	    (< (point) indent-point)
	    (not ignore-keywords)
	    (condition-case nil
		(progn
		  (forward-sexp 1)
		  (setq end-sexp (point))
		  (backward-sexp 1)
		  (setq last-sexp-start (point))
		  (setq form-count (1+ form-count))
		  (if (> form-count special-count)
		      (if (not last-was-keyword)
			  (progn
			    (if (and (null keywords)
				     (eq (following-char) ?:))
				(progn
				  (setq count (1+ count))
				  (setq last-was-keyword t)
				  (setq last-keyword-arg-start nil)
				  (setq last-keyword-start (point)))
			      (let ((keyword
				     (buffer-substring (point) end-sexp))
				    (keys keywords)
				    (matched nil)
				    (test-function
				     (if lisp-case-sensitive
					 'string-equal
				       'string-equal-nocase)))
				(while (and keys (not matched))
				  (if (funcall test-function
					       (car keys) keyword)
				      (progn
					(setq count (1+ count))
					(setq last-was-keyword t)
					(setq matched t)
					(setq last-keyword-arg-start nil)
					(setq last-keyword-start (point))))
				  (setq keys (cdr keys)))))
			    (if (and (not last-was-keyword)
				     (or keyword-arg-pairs-p (zerop count)))
				(setq nonkeyword-count (1+ nonkeyword-count)))
			    (if (and ignore-after-count
				     (if (integerp ignore-after-count)
					 (>= nonkeyword-count
					     ignore-after-count)
				       (not (zerop nonkeyword-count))))
				(setq ignore-keywords t)))
			(progn
			  (setq last-keyword-arg-start (point))
			  (setq last-was-keyword nil))))
		  (goto-char end-sexp)
		  (parse-partial-sexp (point) indent-point 1 t nil
				      lisp-indent-state-temp))
	      (error nil))))
    (list last-keyword-start last-keyword-arg-start last-sexp-start
	  count (or ignore-keywords nonkeyword-count))))

(defun lisp-indent-tagbody (depth count state indent-point
			    &optional spec-count
			    &rest keywords)
  "Function for indenting TAGBODY and related forms.
This function indents special forms that have `tags' or `keywords' that
should be treated specially.  For example, TAGBODY forms have `tags' for
GOTO.  An optional argument SPEC-COUNT is accepted, specifying the number
of distinguished s-expressions in the form.  Any further arguments
constitute the `keywords' or `tags' that are to be recognized.  In the
absence of an explicit list, any atomic expression is considered a
keyword."
  (if (> depth 0)
      nil
    (save-excursion
      (goto-char indent-point)
      (beginning-of-line 1)
      (skip-chars-forward " \t")
      (let* ((tag-at (point))
	     (tag (condition-case nil
		      (save-excursion
			(buffer-substring tag-at
					  (progn (forward-sexp 1) (point))))
		    (error nil)))
	     (is-tag (or (and (null keywords)
			      (and (not (eobp))
				   (memq (char-syntax (following-char))
					 '(?w ?_))))
			 (and keywords
			      tag
			      (let ((keys keywords)
				    (matched nil)
				    (test-function
				     (if lisp-case-sensitive
					 'string-equal
				       'string-equal-nocase)))
				(while (and keys (not matched))
				  (if (funcall test-function (car keys) tag)
				      (setq matched t))
				  (setq keys (cdr keys)))
				matched))))
	     (sexp-at (nth 1 state))
	     (sexp-column (save-excursion
			    (goto-char sexp-at)
			    (current-column))))
	(list
	 (if is-tag
	     (if (and (boundp 'lisp-tag-indentation-hook)
		      lisp-tag-indentation-hook)
		 (funcall lisp-tag-indentation-hook tag-at state indent-point)
	       (+ sexp-column lisp-tag-indentation))
	   (let ((spec-indent (if (integerp spec-count)
				  (lisp-indent-specform
				   spec-count depth count state indent-point)
				nil)))
	     (if (consp spec-indent)
		 (car spec-indent)
	       (cond
		((integerp lisp-tag-body-indentation)
		 (+ sexp-column lisp-tag-body-indentation))
		((eq lisp-tag-body-indentation t) (condition-case nil
						      (progn (backward-sexp 1)
							     (current-column))
						    (error (+ sexp-column 1))))
		(t (+ sexp-column lisp-body-indent))))))
	 sexp-at)))))

(defun lisp-if-indent (depth count state indent-point test then-spec else-spec)
  "Indent a form conditionally.  The TEST form is used to generate a Lisp
form that is evaluated.  The form evaluated will be:
	(apply (car TEST) depth count state indent-point (cdr TEST))
If this application returns non-NIL, the indentation specification given by
THEN-SPEC is used, otherwise the indentation specification given by
ELSE-SPEC is used."
  (if (apply (car test) depth count state indent-point
	     (cdr test))
      then-spec
    else-spec))

(defun lisp-atom-p (depth count state indent-point element)
  "Predicate to test whether the specified ELEMENT of the current s-expression
being parsed is atomic.  Returns T if the element is atomic, returns NIL if
the element is not atomic or if the s-expression does not contain enough
elements."
  (car (lisp-atom-info depth count state indent-point element)))

(defun lisp-atom-info (depth count state indent-point element)
  "Return information about the atomicity of the specified ELEMENT
of the current s-expression being parsed.
Returns a list of two elements.  The first is non-NIL if the element
is atomic.  It will be NIL if the element of the s-expression was not
atomic or if the s-expression is not a list or if the s-expression
is a list but does not contain enough elements.  The second element of
the returned list will be non-NIL if the specified element was found,
NIL otherwise.  The ELEMENT is the number of the element, zero indexed."
  (let ((containing-form-start (car (cdr state)))
	(atomic nil)
	(found nil)
	(count 0))
    (goto-char containing-form-start)
    (forward-char 1)
    (while (and
	    (not found)
	    (< (point) indent-point)
	    (condition-case nil
		(progn
		  (forward-sexp 1)
		  (setq end-sexp (point))
		  (backward-sexp 1)
		  (if (= count element)
		      (progn
			(setq found t)
			(setq atomic (not (eq (following-char) ?\()))))
		  (goto-char end-sexp)
		  (setq count (1+ count))
		  (parse-partial-sex{ (point) indent-point 1 t nil
				      lisp-indent-state-temp))
	      (error nil))))
    (list atomic found)))

(defun lisp-indent-predicated-special (depth count state indent-point
				       &optional spec-count predicate)
  "Function for indenting forms whose distinguished forms are predicated.
An example is the Common Lisp LOCALLY special form.  All initial subforms
that are declarations are distinguished forms, and any remaining forms are
body forms.  An optional SPEC-COUNT is accepted, indicating that there will
be at least that many distinguished forms.  If PREDICATE is not supplied,
this funciton uses a predicate that returns non-NIL for subforms that are
declarations (i.e., forms whose car is DECLARE).  The PREDICATE must name a
function of one argument, the buffer position of the subform to be
examined.  The predicate must return non-NIL if that subform is to be
distinguished."
  (or spec-count (setq spec-count 0))
  (or predicate (setq predicate 'lisp-form-declare-p))
  (if (> depth 0)
      nil
    (or (save-excursion
	  (let ((containing-form-start (nth 1 state))
		(count 0)
		containing-form-column)
	    (goto-char containing-form-start)
	    (setq containing-form-column (current-column))
	    (forward-char 1)
	    (while (and (< (point) indent-point)
			(condition-case nil
			    (progn
			      (forward-sexp 1)
			      (if (> count spec-count)
				  (progn
				    (backward-sexp 1)
				    (if (funcall predicate (point))
					(setq spec-count (1+ spec-count)))
				    (forward-sexp 1)))
			      (setq count (1+ count))
			      (parse-partial-sexp
			       (point) indent-point 1 t nil
			       lisp-indent-state-temp))
			  (error nil))))
	    (if (<= count spec-count)
		nil
	      (progn
		(goto-char indent-point)
		(beginning-of-line 1)
		(skip-chars-forward " \t")
		(if (funcall predicate (point))
		    (list
		     (max normal-indent
			  (+ containing-form-column (* 2 lisp-body-indent)))
		     containing-form-start)
		  nil)))))
	(lisp-indent-specform spec-count depth count state indent-point))))

(defun lisp-form-declare-p (sexp)
  (let ((car (lisp-form-car sexp)))
    (and car
	 (funcall (if lisp-case-sensitive
		      'string-equal
		    'string-equal-nocase)
		  "declare"
		  car))))

(defun lisp-form-car (at)
  "Return a string that is the car of the form at point AT."
  (save-excursion
    (goto-char at)
    (if (= (following-char) ?\()
	(forward-char 1))
    (condition-case nil
	(let (end)
	  (forward-sexp 1)
	  (setq end (point))
	  (backward-sexp 1)
	  (buffer-substring (point) end))
      (error nil))))

(defun lisp-indent-specform (spec-count depth count state indent-point)
  (if (> depth 0)
      nil
    (let ((containing-form-start (car (cdr state))) (i spec-count)
	  body-indent containing-form-column)
      ;; Move to the start of containing form, calculate indentation
      ;; to use for non-distinguished forms (> spec-count), and move past the
      ;; function symbol.  lisp-indent-hook guarantees that there is at
      ;; least one word or symbol character following open paren of containing
      ;; form.
      (goto-char containing-form-start)
      (setq containing-form-column (current-column))
      (setq body-indent (+ lisp-body-indent containing-form-column))
      (forward-char 1)
      (forward-sexp 1)
      ;; Now find the start of the last form.
      (parse-partial-sexp (point) indent-point 1 t nil
			  lisp-indent-state-temp)
      (while (and (< (point) indent-point)
		  (condition-case nil
		      (progn
			(setq spec-count (1- spec-count))
			(forward-sexp 1)
			(parse-partial-sexp (point) indent-point 1 t nil
					    lisp-indent-state-temp))
		    (error nil))))
      ;; Point is sitting on first character of last (or spec-count) sexp.
      (if (> spec-count 0)
	  ;; A distinguished form.  If it is the first or second form use
	  ;; double lisp-body-indent, else normal indent.  With
	  ;; lisp-body-indent bound to 2 (the default), this just happens
	  ;; to work the same with if as the older code, but it makes
	  ;; unwind-protect, condition-case, with-output-to-temp-buffer,
	  ;; et. al. much more tasteful.  The older, less hacked, behavior
	  ;; can be obtained by replacing below with `(list normal-indent
	  ;; containing-form-start)'.
	  (if (<= (- i spec-count) 1)
	      (list (+ containing-form-column (* 2 lisp-body-indent))
		    containing-form-start)
	    (list normal-indent containing-form-start))
	;; Non-distinguished form. Use body-indent if there are no
	;; distinguished forms and this is the first undistinguished form,
	;; or if this is the first undistinguished form and the preceding
	;; distinguished form has indentation at least as great as
	;; body-indent.
	(if (or (and (= i 0) (= spec-count 0))
		(and (= spec-count 0) (<= body-indent normal-indent)))
	    body-indent
	  normal-indent)))))

(defun indent-sexp ()
  "Indent each line of the list starting just after point."
  (interactive)
  (let ((indent-stack (list nil)) (next-depth 0) bol
	outer-loop-done inner-loop-done state this-indent
	(current-parse-result nil)
	(previous-parse-result nil))
    (save-excursion (forward-sexp 1))
    (save-excursion
      (setq outer-loop-done nil)
      (while (not outer-loop-done)
	(setq last-depth next-depth
	      innerloop-done nil)
	(while (and (not innerloop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq previous-parse-result current-parse-result)
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  nil nil state))
	  (setq current-parse-result (cons (point) state))
	  (setq next-depth (car state))
	  (if (nth 4 state)
	      (let (comment-at)
		(beginning-of-line 1)
		(if (setq comment-at (find-line-comment))
		    (save-excursion
		      (goto-char comment-at)
		      (if previous-parse-result
			  (indent-lisp-semicolon (car previous-parse-result)
						 (cdr previous-parse-result))
			(indent-lisp-semicolon))))
		(end-of-line)
		(setcar (nthcdr 4 state) nil)))
	  (if (nth 3 state)
	      (progn
		(if (or lisp-multiline-string-indentation
			lisp-multiline-comment-indentation)
		    ;; Pretty hokey.
		    (progn
		      (lisp-indent-line)
		      (forward-line 1)
		      (lisp-indent-line)
		      (beginning-of-line 1))
		  (forward-line 1))
		(setcar (nthcdr 5 state) nil))
	    (setq innerloop-done t)))
	(if (setq outer-loop-done (<= next-depth 0))
	    nil
	  (while (> last-depth next-depth)
	    (setq indent-stack (cdr indent-stack)
		  last-depth (1- last-depth)))
	  (while (< last-depth next-depth)
	    (setq indent-stack (cons nil indent-stack)
		  last-depth (1+ last-depth)))
	  (forward-line 1)
	  (setq bol (point))
	  (skip-chars-forward " \t")
	  (if (or (eobp) (looking-at "[;\n]"))
	      nil
	    (if (and (car indent-stack)
		     (>= (car indent-stack) 0))
		(setq this-indent (car indent-stack))
	      (let ((val (calculate-lisp-indent
			  (if (car indent-stack) (- (car indent-stack))))))
		(if (integerp val)
		    (setcar indent-stack
			    (setq this-indent val))
		  (setcar indent-stack (- (car (cdr val))))
		  (setq this-indent (car val)))))
	    (if (/= (current-column) this-indent)
		(progn (delete-region bol (point))
		       (indent-to this-indent)))))))))

(defun indent-code-rigidly (start end arg &optional nochange-regexp)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them. Called from a program, takes
args START, END, COLUMNS and NOCHANGE-REGEXP. The last is a regexp which,
if matched at the beginning of a line, means don't indent that line."
  (interactive "r\np")
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state
	    (parse-partial-sexp (point) (progn (forward-line 1) (point)))))
      (if (null state) (setq state '(nil nil nil nil nil nil nil)))
      (while (< (point) end)
	(or (and (nth 3 state)
		 (not (or (and lisp-multiline-string-indentation
			       (memq (nth 3 state)
				     lisp-string-delimiters))
			  (and lisp-multiline-comment-indentation
			       (memq (nth 3 state)
				     lisp-comment-delimiters)))))
	    (nth 4 state)
	    (and nochange-regexp
		 (looking-at nochange-regexp))
 	    ;; If line does not start in string or comment, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point)
			     (progn (skip-chars-forward " \t") (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp
		     (point) (progn (forward-line 1) (point))
		     nil nil state state))))))

(defun find-line-comment ()
  "Return point of first comment character on this line, or nil."
  (save-excursion
    (let ((end-of-line-point (progn (end-of-line nil) (point))))
      (beginning-of-line nil)
      (if (re-search-forward comment-start-skip end-of-line-point t 1)
	  (match-beginning 0)
	nil))))

(defun string-equal-nocase (a b)
  "Returns NIL if the two string arguments are not equal, case ignored."
  (string-equal (upcase a) (upcase b)))

;;; Lisp form indentation specifications.
;;; Note that `t' specifies that the form is not special and `shadows'
;;;   any indentation specified with the property stored under the
;;;   indicator `lisp-indent-hook'.

(let ((tag 'lisp-indent-hook))
  (put 'assert tag '((1 2 quote) (0 t 2)))
  (put 'block tag 1)
  (put 'case tag '((1 (2 t) ((1 0 quote) (0 t nil))) (0 t 1)))
  (put 'catch tag 1)
  (put 'ccase tag 'case)
  (put 'check-type tag 2)
  (put 'compiler-let tag '((1 1 quote) (0 t 1)))
  (put 'concatenate tag 1)
  (put 'ctypecase tag 'case)
  (put 'defconstant tag 'defvar)
  (put 'define-modify-macro tag '((1 2 lambda-list) (0 t 2)))
  (put 'define-setf-method tag '((1 2 lambda-list) (0 t 2)))
  (put 'defmacro tag '((1 2 lambda-list) (0 t 2)))
  (put 'defparameter tag 'defvar)
  (put 'defsetf tag '((1 2 lambda-list) (0 t 3)))
  (put 'defstruct tag '((1 1 quote) (0 t 1)))
  (put 'deftype tag '((1 2 lambda-list) (0 t 2)))
  (put 'defun tag '((1 2 lambda-list) (0 t 2)))
  (put 'defvar tag 2)
  (put 'do tag '((1 1 1) (1 2 1) (0 t (lisp-indent-tagbody 2))))
  (put 'do* tag 'do)
  (put 'do-all-symbols tag 'dolist)
  (put 'do-external-symbols tag 'dolist)
  (put 'do-symbols tag 'dolist)
  (put 'dolist tag '((1 1 1) (0 t (lisp-indent-tagbody 1))))
  (put 'dotimes tag 'dolist)
  (put 'ecase tag 'case)
  (put 'etypecase tag 'case)
  (put 'eval-when tag 1)
  (put 'flet tag '((2 1 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'if tag 2)
  (put 'labels tag 'flet)
  (put 'lambda tag '((1 1 lambda-list) (0 t 1)))
  (put 'let tag '((1 1 quote) (0 t 1)))
  (put 'let* tag 'let)
  (put 'locally tag 'lisp-indent-predicated-special)
  (put 'loop tag 'tagbody)
  (put 'macrolet tag 'flet)
  (put 'map tag 1)
  (put 'multiple-value-bind tag '((1 1 quote) (0 t 2)))
  (put 'multiple-value-call tag 1)
  (put 'multiple-value-list tag 1)
  (put 'multiple-value-prog1 tag 1)
  (put 'multiple-value-setq tag '((1 1 quote) (0 t 1)))
  (put 'prog tag '((0 1 1) (0 t tagbody)))
  (put 'prog* tag 'prog)
  (put 'prog1 tag 1)
  (put 'prog2 tag 2)
  (put 'progn tag 0)
  (put 'progv tag 2)
  (put 'return tag 0)
  (put 'return-from tag 1)
  (put 'setf tag 1)
  (put 'setq tag 1)
  (put 'tagbody tag 'tagbody)
  (put 'the tag 1)
  (put 'throw tag 1)
  (put 'typecase tag 'case)
  (put 'unless tag 1)
  (put 'unwind-protect tag 1)
  (put 'when tag 1)
  (put 'with-input-from-string tag '((1 1 quote) (0 t 1)))
  (put 'with-open-file tag '((1 1 quote) (0 t 1)))
  (put 'with-open-stream tag '((1 1 quote) (0 t 1)))
  (put 'with-output-to-string tag '((1 1 quote) (0 t 1)))

  ;; the condition system (v18)

  (put 'define-condition tag '((1 (4 t) nil) (1 3 quote) (0 t 3)))
  (put 'handler-bind tag 'case)
  (put 'handler-case tag 'let)

  (put 'restart-bind tag
       '((2 1 ((0 t (lisp-indent-keyword-list
		     nil	; quotedp
		     t		; keyword-arg-pairs-p
		     2		; keyword-count
		     2		; special-keyword-count
		     1		; special-count
		     t		; ignore-after-count
		     ;; keywords recognized:
		     ":interactive-function" ":report-function"))))
	 (0 t 1)))
  (put 'restart-case tag
       '((1 (2 t) ((1 1 lambda-list)
		   (0 t (lisp-indent-keyword-list
			 nil	; quotedp
			 t	; keyword-arg-pairs-p
			 2	; keyword-count
			 2	; special-keyword-count
			 1	; special-count
			 t	; ignore-after-count
			 ;; keywords recognized:
			 ":report" ":interactive"))))
	 (0 t 1)))

  (put 'with-simple-restart tag 'when)

  ;; CLOS

  (put 'make-instance tag 1)

  ;; Flavors

  (put 'defflavor tag '((1 2 quote) (1 3 quote) (0 t 3)))
  (put 'defmethod tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  (put 'defwhopper tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  (put 'defwrapper tag '((1 1 quote) (1 2 lambda-list) (0 t 2)))
  )

(let ((tag 'common-lisp-indent-hook))
  
  ;; generic Common Lisp
  
  (put 'defmacro tag '((1 2 (recursive lambda-list)) (0 t 2)))
  (put 'destructuring-bind tag 'defmacro)
  (put 'defsetf tag '(if (lisp-atom-p 2) 2 ((1 2 lambda-list) (0 t 3))))

  ;; Allegro CL

  (put 'tpl:alias tag 2)
  (put 'if* tag
       '(lisp-indent-keyword-list
	 nil nil 4 0 0 nil "then" "thenret" "else" ("elseif" . 1)))
  )

(let ((tag 'franz-lisp-indent-hook))
  (put 'caseq tag 'case)
  (put 'c-declare tag '((1 t 1) (0 t 0)))
  (put '*catch tag 1)
  (put 'def tag '((1 2 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'defcmacro tag '((1 2 lambda-list) (0 t 2)))
  (put 'defsubst tag '((1 2 lambda-list) (0 t 2)))
  (put 'errset tag 1)
  (put 'fexpr tag '((1 1 lambda-list) (0 t 1)))
  ;;(put 'if tag 'if*)
  ;;(put 'label tag '((1 2 ((1 1 lambda-list) (0 t 1))) (0 t 1)))
  (put 'let-closed tag '((1 1 quote) (0 t 1)))
  (put 'lexpr tag '((1 1 lambda-list) (0 t 1)))
  (put 'macro tag '((1 1 lambda-list) (0 t 1)))
  (put 'nlambda tag '((1 1 lambda-list) (0 t 1)))
  (put '*throw tag 1)
  )

(let ((tag 'emacs-lisp-indent-hook))
  (put 'condition-case tag 2)
  (put 'defconst tag 2)
  (put 'save-excursion tag 0)
  (put 'save-restriction tag 0)
  (put 'save-window-excursion tag 0)
  (put 'setq-default tag 1)
  (put 'while tag 1)
  (put 'with-output-to-temp-buffer tag 1)
  )
