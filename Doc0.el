;; $Id: Doc0.el,v 1.1.2.2 1998/06/24 23:06:53 layer Exp $

(defvar current-local-map-var)

(defun generate-eli-documentation (input-file output-file)
  (switch-to-buffer "*foo*")
  (erase-buffer)

  (fi:scan-stack-mode)
  (toggle-read-only)
  (fi:definition-mode)
  (fi:common-lisp-mode)
  (fi:inferior-common-lisp-mode)
  (fi:franz-lisp-mode)
  (fi:lisp-listener-mode)
  (fi:emacs-lisp-mode)
  (fi:shell-mode)
  (fi:su-mode)
  (fi:telnet-mode)
  (fi:rlogin-mode)
  (fundamental-mode)

  (insert-file input-file)

  (beginning-of-buffer)

  (while (re-search-forward "^%%" nil t)
    (beginning-of-line)
;;;    (message "foo: %s" (save-excursion
;;;			 (buffer-substring (point)
;;;					   (progn (end-of-line) (point)))))
    (cond
     ((looking-at "^%%include \\(.*\\)")
      (message "default-directory is %s" default-directory)
      (let ((file (buffer-substring (match-beginning 1) (match-end 1))))
	(replace-match "")
	(message "inserting contents of %s" file)
	(insert-file-contents-indented file 4)))
     ((looking-at "^%% ")
      (let* ((verbose t)
	     (xx (re-search-forward "^%% \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
				    (save-excursion (end-of-line) (point))))
	     (var-string
	      (buffer-substring (match-beginning 1) (match-end 1)))
	     (mode-string
	      (when (match-beginning 2)
		(buffer-substring (match-beginning 2) (match-end 2))))
	     (var (intern var-string))
	     (mode (when (and mode-string (not (string= "" mode-string)))
		     (intern mode-string)))
	     (xmode-name
	      (when mode-string
		(if (string-match "\\(.*\\)-map" mode-string)
		    (substring mode-string (match-beginning 1)
			       (match-end 1)))))
	     (yy (progn (beginning-of-line)
			(re-search-forward
			 "^\\(%%\\|@@\\) \\([^ \t]+\\)[ \t]*\\([^ \t]+\\)?$"
			 (save-excursion (end-of-line) (point)))
			(replace-match "")))
	     (xfunc (and (fboundp var) (symbol-function var)))
	     (func (if (and xfunc
			    (consp xfunc)
			    (or (and (stringp (third xfunc))
				     (not
				      (eq 'interactive
					  (car (fourth xfunc)))))
				(and (not (stringp (third xfunc)))
				     (not (eq 'interactive
					      (car (third xfunc)))))))
		       "function"
		     "command"))
	     (line-pad 88))
	xx yy ;; get rid of compile warnings
	(cond
	 ((boundp var)
	  (let* ((val (symbol-value var))
		 (type (cond ((syntax-table-p val) "syntax-table")
			     ((keymapp val) "keymap")
			     (t "variable")))
		 (doc (or (frob-docstring
			   (documentation-property
			    var 'variable-documentation))
			  (error "no documentation available for %s" var))))
	    (insert
	     (variable-definition var (value-to-string val var) doc))))
	 ((fboundp var)
	  (setq current-local-map-var
	    (cond ((symbol-value mode))
		  (t nil)))
	  (let* ((xx (symbol-function var))
		 (xarglist (and (consp xx) (car (cdr xx))))
		 (arglist
		  (when xarglist
		    (mapconcat 'symbol-name xarglist " ")))
		 (key (when current-local-map-var
			(substitute-command-keys
			 (format "\\<current-local-map-var>\\[%s]" var)))))
	    (insert
	     (function-definition
	      var
	      func
	      arglist
	      (if key
		  (format "%s%s" key
			  (if (and key (null (string-match "M-x" key)))
			      (insert (format " in %s" xmode-name))))
		(format "M-x %s" var))
	      (or (frob-docstring (documentation var))
		  (error "no documentation available for %s" var))))))
	 (t (error "Variable %s is not bound or fbound" var)))))))

  (write-region (point-min) (point-max) output-file))

(defun insert-file-contents-indented (file indent)
  (insert-string
   (save-window-excursion
     (save-excursion
       (switch-to-buffer "*ifci*")
       (erase-buffer)
       (insert-file-contents file)
       (goto-char (point-min))
       (while (not (eobp))
	 (insert-string (make-string indent ? ))
	 (forward-line 1))
       (buffer-string)))))

(defun insert-doc-string (string)
  (insert "\n")
  (let* ((start (point))
	 (end (progn
		(insert string)
		(if (not (bolp)) (forward-line 1))
		(point)))
	 (lines (count-lines start end)))
    (goto-char start)
    (while (> lines 0)
      (insert "   ")
      (forward-line 1)
      (setq lines (- lines 1)))))

(defun frob-newlines (string)
  (let ((i 0)
	(max (length string))
	(res nil)
	c)
    (while (< i max)
      (if (= 10 (setq c (aref string i)))
	  (progn (setq res (cons ?\\ res))
		 (setq res (cons ?n res)))
	(setq res (cons c res)))
      (setq i (+ i 1)))
    (concat (nreverse res))))

(defun variable-definition (variable value description)
  (format "</pre><table border=\"0\" width=\"95%%\" cellpadding=\"0\" cellspacing=\"0\">
  <tr>
    <td width=\"75%%\"><strong><font face=\"Courier New\">%s</font></em></strong></td>
    <td width=\"20%%\"><strong>[Emacs variable]</strong></td>
  </tr>
  <tr>
    <td width=\"77%%\" colspan=\"2\"><strong>Initial value</strong>: %s</td>
  </tr>
</table>

<ul>
  <li><pre><font face=\"Times New Roman\"><big>%s</big></font></pre></li>
</ul><pre>"
	  variable value description))

(defun function-definition (name type arglist invoke-with description)
  (format "</pre><table border=\"0\" width=\"95%%\" cellpadding=\"0\" cellspacing=\"0\">
  <tr>
    <td width=\"75%%\"><strong><font face=\"Courier New\">%s</font></strong></td>
    <td width=\"20%%\"><strong>[Emacs %s]</strong></td>
  </tr>
  <tr>
    <td width=\"75%%\" colspan=\"2\"><strong>Arguments</strong>: %s</td>
  </tr>
</table>

<ul>
  <li>Invoke with %s.</li>
  <li><pre><font face=\"Times New Roman\"><big>%s</big></font></pre></li>
</ul><pre>"
	  name type arglist invoke-with description))

(defun value-to-string (value name)
  (cond ((syntax-table-p value) "")
	((keymapp value) (substitute-command-keys (format "\\{%s}" name)))
	(t (frob-newlines (fi::prin1-to-string value)))))

(defun frob-docstring (string)
  ;; remove leading *
  (if (= ?* (aref string 0))
      (substring string 1)
    string))
