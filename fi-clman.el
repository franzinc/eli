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

;; $Header: /repo/cvs.copy/eli/Attic/fi-clman.el,v 1.7 1989/07/11 18:20:33 layer Exp $

(defvar fi:clman-package-info nil
  "A list of (PRODUCT DIRECTORY) which tells where the manual pages are (in
DIRECTORY) for PRODUCT.")

(if (not (boundp 'fi::clman-oblist)) (load "fi/clman.data"))

(defvar fi:clman-mode-map nil
  "Major mode key definitions for viewing a clman page.")

(defvar fi:clman-displaying-function 'fi:clman-find-file
  "This function will be funcalled with two arguments, the .doc file to be
displayed, and the buffer which is the value of fi:clman-displaying-buffer.
If you wish, you can set this variable to your own displaying function.")

(defvar fi:clman-displaying-buffer "*CLMan*"
  "Either nil or a string naming the buffer that the system will use for
displaying documentation pages.  If nil, then the system will not try to
reuse the same buffer.")

(defvar fi::clman-window-configuration nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Interactive Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi:clman (&optional symbol)
  "Look up SYMBOL in the online manual, with completion.  The optional
argument SYMBOL is prompted for in the minibuffer, if not supplied.  As a
guess, the system uses the symbol immediately to the left of the text
cursor.  To get completion for a symbol in a package other than the :lisp
package, use the nickname of that package, followed by a colon (e.g. cw: or
math:).  The buffer that is displayed will be in CLMAN mode."
  (interactive)
  (if (null fi:clman-package-info)
      (setq fi:clman-package-info (fi:setup-clman-package-info)))
  (setq fi::clman-window-configuration (current-window-configuration))
  (let* ((temp-info fi:clman-package-info)(package nil)
         (doc-page nil)(sym nil)
         (done nil)(found-it nil)(pack nil)(sym-pack-pair nil)
	 (pack-dir nil))
    (setq sym (or symbol (fi::clman-get-sym-to-lookup)))
    (if (not sym)
	(setq sym-pack-pair (list "" ""))
      (setq  sym-pack-pair (fi::clman-strip-leading-package-name sym)))
    (setq pack (car  sym-pack-pair))
    (setq sym (car (cdr  sym-pack-pair)))
    (setq packdir (car (cdr (assoc pack fi:clman-package-info))))
    ;; they did not specify a package
    (if (not packdir)
	(progn 
	  (while (not done) 
	    (setq package (car temp-info))
	    (if (not package)
		(progn (setq done t)
		       (message "Couldn't find the doc page for %s " sym))
	      (progn
		(setq doc-page
		  (concat (car (cdr package))
			  (fi::clman-file-nameify (fi::clman-downcase sym)))))
	      (if (file-exists-p doc-page)
		  (progn
		    (setq done t)
		    (fi::clman-display-file
		     doc-page fi:clman-displaying-buffer)
		    (setq found-it t))
		(setq temp-info (cdr temp-info)))
	      found-it)))
      ;; they did specify a package
      (progn
	(setq doc-page
	  (concat  packdir
		   (fi::clman-file-nameify (fi::clman-downcase sym))))
	(if (file-exists-p doc-page)
	    (progn
	      (setq done t)
	      (fi::clman-display-file doc-page fi:clman-displaying-buffer)
	      (setq found-it t)))
	found-it))))

(defun fi:clman-apropos ()
  "Prompts for a string on which an apropos search is done.  Displays a
buffer which lists all documented symbols which match the string.  The
buffer will be in CLMAN mode."
  (interactive)
  (if (null fi:clman-package-info)
      (setq fi:clman-package-info (fi:setup-clman-package-info)))
  (let* ((string (downcase (read-string "clman apropos: ")))
	 (apropos-buffer-name "*CLMan-Apropos*"))
    (with-output-to-temp-buffer apropos-buffer-name
      (let ((done nil) (lis fi::clman-oblist)
	    (item nil))
	(while lis
	  (setq item (car (car lis)))
	  (if (string-match string item)
	      (progn (prin1 item)
		     (princ "\n")))
	  (setq lis (cdr lis)))))
    (switch-to-buffer-other-window apropos-buffer-name)
    (replace-string "\"" "")
    (fi:clman-mode)(goto-char (point-min))))

(defun fi:clman-mode ()
  "Major mode for viewing Allegro manual pages.  text-mode-syntax-table and
text-mode-abbrev-table are `used' in this mode."
  (interactive)
  (set-syntax-table text-mode-syntax-table)
  (use-local-map fi:clman-mode-map)
  (setq local-abbrev-table text-mode-abbrev-table)
  (setq major-mode 'fi:clman-mode)
  (setq mode-name "CLMAN")
  (run-hooks 'text-mode-hook))

(defun fi:clman-search-forward-see-alsos ()
  "Move text cursor directly to the beginnig of the SEE ALSO section of a
clman buffer, from anywhere in the buffer."
  (interactive)
  (if (search-forward "SEE ALSO" nil t)
      (beginning-of-line)
    (if (search-backward "SEE ALSO" nil t)
	(beginning-of-line)))
  (recenter))

(defun fi:clman-next-entry ()
  (interactive)
  (if (search-forward "DESCRIPTION" nil t)
      (progn (beginning-of-line)(forward-line 1))
    (progn 
      (goto-char (point-min))
      (search-forward "DESCRIPTION" nil t)
      (beginning-of-line)(forward-line 1) )))

(defun fi:clman-flush-doc ()
  "Flush the current clman buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (set-window-configuration fi::clman-window-configuration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun fi::clman-downcase (str)
  (let ((index (string-match "~" str)))
    (if index 
	(progn
	  (setq saved-string (substring str index (length str)))
	  (concat (substring (downcase str)0 index) saved-string))
      (downcase str))))

(defun fi::clman-get-sym-to-lookup ()
  (interactive)		   
  (let* ((str nil)(sym nil)(ans nil)(pack nil))
    ;; make sure we have a symbol table
    ;; get a symbol to look up, if the user did not provide one
    (setq str (fi::clman-backward-copy-symbol-as-kill))
    (setq sym (if (or (string= str "")
		      (string= (substring str 0 1) "(")
		      (string= (substring str 0 1) "."))
		  nil (read-from-string str)))
    (if (listp (car sym))(setq str nil))
          
    (setq ans (completing-read  
	       (concat "Symbol (" str "): ") fi::clman-oblist))
    (if (string=  ans "")(setq ans str))
    (if (setq pack  (assoc ans fi:clman-package-info))
	(progn 
	  (setq oblist-name 
	    (concat "fi::clman-" ans "-oblist"))
	  (setq ans 
	    (concat ans ":"
		    (completing-read 
		     (concat "Symbol (" ans ":): ") 
		     (eval (car (read-from-string oblist-name))))))))
    ans))

(defun fi::clman-strip-leading-package-name (str)
  (interactive)
  ;; now this returns a list of the package and the symbol
  (let ((pos (string-match "[a-zA-Z]:" str)))
    (if (and pos (not (= pos 0)))
	;; There is a leading package qualifier
	(let ((first-string (substring str 0 (+ 1 pos)))
	      (second-string (substring str (+ 2 pos) (length str))))
	  (if (not (string= second-string ""))
	      (if (string= (substring second-string 0 1) ":")
		  (setq second-string 
		    (substring second-string 1 (length second-string)))))
	  (list first-string second-string))
      (list "" str))))


(defun fi::clman-retrieve-doc-page (str table doc-dir)
  "Retrieve the documentation page for the string argument, which is 
the name of a symbol that we want to look up. If the symbol is 
not found, you will be prompted for an alternate package. If you just
hit return, this function returns nil."
  (let ((name (fi::clman-man-page-lookup str table doc-dir)))
    ;; name is the full pathname of the doc page we want
    (bury-buffer)
    (if name name nil)))
		       
(defun fi::clman-display-file (name buf)
  "Display name, which is an clman .doc file according to a displaying
style. The displaying style is the value of the global var
fi:clman-displaying-function.  The two built in displaying functions are
'fi:clman-view-file, which uses 'view,  and fi:clman-find-file, which
inserts the .doc file into the buffer named by the value of the variable
fi:clman-displaying-buffer"
  ;; If buf is non-nil then we want to reuse the displaying buffer,
  ;; so have to erase it first
  (if buf
      (if (get-buffer buf)
	  (save-excursion 
	    (switch-to-buffer buf)
	    (erase-buffer))))
  (funcall fi:clman-displaying-function name buf)
  (fi:clman-mode))

(defun fi:clman-view-file (name buf)
  "A built-in function that you may use for the value of
fi:clman-displaying-function.  This function uses the function 'view-file."
  (view-file name))

(defun fi:clman-find-file (name buf)
  "A built-in function that you may use for the value of
fi:clman-displaying-function.  This function uses the function 'insert-file
to insert the file that is named by the first argument into the buffer
named by the second argument."
  (if (not (string=  buf (buffer-name(current-buffer))))
      (switch-to-buffer-other-window buf))
  (insert-file name))

(defun fi::clman-man-page-lookup (str table doc-dir)
  "Lookup  a string in the filename/symbol table.  The system used the
buffer which is named by the third element in clman-current-package-info.
Return the full pathname of the file the symbol is in. "
  (interactive)
  (switch-to-buffer table)
  (let ((buf (current-buffer))
	(new-str (concat " " str " "))
	(success nil))
    (beginning-of-buffer)
    (setq success (search-forward new-str (point-max) t))
    (if (not success) 
	nil
      (beginning-of-line)
      (setq begin (point))
      (search-forward " ")
      (backward-char)
      (concat doc-dir "/" (buffer-substring begin (point))))))

(defun fi::clman-backward-copy-sexp-as-kill ()
  (backward-sexp)
  (let* ((begin (point)) end sym)
    (forward-sexp)
    (fi::clman-remove-chars-from-string '(?\ ?\n)
					(buffer-substring begin (point)))))


(defun fi::clman-backward-copy-symbol-as-kill ()
  (skip-chars-backward "[a-zA-Z\-:+*0-9]")
  (let* ((begin (point)) end sym)
    (skip-chars-forward "[a-zA-Z\-:+*0-9]")
    (fi::clman-remove-chars-from-string '(?\ ?\n)
					(buffer-substring begin (point)))))

(defun fi::clman-escape-funny-chars (sym)
  ;; the shell requires that certain chars be preceded by \
  ;; and that entire command be surrounded by '  '
  (let ((temp sym)
	(star "*")
        (circumflex "^")
        (dollar "$")
	(result "")
        (leftbrack "\[")
        (rightbrack "\]")
        (quote "'")
        (backquote "`")
        (counter 1))
    (while (not (string= temp ""))
      (setq ch (substring temp 0 1))
      (if (or (string= ch star)
              (string= ch circumflex)
              (string= ch dollar)
              (string= ch leftbrack)
              (string= ch rightbrack)
              (string= ch quote)
              (string= ch backquote))
	  (setq result (concat result "\\" ch))
	(setq result (concat result ch)))
      (setq temp (substring temp 1)))
    (setq result (concat "\"" result "\""))))

(defun fi::clman-sub-chars-in-string (char-assoc-list string)
  "Substitute character pairs of CHAR-ASSOC-LIST in STRING."
  (let (pair)
    (mapconcat '(lambda (char)
		 (if (setq pair (assq char char-assoc-list))
		     (char-to-string (cdr pair))
		   (char-to-string char)))
	       string
	       nil)))

(defun fi::clman-remove-chars-from-string (char-list string)
  "Remove characters in CHAR-LIST from string STRING and return the result."
  (mapconcat '(lambda (char)
	       (if (memq char char-list)
		   nil
		 (char-to-string char)))
	     string
	     nil))

(defun fi::clman-file-nameify (str)
  (let ((result
         (fi::clman-sub-chars-in-string '((?* . ?S)(?~ . ?T)
					  (?< . ?L)(?> . ?G)
					  (?/ . ?D)(?& . ?A) (?: . ?C)
					  (?= . ?E)(?\\ . ?B)
					  (?$ . ?d)(?% . ?p)
					  (?\? . ?Q) (?\( . ?o)
					  (?\) . ?c)(?| . ?V)
					  (?^ . ?K)(?\[ . ?b)
					  (?\' . ?q)(?\" . ?Z)
					  (?\# . ?h)(?\` . ?b)
					  (?\; . ?s)(?- . ?H)
					  (?, . ?k)(?+ . ?a)
					  (?\. . ?e)(?\  . ?B))
					str)))
    (concat result ".doc")))

(defun fi::setup-clman-package-info ()
  (mapcar (function
	   (lambda (xxx)
	     (let* ((p load-path)
		    (string "fi/manual/")
		    (done nil)
		    res)
	       (while (and (not done) p)
		 (if (file-exists-p (setq res (concat (car p) "/" string)))
		     (setq done t)
		   (setq res nil))
		 (setq p (cdr p)))
	       (rplaca (cdr xxx) (format "%s%s" res (car (cdr xxx))))
	       xxx)))
	  '(("xcw" "xcw/")
	    ("math" "matrix/")
	    ("lisp" "cl/"))))

(if fi:clman-mode-map
    nil
  (setq fi:clman-mode-map (make-sparse-keymap))
  (define-key fi:clman-mode-map "\C-C\C-C" 'fi:clman-flush-doc)
  (define-key fi:clman-mode-map "a" 'fi:clman-apropos)
  (define-key fi:clman-mode-map "m" 'fi:clman)
  (define-key fi:clman-mode-map "s" 'fi:clman-search-forward-see-alsos)
  (define-key fi:clman-mode-map "n" 'fi:clman-next-entry))
