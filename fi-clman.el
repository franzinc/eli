;;
;; copyright (C) 1987, 1988, 1989, 1990, 1991 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/Attic/fi-clman.el,v 1.16 1991/04/22 16:12:57 layer Exp $

(defun fi::setup-default-clman-package-info ()
  ;;  Returns a list that 
  (mapcar (function
	   (lambda (xxx)
	     (let* ((p load-path)
		    (string "fi/manual/")
		    (done nil)
		    (res nil))
	       (while (and (not done) p)
		 (if (file-exists-p
		      (setq res (concat (file-name-as-directory (car p))
					string)))
		     (setq done t)
		   (setq res nil))
		 (setq p (cdr p)))
	       (rplaca (cdr xxx) (format "%s%s" res (car (cdr xxx))))
	       xxx)))
	    
	  '(("about" "about/")
	    ("clos" "clos/")
	    ("compiler" "compiler/")
	    ("cond" "cond/")
	    ("excl" "excl/")
	    ("foreign" "foreign/")
	    ("graph" "graph/")
	    ("inspect" "inspect/")
	    ("mp" "mp/")
	    ("sys" "sys/")
	    ("tpl" "tpl/")
	    ("xcw" "xcw/")
	    ("xref" "xref") )))

(defvar fi:clman-package-info 
    (fi::setup-default-clman-package-info)
  "*A list of (PRODUCT DIRECTORY) which tells where the manual pages are (in
DIRECTORY) for PRODUCT.")

(defvar fi:clman-mode-map nil
  "*Major mode key definitions for viewing a clman page.")

(defvar fi:clman-displaying-function 'fi:clman-find-file
  "*This function will be funcalled with two arguments, the .doc file to be
displayed, and the buffer which is the value of fi:clman-displaying-buffer.
If you wish, you can set this variable to your own displaying function.")

(defvar fi:clman-displaying-buffer "*CLMan*"
  "*Either nil or a string naming the buffer that the system will use for
displaying documentation pages.  If nil, then the system will not try to
reuse the same buffer.")

(defvar fi::clman-window-configuration nil)

(defvar clman-debug nil)

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
  (if (or (not (boundp 'fi::clman-oblist))
	  (null fi::clman-oblist)
	  (null fi:clman-package-info))
      (fi::setup-clman-oblist))
  (setq fi::clman-window-configuration (current-window-configuration))
  (let* ((doc-page nil)(sym nil)
         (done nil)(found-it nil)(pack nil)(sym-pack-pair nil)
	 (pack-dir nil)(info-item nil))
    (setq sym (or (and symbol
		       (setq symbol 
			 (fi::clman-check-for-package symbol)))
		  (fi::clman-get-sym-to-lookup)))
    (if (not sym)
	(setq sym-pack-pair (list "" ""))
      (setq  sym-pack-pair (fi::clman-strip-leading-package-name sym)))
    (setq pack (car  sym-pack-pair))
    (setq sym (car (cdr  sym-pack-pair)))
    (setq info-item (assoc pack fi:clman-package-info))
    ;; they did not specify a package
    (if (not info-item)
	(retrieve-doc-page-no-pack-specified) 
      (retrieve-doc-page-yes-pack-specified info-item))))

(defun fi:clman-apropos ()
  "Prompts for a string on which an apropos search is done.  Displays a
buffer which lists all documented symbols which match the string.  The
buffer will be in CLMAN mode."
  (interactive)
  (if (null fi:clman-package-info)
      (fi::setup-clman-oblist))
  (let* ((string (downcase (read-string "clman apropos: ")))
	 (apropos-buffer-name "*CLMan-Apropos*"))
    (with-output-to-temp-buffer apropos-buffer-name
      (let ((package-name-list (fi:clman-package-nicknames))
	    (item nil)(package nil))
	(while package-name-list
	  (setq package (car package-name-list))
	  (setq lis (eval 
		     (car (read-from-string 
			   (concat "fi::clman-" package "-oblist")))))
	  (let ((item nil))
	    (while lis
	      (setq item (car (car lis)))
	      (if (string-match string item)
		  (progn (prin1 (concat package ":" item))
			 (princ "\n")))
	      (setq lis (cdr lis))))
	  (setq package-name-list (cdr package-name-list)))))
      
    (switch-to-buffer-other-window apropos-buffer-name)
    (replace-string "\"" "")
    (fi:clman-mode)(goto-char (point-min))))

(defun fi:clman-mode ()
  "Major mode for viewing Allegro manual pages.  text-mode-syntax-table and
text-mode-abbrev-table are `used' in this mode.
\\{fi:clman-mode-map}"
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
	(beginning-of-line))))

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

(defun fi:clman-package-help ()
  (interactive)
  (with-output-to-temp-buffer "*CLMAN-NICKNAMES"
    (princ  "The following is a list of all the")
    (princ "\n")
    (princ  "CLMAN packages for which man pages exist.")
    (princ "\n")
    (princ  "You can type one of them to the CLMAN prompt to narrow")
    (princ "\n")
    (princ  "the scope of a search to a particular \"package\"")
    (princ "\n")
    (let ((lis (fi:clman-package-nicknames)))
      (while lis
	(prin1 (car (read-from-string (car lis))))
	(princ "\n")
	(setq lis (cdr lis)))
      (save-excursion 
	(switch-to-buffer "*CLMAN-NICKNAMES")
	(fi:clman-mode))
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Internal stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retrieve-doc-page-no-pack-specified ()
  (let ((temp-info fi:clman-package-info (info-item nil))
	(info-item nil)(named nil))
    (while (not done) 
      (setq info-item (car temp-info))
      (if (not info-item)
	  ;; That's it. We checked them all.
	  (progn (setq done t)
		 (message "Couldn't find the doc page for %s " sym))
	(let ((pack (car info-item))
	      (doc-dir (car (cdr info-item))))
	  (setq named (fi::clman-file-nameify 
		       pack
		       (fi::clman-downcase sym)))
	  (if (not named) 
	      nil			;ignore failures
	    (progn 
	      (setq doc-page (concat doc-dir named))
	      (if (file-exists-p doc-page)
		  (progn
		    (setq done t)
		    (fi::clman-display-file
		     doc-page fi:clman-displaying-buffer)
		    (setq found-it t)))))
	  (setq temp-info (cdr temp-info)))
	found-it))))

(defun retrieve-doc-page-yes-pack-specified (info-item)
  (let ((pack (car info-item))
	(doc-dir (car (cdr info-item)))
	(named nil))
    ;; If we do not generate and error, we will display
    ;;  doc page and return t
    (if (not (file-exists-p doc-dir ))
	(error "Directory %s not found" doc-dir))
    (if (not (setq named (fi::clman-file-nameify 
			  pack
			  (fi::clman-downcase sym))))
	(error "Could not file-nameify %s and %s"))	
    (setq doc-page (concat doc-dir named))
    (if (file-exists-p doc-page)
	(progn
	  (setq done t)
	  (fi::clman-display-file doc-page fi:clman-displaying-buffer)
	  (setq found-it t))
      (error  "No Doc Page for %s in %s" sym  pack))
    t))

(defun fi::clman-file-nameify (package str)
  (if (string= package "") (error "Specified package is NIL"))
  (if (string= package "") (error "Specified string is NIL"))
  (let* ((oblist-name  (concat "fi::clman-" package "-oblist"))
	 (oblist-symbol (car (read-from-string oblist-name)))
	 (oblist nil)
	 (new-string (fi::clman-string-clean str))
	 (file-name nil))
    (if (not (boundp oblist-symbol))
	nil
      (progn (setq oblist (eval oblist-symbol))
	     (setq file-name 
	       (car (cdr (tg:string-assoc  new-string oblist))))
	     file-name 
	     ))))

(defun fi::clman-downcase (str)
  (let ((index (string-match "~" str)))
    (if index 
	(progn
	  (setq saved-string (substring str index (length str)))
	  (concat (substring (downcase str)0 index) saved-string))
      (downcase str))))


(defun fi::clman-get-sym-to-lookup ()
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
    (setq ans  (fi::clman-check-for-package ans))))

(defun fi::clman-check-for-package (ans)
  (let ((pack nil)(answer nil))
    (if (setq pack  (assoc ans fi:clman-package-info))
	(progn 
	  (setq oblist-name 
	    (concat "fi::clman-" ans "-oblist"))
	  (setq answer (completing-read 
			(concat "Symbol (" ans ":): ") 
			(eval (car (read-from-string oblist-name)))))
	  (if (string= (substring answer 0 1) ":")
	      (setq answer (concat ":" answer)))
	  ;;  for keywords
	  (setq ans 
	    (concat ans ":" answer))
	  ans)
      ans)))

(defun fi::clman-strip-leading-package-name (str)
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
  (if clman-debug (with-output-to-temp-buffer "*clman-debug*"
		    (princ name)))
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
  (buffer-flush-undo (current-buffer))
  (insert-file name)
  ;; Get rid of garbage at the top of the buffer
  (beginning-of-buffer)(kill-line 5)
  (buffer-flush-undo (current-buffer)))

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
  (skip-chars-backward "[&a-zA-Z\-~:+*#0-9]")
  (let* ((begin (point)) end sym)
    (skip-chars-forward "[&a-zA-Z\-:+~*#0-9]")
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

(defun tg:string-assoc (string lis)
  (let ((lis1 lis)(item nil)(done nil)(result nil))
    (while (and lis1 (not done))
      (setq item (car lis1))
      (if (string= (car item) string)
	  (progn
	    (setq result item)(setq done t))
	)
      (setq lis1 (cdr lis1)))
    result))

(defun fi::clman-string-clean (string)
  (setq string (fi::clman-sub-chars-in-string '((?\  . ?-) 
						;; (?\# . ?H)
						)
					      string))
  (setq string (string-sub string "\*(Tl" "~"))
  string)

(defun string-sub (string old new)
  (let ((len (length string))
	(match (string-match old string)))
    (if match
	(setq result (concat
		      (substring string 0 match)
		      new
		      (substring string (+ match (length old)) len)))
      string)))




(defun fi:clman-package-nicknames ()
  (mapcar 'car fi:clman-package-info))

(defun fi::setup-clman-oblist ()
  ;;  Create the list FI:CLMAN-PACKAGE-INFO.
  ;;  If you defvar this variable in your .emacs file, the following
  ;;  setq will have no effect.

  ;; now load all the oblists and append them together
  (load-all-OBLIST-files)
  ;; Append OBLISTs together to make fi::clman-oblist
  (setq fi::clman-oblist (fi::append-OBLISTs-together)))

(defun load-all-OBLIST-files ()
  ;;  This will define a set of lists, pointed to by variables, on the
  ;;  pattern of fi:clman-<pack>-oblist
  (let ((doc-dirs (mapcar '(lambda (item)(car (cdr item)))
			  fi:clman-package-info)))
    (with-output-to-temp-buffer "OBLIST loading record"
      (while doc-dirs
	(let ((oblist-file (concat (car doc-dirs) "OBLIST")))
	  (if (file-exists-p oblist-file)
	      (load-file oblist-file)
	    (progn (princ (concat oblist-file " does not exist"))
		   (terpri)))
	  (setq doc-dirs (cdr doc-dirs)))))))

(defun fi::append-OBLISTs-together ()
  ;;  we take the names of all the oblists, and cons 
  ;;  up the list fi::clman-oblist, by appending all the 
  ;;  oblists together
  (let ((clman-package-nicknames nil)
	(clman-oblist-names nil))
    (setq  clman-package-nicknames
      (mapcar 'car fi:clman-package-info))
    (setq clman-oblist-names
      (let ((lis clman-package-nicknames)
	    (oblist-name)
	    (result nil))
	(while lis 
	  (setq oblist-name 
	    (car (read-from-string 
		  (concat "fi::clman-" (car lis) "-oblist"))))
	  (setq result (cons oblist-name result))
	  (setq lis (cdr lis)))
	result))

    (let 
	((result nil) (lis clman-oblist-names))
      (while lis
	(let ((oblist (car lis)))
	  (if (boundp oblist)
	      (setq result 
		(append (eval oblist) result))))
	(setq lis (cdr lis)))
      result)))
       



(defun fi:clman-help ()
  (with-output-to-temp-buffer "*CLMAN-NICKNAMES"
    (princ  "HERE ARE ALL THE PACKAGES")
    (princ "\n")
    (let ((lis   (fi:clman-package-nicknames)))
      (while lis
	(prin1 (car (read-from-string (car lis))))
	(princ "\n")
	(setq lis (cdr lis))))
    (save-excursion (switch-to-buffer  "*CLMAN-NICKNAMES")
		    (fi:clman-mode))))

(if fi:clman-mode-map
    nil
  (setq fi:clman-mode-map (make-keymap))
  (define-key fi:clman-mode-map "\C-C\C-C" 'fi:clman-flush-doc)
  (define-key fi:clman-mode-map "a" 'fi:clman-apropos)
  (define-key fi:clman-mode-map "m" 'fi:clman)
  (define-key fi:clman-mode-map "p" 'fi:clman-package-help)
  (define-key fi:clman-mode-map "s" 'fi:clman-search-forward-see-alsos)
  (define-key fi:clman-mode-map " " 'scroll-up)
  (define-key fi:clman-mode-map "n" 'fi:clman-next-entry))
