;; Copyright (c) 1987-1993 Franz Inc, Berkeley, Ca.
;;
;; Permission is granted to any individual or institution to use, copy,
;; modify, and distribute this software, provided that this complete
;; copyright and permission notice is maintained, intact, in all copies and
;; supporting documentation.
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.

;; $Header: /repo/cvs.copy/eli/fi-changes.el,v 1.12 1993/08/12 23:45:04 layer Exp $
;;
;; Support for changed definitions

(defvar fi:change-definitions-since-default
    'last-compile-or-eval
  "*The value of this variable is used as the default SINCE
argument to the changed-definition commands.  The value must be one of the
symbols file-first-read, buffer-save or last-compile-or-eval, which
correspond to SINCE arguments of 1, 2 and 3.")

(defun fi::change-definition-convert-prefix-argument ()
  (list
   (if (null current-prefix-arg)
       (cond ((eq 'file-first-read fi:change-definitions-since-default) 1)
	     ((eq 'buffer-save fi:change-definitions-since-default) 2)
	     ((eq 'last-compile-or-eval fi:change-definitions-since-default) 3)
	     ((eq 'comma-zero fi:change-definitions-since-default) 4)
	     (t (error "bad value of fi:change-definitions-since-default")))
     current-prefix-arg)))

(defun fi:list-buffer-changed-definitions (since)
  "List the definitions in the current buffer which have been added,
deleted or changed since it was first read by Common Lisp.  When prefix arg
SINCE is 2, list changes since the current buffer was last saved, or when
SINCE is 3, list the changes since the changed definitions in the current
buffer were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':list since))

(defun fi:list-changed-definitions (since)
  "List the definitions in all buffers which have been added,
deleted or changed since they were first read by Common Lisp.  When prefix
arg SINCE is 2, list changes since they were last saved, or when
SINCE is 3, list the changes since the changed definitions in all buffers
were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':list since t))

(defun fi:eval-buffer-changed-definitions (since)
  "Eval the definitions in the current buffer which have been added or
changed since it was first read by Common Lisp.  When prefix arg
SINCE is 2, eval changes since the current buffer was last saved, or when
SINCE is 3, eval the changes since the changed definitions in the current
buffer were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':eval since))

(defun fi:eval-changed-definitions (since)
  "Eval the definitions in all buffers which have been added or
changed since they were first read by Common Lisp.  When prefix
arg SINCE is 2, eval changes since they were last saved, or when
SINCE is 3, eval the changes since the changed definitions in all buffers
were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':eval since t))

(defun fi:compile-buffer-changed-definitions (since)
  "Compile the definitions in the current buffer which have been added or
changed since it was first read by Common Lisp.  When prefix arg
SINCE is 2, compile changes since the current buffer was last saved, or when
SINCE is 3, compile the changes since the changed definitions in the current
buffer were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':compile since))

(defun fi:compile-changed-definitions (since)
  "Compile the definitions in all buffers which have been added or
changed since they were first read by Common Lisp.  When prefix
arg SINCE is 2, compile changes since they were last saved, or when
SINCE is 3, compile the changes since the changed definitions in all buffers
were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':compile since t))

(defun fi:copy-buffer-changed-definitions (since)
  "Copy into the kill ring the definitions in the current buffer which have
been added or changed since it was first read by Common Lisp.  When prefix arg
SINCE is 2, copy changes since the current buffer was last saved, or when
SINCE is 3, copy the changes since the changed definitions in the current
buffer were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':copy since))

(defun fi:copy-changed-definitions (since)
  "Copy into the kill ring the definitions in all buffers which have been
added or changed since they were first read by Common Lisp.  When prefix
arg SINCE is 2, copy changes since they were last saved, or when
SINCE is 3, copy the changes since the changed definitions in all buffers
were last evaluated."
  (interactive (fi::change-definition-convert-prefix-argument))
  (fi::do-buffer-changed-definitions ':copy since t))

(defun fi:compare-source-files (new-file old-file)
  "Compare two files, NEW-FILE and OLD-FILE, listing the definitions in the
in NEW-FILE which have been added, deleted or changed with respect to
OLD-FILE."
  (interactive "fNew file: \nfOld file: ")
  (find-file new-file)
  (let ((package fi:package))
    (fi::make-request
	(scm::list-changed-definitions
	 :transaction-directory fi:emacs-to-lisp-transaction-directory
	 :operation ':list
	 :old-file old-file
	 :new-file new-file)
     ((package) (changes)
      (if changes
	  (fi::show-changes changes nil package)
	(message "There are no changes.")))
     (() (error)
      (error "Cannnot list changed definitions: %s" error)))))

;;; The guts of the problem

(defun fi::do-buffer-changed-definitions (operation since
					  &optional all-buffers)
  (message "Computing changes...")
  (setq since (fi::convert-since-prefix since))
  (let ((buffer (current-buffer))
	(copy-file-name (and (eq operation ':copy)
			     (format "%s/%s.cl"
				     fi:emacs-to-lisp-transaction-directory
				     (make-temp-name "EtoL"))))
	(package fi:package))
    (if all-buffers
	(let ((args nil))
	  (save-excursion
	    (dolist (buffer (buffer-list))
	      (set-buffer buffer)
	      (if (fi::check-buffer-for-changes-p since)
		  (push (fi::compute-file-changed-values-for-current-buffer)
			args))))
	  (if args
	      (apply (function fi::do-buffer-changed-definitions-1)
		     copy-file-name
		     (fi::transpose-list args))
	    (message "There are no changes.")))
      (if (fi::check-buffer-for-changes-p since)
	(apply
	 (function fi::do-buffer-changed-definitions-1)
	 copy-file-name
	 (fi::compute-file-changed-values-for-current-buffer))
	(message "There are no changes.")))))

(defun fi::check-buffer-for-changes-p (since)
  "Decide whether this buffer is worth checking for changes"
  (and (eq major-mode 'fi:common-lisp-mode)
       (buffer-file-name)
       (ecase since
	 (:comma-zero
	  (file-exists-p (concat (buffer-file-name) unlock-file-suffix)))
	 (:read
	  (or (buffer-modified-p)
	      buffer-backed-up))
	 (:saved
	  (buffer-modified-p))
	 (:eval
	  ;; Its like this buffer needs an every-modified-flag
	  t))))

(defun fi::compute-file-changed-values-for-current-buffer ()
  (let ((actual-file (buffer-file-name))
	(old-file 
	 (case since
	   (:comma-zero
	    (concat (buffer-file-name) unlock-file-suffix))
	   (t
	   (if (and (not (eq since ':saved))
		    buffer-backed-up)
	       (car (find-backup-file-name (buffer-file-name)))
	     (buffer-file-name)))))
	(new-file (buffer-file-name)))
    (list actual-file old-file new-file)))

(defun fi::do-buffer-changed-definitions-1 (copy-file-name actual-file
					    old-file new-file)
  (fi::make-request
   (scm::list-changed-definitions
    :operation operation
    :copy-file-name copy-file-name
    :actual-file actual-file
    :old-file old-file
    :new-file new-file
    :since since)
   ((operation copy-file-name) (changes)
    (if changes
	(progn
	  (if (eq operation ':copy)
	      (fi::insert-file-contents-into-kill-ring
	       copy-file-name))
	  (fi::show-changes changes))
      (message "There are no changes.")))
   ((operation) (error)
    (error 		 
     (ecase operation
       (:copy "copy changed definitions: %s")
       (:list "Cannnot list changed definitions: %s")
       (:eval "Cannnot evaluate changed definitions: %s")
       (:compile "Cannnot compile changed definitions: %s"))
     error))))

(defun fi::show-changes (changes &optional buffer-name package)
  (lep:display-some-definitions (or package fi:package)
				changes
				(list 'lep::find-buffer-definition)
				(or buffer-name "*changes*")))

(defun fi::convert-since-prefix (since)
  (ecase since
    (1 ':read)
    (2 ':saved)
    (3 ':eval)
    (4 ':comma-zero)))
