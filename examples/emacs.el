;; Sample .emacs file
;;
;; $Header: /repo/cvs.copy/eli/examples/emacs.el,v 1.1 1991/08/23 10:37:15 layer Exp $

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The following code implements selection of a particular version of the
;; emacs-lisp interface on the command line of emacs.
;;
;; Edit EMACS-INTERFACE-DIRECTORY-VERSION-ALIST (below) appropriately, and
;; you will be able to start emacs as:
;;
;;   % emacs -1.5.2
;;
;; and run ACL 3.1 with the 1.5.2 version of the emacs-lisp interface.

(defvar emacs-interface-directory-version-alist
    '(("-1.5.2" . "/net/ice/usr/emacs/lisp-interface/Dists/dist-1.5.2"))
  "*An alist of the command line switch and directory in which fi/site-init
lives for that version.")

(defun handle-command-line-argument (arg function)
  (if (member-equal arg command-line-args)
      (progn
	(setq command-switch-alist
	  (cons (cons arg 'identity) command-switch-alist))
	(or (funcall function arg) t))))

(defun member-equal (item list)
  "same as common lisp (member item list :test #'equal)"
  (let ((ptr list)
        (done nil)
        (result '()))
    (while (not (or done (atom ptr)))
      (cond ((equal item (car ptr))
             (setq done t)
             (setq result ptr)))
      (setq ptr (cdr ptr)))
    result))

(defun eli-version-switch-handler (version)
  (let ((dir (cdr (assoc version emacs-interface-directory-version-alist))))
    (if (null dir) (error "No directory for version %s" version))
    (if (not (file-exists-p dir)) (error "%s doesn't exist" dir))
    (setq load-path (cons dir load-path))))

(cond
 ((handle-command-line-argument "-1.5.2" 'eli-version-switch-handler)
  ;; variables for 1.5.2...
  )
 (t
  ;; variables for default version
  ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "fi/site-init")
