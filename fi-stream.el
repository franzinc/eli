;;
;; copyright (C) 1991 Franz Inc, Berkeley, Ca.
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
;;
;; $Header: /repo/cvs.copy/eli/fi-stream.el,v 1.1 1991/01/29 15:24:57 layer Exp $
;;

(defun lep::display-string-in-buffer (string buffer)
  (switch-to-buffer (get-buffer-create buffer))
  (erase-buffer)
  (insert string))


(defun lep::create-listener-stream ()
  (let ((fi::listener-protocol ':stream))
    (send-string (fi:tcp-common-lisp -1)
		 (format "%d\n" (session-id session)))))
