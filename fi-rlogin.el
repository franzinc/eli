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

;; $Header: /repo/cvs.copy/eli/fi-rlogin.el,v 1.4 1988/05/11 14:47:57 layer Exp $

(defvar fi:rlogin-mode-map nil
  "The rlogin major-mode keymap.")

(defvar fi:rlogin-mode-super-key-map nil
  "Used for super-key processing in rlogin mode.")

(defvar fi:rlogin-image-name "rlogin"
  "Default remote-login image to invoke from (fi:rlogin).")

(defvar fi:rlogin-image-arguments nil
  "Default remote-login image arguments when invoked from (fi:rlogin).")

(defvar fi:rlogin-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "Regexp used by Newline command in rlogin mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defun fi:rlogin-mode ()
  "Major mode for interacting with an inferior rlogin."
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'fi:rlogin-mode)
  (setq mode-name "Rlogin")

  (if (null fi:rlogin-mode-super-key-map)
      (let ((map (make-sparse-keymap)))
	(setq map (fi::subprocess-mode-super-keys map 'rlogin))
	(define-key map "\C-z"	'fi:rlogin-send-stop)
	(define-key map "\C-c"	'fi:rlogin-send-interrupt)
	(define-key map "\C-d"	'fi:rlogin-send-eof)
	(define-key map "\C-\\"	'fi:rlogin-send-quit)
	(setq fi:rlogin-mode-super-key-map map)))

  (if (null fi:rlogin-mode-map)
      (setq fi:rlogin-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:rlogin-mode-super-key-map
				      'rlogin)))
  (use-local-map fi:rlogin-mode-map)
  (setq fi:subprocess-super-key-map fi:rlogin-mode-super-key-map)
  (setq fi:shell-popd-regexp nil)
  (setq fi:shell-pushd-regexp nil)
  (setq fi:shell-cd-regexp nil)
  (run-hooks 'fi:subprocess-mode-hook 'fi:rlogin-mode-hook))

(defun fi:rlogin (&optional buffer-number host)
  (interactive "p\nsRemote login to host: ")
  (let ((proc
	 (fi::make-subprocess
	  buffer-number host 'fi:rlogin-mode
	  fi:rlogin-prompt-pattern fi:rlogin-image-name
	  (append (list host) fi:rlogin-image-arguments))))
    (set-process-filter proc 'fi::rlogin-filter)))

(defun fi:explicit-rlogin (&optional buffer-number host
				     image-name image-arguments)
  (interactive
   "p\nsRemote login to host: \nsImage name: \nxImage arguments (a list): ")
  (let ((proc
	 (fi::make-subprocess
	  buffer-number host 'fi:rlogin-mode
	  fi:rlogin-prompt-pattern image-name
	  (append (list host) image-arguments))))
    (set-process-filter proc 'fi::rlogin-filter)))

(defun fi:rlogin-send-eof ()
  "Send eof to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-d"))

(defun fi:rlogin-send-interrupt ()
  "Send interrupt to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-c"))

(defun fi:rlogin-send-quit ()
  "Send quit to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-\\"))

(defun fi:rlogin-send-stop ()
  "Send stop to process running through remote login subprocess buffer."
  (interactive)
  (send-string (get-buffer-process (current-buffer)) "\C-z"))

(defun fi::rlogin-filter (process output)
  "Filter for `fi:rlogin' subprocess buffers.
Watch for the first shell prompt from the remote login, then send the string
\"stty -echo nl\", and turn ourself off."
  (let ((old-buffer (fi::subprocess-filter process output t)))
    (if (save-excursion (beginning-of-line)
			(looking-at subprocess-prompt-pattern))
	(progn
	  (set-process-filter process 'fi::subprocess-filter)
	  (fi::send-string-split process "stty -echo nl\n" nil)))
    (if old-buffer
	(set-buffer old-buffer))))
