;;
;; copyright (C) 1987, 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-rlogin.el,v 1.19 1991/02/21 22:01:30 layer Exp $

(defvar fi:rlogin-mode-map nil
  "The rlogin major-mode keymap.")

(defvar fi:rlogin-mode-super-key-map nil
  "Used for super-key processing in rlogin mode.")

(defvar fi:rlogin-image-name "rlogin"
  "*Default remote-login image to invoke from (fi:rlogin).  If the value
is a string then it names the image file or image path that
`fi:rlogin' invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:rlogin-image-arguments nil
  "*Default remote-login image arguments when invoked from (fi:rlogin).")

(defvar fi:rlogin-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in rlogin mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:rlogin-initial-input "stty -echo nl\n"
  "*The initial input sent to the rlogin subprocess, after the first prompt
is seen.")

(defun fi:rlogin-mode (&optional mode-hook)
  "Major mode for interacting with an inferior rlogin.
The keymap for this mode is bound to fi:rlogin-mode-map:
\\{fi:rlogin-mode-map}
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:rlogin-mode-hook

in the above order."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:rlogin-mode)
  (setq mode-name "Rlogin")

  (if (null fi:rlogin-mode-super-key-map)
      (setq fi:rlogin-mode-super-key-map
	(fi::subprocess-mode-super-keys (make-sparse-keymap) 'rlogin)))

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
  "Start an rlogin in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*HOSTNAME'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*HOSTNAME-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen.

The host name is read from the minibuffer.

The image file and image arguments are taken from the variables
`fi:rlogin-image-name' and `fi:rlogin-image-arguments'.

See fi:explicit-shell."
  (interactive "p\nsRemote login to host: ")
  (let ((fi:subprocess-env-vars
	 '(("EMACS" . "t")
	   ("TERM" . "dumb")
	   ("DISPLAY" . (getenv "DISPLAY")))))
    (fi::make-subprocess buffer-number host 'fi:rlogin-mode
			 fi:rlogin-prompt-pattern
			 fi:rlogin-image-name
			 (cons host fi:rlogin-image-arguments)
			 'fi::rlogin-filter)))

(defun fi:explicit-rlogin (&optional buffer-number host
				     image-name image-arguments)
  "The same as fi:rlogin, except that the image and image arguments
are read from the minibuffer."
  (interactive
   "p\nsRemote login to host: \nsImage name: \nxImage arguments (a list): ")
  (let ((fi:subprocess-env-vars
	 '(("EMACS" . "t")
	   ("TERM" . "dumb")
	   ("DISPLAY" . (getenv "DISPLAY")))))
    (fi::make-subprocess buffer-number host 'fi:rlogin-mode
			 fi:rlogin-prompt-pattern
			 image-name
			 (cons host image-arguments)
			 'fi::rlogin-filter)))

(defun fi::rlogin-filter (process output)
  "Filter for `fi:rlogin' subprocess buffers.
Watch for the first shell prompt from the remote login, then send the
string bound to fi:rlogin-initial-input, and turn ourself off."
  (let ((old-buffer (fi::subprocess-filter process output t)))
    (if (save-excursion (beginning-of-line)
			(looking-at subprocess-prompt-pattern))
	(progn
	  (set-process-filter process 'fi::subprocess-filter)
	  (send-string process fi:rlogin-initial-input)))
    (if old-buffer
	(set-buffer old-buffer))))
