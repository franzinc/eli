;;
;; copyright (C) 1988, 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-telnet.el,v 1.8 1991/06/19 22:16:23 layer Exp $

(defvar fi:telnet-mode-map nil
  "The telnet major-mode keymap.")

(defvar fi:telnet-mode-super-key-map nil
  "Used for super-key processing in telnet mode.")

(defvar fi:telnet-image-name "telnet"
  "*Default telnet image to invoke from FI:TELNET.  If the value
is a string then it names the image file or image path that
FI:TELNET invokes.  Otherwise, the value of this variable is given
to funcall, the result of which should yield a string which is the image
name or path.")

(defvar fi:telnet-image-arguments nil
  "*Default telnet image arguments when invoked from FI:TELNET.")

(defvar fi:telnet-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in telnet mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:telnet-initial-input "stty -echo nl\n"
  "*The initial input sent to the telnet subprocess, after the first prompt
is seen.")

(defun fi:telnet-mode (&optional mode-hook)
  "Major mode for interacting with an inferior telnet.
The keymap for this mode is bound to fi:telnet-mode-map:
\\{fi:telnet-mode-map}
Entry to this mode runs the following hooks:

	fi:subprocess-mode-hook
	fi:telnet-mode-hook

in the above order.

When calling from a program, argument is MODE-HOOK,
which is funcall'd just after killing all local variables but before doing
any other mode setup."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:telnet-mode)
  (setq mode-name "Telnet")

  (if (null fi:telnet-mode-super-key-map)
      (let ((map (make-keymap)))
	(setq map (fi::subprocess-mode-super-keys map 'rlogin))
	(define-key map "m"	'fi:telnet-start-garbage-filter)
	(setq fi:telnet-mode-super-key-map map)))

  (if (null fi:telnet-mode-map)
      (setq fi:telnet-mode-map
	(fi::subprocess-mode-commands (make-keymap)
				      fi:telnet-mode-super-key-map
				      'telnet)))
  (use-local-map fi:telnet-mode-map)
  (setq fi:subprocess-super-key-map fi:telnet-mode-super-key-map)
  (setq fi:shell-popd-regexp nil)
  (setq fi:shell-pushd-regexp nil)
  (setq fi:shell-cd-regexp nil)
  (run-hooks 'fi:subprocess-mode-hook 'fi:telnet-mode-hook))

(defun fi:telnet (&optional buffer-number host)
  "Start an telnet in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER and the HOST.  Telnet buffer names start with
`*HOST*' and end with an optional \"<N>\".  If BUFFER-NUMBER is not given
it defaults to 1.  If BUFFER-NUMBER is 1, then the trailing \"<1>\" is
omited.  If BUFFER-NUMBER is < 0, then the first available buffer name is
chosen (a buffer with no process attached to it.

The host name is read from the minibuffer.

The telnet image file and image arguments are taken from the variables
`fi:telnet-image-name' and `fi:telnet-image-arguments'."
  (interactive "p\nsTelnet to host: ")
  (let ((fi:subprocess-env-vars
	 '(("EMACS" . "t")
	   ("TERM" . "dumb")
	   ("DISPLAY" . (getenv "DISPLAY")))))
    (fi::make-subprocess nil
			 host
			 buffer-number
			 default-directory
			 'fi:telnet-mode
			 fi:telnet-prompt-pattern
			 fi:telnet-image-name
			 (cons host fi:telnet-image-arguments)
			 'fi::telnet-filter)))

(defun fi:telnet-start-garbage-filter ()
  "Start a filter that removes ^M's at the end of lines." 
  (interactive)
  (set-process-filter (get-buffer-process (current-buffer))
		      'fi::telnet-garbage-filter))

(defun fi::telnet-filter (process output)
  "Filter for `fi:telnet' subprocess buffers.
Watch for the first shell prompt from the telnet, then send the
string bound to fi:telnet-initial-input, and turn ourself off."
  (let ((old-buffer (fi::subprocess-filter process output t)))
    (cond
     ((string-match "assword" output)
      (setq password (fi::read-password))
      (send-string process (concat password "\n")))
     (t
      (if (save-excursion (beginning-of-line)
			  (looking-at subprocess-prompt-pattern))
	  (progn
	    (set-process-filter process 'fi::subprocess-filter)
	    (send-string process fi:telnet-initial-input)))
      (if old-buffer (set-buffer old-buffer))))))

(defun fi::telnet-garbage-filter (process output)
  "Filter for telnet subprocess buffers when \"stty nl\" doesn't cause
those nasty ^M's to go away."
  (let ((old-buffer (fi::subprocess-filter process output t t)))
    (if old-buffer (set-buffer old-buffer))))
