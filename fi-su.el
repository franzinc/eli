;;
;; copyright (C) 1989, 1990 Franz Inc, Berkeley, Ca.
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

;; $Header: /repo/cvs.copy/eli/fi-su.el,v 1.3 1990/10/13 19:37:10 layer Exp $

(defvar fi:su-mode-map nil
  "The su major-mode keymap.")

(defvar fi:su-mode-super-key-map nil
  "Used for super-key processing in su mode.")

(defvar fi:remote-su-mode-map nil
  "The su major-mode keymap.")

(defvar fi:remote-su-mode-super-key-map nil
  "Used for super-key processing in su mode.")

(defvar fi:su-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "*Regexp used by Newline command in su mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defvar fi:su-initial-input "stty -echo nl tabs\n"
  "*The initial input sent to the su subprocess, after the first prompt
is seen.")

(defun fi:su-mode (&optional mode-hook)
  "Major mode for interacting with an inferior su."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:su-mode)
  (setq mode-name "Su")

  (if (null fi:su-mode-super-key-map)
      (progn
	(setq fi:su-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys fi:su-mode-super-key-map 'shell)))

  (if (null fi:su-mode-map)
      (setq fi:su-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:su-mode-super-key-map
				      'shell)))
  (use-local-map fi:su-mode-map)
  (setq fi:subprocess-super-key-map fi:su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:shell-mode-hook))

(defun fi:remote-su-mode (&optional mode-hook)
  "Major mode for interacting with an inferior su."
  (interactive)
  (kill-all-local-variables)
  (if mode-hook (funcall mode-hook))
  (setq major-mode 'fi:remote-su-mode)
  (setq mode-name "Remote Su")

  (if (null fi:remote-su-mode-super-key-map)
      (progn
	(setq fi:remote-su-mode-super-key-map (make-sparse-keymap))
	(fi::subprocess-mode-super-keys fi:remote-su-mode-super-key-map
					'rlogin)))

  (if (null fi:remote-su-mode-map)
      (setq fi:remote-su-mode-map
	(fi::subprocess-mode-commands (make-sparse-keymap)
				      fi:remote-su-mode-super-key-map
				      'rlogin)))
  (use-local-map fi:remote-su-mode-map)
  (setq fi:subprocess-super-key-map fi:remote-su-mode-super-key-map)
  (run-hooks 'fi:subprocess-mode-hook 'fi:rlogin-mode-hook))

(defun fi:su (&optional buffer-number)
  "Start an su in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*root'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*root-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen."
  (interactive "p")
  (fi::make-subprocess buffer-number "root" 'fi:su-mode
		       fi:su-prompt-pattern
		       (format "%senv" exec-directory)
		       (list "TERM=dumb" "su")
		       'fi::su-filter))

(defun fi:remote-root-login (&optional buffer-number host)
  "Start an su in a buffer whose name is determined from the optional
prefix argument BUFFER-NUMBER.  Shell buffer names start with `*root'
and end with `*', with an optional `-N' in between.  If BUFFER-NUMBER is
not given it defaults to 1.  If BUFFER-NUMBER is >= 0, then the buffer is
named `*root-<BUFFER-NUMBER>*'.  If BUFFER-NUMBER is < 0, then the first
available buffer name is chosen."
  (interactive "p\nsRemote host: ")
  (fi::make-subprocess buffer-number (format "root-%s" host)
		       'fi:remote-su-mode fi:su-prompt-pattern
		       (format "%senv" exec-directory)
		       (list "TERM=dumb" "rlogin" host "-l" "root")
		       'fi::su-filter))

(defun fi::su-filter (process output)
  "Filter for `fi:su' subprocess buffers.
Watch for the first shell prompt from the su, then send the
string bound to fi:su-initial-input, and turn ourself off."
  (let ((old-buffer (fi::subprocess-filter process output t)))
    (cond
     ((string-match "assword" output)
      (setq password (fi::read-password))
      (fi::send-string-split process (concat password "\n") nil))
     (t
      (if (save-excursion (beginning-of-line)
			  (looking-at subprocess-prompt-pattern))
	  (progn
	    (set-process-filter process 'fi::subprocess-filter)
	    (if fi:su-initial-input
		(fi::send-string-split process fi:su-initial-input nil))))
      (if old-buffer (set-buffer old-buffer))))))
