(defvar fi:rlogin-mode-map nil
  "The rlogin major-mode keymap.")

(defvar fi:rlogin-mode-super-key-map nil
  "Used for super-key processing in rlogin mode.")

(defvar fi:explicit-rlogin-file-name nil
  "Explicit remote-login image to invoke from (fi:rlogin).")

(defvar fi:explicit-rlogin-image-arguments nil
  "Explicit remote-login image arguments when invoked from (fi:rlogin).")

(defvar fi:default-rlogin-file-name "rlogin"
  "Default remote-login image to invoke from (fi:rlogin).")

(defvar fi:default-rlogin-image-arguments nil
  "Default remote-login image arguments when invoked from (fi:rlogin).")

(defvar fi:rlogin-prompt-pattern
  "^[-_.a-zA-Z0-9]*[#$%>] *"
  "Regexp used by Newline command in rlogin mode to match subshell prompts.
Anything from beginning of line up to the end of what this pattern matches
is deemed to be prompt, and is not re-executed.")

(defun fi:rlogin-mode ()
  "Major mode for interacting with an inferior rlogin.

This mode is essentially the same as `fi:shell-mode' except that:
the working directory is not tracked (in the presence of NFS mounted file
systems doing this would present problems unless the mount points where the
same on the local and remote machines) and interrupt, end-of-file, quit and
stop actions are handled specially.

The local keymap for this mode is bound to `fi:rlogin-mode-map' and
super-keys are obtained from `fi:rlogin-mode-super-key-map'.  The prompt
pattern is taken from `fi:rlogin-prompt-pattern'.

Entry to this mode applies the values of `fi:subprocess-mode-hook' and
`fi:rlogin-mode-hook', in this order and each with no args, if their values
are the names of functions."
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

(defun fi:rlogin (host)
  "Run an inferior remote login, with input/output through buffer *<host>*.
See `fi::make-process'.  Hook function `rlogin-subprocess-hook' will be
applied in the buffer if defined."
  (interactive "sRemote login to host: \n")
  (fi::make-process "rlogin" host 'fi:rlogin-mode nil nil (list host))
  (set-process-filter (get-buffer-process (current-buffer))
		      'fi::rlogin-filter))

(defun fi:another-rlogin (host)
  "Run a new remote login, with input/output through buffer *<host>-N*.
This function always creates a new subprocess and buffer.  See
`fi::make-process'. Hook function `rlogin-subprocess-hook' will be applied in
the newly-created buffer if defined."
  (interactive "sRemote login to host: \n")
  (fi::make-process "rlogin" host 'fi:rlogin-mode nil t (list host))
  (set-process-filter (get-buffer-process (current-buffer))
		      'fi::rlogin-filter))

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
