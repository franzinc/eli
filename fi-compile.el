;; $Id: fi-compile.el,v 2.5 1997/12/12 17:14:43 layer Exp $

(require 'cl)

(setq byte-compile-compatibility t)

(require 'font-lock) ;; makes some compiler warnings go away
(require 'comint) ;; makes some compiler warnings go away
(defvar mode-motion-hook) ;; makes xemacs compiler warning go away

(push (expand-file-name ".") load-path)

(setq fi::load-el-files-only t)
(load "fi-site-init")
(load "fi-leep0.el")

(setq fi-files
  '("Doc" "fi-vers" "fi-basic-lep" "fi-changes" "fi-composer" "fi-db"
    "fi-dmode" "fi-emacs18" "fi-emacs19" "fi-filec" "fi-gnu"
    "fi-indent" "fi-keys" "fi-leep0"
    "fi-lep" "fi-lze" "fi-modes" "fi-ring" "fi-rlogin" "fi-shell"
    "fi-stream" "fi-su" "fi-sublisp" "fi-subproc" "fi-telnet" "fi-utils"))

(setq fi-developer-files '("local-fi-developer-hooks"))

(dolist (file fi-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (unless (file-newer-than-file-p elc el)
      (message "--------------------------------------------------------")
      (byte-compile-file el))))

(dolist (file fi-developer-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (when (file-exists-p el)
      (unless (file-newer-than-file-p elc el)
	(message "--------------------------------------------------------")
	(byte-compile-file el)))))
