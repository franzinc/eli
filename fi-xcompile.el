;; $Id: fi-xcompile.el,v 1.1 1997/02/27 17:35:40 layer Exp $

(require 'cl)

(setq byte-compile-compatibility t)

(require 'font-lock) ;; makes some compiler warnings go away
(require 'comint) ;; makes some compiler warnings go away
(defvar mode-motion-hook) ;; makes xemacs compiler warning go away

(push (expand-file-name ".") load-path)

(setq fi::load-el-files-only t)
(load "fi-site-init")
(load "fi-leep0.el")

(setq fi-files '("fi-leep-xemacs" "fi-xemacs"))

(dolist (file fi-files)
  (let ((el (format "%s.el" file))
	(elc (format "%s.elc" file)))
    (unless (file-newer-than-file-p elc el)
      (message "--------------------------------------------------------")
      (byte-compile-file el))))

