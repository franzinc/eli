;; $Id: fi-build.el,v 3.0 2003/12/15 22:52:57 layer Exp $
;; This file can be loaded at emacs build time to have the emacs-lisp
;; interface preloaded into the image.

(setq fi::build-time t)

(load "fi-site-init")
