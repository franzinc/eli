;; $Id: fi-build.el,v 2.2 1996/08/01 22:35:34 layer Exp $
;; This file can be loaded at emacs build time to have the emacs-lisp
;; interface preloaded into the image.

(setq fi::build-time t)

(load "fi-site-init")
