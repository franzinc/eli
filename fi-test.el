;; $Id: fi-test.el,v 1.3 1996/08/01 22:36:34 layer Exp $

(setq load-path (cons default-directory load-path))

(load "fi-site-init")

(defmacro test-equal (compare test)
  (list 'let (list (list 'result test))
	(list 'unless
	      (list 'equal compare 'result)
	      (list 'error
		    "test-equal test failed: wanted %s, got %s"
		    compare
		    'result))))

(defun fi:test ()
  (interactive)
  (test-equal '("12345612345" "6")
	      (fi::explode "123456123456" 1))
  (test-equal '("123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "123456123456" "")
	      (fi::explode "123456123456" 1))
  (test-equal '("" "" "" "")
	      (fi::explode "" 1))
  (test-equal '("a" "b" "c" "d" "e")
	      (fi::explode "abcde" 1))
  (test-equal '("aaaa")
	      (fi::explode "aaaa" 1))
  (test-equal '("aa" "bb" "cc" "dd" "ee")
	      (fi::explode "aabbccddee" 1))
  )

(fi:test)
(kill-emacs)
