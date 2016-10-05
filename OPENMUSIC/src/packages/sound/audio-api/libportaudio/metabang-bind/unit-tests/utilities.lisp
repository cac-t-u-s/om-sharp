(in-package #:metabang-bind-test)

(defun collect-tree (tree &key transform)
  "Maps FN over every atom in TREE."
  (bind ((transform (or transform #'identity))
	 ((:labels doit (x))
	  (cond
	    ;; ((null x) nil)
	    ((atom x) (funcall transform x))
	    (t
	     (cons
	      (doit (car x))
	      (when (cdr x) (doit (cdr x))))))))
    (doit tree)))

(defun remove-gensyms (tree)
  (collect-tree tree :transform (lambda (x) (when (or (not (symbolp x))
						      (symbol-package x))
					      x))))
