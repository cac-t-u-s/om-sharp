(in-package #:metabang-bind-test)

(deftestsuite test-flet (metabang-bind-test)
  ())

(addtest (test-flet)
  basic-access
  (bind (((:flet doit (x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-flet)
  declarations
  (bind (((:flet doit (x))
	  (declare (type fixnum x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-flet)
  docstring
  (bind (((:flet doit (x))
	  "if I knew how to get the docstring out of flet, I'd test it."
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-flet)
  docstring-and-declarations
  (bind (((:flet doit (x))
	  "whatever"
	  (declare (type fixnum x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-flet)
  docstring-and-declarations
  (bind (((:flet constant (x))
	  (declare (ignore x))
	  42))
    (ensure-same (constant 1) 42)))


(deftestsuite test-labels (metabang-bind-test)
  ())

(addtest (test-labels)
  basic-access
  (bind (((:labels my-oddp (x))
	  (cond ((<= x 0) nil)
		((= x 1) t)
		(t (my-oddp (- x 2))))))
    (ensure (my-oddp 1))
    (ensure (my-oddp 7))
    (ensure-null (my-oddp 2))))

(addtest (test-labels)
  declarations
  (bind (((:labels doit (x))
	  (declare (type fixnum x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-labels)
  docstring
  (bind (((:labels doit (x))
	  "if I knew how to get the docstring out of flet, I'd test it."
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-labels)
  docstring-and-declarations
  (bind (((:labels doit (x))
	  "whatever"
	  (declare (type fixnum x))
	  (setf x (* 2 x)) 
	  (setf x (+ x 3))
	  x))
    (ensure-same (doit 1) 5)
    (ensure-same (doit 2) 7)))

(addtest (test-labels)
  docstring-and-declarations
  (bind (((:labels constant (x))
	  (declare (ignore x))
	  42))
    (ensure-same (constant 1) 42)))
