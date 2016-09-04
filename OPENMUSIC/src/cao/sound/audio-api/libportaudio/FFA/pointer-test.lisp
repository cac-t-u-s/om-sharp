(in-package :ffa)


(defun cffi-add (pointer size type)
  "Add size of type numbers at pointer."
  (let ((sum 0))
    (dotimes (i size)
      (incf sum (cffi:mem-aref pointer type i)))
    sum))

(defun cffi-fill-double (pointer size)
  "Fill array at pointer with size doubles."
  (dotimes (i size)
    (setf (cffi:mem-aref pointer :double i) (coerce i 'double-float))))

(defun cffi-fill-int32 (pointer size)
  "Fill array at pointer with size integers."
  (dotimes (i size)
    (setf (cffi:mem-aref pointer :int32 i) i)))

(defun pointer-test-in (size lisp-type cffi-type)
  (let ((array (make-ffa size lisp-type)))
    (dotimes (i size)
      (setf (aref array i) (coerce i lisp-type)))
    (let ((lisp-sum (reduce #'+ array))
	  (cffi-sum (with-pointer-to-array (array pointer cffi-type size :copy-in)
		      (cffi-add pointer size cffi-type))))
      (values (= lisp-sum cffi-sum) lisp-sum cffi-sum))))

(defun pointer-test-double-out (size)
  (let ((array (make-ffa size :double)))
    (with-pointer-to-array (array pointer :double size :copy-out)
      (cffi-fill-double pointer size))
    array))

(defun pointer-test-int32-out (size lisp-type)
  (let ((array (make-ffa size lisp-type)))
    (with-pointer-to-array (array pointer :int32 size :copy-out)
      (cffi-fill-int32 pointer size))
    array))

;; should evaluate to T, num, num
(pointer-test-in 5 'double-float :double)
(pointer-test-in 10 'rational :double)
(pointer-test-in 17 '(unsigned-byte 32) :float)

;; should return numbers 0, 1, 2, ...
(pointer-test-double-out 10)
(pointer-test-int32-out 9 'double-float)
(pointer-test-int32-out 11 'fixnum)

;; test non-simple arrays
(let* ((a (make-array '(2 9) :element-type 'integer))
	      (a-d (displace-array a 9 3)))
  (with-pointer-to-array (a-d pp :int32 7 :copy-in-out)
    (cffi-fill-int32 pp 7))
  a) ; =>  #2A((0 0 0 0 1 2 3 4 5) (6 0 0 0 0 0 0 0 0)))
