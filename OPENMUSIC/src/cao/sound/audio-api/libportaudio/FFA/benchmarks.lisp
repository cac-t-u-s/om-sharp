(defparameter *size* 1000000)
(defparameter *simple* (make-array *size* :element-type 'double-float))
(defparameter *displaced* (make-array (- *size* 100) :element-type 'double-float
				      :displaced-index-offset 100 
				      :displaced-to *simple*))

(dotimes (i (array-dimension *simple* 0))
  (setf (aref *simple* i) (random 1d0)))

(time (reduce #'+ *simple* :start 100))
(time (reduce #'+ *displaced*))

(defparameter *size* 1000000)
(defparameter *a* (make-ffa *size* :double))
(cffi:with-foreign-object (pointer :double *size*)
  (time (copy-array-to-pointer *a* pointer :double 'double-float 0 *size*))
  (format t "~a~%" pointer)
  (setf pointer (cffi:null-pointer))
  (format t "~a~%" pointer))
