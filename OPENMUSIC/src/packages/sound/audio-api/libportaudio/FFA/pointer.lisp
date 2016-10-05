(in-package :ffa)

;;;; NOTE: as CFFI does not handle C99 complex types yet (as of March
;;;; 2009), I have to deal with that case separately, hence the
;;;; hideous workaround.  part-type returns the type of the real and
;;;; imaginary parts of a complex type, and complex-mem-aref is a
;;;; settable location that should really be in CFFI in some form.
;;;; Once CFFI matures, this can be removed.

(defun part-type (cffi-type)
  "Return the appropriate part-type if cffi-type is complex, otherwise NIL."
  (case cffi-type
    (:complex-float :float)
    (:complex-double :double)
    (otherwise nil)))
      
(declaim (inline complex-mem-aref))
(defun complex-mem-aref (pointer part-type index)
  "aref workaround for C99 complex types.  Part-type is NOT checked."
  (let ((index (* 2 index)))
    (complex (cffi:mem-aref pointer part-type index)
	     (cffi:mem-aref pointer part-type (1+ index)))))
(defun (setf complex-mem-aref) (c pointer part-type index)
  "setf aref workaround for C99 complex types.  Part-type is NOT checked."
  (let ((index (* 2 index)))
    (setf (cffi:mem-aref pointer part-type index) (realpart c)
	  (cffi:mem-aref pointer part-type (1+ index)) (imagpart c))))

;;;; Functions that copy a lisp array to/from a memory location.
;;;; Coercion is performed when necessary.  The non-hygienic macro
;;;; below is for making code more compact.

(defmacro iter% (&body body)
  "Common iteration for all cases below.  IMPORTANT: This macro
captures pointer-index, size, array-index and index-offset."
  `(iter
      (for pointer-index :from 0 :below size)
      (for array-index :from index-offset)
      (symbol-macrolet ((elt (row-major-aref array array-index)))
	,@body)))

(defun copy-array-to-pointer (array pointer cffi-type lisp-type
			      index-offset size)
  "Copy size elements from array (starting at index-offset) of type
lisp-type to the memory area that starts at pointer, coercing the
elements if necessary."
  (let* ((elt-type (elt-type cffi-type))
	 (match-p (equal elt-type lisp-type))
	 (part-type (part-type cffi-type)))
    (unless elt-type
      (error "don't know how what array element type matches ~a" cffi-type))
    (cond
      ((and match-p part-type)		; complex, no coercion
       (iter%
	(setf (complex-mem-aref pointer part-type pointer-index) elt)))
      (match-p				; not complex, no coercion
       (iter%
	(setf (cffi:mem-aref pointer cffi-type pointer-index) elt)))
      (part-type
       (iter%				; complex, with coercion
	(setf (complex-mem-aref pointer part-type pointer-index)
	      (coerce elt elt-type))))
      (t				; not complex, with coercion
       (iter%
	(setf (cffi:mem-aref pointer cffi-type pointer-index) 
	      (coerce elt elt-type)))))))

(defun copy-array-from-pointer (array pointer cffi-type lisp-type
				index-offset size)
  "Copy size elements to an array (starting at index-offset) of
element type lisp-type from the memory area that starts at pointer,
coercing the elements if necessary."
  (let* ((elt-type (elt-type cffi-type))
	 (match-p (equal elt-type lisp-type))
	 (part-type (part-type cffi-type)))
    (unless elt-type
      (error "don't know how what array element type matches ~a" cffi-type))
    (cond
      ((and match-p part-type)		; complex, no coercion
       (iter%
	(setf elt (complex-mem-aref pointer part-type pointer-index))))
      (match-p				; not complex, no coercion
       (iter%
	(setf elt (cffi:mem-aref pointer cffi-type pointer-index))))
      (part-type
       (iter%				; complex, with coercion
	(setf elt
	      (coerce 
	       (complex-mem-aref pointer part-type pointer-index)
	       lisp-type))))
      (t				; not complex, with coercion
       (iter%
	(setf elt
	      (coerce 
	       (cffi:mem-aref pointer cffi-type pointer-index)
	       lisp-type)))))))

#+sbcl
(defmacro pin-to-pointer ((array pointer cffi-type size index-offset)
			  &body body)
  "Use SBCL's sb-sys:with-pinned-objects and
sb-ext:array-storage-vector for mapping an array to a memory location.
NOTE: checking that cffi-type matches the type of the array is the
responsibility of the user of this macro.  The total size of the array
is checked."
  (once-only (array)
  `(sb-sys:with-pinned-objects (,array)
     (assert (<= (+ ,index-offset ,size) (array-total-size ,array)))
     (let ((,pointer (cffi:inc-pointer (sb-sys:vector-sap 
					(sb-ext:array-storage-vector ,array))
				       (* ,index-offset 
					  (elt-type-size ,cffi-type)))))
       ,@body))))

(defmacro copy-to-pointer ((array pointer cffi-type size index-offset direction)
			   &body body)
  "Allocate memory area and establish desired mapping between array
and pointer (copy in and/or out as needed).  Array will be available
at pointer, which is a local binding so you do whatever you want with
it (change its value etc)."
  (with-unique-names (hidden-pointer lisp-type)
    (once-only (array cffi-type size index-offset direction)
      `(let ((,lisp-type (array-element-type ,array)))
	 (cffi:with-foreign-object (,hidden-pointer ,cffi-type ,size)
	   (when (or (eq ,direction :copy-in) (eq ,direction :copy-in-out))
	     (copy-array-to-pointer ,array ,hidden-pointer ,cffi-type
				    ,lisp-type ,index-offset ,size))
	   (multiple-value-prog1 
	       (let ((,pointer ,hidden-pointer))
		 ,@body)
	     (when (or (eq ,direction :copy-in-out) (eq ,direction :copy-out))
	       (copy-array-from-pointer ,array ,hidden-pointer ,cffi-type
					,lisp-type ,index-offset ,size))))))))

(defun valid-direction-p (direction)
  "Test if the given direction is valid."
  (or (eq direction :copy-in) (eq direction :copy-out)
      (eq direction :copy-in-out)))

#+sbcl
(defmacro with-pointer-to-array ((array pointer cffi-type size direction)
				 &body body)
  "See the documentation."
  (assert (symbolp pointer))
  (once-only (array cffi-type direction)
    (with-unique-names (original-array index-offset lisp-type)
      `(bind (((:values ,original-array ,index-offset) 
	       (find-original-array ,array))
	      (,lisp-type (array-element-type ,original-array)))
	 (assert (valid-direction-p ,direction))
	 (cond
	   ((and (typep ,original-array 'simple-array)
		 ,lisp-type 		; no nil arrays
		 (equal ,lisp-type (elt-type ,cffi-type)))
	    (pin-to-pointer (,original-array ,pointer ,cffi-type
			     ,size ,index-offset)
	      ,@body))
	   (t
	    (warn "array is copied and/or coerced, lisp type is ~a, requested CFFI type is ~a" ,lisp-type ,cffi-type)
	    (copy-to-pointer (,original-array ,pointer ,cffi-type
			      ,size ,index-offset ,direction)
	       ,@body)))))))


#-sbcl
(defmacro with-pointer-to-array ((array pointer cffi-type size direction)
				 &body body)
  "See the documentation."
  (assert (symbolp pointer))
  (once-only (array cffi-type)
    (with-unique-names (original-array index-offset)
      `(bind (((:values ,original-array ,index-offset) 
	       (find-original-array ,array)))
	 (assert (valid-direction-p ,direction))
	 (copy-to-pointer (,original-array ,pointer ,cffi-type
			   ,size ,index-offset ,direction)
	       ,@body)))))

(defmacro with-pointers-to-arrays (parameter-lists &body body)
  "Same as with-pointer-to-array, but with multiple arrays, pointers,
etc.  parameter-lists needs to be a list of lists."
  (labels ((internal (plists)
	     (let ((plist (car plists))
		   (rest (cdr plists)))
	       (unless (= (length plist) 5)
		 (error "invalid parameter list(s)"))
	       `(with-pointer-to-array ,plist
		  ,@(if rest
			(list (internal rest))
			body)))))
    (internal parameter-lists)))
