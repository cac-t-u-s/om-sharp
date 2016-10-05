(in-package :ffa)

(defparameter *cffi-and-lisp-types*
  '((:int8 . (signed-byte 8))
    (:uint8 . (unsigned-byte 8))
    (:int16 . (signed-byte 16))
    (:uint16 . (unsigned-byte 16))
    (:int32 . (signed-byte 32))
    (:uint32 . (unsigned-byte 32))
    (:float . single-float)
    (:double . double-float)
    (:complex-float . (complex single-float))
    (:complex-double . (complex double-float))))

(defun elt-type (cffi-elt-type)
  "Return the Lisp array element-type matching cffi-elt-type, nil
if not found."
  (cdr (assoc cffi-elt-type *cffi-and-lisp-types*)))

(defun elt-type-size (cffi-elt-type)
  (case cffi-elt-type
    (:complex-float (* 2 (cffi:foreign-type-size :float)))
    (:complex-double (* 2 (cffi:foreign-type-size :double)))
    (otherwise (cffi:foreign-type-size cffi-elt-type))))
