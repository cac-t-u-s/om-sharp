;============================================================================
; o.OM : odot OSC interface in OM
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

(in-package :om)

;;;======================================
;;; GENERATING/EXECUTING ODOT EXPRESSIONS
;;;======================================

(defmethod! o.sequence (&rest expression)
  (reduce 
   #'(lambda (s1 s2) (concatenate 'string s1 ", " s2))
   expression))

(defmethod! o.call (function &rest argument) 
  (format nil "~A(~{~a~^, ~})" function argument))

;;; SPECIFIC PREDEFINED FUNCTIONS
(defmethod! o.delete (address) 
  (format nil "delete(~A)" address))

(defmethod! o.assign (address value) 
  (if (listp value)
      (format nil "assign(~A, [~{~a~^, ~}])" address value)
    (format nil "assign(~A, ~A)" address value)))

(defmethod! o.lambda (var expr)
  (if (listp var)
      (format nil "lambda([~{~a~^, ~}],~A)" var expr)
    (format nil "lambda(~A, ~A)" var expr)))

(defmethod! o.map (fun list_name)
  (format nil "map(~A, ~A)" fun list_name))

(defmethod! o.eval (function) 
  (format nil "eval(~A)" function))


;;; MAIN EVAL FUNCTION
(defmethod! o.eval-expr (expression o.bundle)
  (let ((newptr (odot::execute_osc_expr 
                 expression 
                 (and o.bundle (bundle_s o.bundle) (oa::om-pointer-ptr (bundle_s o.bundle)))))
        (out (make-instance 'o.bundle)))
    (assign-foreign-pointer out newptr)
    (setf (date out) (date o.bundle))
    out))


(defun test_bundle (o.bundle)
  (let* ((bndl_out (oa::om-pointer-ptr (bundle_s o.bundle)))
         (out (make-instance 'o.bundle))
         (bndl_out_ptr (odot::osc_bundle_s_getPtr bndl_out)))
         ;(bndl_out_ptr_* (cffi:foreign-alloc :pointer :initial-element (odot::osc_bundle_s_getPtr bndl_out))))
    (odot::osc_bundle_s_setPtr bndl_out (odot::osc_bundle_s_getPtr bndl_out))
    (assign-foreign-pointer out bndl_out)
    ;(cffi-sys:foreign-free bndl_out_ptr_*)
    out))

    

