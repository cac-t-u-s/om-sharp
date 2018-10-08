;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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


;;;============================
;;; A WRAPPER AROUND O. BUNDLE
;;;============================

(defclass o.bundle (data-frame om-cleanup-mixin)
  ((bundle_s :accessor bundle_s :initform nil :initarg :bundle_s)))

(defstruct (o.pointer (:include oa::om-pointer)))


(defmethod free-foreign-pointer ((self o.bundle))
  (when (bundle_s self)
    (oa::om-release (bundle_s self))))

(defmethod assign-foreign-pointer ((self o.bundle) ptr)
  (setf (bundle_s self) (make-o.pointer :ptr ptr))
  (oa::om-retain (bundle_s self)))

;; (gc-all)

(defmethod oa::om-release ((bundle_s o.pointer))
  (when (<= (decf (oa::om-pointer-count bundle_s)) 0)
    (unless (om-null-pointer-p (oa::om-pointer-ptr bundle_s))
      ;(om-print (format nil "Free OSC bundle ~A" bundle_s) "O.DEBUG")
      (odot::osc_bundle_s_deepFree (oa::om-pointer-ptr bundle_s)))))

(defmethod om-cleanup ((self o.bundle))
  ;(free-foreign-pointer self)
  (when (bundle_s self)
    (oa::om-release (bundle_s self))))

(defmethod clone-object ((self o.bundle) &optional clone)
  (let ((o.bndl (call-next-method)))
    (setf (bundle_s o.bndl) (bundle_s self))
    (when (bundle_s o.bndl) (oa::om-retain (bundle_s o.bndl)))
    o.bndl))

(defgeneric make-osc-atom-from-data (data))

;;; who calls this must free the result with odot::osc_bundle_u_free
(defun make-foreign-bundle-u-pointer (data)
  (let ((bndl_u (odot::osc_bundle_u_alloc)))
    (loop for msg in data do
          (let ((msg_u (odot::osc_message_u_allocWithAddress (car msg))))
            (loop for data in (cdr msg) do
                  (let ((atom_u (make-osc-atom-from-data data)))
                    (when atom_u 
                      (odot::osc_message_u_appendAtom msg_u atom_u))))
            (odot::osc_bundle_u_addMsg bndl_u msg_u)))
    bndl_u))

(defmethod make-osc-atom-from-data ((data integer)) 
  (odot::osc_atom_u_allocWithInt32 data))
(defmethod make-osc-atom-from-data ((data float)) 
  (odot::osc_atom_u_allocWithFloat data))
(defmethod make-osc-atom-from-data ((data double-float)) 
  (odot::osc_atom_u_allocWithDouble data))
(defmethod make-osc-atom-from-data ((data string)) 
  (odot::osc_atom_u_allocWithString data))
(defmethod make-osc-atom-from-data ((data list)) 
  (odot::osc_atom_u_allocWithBndl (make-foreign-bundle-u-pointer data)))

(defmethod make-osc-atom-from-data ((data null)) 
  (odot::osc_atom_u_allocWithInt32 0))

(defmethod make-osc-atom-from-data ((data symbol))
  (if (equal data t) (odot::osc_atom_u_allocWithInt32 1)
    (call-next-method)))

;;; we suppose Lisp BIGNUM = OSC Timetag...
(defmethod make-osc-atom-from-data ((data bignum)) 
  (odot::osc_atom_u_allocWithTimetag (odot::make_osc_timetag data)))


(defmethod make-osc-atom-from-data ((n t)) 
  (om-beep-msg "Unknown type for OSC atom: ~A" (type-of n)) nil)

;;; who calls this must free the result with odot::osc_bundle_s_free
(defun make-foreign-bundle-s-pointer (data &optional timetag)
  (let ((bndl_u (make-foreign-bundle-u-pointer data)))
    (unwind-protect 
        (let ((bndl_s (odot::osc_bundle_u_serialize bndl_u)))
          (when timetag
            (odot::osc_bundle_s_setTimetag 
             (odot::osc_bundle_s_getLen bndl_s) 
             (odot::osc_bundle_s_getPtr bndl_s) 
             (odot::make_osc_timetag timetag)))
          bndl_s)
      (odot::osc_bundle_u_free bndl_u)
      )))

(defmethod get-timetag ((self o.bundle))
  (when (bundle_s self)
    (odot::decode_osc_timetag
     (odot::osc_bundle_s_getTimetag 
      (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self)))
      (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self)))))))
  

(defmethod date ((self o.bundle)) (get-timetag self))
(defmethod (setf date) (date (self o.bundle)) (update-bundle-pointer-timetag self date))

(defun decode-atom-s-data (a)
  (case (code-char (odot::osc_atom_s_getTypetag a))
    (#\f (odot::osc_atom_s_getFloat a))
    (#\d (odot::osc_atom_s_getDouble a))
    (#\i (odot::osc_atom_s_getInt a))
    (#\s (odot::osc_atom_s_get_str a))
    (#\T t) (#\F nil) 
    ;;; (#\. (decode-bundle-s-data (odot::osc_atom_s_getBndl_s a)))
    (otherwise :unknown-osc-type)))

(defun decode-bundle-s-pointer-data (ptr)
  (let ((b_it_s (odot::osc_bundle_iterator_s_getIterator 
                 (odot::osc_bundle_s_getLen ptr) 
                 (odot::osc_bundle_s_getPtr ptr))))
    (unwind-protect 
        (loop while (= 1 (odot::osc_bundle_iterator_s_hasNext b_it_s)) collect
              (let ((m (odot::osc_bundle_iterator_s_next b_it_s)))
                ;(print (list "MESSAGE n args = " (odot::osc_message_s_getArgCount m)))
                (cons (odot::osc_message_s_getAddress m)
                      (let ((m_it_s (odot::osc_message_iterator_s_getIterator m)))
                        (unwind-protect 
                            (loop while (= 1 (odot::osc_message_iterator_s_hasNext m_it_s)) collect
                                  (decode-atom-s-data (odot::osc_message_iterator_s_next m_it_s)))
                          (odot::osc_message_iterator_u_destroyIterator m_it_s))))
                ))
      (odot::osc_bundle_iterator_s_destroyIterator b_it_s))))

(defun decode-bundle-s-data (bundle_s)
  (decode-bundle-s-pointer-data (oa::om-pointer-ptr bundle_s)))

(defmethod get-messages ((self o.bundle))
  (when (bundle_s self)
    (decode-bundle-s-data (bundle_s self))))

;;; change bundle
(defmethod update-bundle-pointer ((self o.bundle) data timetag)
  (free-foreign-pointer self)
  (assign-foreign-pointer self (make-foreign-bundle-s-pointer data timetag))
  self)

;;; change the time tag (same bundle)
(defmethod update-bundle-pointer-timetag ((self o.bundle) timetag)
  (when (and (bundle_s self) timetag)
    (odot::osc_bundle_s_setTimetag 
     (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))) 
     (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self))) 
     (odot::make_osc_timetag timetag))))

;;; change the bundle but restore existing timetag (if any)
(defmethod update-bundle-pointer-data ((self o.bundle) data)
  (let ((timetag nil))
    (when (bundle_s self)
      (setq timetag (odot::osc_bundle_s_getTimetag (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))) 
                                                   (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self)))))
      (free-foreign-pointer self))
    (assign-foreign-pointer self (make-foreign-bundle-s-pointer data))
    (when timetag (update-bundle-pointer-timetag self timetag))
    self))

(defmethod data-frame-text-description ((self o.bundle))
  (cons "O. BUNDLE" (flat (mapcar 'format-message (get-messages self)))))

(defmethod data-size ((self o.bundle))
  (length (flat (get-messages self))))

(defmethod get-frame-action ((self o.bundle))
  #'(lambda () 
      (when (bundle_s self)
        (odot::osc_send_bundle_s 3000 "localhost" (oa::om-pointer-ptr (bundle_s self))))))

(defmethod make-o.bundle ((self osc-bundle))
  (let ((b (make-instance 'o.bundle)))
    (objfromobjs self b)
    b))

(defmethod make-o.bundle ((messages list))
  (let ((ob (make-instance 'o.bundle)))
    (assign-foreign-pointer ob (make-foreign-bundle-s-pointer messages))
    ob))

(defmethod o.bundle-size ((self o.bundle)) (odot::osc_bundle_s_getLen (oa::om-pointer-ptr (bundle_s self))))
(defmethod o.bundle-ptr ((self o.bundle)) (odot::osc_bundle_s_getPtr (oa::om-pointer-ptr (bundle_s self))))

(defmethod objfromobjs ((model osc-bundle) (target o.bundle))
  (update-bundle-pointer target (messages model) (date model))
  target)

(defmethod objfromobjs ((model o.bundle) (target osc-bundle))
  (setf (date target) (get-timetag model)
        (messages target) (get-messages model))
  target)


;;; EXPRESSION LANGUAGE
(defun o.sequence (&rest expression)
  (reduce 
   #'(lambda (s1 s2) (concatenate 'string s1 ", " s2))
   expression))

(defun o.call (function &rest argument) 
  (format nil "~A(~{~a~^, ~})" function argument))

;;; SPECIFIC PREDEFINED FUNCTIONS
(defun o.delete (address) 
  (format nil "delete(~A)" address))

(defun o.assign (address value) 
  (if (listp value)
      (format nil "assign(~A, [~{~a~^, ~}])" address value)
    (format nil "assign(~A, ~A)" address value)))

(defun o.lambda (var expr)
  (if (listp var)
      (format nil "lambda([~{~a~^, ~}],~A)" var expr)
    (format nil "lambda(~A, ~A)" var expr)))

(defun o.map (fun list_name)
  (format nil "map(~A, ~A)" fun list_name))

(defun o.eval (function) 
  (format nil "eval(~A)" function))


;;; MAIN EVAL FUNCTION
(defun o.eval-expr (expression o.bundle)
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

    

