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

;;;----------------------------------
;;; REPEAT-N: A COMPACT LOOP UTILITY
;;; SIMULATES N EVALUATIONS OF ITS INPUT
;;;----------------------------------
;;; NOTE: REINITIALIZES THE "EVAL-ONCE" BETWEEN EACH EVALUATION

(in-package :om)

;;; THE EQUIVALENT FUNCTION IN TEXT CODE WOULD BE:

(defmacro repeat-n (body count)
  `(loop for ,(gensym) from 1 to ,count
         collect ,body))

(defclass Repeater (OMPatchComponent) 
  ((n-iter :accessor n-iter :initform 0 :initarg :n-iter)))

(defclass OMRepeatNBoxCall (OMPatchComponentBox) ())

(defmethod special-box-p ((name (eql 'repeat-n))) t)
(defmethod get-box-class ((self Repeater)) 'OMRepeatNBoxCall)
(defmethod get-icon-id ((self OMRepeatNBoxCall)) :repeat)
(defmethod object-name-in-inspector ((self OMRepeatNBoxCall)) "repeat-n box")

(defmethod box-symbol ((self Repeater)) 'repeat-n)

(defmethod omNG-make-special-box ((reference (eql 'repeat-n)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'Repeater :name "repeat-n" 
                  :n-iter (if (integerp (car init-args)) 
                              (car init-args)
                            0))
   pos
   '(:icon-pos :left)))


(defmethod create-box-inputs ((self OMRepeatNBoxCall)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "program to repeat")
   (make-instance 
    'box-input :box self :value (n-iter (reference self))
    :name "n")))

(defmethod create-box-outputs ((self OMRepeatNBoxCall)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "collected results")))


(defmethod get-ev-once-flag ((self OMRepeatNBoxCall)) (list self (n-iter (reference self))))

(defmethod boxcall-value ((self OMRepeatNBoxCall)) 
  (let ((n (omNG-box-value (cadr (inputs self))))
        (old-context *ev-once-context*))
    (unwind-protect 
      (progn
        (setf (n-iter (reference self)) 0)
        (setf *ev-once-context* self)
        (loop for i from 1 to n
              do (setf (n-iter (reference self)) (1+ (n-iter (reference self))))
              ; do (print (list " ======= repeat: " (n-iter (reference self))))
              collect (omNG-box-value (car (inputs self))))
        )
      (setf *ev-once-context* old-context)
      (clear-ev-once (container self)))
    ))


;;; known issue: the compiled version does not behave the same with regards to ev-once behaviour
;;; in first-level patch the value is stored between evaluations and after, while in compiled version the variables are scoped.

(defmethod gen-code-for-call ((self OMRepeatNBoxCall) &optional args)
  
  (push-let-context)
  
  (let* ((body (gen-code (car (inputs self))))
         (code 
          `(loop for i from 1 to ,(gen-code (cadr (inputs self))) 
                 collect 
                 (let* ,(output-current-let-context) ,body)
                 )))
     
    (pop-let-context)

    code))


;;; NO LAMBDA OR OTHER FUNKY EVAL MODES FOR SPECIAL BOXES LIKE THIS...
;;; (the following code should work though...)
#|
(defmethod box-lambda-value ((self OMRepeatNBoxCall))
   (multiple-value-bind (new-symbs args) 
       (get-args-eval-curry self #'(lambda (input) `(omNG-box-value ,input)))
     (let ((arglist (apply 'append args)))  ;;; flat the arg-list (for keywords etc.)
       (eval 
        `#'(lambda ,new-symbs
             (setf (n-iter ,(reference self)) 0)
             (setf *ev-once-context* ,self)
             (loop for i from 1 to ,(cadr arglist)
                   do (setf (n-iter ,(reference self)) (1+ (n-iter ,(reference self))))
                   collect ,(car arglist))
             )))))

(defmethod gen-code-lambda ((self OMRepeatNBoxCall) &optional numout)
  (declare (ignore numout))
   (let ((oldletlist *let-list*))
     (setf *let-list* nil)
  
     (multiple-value-bind (new-symbs args) 
         (get-args-eval-curry self #'gen-code)
       
       (let* ((body (caar args))
              (code `#'(lambda ,new-symbs
                        (loop for i from 1 to ,(caadr args) 
                              collect (let* ,(reverse *let-list*) ,body)))))
         
       (setf *let-list* oldletlist)
       code))
     ))
|#