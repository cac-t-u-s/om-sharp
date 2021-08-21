;============================================================================
; om#: visual programming language for computer-assisted music composition
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

(in-package :om)

;;; EQUIVALENT FUNCTION IN LISP CODE:
(defmacro repeat-n (body count)
  `(loop for ,(gensym) from 1 to ,count
         collect ,body))

(defclass Repeater (OMPatchComponent)
  ((n-iter :accessor n-iter :initform 0 :initarg :n-iter)
   (scope :accessor scope :initform :local :initarg :scope))
  (:documentation "Evaluate its main input <N> times. Retrun a list with all results.

The <scope> input (optional) determines the behaviour of the 'EVAL-ONCE' mechanism during the iteration:
- :local reset the eval-once context at each iteration (each iteration is considered as a separate 'eval')
- :global set a single global context for the <N> iterations."))


;;; since we defined the repeat-n the package browser will look for it
;;; as reference for documentation.
(setf (documentation 'repeat-n 'function) (class-documentation 'Repeater))


(defclass OMRepeatNBoxCall (OMPatchComponentBox) ())

(defmethod special-box-p ((name (eql 'repeat-n))) t)
(defmethod get-box-class ((self Repeater)) 'OMRepeatNBoxCall)
(defmethod get-icon-id ((self OMRepeatNBoxCall)) :repeat)
(defmethod object-name-in-inspector ((self OMRepeatNBoxCall)) "REPEAT-N box")


;;; returns the default value
(defmethod next-optional-input ((self OMRepeatNBoxCall))
  (<= (length (inputs self)) 2))

(defmethod more-optional-input ((self OMRepeatNBoxCall) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore name doc))
  (add-optional-input self :name "scope"
                      :value (if val-supplied-p value :local)
                      :reactive reactive)
  t)

(defmethod get-input-menu ((self OMRepeatNBoxCall) name)
  (when (string-equal name "scope")
    '(("global" :global)
      ("local" :local))
    ))


;;; as compared to other OMPatchComponentBox, REPEAT-N has a lock option
(defmethod valid-property-p ((self OMRepeatNBoxCall) (prop-id (eql :lock))) t)
(defmethod get-properties-list ((self OMRepeatNBoxCall))
  (add-properties
   (call-next-method)
   "Execution"
   `((:lock "Lock state (b/1)" ,(lock-modes-for-box self) lock-state))))


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
    :name "self"
    :doc-string "program to repeat")
   (make-instance
    'box-input :box self :value (n-iter (reference self))
    :name "num" :doc-string "number of times")))

(defmethod create-box-outputs ((self OMRepeatNBoxCall))
  (list
   (make-instance
    'box-output :box self :value NIL
    :name "collected results")))


(defmethod get-ev-once-flag ((self OMRepeatNBoxCall)) (list self (n-iter (reference self))))


(defmethod boxcall-value ((self OMRepeatNBoxCall))
  (let ((n (omNG-box-value (cadr (inputs self))))
        (scope (if (third (inputs self))
                   (omNG-box-value (third (inputs self)))
                 (scope (reference self))))
        (old-context *ev-once-context*))

    (unwind-protect
        (progn
          (setf (n-iter (reference self)) 0)

          ;;; creates a new context:
          (when (equal scope :local) (setf *ev-once-context* self))

          (loop for i from 1 to n
                do (setf (n-iter (reference self)) (1+ (n-iter (reference self))))
                collect (omNG-box-value (car (inputs self))))
          )
      ;;; restores previous context after iteration:
      (when (equal scope :local) (setf *ev-once-context* old-context))
      )
    ))


;;; known issue: the compiled version does not behave the same with regards to ev-once behaviour
;;; in first-level patch the value is stored between evaluations and after, while in compiled version the variables are scoped.
;;; update 25/03/2019: I think this is fixed now => otherwise try to single out an example

(defmethod gen-code-for-call ((self OMRepeatNBoxCall) &optional args)

  (let ((scope (if (third (inputs self))
                   (eval (gen-code (third (inputs self))))
                 (scope (reference self)))))

    (if (equal scope :global)

        (let* ((body (gen-code (car (inputs self)))))
          `(loop for i from 1 to ,(gen-code (cadr (inputs self)))
                 collect
                 ;;; keep this for a new context at each iteration:
                     ; (let* ,(output-current-let-context) ,body)
                 ;;; ... or use this for a global context:
                 (progn ,body)
                 )
          )

      ;;; :local
      (progn
        ;;; a new context will be created at each iteration:

        (let ((n (gen-code (cadr (inputs self)))))

          (push-let-context)

          (unwind-protect

              (let* ((body (gen-code (car (inputs self)))))
                `(loop for i from 1 to ,n
                       collect
                       ;;; new context at each iteration:
                       (let* ,(output-current-let-context) ,body)
                       ))

            (pop-let-context)

            ))
        ))
    ))



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
