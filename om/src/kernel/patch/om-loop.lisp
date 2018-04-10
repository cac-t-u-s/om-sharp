;============================================================================
; o7: visual programming language for computer-aided music composition
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

;---------------------------------------
; Loop Boxes
;---------------------------------------

(in-package :om)


;;;================================
;;; ITERATORS
;;; for / while / in-list / on-list
;;;================================

;;; MUST BE USED ALONG WITH ITERATE
;;; OTHERWISE THE VARIABLE IS NOT DECLARED / UPDATED

(defclass OMPatchLoop (OMPatchComponent) 
  ((it-var :initform (gentemp "IT-") :accessor it-var)
   (it-value :initform nil :accessor it-value)))

(defclass OMPatchLoopBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMPatchLoop)) 'OMPatchLoopBox)

(defmethod get-icon-id ((self OMPatchLoopBox)) 'm-iter)
(defmethod get-icon-size ((self OMPatchLoopBox)) 20)

(defmethod create-box-inputs ((self OMPatchLoopBox)) 
  (create-box-inputs-for-loop-box (reference self) self))

(defmethod create-box-outputs ((self OMPatchLoopBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL :name "i"
    :doc-string "current value of loop iterator (bound during iterate)")))


(defmethod gen-loop-code-for-eval ((self OMPatchLoopBox)) 
  (let ((input-values (mapcar 'omng-box-value (inputs self)))) 
    (gen-iteration-code (reference self) input-values)))

(defmethod gen-loop-box-update-code ((self OMPatchLoopBox)) 
  `(do (setf (it-value ,(reference self)) ,(it-var (reference self)))))


(defmethod boxcall-value ((self OMPatchLoopBox)) 
  (it-value (reference self)))

(defmethod gen-code-for-call ((self OMPatchLoopBox) &optional args)
   (it-var (reference self)))


;;;------------------------------------

(defclass OMLoopFor (OMPatchLoop) ())

(defmethod special-box-p ((name (eql 'loop-for))) t)

(defmethod omNG-make-special-box ((reference (eql 'loop-for)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLoopFor :name "for")
   pos init-args))

(defmethod gen-iteration-code ((self OMLoopFor) &optional input-values) 
  `(for ,(it-var self) from ,(or (car input-values) 0) to ,(or (cadr input-values) 0)))


(defmethod create-box-inputs-for-loop-box ((self OMLoopFor) box) 
  (list 
   (make-instance 
    'box-input :box box :value 0
    :name "from")
   (make-instance 
    'box-input :box box :value 10
    :name "to")))

;;;------------------------------------

(defclass OMLoopWhile (OMPatchLoop) 
  ((loop-cond :initform nil :accessor loop-cond :initarg :loop-cond)))

(defmethod special-box-p ((name (eql 'loop-while))) t)

;;;------------------------------------

(defclass OMLoopIn (OMPatchLoop) 
  ((looped-list :initform nil :accessor looped-list :initarg :looped-list)))

(defmethod special-box-p ((name (eql 'loop-list))) t)

;;;------------------------------------

(defclass OMLoopOn (OMPatchLoop) 
  ((looped-list :initform nil :accessor looped-list :initarg :looped-list)))

(defmethod special-box-p ((name (eql 'loop-onlist))) t)



;;;====================
;;; ITERATE
;;;====================
;;; - triggers iteration according to the ITERATORs found in the patch
;;; ALSO WORKS IN EVAL MODE:
;;; - can be evaluated alone 
;;; - reinitializes the COLLECTORS before to start (or not?)
;;; - reinitializes the ITERATORS before to start
;;; - evaluates all iterators

(defclass OMPatchIterator (OMPatchComponent) 
  ((n-iter :accessor n-iter :initform 0 :initarg :n-iter)))

(defclass OMPatchIteratorBox (OMPatchComponentBox) ())

(defmethod special-box-p ((name (eql 'iterate))) t)
(defmethod get-box-class ((self OMPatchIterator)) 'OMPatchIteratorBox)

(defmethod get-ev-once-flag ((self OMPatchIteratorBox)) (list self (n-iter (reference self))))

(defmethod get-icon-id ((self OMPatchIteratorBox)) 'm-loop)

(defmethod object-name-in-inspector ((self OMPatchIteratorBox)) "iterator box")

(defmethod omNG-make-special-box ((reference (eql 'iterate)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMPatchIterator :name (if name (string name) "iterate"))
     pos init-args)))

(defmethod create-box-inputs ((self OMPatchIteratorBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "action")))

(defmethod next-optional-input ((self OMPatchIteratorBox)) t)

(defmethod get-input-doc ((self OMPatchIteratorBox) name) "to do at each iteration")
 
(defmethod more-optional-input ((self OMPatchIteratorBox) &key name (value nil val-supplied-p) doc reactive)
  (add-optional-input self :name "action"
                      :value (if val-supplied-p value nil) 
                      :reactive reactive)
  t)

(defmethod collect-loop-boxes ((self OMPatch))
  (get-boxes-of-type self 'OMPatchLoopBox))

(defmethod boxcall-value ((self OMPatchIteratorBox))
  
  (let ((old-context *ev-once-context*)
        (loopboxes (remove-if 
                    #'(lambda (b) (not (is-connected-up-to self b)))
                    (collect-loop-boxes (container self)))))
    (unwind-protect 
        (progn
          (setf (n-iter (reference self)) 0)
          (setf *ev-once-context* self)
          (let* ((iterators-code (apply 'append (mapcar 'gen-loop-code-for-eval loopboxes)))
                 (update-iterators-value-code (apply 'append (mapcar 'gen-loop-box-update-code loopboxes)))
                 (loop-code 
                  `(loop 
                    ,.iterators-code
                    ,.update-iterators-value-code
                    
                    do (setf (n-iter ,(reference self)) (1+ (n-iter ,(reference self))))
                    ; do (om-print (n-iter ,(reference self)) "LOOP")
                    collect ,(if (= 1 (length (inputs self)))
                                 `(omNG-box-value ,(car (inputs self)))
                               `(loop for inp in ',(inputs self) collect (omNG-box-value inp)))
                    )))
            ;(pprint loop-code) (terpri)
            (when iterators-code (eval loop-code))
            ))
     
      (setf *ev-once-context* old-context)
      (clear-ev-once (container self)))
    ))


(defmethod gen-code ((self OMPatchIteratorBox) &optional args)
  (let* ((loopboxes (remove-if 
                    #'(lambda (b) (not (is-connected-up-to self b)))
                    (collect-loop-boxes (container self))))
         (iterators-code (apply 'append (mapcar 'gen-loop-code-for-eval loopboxes))))
    
    
    (list 
     `(loop ,.iterators-code
            do ,(if (> (length (inputs self)) 1)
                    `(progn 
                       ,.(loop for inp in (inputs self) collect
                               (gen-code inp)))
                  (gen-code (car (inputs self)))
                  ))
     )))








