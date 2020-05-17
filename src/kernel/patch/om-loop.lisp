;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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


(defmethod index ((self OMPatchLoop)) 0)

(defclass OMPatchLoopBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMPatchLoop)) 'OMPatchLoopBox)
(defmethod box-symbol ((self OMPatchLoop)) 'loop)

(defmethod get-icon-id ((self OMPatchLoopBox)) :m-iter)

(defmethod create-box-inputs ((self OMPatchLoopBox)) 
  (create-box-inputs-for-loop-box (reference self) self))

(defmethod create-box-outputs ((self OMPatchLoopBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL :name "i"
    :doc-string "current value of loop iterator (bound during iterate)")))

(defmethod next-optional-input ((self OMPatchLoopBox)) 
  (allow-optional-input (reference self) (inputs self)))

;; also returns the default value
(defmethod allow-optional-input ((self OMPatchLoop) existing-inputs) nil)

(defmethod more-optional-input ((self OMPatchLoopBox) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore name doc))
  (add-optional-input self :name "by"
                      :value (if val-supplied-p value (allow-optional-input (reference self) (inputs self))) 
                      :reactive reactive)
  t)

(defmethod gen-loop-code-for-eval ((self OMPatchLoopBox))
  (let ((inputs-code (loop for inp in (inputs self) collect `(omng-box-value ,inp))))
    (gen-iteration-code (reference self) inputs-code)))

(defmethod gen-loop-code-for-compile ((self OMPatchLoopBox) &optional (gen-fun 'gen-code)) 
  ;(let ((curr-loop-context (pop-let-context)))
    (unwind-protect 
        (gen-iteration-code (reference self) (loop for inp in (inputs self) collect (gen-code inp)))
      ;(push-let-context curr-loop-context)
      )
  ; )
)


(defmethod gen-loop-iterator-update-code ((self OMPatchLoop)) 
  `(do (setf (it-value ,self) ,(it-var self))))

;;; called at the beginning of a loop
(defmethod box-init-iterator-value ((self OMPatchLoopBox))
  (init-iterator-value (reference self) (mapcar 'omng-box-value (inputs self))))

(defmethod init-iterator-value ((self OMPatchLoop) input-values)
  (declare (ignore input-values))
  (setf (it-value self) nil))
  

(defmethod boxcall-value ((self OMPatchLoopBox)) 
  (it-value (reference self)))

(defmethod gen-code ((self OMPatchLoopBox) &optional numout)
   (it-var (reference self)))


;;;------------------------------------
;;; DIFFERENT KINDS OF ITERATOR BOXES
;;;------------------------------------

(defclass OMLoopFor (OMPatchLoop) ()
  (:documentation "FOR ITERATOR FOR LOOPS.

Can be connected to an ITERATE box to control the steps of the iteration (FOR I FROM <a> TO <b> BY <step>).")
  )

(defmethod special-box-p ((name (eql 'loop-for))) t)
(defmethod box-symbol ((self OMLoopFor)) 'loop-for)

(defmethod omNG-make-special-box ((reference (eql 'loop-for)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLoopFor :name "loop-for")
   pos init-args))

(defmethod gen-iteration-code ((self OMLoopFor) &optional inputs) 
  (append `(for ,(it-var self) from ,(or (car inputs) 0) to ,(or (cadr inputs) 0))
          (when (caddr inputs) `(by ,(caddr inputs)))))

(defmethod init-iterator-value ((self OMLoopFor) input-values)
  (setf (it-value self) (car input-values)))

(defmethod create-box-inputs-for-loop-box ((self OMLoopFor) box) 
  (list 
   (make-instance 
    'box-input :box box :value 0
    :name "from")
   (make-instance 
    'box-input :box box :value 10
    :name "to")))

;;; returns the default value
(defmethod allow-optional-input ((self OMLoopFor) existing-inputs)
  (and (<= (length existing-inputs) 2) 1))


;;;------------------------------------

(defclass OMLoopList (OMPatchLoop) 
  ((looped-list :initform nil :accessor looped-list :initarg :looped-list))
  (:documentation "LIST ITERATOR FOR LOOPS.

Can be connected to an ITERATE box to control the iteration (FOR ITEM IN <list> ...)."))

(defmethod special-box-p ((name (eql 'loop-list))) t)
(defmethod box-symbol ((self OMLoopList)) 'loop-list)

(defmethod omNG-make-special-box ((reference (eql 'loop-list)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLoopList :name "loop-list")
   pos init-args))

(defmethod gen-iteration-code ((self OMLoopList) &optional inputs) 
  (append `(for ,(it-var self) in ,(car inputs))
          (when (cadr inputs) `(by ,(cadr inputs)))))
 
(defmethod init-iterator-value ((self OMLoopList) input-values)
  (setf (it-value self) (car (car input-values))))

(defmethod create-box-inputs-for-loop-box ((self OMLoopList) box) 
  (list 
   (make-instance 
    'box-input :box box :value nil
    :name "list")))

;;; returns the default value
(defmethod allow-optional-input ((self OMLoopList) existing-inputs)
  (and (<= (length existing-inputs) 1) 'cdr))

;;;------------------------------------

(defclass OMLoopTail (OMPatchLoop) 
  ((looped-list :initform nil :accessor looped-list :initarg :looped-list))
  (:documentation "LIST TAIL ITERATOR FOR LOOPS.

Can be connected to an ITERATE box to control the iteration (FOR ITEM ON <list> ...)."))

(defmethod special-box-p ((name (eql 'loop-tail))) t)
(defmethod box-symbol ((self OMLoopTail)) 'loop-tail)

(defmethod omNG-make-special-box ((reference (eql 'loop-tail)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLoopTail :name "loop-tail")
   pos init-args))

(defmethod gen-iteration-code ((self OMLoopTail) &optional inputs) 
  (append `(for ,(it-var self) on ,(car inputs))
          (when (cadr inputs) `(by ,(cadr inputs)))))

(defmethod init-iterator-value ((self OMLoopTail) input-values)
  (setf (it-value self) (car input-values)))


(defmethod create-box-inputs-for-loop-box ((self OMLoopTail) box) 
  (list 
   (make-instance 
    'box-input :box box :value nil
    :name "list")))

;;; returns the default value
(defmethod allow-optional-input ((self OMLoopTail) existing-inputs)
  (and (<= (length existing-inputs) 1) 'cdr))


;;;------------------------------------

(defclass OMLoopWhile (OMPatchLoop) 
  ((loop-cond :initform nil :accessor loop-cond :initarg :loop-cond))
  (:documentation "WHILE ITERATOR FOR LOOPS.

Can be connected to an ITERATE box to control when the iteration should stop (WHILE <condition> DO ...)."))

(defmethod special-box-p ((name (eql 'loop-while))) t)
(defmethod box-symbol ((self OMLoopWhile)) 'loop-while)

(defmethod omNG-make-special-box ((reference (eql 'loop-while)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLoopWhile :name "loop-while")
   pos init-args))

;;; while goes after other iterators declarations
(defmethod index ((self OMLoopWhile)) 1)

(defmethod gen-iteration-code ((self OMLoopWhile) &optional inputs)
  (push-let-statement `(,(it-var self) nil))
  ; with ,(it-var self) = nil 
  `(while (setf ,(it-var self) ,(car inputs))))

(defmethod create-box-inputs-for-loop-box ((self OMLoopWhile) box) 
  (list 
   (make-instance 
    'box-input :box box :value t
    :name "condition")))



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
  ((n-iter :accessor n-iter :initform 0 :initarg :n-iter))
  (:documentation "ITERATOR FOR LOOPS.

Can be connected to other boxes (including iterator boxes: LOOP-LIST, LOOP-FOR, LOOP-WHILE...) to program an iterative process."))

(defclass OMPatchIteratorBox (OMPatchComponentBox) ())

(defmethod special-box-p ((name (eql 'iterate))) t)
(defmethod get-box-class ((self OMPatchIterator)) 'OMPatchIteratorBox)
(defmethod box-symbol ((self OMPatchIterator)) 'iterate)


(defmethod get-ev-once-flag ((self OMPatchIteratorBox)) (list self (n-iter (reference self))))

(defmethod get-icon-id ((self OMPatchIteratorBox)) :m-loop)

(defmethod object-name-in-inspector ((self OMPatchIteratorBox)) "ITERATOR box")

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
  (declare (ignore name doc))
  (add-optional-input self :name "action"
                      :value (if val-supplied-p value nil) 
                      :reactive reactive)
  t)

;;; we want to put the while boxes at the end 
;;; (they are the only ones that can be reevaluated and depend on other ones during the loop)
(defmethod collect-loop-boxes ((self OMPatch))
  (sort (get-boxes-of-type self 'OMPatchLoopBox)
        #'(lambda (type1 type2) 
            (declare (ignore type2))
            (if (subtypep type1 'OMLoopWhile) t nil)) 
        :key 'type-of))

(defmethod boxcall-value ((self OMPatchIteratorBox))
  
  (let ((old-context *ev-once-context*)
        (loopboxes (sort-boxes 
                    (remove-if 
                     #'(lambda (b) (not (is-connected-up-to self b)))
                     (collect-loop-boxes (container self))))))

    (if loopboxes 
        (mapc 'box-init-iterator-value loopboxes)
      (om-beep-msg "Warning: ITERATE box used without iterator in patch ~A !!" (name (container self))))

    (unwind-protect 
        (progn
          (setf (n-iter (reference self)) 0)
          (setf *ev-once-context* self)
          (push-let-context)
          (let* ((iterators-code 
                  (apply 'append 
                         (mapcar 'gen-loop-code-for-eval loopboxes)))
                 (update-iterators-value-code 
                  (apply 'append 
                         (mapcar #'(lambda (b) 
                                     (gen-loop-iterator-update-code (reference b))) 
                                 loopboxes)))
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
            
            (pop-let-context)
            ;;; (pprint loop-code) (terpri)
            (when iterators-code (eval loop-code))
            ))
     
      (setf *ev-once-context* old-context)
      (clear-ev-once (container self)))
    ))


(defun gen-loop-declarations (let-list)
  (loop for item in let-list append
        `(with ,(car item) = ,(cadr item))))

(defmethod gen-code ((self OMPatchIteratorBox) &optional args)

  (let ((loopboxes 
         (sort-boxes 
          (remove-if 
           #'(lambda (b) (not (is-connected-up-to self b)))
           (collect-loop-boxes (container self))))))
         
    ;(push-let-context)

    (unwind-protect 
        
        (if loopboxes 
            (progn
              
              ;;; no eval-once 
              (setf *freeze-eval-once-mechanism* t)
              (push-let-context)
            
              (let ((iterators-code (apply 'append (mapcar 'gen-loop-code-for-compile loopboxes)))
                    (declarations-code (gen-loop-declarations (output-current-let-context))))
                
                (pop-let-context)
                (setf *freeze-eval-once-mechanism* nil)
              
                (push-let-context)
                (unwind-protect 
                    (let ((loop-code (loop for inp in (inputs self) collect (gen-code inp))))
                      
                      (list 
                       `(loop ,.declarations-code
                              ,.iterators-code
                              do 
                              (let* ,(output-current-let-context)
                                ,.loop-code))
                       ))
                  (pop-let-context))
                ))
              
          ;;; else: no loop boxes
          (om-print-format "Warning: ITERATE box used without iterator in patch ~A !!" (list (name (container self))))
          )
      
      ;;; protect cleanup
      ;(pop-let-context)
      )
    ))


