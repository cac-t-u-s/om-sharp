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

;===============================================================================
;SPECIAL BOXES
;===============================================================================

;;;------------------------
;;; SEQUENCE
;;;------------------------

(in-package :om)

(defmethod* seq  ((op t) &rest op+) :numouts 1 
   :initvals '(nil) :indoc '("something to do" "something else to do")
   :icon 161
   :doc "Evaluates sequentially a series of values, functions or subpatches.

Accepts many optional inputs as needed. 

The outputs correspond to each of the inputs. To get the result of the nth item connected to the sequence box after the sequential evaluation, evaluate or connect the nth output (all the inputs will be evaluated anyway).

Mind using this box in 'eval-once' mode when connected to several other boxes."

   (values-list (cons op op+)))
   
(defclass OMBoxSeqCall (OMGFBoxcall) ())

(defmethod boxclass-from-function-name ((self (eql 'seq))) 'OMBoxSeqCall)

;;; shortcut (compatibility)


(defmethod special-box-p ((name (eql 'sequence))) t)
(defmethod omNG-make-special-box ((reference (eql 'sequence)) pos &optional init-args)
  (omNG-make-new-boxcall (fdefinition 'seq) pos init-args))

(defmethod add-optional-input ((self OMBoxSeqCall) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore value doc reactive))
  (call-next-method)
  (set-box-outputs self  
                   (append (outputs self)
                           (list (make-instance 'box-optional-output 
                                                :name (format nil "~A~D" name (length (outputs self)))
                                                :box self
                                                :doc-string (get-input-doc self name)))))
  (setf (lock-state self) :eval-once)
  (update-inspector-for-object self))

(defmethod remove-one-optional-input ((self OMBoxSeqCall))
  (when (call-next-method)
    (set-box-outputs self (butlast (outputs self)))))


;;;------------------------
;; SPLIT LIST
;;;------------------------
(defmethod* list-elements ((list list) &rest add-output)  
  :initvals '(nil) 
  :indoc '("a list")
  :doc 
"Returns the elements of the list on different ouputs (up to 50 elements).

Use > and < to add/remove outputs.

It is advised to use this box in mode 'eval once' in order to avoid useless computations.
"
  :icon 'list
  :numouts 1
  (values-list (first-n list 50)))


(defclass OMBoxSplit (OMGFBoxcall) ())
(defmethod boxclass-from-function-name ((self (eql 'list-elements))) 'OMBoxSplit)

(defmethod boxcall-value ((self OMBoxSplit))
  (values-list (first-n (omNG-box-value (car (inputs self))) (length (outputs self)))))


(defmethod allow-add-inputs ((self OMBoxSplit)) (< (length (outputs self)) 50))
(defmethod allow-remove-inputs ((self OMBoxSplit)) (> (length (outputs self)) 1))

;; on this special box adding an input actually adds an ouput...
(defmethod more-optional-input ((self OMBoxSplit) &key name (value nil val-supplied-p) doc reactive)
  ;;; no checks
  (add-optional-input self) 
  t) 

(defmethod add-optional-input ((self OMBoxSplit) &key name (value nil val-supplied-p) doc reactive)
  (declare (ignore name value doc reactive))
  (set-box-outputs 
   self 
   (append (outputs self)
           (list (make-instance 'box-optional-output 
                                :name (format nil "out~D" (length (outputs self))) 
                                :box self))
           ))
  
  (update-inspector-for-object self)
  t)

(defmethod remove-one-optional-input ((self OMBoxSplit))
  (when (get-optional-inputs self)
    (set-box-outputs self (butlast (outputs self)))))

;; hack: all inputs (actually, ouputs) can be removed as "optional"
(defmethod get-optional-inputs ((self OMBoxSplit)) (cdr (outputs self)))

(defmethod save-outputs? ((self OMBoxSplit)) t)

(defmethod restore-outputs ((self OMBoxSplit) outputs)
  (when (outputs self)
    (setf (outputs self) (list (car (outputs self)))))
  (loop for o in (cdr outputs) for n from 1 do
        (add-optional-input self))
  (call-next-method))
 
(defmethod add-args-to-box ((box OMBoxSplit) args)
  (let ((n (if (numberp (car args)) (car args) 2)))
    (dotimes (i (- n (length (outputs box))))
      (add-optional-input box))
    (setf (lock-state box) :eval-once)))


;;;------------------------
;; HUB / PROXY
;;;------------------------
(defmethod* hub (value &rest add-output)  
  :initvals '(nil) 
  :indoc '("anthing")
  :doc 
"The same value to all its outputs.

Use > and < to add/remove outputs.

It is advised to use this box in mode 'eval once' in order to avoid useless computations.
"
  :icon 235
  :numouts 0
  (values-list (first-n list 50)))


(defclass OMBoxHub (OMBoxSplit) ())
(defmethod boxclass-from-function-name ((self (eql 'hub))) 'OMBoxHub)
(defmethod boxcall-value ((self OMBoxHub))
  (values-list (make-list (length (outputs self)) 
                          :initial-element (omNG-box-value (car (inputs self))))))

(defmethod add-args-to-box ((box OMBoxHub) args)
  (let ((n (if (numberp (car args)) (car args) 1)))
    (dotimes (i n) (add-optional-input box))))

;;;------------------------
;;; REPEAT-N
;;;------------------------

(defmethod* repeat-n  ((self t) (n integer)) :numouts 1 :initvals '(nil 0) :indoc '("something" "times")
  :doc "Repeats <n> times the evaluation of <self> and collects the <n> results into a list.

Ex. (repeat-n (+ 1 1) 4) ==> (2 2 2 2)" 
  :icon 'repeat
  (loop for i from 1 to n collect (eval self)))

(defclass OMRepeatNBoxCall (OMGFBoxcall) ())
(defmethod boxclass-from-function-name ((self (eql 'repeat-n))) 'OMRepeatNBoxCall)

(defmethod boxcall-value ((self OMRepeatNBoxCall)) 
   (loop for i from 1 to (omNG-box-value (cadr (inputs self)))
         collect (omNG-box-value (car (inputs self)))))

(defmethod box-lambda-value ((self OMRepeatNBoxCall))
   (multiple-value-bind (new-symbs args) 
       (get-args-eval-curry self #'(lambda (input) `',(omNG-box-value input)))
     (let ((arglist (apply 'append args)))  ;;; flat the arg-list (for keywords etc.)
       (eval `#'(lambda ,new-symbs
                  (loop for i from 1 to ,(cadr arglist)
                        collect ,(car arglist))
                  )))))

(defmethod gen-code-for-call ((self OMRepeatNBoxCall) &optional args)
  `(loop for i from 1 to ,(gen-code (cadr (inputs self))) 
         collect ,(gen-code (car (inputs self)))))

(defmethod gen-code-lambda ((self OMRepeatNBoxCall) &optional numout)
  (declare (ignore numout))
  (multiple-value-bind (new-symbs args) 
      (get-args-eval-curry self #'gen-code)
    `#'(lambda ,new-symbs
         (loop for i from 1 to ,(caadr args) 
               collect ,(caar args))
         )))

;;;------------------------
;;; IF
;;;------------------------

(defmethod* OMIF ((test t) (action t) &optional else) 
   :numouts 1 
   :initvals '(nil nil nil) 
   :indoc '("IF" "THEN" "ELSE")
   :doc "IF <test> THEN <action> ELSE <else>.

If the evaluation of <test> is not NIL, evaluates <action>. 
Otherwise evaluates <else> (when supplied) or returns NIL.

Ex. (omif (= 4 4) 'A 'B)  ==>  'A
Ex. (omif (= 4 5) 'A 'B)  ==>  'B
Ex. (omif (= 4 5) 'A)  ==>  NIL

" 
   :icon 180
   (if test action else))

(defclass OMIFBoxCall (OMGFBoxcall) ())
(defmethod boxclass-from-function-name ((self (eql 'omif))) 'OMIFBoxCall)

(defmethod boxcall-value ((self OMIFBoxCall)) 
  (let ((test (omNG-box-value (first (inputs self)))))
    (if test
        (omNG-box-value (second (inputs self)))
      (and (third (inputs self)) (omNG-box-value (third (inputs self)))))))

(defmethod gen-code-for-call ((self OMIFBoxCall) &optional args)
  (if args `(if ,(car args) ,(second args) ,(third args))
    `(if ,(gen-code (first (inputs self)))
         ,(gen-code (second (inputs self)))
       ,(gen-code (third (inputs self))))))


;;;------------------------
;;; LOGICAL CONTROLS (AND/OR)
;;;------------------------

(defmethod* OMOR  ((self t) &rest rest) :numouts 1 :initvals '(nil nil) :indoc '("something" "other things")
   :doc "Logical OR:
Yields the value T (true) if one at least among the values connected to it evaluates to T. 

Accepts as many optional inputs as needed.

OMOR can be used to compose conditions as input to an OMIF" 
   :icon 219
   (eval `(or ,self ,.rest)))


(defmethod* OMAND  ((self t) &rest rest) :numouts 1 :initvals '(nil nil) :indoc '("something" "other things")
   :doc "Logical AND :
Yields the value T (true) if all the values connected to it evaluates to T. 

Accepts as many optional inputs as needed.

OMAND can be used to compose conditions as input to an OMIF"
   :icon 218
   (eval `(and ,self ,.rest)))

(defclass OMAndBoxCall (OMGFBoxcall) ())
(defmethod boxclass-from-function-name ((self (eql 'omand))) 'OMAndBoxCall)
(defclass OMOrBoxCall (OMGFBoxcall) ())
(defmethod boxclass-from-function-name ((self (eql 'omor))) 'OMOrBoxCall)

(defmethod boxcall-value ((self OMAndBoxCall)) 
  (let ((rep t))
    (loop while rep
          for inp in (inputs self) do
          (setf rep (omNG-box-value inp)))
    rep))

(defmethod boxcall-value ((self OMorBoxCall)) 
  (let ((rep nil))
    (loop while (not rep) 
          for inp in (inputs self) do
          (setf rep (omNG-box-value inp)))
    rep))

(defmethod gen-code-for-call ((self OMAndBoxCall) &optional args)
  (let ((arguments (or args (gen-code-inputs self))))
    `(and ,.arguments)))
    
(defmethod gen-code-for-call ((self OMOrBoxCall) &optional args)
  (let ((arguments (or args (gen-code-inputs self))))
    `(or ,.arguments)))

;;;------------------------
;;; DEFAULT VALUE UTIL
;;;------------------------

(defmethod* default (value in)
  :indoc '("a default value" "input data")
  :outdoc '("the input data or the default value")
  :icon nil
  (or in value))
           
(defclass OMDefBoxCall (OMGFBoxcall) 
  ((def-value :initform nil :accessor def-value)))

(defmethod boxclass-from-function-name ((self (eql 'default))) 'OMDefBoxCall)

(defmethod boxcall-value ((self OMDefBoxCall)) 
  (setf (def-value self) (omNG-box-value (first (inputs self))))
  (call-next-method))

(defmethod box-draw ((self OMDefBoxCall) (frame OMBoxFrame))
  (om-with-font 
   (om-def-font :font1b)
   (om-draw-string 65 18 (format nil "~A" (or (def-value self) "?")))))

