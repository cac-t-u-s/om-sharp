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

;===============================================================================
; CONTROL BOXES
; INFLUENCE THE EVALUATION ORDER 
;===============================================================================

(in-package :om)

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