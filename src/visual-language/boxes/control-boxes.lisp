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

;===============================================================================
; CONTROL BOXES
; INFLUENCE THE EVALUATION ORDER
;===============================================================================

(in-package :om)

; The functions IF, AND, OR in this file are special function that are presented
; in the packages as special-boxes.

;;;------------------------
;;; IF
;;;------------------------

(defclass OMIFBoxCall (OMFunBoxcall) ())

(defmethod special-box-p ((name (eql 'if))) t)
(defmethod get-box-class ((self (eql 'if))) 'OMIFBoxCall)
(defmethod get-icon-id ((self OMIFBoxCall)) :cond)
(defmethod box-type ((self OMIFBoxCall)) :special)

(defmethod omNG-make-special-box ((reference (eql 'if)) pos &optional (init-args nil args-supplied-p))
  (omNG-make-new-boxcall reference pos init-args))

(defmethod create-box-inputs ((self OMIFBoxCall)) (call-next-method))

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


(setf (documentation 'if 'function)
      "Conditional statement:
Syntax: (IF <predicate> <then> &optional <else>).

- If <predicate> evaluates to non-null, eval <then> input and return the result.
- If not, eval and return <else> input, or return NIL.
")


;;; compatibility
(defmethod function-changed-name ((reference (eql 'omif))) 'if)


;;;------------------------
;;; LOGICAL CONTROLS (AND/OR)
;;;------------------------

(defclass OMAndBoxCall (OMFunBoxcall) ())
(defmethod get-box-class ((self (eql 'and))) 'OMAndBoxCall)

(defclass OMOrBoxCall (OMFunBoxcall) ())
(defmethod get-box-class ((self (eql 'or))) 'OMOrBoxCall)

(defmethod special-box-p ((name (eql 'and))) t)
(defmethod special-box-p ((name (eql 'or))) t)

(defmethod get-icon-id ((self OMAndBoxCall)) :and)
(defmethod get-icon-id ((self OMOrBoxCall)) :or)

(defmethod box-type ((self OMAndBoxCall)) :special)
(defmethod box-type ((self OMOrBoxCall)) :special)

(defmethod omNG-make-special-box ((reference (eql 'and)) pos &optional init-args)
  (omNG-make-new-boxcall reference pos init-args))

(defmethod omNG-make-special-box ((reference (eql 'or)) pos &optional init-args)
  (omNG-make-new-boxcall reference pos init-args))

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


(setf (documentation 'and 'function)
      "Evaluate inputs in order, left to right.

- If any eval to NIL, quit and return NIL.
- Else, return the value(s) of the last input.")

(setf (documentation 'or 'function)
      "Evaluate inputs in order, left to right.

- If any eval to non-NIL, quit and return that (single) value.
- If the last input is reached, return whatever value(s) it returns.")


;;; compatibility
(defmethod function-changed-name ((reference (eql 'omand))) 'and)
(defmethod function-changed-name ((reference (eql 'omor))) 'or)
(defmethod function-changed-name ((reference (eql 'conditional))) 'or)


