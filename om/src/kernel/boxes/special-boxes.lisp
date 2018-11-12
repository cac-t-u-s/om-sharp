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
; SPECIAL BOXES
; BUT STILL 'FUNCTIONS' 
;===============================================================================


(in-package :om)


;;;--------------------------
;;; SEQ: SEQUENCE OPERATIONS
;;;--------------------------

;;; !! todo check in/outs with undo

(defmethod* seq  ((op t) &rest op+) :numouts 1 
   :initvals '(nil) :indoc '("something to do" "something else to do")
   :icon :seq
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
  ;(setf (lock-state self) :eval-once)
  (update-inspector-for-object self))

(defmethod remove-one-optional-input ((self OMBoxSeqCall))
  (when (call-next-method)
    (set-box-outputs self (butlast (outputs self)))))


;;;------------------------------------------
;; SPLIT A LIST in ITS OUTPUTS
;;;------------------------------------------
(defmethod* split ((list list) &rest add-output)  
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
(defmethod boxclass-from-function-name ((self (eql 'split))) 'OMBoxSplit)

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
    ;;(setf (lock-state box) :eval-once)
    ))


;;;---------------------------------------------------------------------
;; HUB: PROXY TO USE THE SAME (UNDEFINED) VALUE AT DIFFERENT OUTPUT PORTS
;;;---------------------------------------------------------------------
;;; todo: allow several inputs, too => all return the same

;; It is advised to use this box in mode 'eval once' in order to avoid useless computations.
(defmethod* hub (value &rest add-output)  
  :initvals '(nil) 
  :indoc '("anthing")
  :doc 
"The same value to all its outputs.

Use > and < to add/remove outputs.

"
  :icon 'through
  :numouts 1
  value)


(defclass OMBoxHub (OMBoxSplit) ())
(defmethod boxclass-from-function-name ((self (eql 'hub))) 'OMBoxHub)

(defmethod boxcall-value ((self OMBoxHub))
  (values-list (make-list (length (outputs self)) 
                          :initial-element (omNG-box-value (car (inputs self))))))


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




