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

(in-package :om)

;===========================
;METAOBJECTS
;===========================
;; OMPersistantObject
(defclass OMClass (standard-class OMBasicObject) 
   ((library :initform nil :accessor library))
   (:documentation "Metaclass for classes."))

(defmethod omclass-p ((self t)) nil)
(defmethod omclass-p ((self OMClass)) t)
(defmethod omclass-p ((self symbol)) 
  (and (find-class self nil) (omclass-p (find-class self))))


(defmethod slot-missing ((class OMClass) instance slot-name operation &optional new-value)
  (om-beep-msg "!!! Attempt to access non existing slot ~A in class ~A !!!" slot-name (class-name class)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class omclass) (super standard-class)) t)
)

(defclass OMStandardClass (OMClass)  ()
   (:documentation "This is the current main metaclass, you can sub-class it and use the new class as current metaclass.
This class is an OMClass (unlike OMClass itself!).")
   (:metaclass OMClass))

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmethod validate-superclass ((class omstandardclass) (super standard-class)) t)
  (defmethod validate-superclass ((class standard-class) (super omstandardclass)) t)
  (defmethod validate-superclass ((class standard-class) (super omclass)) t)

)

(defclass OMGenericFunction (OMFuncallableBasicObject) 
   ((numouts :initform nil :accessor numouts)
    (protected-p :initform nil :accessor protected-p :initarg :protected-p)
    (inputs-default :initform nil :accessor inputs-default)
    (library :initform nil :accessor library)
    (inputs-doc :initform nil :accessor inputs-doc)
    (outputs-doc :initform nil :accessor outputs-doc)
    (inputs-menus :initform nil :accessor inputs-menus))
  (:default-initargs :protected-p t)
  (:documentation "The generic function meta-object.")
  (:metaclass  funcallable-standard-class)
  )

(defmethod omgenericfunction-p ((self t)) nil)
(defmethod omgenericfunction-p ((self OMGenericFunction)) t)
(defmethod omgenericfunction-p ((self symbol)) 
  (and (fboundp self) (omgenericfunction-p (fdefinition self))))

; OMPersistantObject
(defclass OMMethod (OMBasicObject standard-method) 
   ((saved-connections :initform nil :accessor saved-connections)
    (graph-fun :initform nil :accessor graph-fun)
    (compiled? :initform t :accessor compiled?)
    (class-method-p :initform nil :accessor class-method-p))
   (:documentation "The class of the method metaobject.")
   (:default-initargs :protected-p t)
   (:metaclass omstandardclass))


(defclass OMSlot (OMBasicObject)
   ((classname  :initarg :classname :accessor classname)
    (thetype :initform t :initarg :thetype :accessor thetype)
    (theinitform :initarg :theinitform :accessor theinitform)
    (alloc  :initarg :alloc :accessor alloc)
    (slot-package  :initarg :slot-package :accessor slot-package)
    (io-p  :initarg :io-p :accessor io-p)
    (doc  :initarg :doc :accessor doc))
   (:documentation "Instance of this class allow define graphicly slot of omClasses.")
   (:metaclass omstandardclass))

; There are not all basic classes from lisp, 
; you can add easy new basic types if they have a building class correspondant, 
; for this see the function initbasic-lisp-types.
(defclass OMBasictype (OMBasicObject)
   ((defval :initform nil :initarg :defval :accessor defval))
   (:documentation "This class implements building class in Common Lisp because we can not inherit for building class we use delegation."))



