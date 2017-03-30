;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;*** 
; This file contains the super classes for OM laguage objects and metaobjects.
;***

(in-package :om)


;===========================
;METAOBJECTS
;===========================
;; OMPersistantObject
(defclass OMClass (standard-class OMBasicObject) 
   ((library :initform nil :accessor library)
    (internal-met :initform nil :accessor internal-met)
    ;(slot-docs :initform nil :accessor slot-docs)
    )
   (:documentation "OM meta-class. #enddoc#
#seealso# (omslot ommethod omstandardclass) #seealso#
#lib-class-p# Non nil, if the function was defined in a Library. #lib-class-p#
#internal-met# A list with initialization and R/W slots methods. #internal-met#
#doc# Doc of somes persistants object are saved also. #doc#"))

(defmethod omclass-p ((self t)) nil)
(defmethod omclass-p ((self OMClass)) t)

(defmethod slot-missing ((class OMClass) instance slot-name operation &optional new-value)
  (om-beep-msg "!!! Attempt to access non existing slot ~A in class ~A !!!" slot-name (class-name class))) 

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod validate-superclass ((class omclass) (super standard-class)) t)
)

(defclass OMStandardClass (OMClass)  ()
   (:documentation "This is the current OM meta-class, you can sub-class it and used the new class as current meta-class.
This class is a OMClass a diferencia of OMClass.#enddoc#
#seealso# (omclass) #seealso#")
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
  (:documentation "The generic function meta-object in OM. #enddoc#
#seealso# (ommethod) #seealso#
#numouts# Multiple values are allowed in OM, but it must be specified in the definition of the function. #numouts#
#inputs-default# This slot containt a list of default values for each arg in the function's lambda list. #inputs-default#
#library# Non nil if the function was defined in a Library. #library#
#inputs-doc# This slot containt a list of string doc for each arg in the function's lambda list. #inputs-doc#
#inputs-menus# Some arg choose values from a pop-up-menu, this slot contains this information #inputs-menus#")
  (:metaclass  funcallable-standard-class)
  )


; OMPersistantObject
(defclass OMMethod (OMBasicObject standard-method) 
   ((saved-connections :initform nil :accessor saved-connections)
    (graph-fun :initform nil :accessor graph-fun)
    (compiled? :initform t :accessor compiled?)
    (class-method-p :initform nil :accessor class-method-p))
   (:documentation "The class of the OM method metaobject. #enddoc#
#seealso# (omgenericfunction) #seealso#
#saved-connections# Used to save connections beetween boxes in the method definition. #saved-connections#
#graph-fun# A list of the boxes which define the method. #graph-fun#
#compiled?# Nil if the method was modified and not compiled. #compiled?#
#class-method-p# T if the method is a class method. 
Class methods are the init-instance method and slot reader and writer. #class-method-p#")
   (:default-initargs :protected-p t)
   (:metaclass omstandardclass))

;========== OM

(defclass OMSlot (OMBasicObject)
   ((classname  :initarg :classname :accessor classname)
    (thetype :initform t :initarg :thetype :accessor thetype)
    (theinitform :initarg :theinitform :accessor theinitform)
    (alloc  :initarg :alloc :accessor alloc)
    (slot-package  :initarg :slot-package :accessor slot-package)
    (io-p  :initarg :io-p :accessor io-p)
    (doc  :initarg :doc :accessor doc))
   (:documentation "Instance of this class allow define graphicly slot of omClasses. #enddoc#
#seealso# (omclass OMBasicObject) #seealso#
#classname# A name with the class which contains the slot. #classname#
#thetype# OM slots are typed, the default type if T, however, there are not type checking in OM. #thetype#
#theinitform# Initform value for the slot. #theinitform#
#alloc# 'class or 'instance #alloc#
#io-p# Public or private slot flag. #io-p#")
   (:metaclass omstandardclass))

(defclass OMBasictype (OMBasicObject)
   ((defval :initform nil :initarg :defval :accessor defval))
   (:documentation "This class implements building class in Common Lisp
because we can not inherite for building class we use delegation.
There are not all basic classes from lisp, you can add eassy new basic types if
they have a building class correspondant, for this see the function initbasic-lisp-types.#enddoc#
#seealso# (OMBasicObject) #seealso#
#defval# The default value for the Basic Type.#defval#"))



