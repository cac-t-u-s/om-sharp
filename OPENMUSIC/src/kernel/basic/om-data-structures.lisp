;===============================================================================
;BASIC TOOLS FOR DATA STRUCTIURES MANAGEMENT IN VISUAL PROGRAMS
;===============================================================================

(in-package :om)

;;;====================
;;; SIMULATION OF VARIABLES
;;;====================

(defclass* store (named-object)
   ((value :initform nil :initarg :value :accessor value :documentation "the value of the variable"))
   (:icon 'store)
   (:documentation "A general storage class used to simulate variables in teh visual language. Can be associated to global variables.

The slot <value> can contain any kind of data, including instances of other classes. 
It can be accessed (get/set) using the methods get-slot / set-slot or using the SLOTS box (type 'store slots'). ")
   )

;;;====================
;;; COPY
;;;====================

(defmethod* clone ((self t)) 
   :icon 205
   :indoc '("object")
   :outdoc '("copy")
   :doc "Makes and returns a copy of an object."
   (om-copy self))


;;;====================
;;; SET/GET SLOTS VALUES
;;;====================

;;;; Alternatively we could use set-slot-val to call standard accessors here...
(defmethod* get-slot ((object t) (slot symbol))
   :initvals '(nil nil) 
   :indoc '("object" "slot name") 
   :icon 'om-get-slot
   :doc "Returns the value of an object's slot. 

<object> must be an object instance (e.g. the first output of a factory box). 
<slot> is the name of an existing slot of the corresponding class's slots. 

Warning : It is advised not to use GET-SLOT with some predefined OM object, which have particular internal slots value management.
In this case, prefer the get/set slots mechanism provided by the SLOTS boxes (shift+drag an object or factory box, type '<class-name> slots')."
   (if (slot-exists-p object slot)
     (slot-value object (intern-pack (string slot) (symbol-package (type-of object))))
     (om-beep-msg "SLOT ~A does not exist in class ~A" slot (type-of object))))

(defmethod* get-slot ((object list) (slot symbol))
   (loop for item in object collect  (get-slot item slot)))

(defmethod* set-slot ((object t) (slot symbol) (value t))
   :initvals '(nil nil nil) 
   :indoc '("object" "slot" "value") 
   :icon 335
   :doc
   "Modifies the value of an object's slot. 

<object> must be an object instance (e.g. the first output of a factory box, or the output of an instance or global variable box). 
<slot> is a slot name corresponding to one of the corresponding classe's slots. 
<value> is the new value to set in the <slot> field of <object>

Returns the modified object <object>.

Warning : It is advised not to use GET-SLOT with predefined OM object, which have particular internal slots value management.
Use rather the get/set slots mechanism provided by the SLOTS boxes (shift+drag an object or factory box).
"
   (if (slot-exists-p object slot)
       (setf (slot-value object (intern (string slot) (symbol-package (type-of object)))) value)
     (om-beep-msg "SLOT ~A does not exist in class ~A" slot (type-of object))))
  
(defmethod* set-slot ((object list) (slot symbol) (value t))
   (loop for item in object do (set-slot item slot value)))


