
(in-package :om)

(defclass* OMArray () 
   ((num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements (a.k.a lines)")
    (num-fields :initform 1 :initarg :num-fields  :accessor num-fields :documentation "number of fields (a.k.a columns)")
    (field-names :initform nil :initarg :field-names :accessor field-names :documentation "name of fields")
    (data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

(defmethod get-col ((self OMArray) (col integer))
  (nth col (data self)))

(defmethod get-col ((self OMArray) (col string))
  (let ((pos (position col (field-names self) :test 'string-equal)))
    (if pos (get-col self pos)
      (om-beep-msg "Field '~A' not found in '~A'" col self))))


