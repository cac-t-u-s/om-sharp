
(in-package :om)

(defclass* OMArray () 
   ((num-rows :initform 1 :initarg :num-rows  :accessor num-rows :documentation "number of lines/rows")
    (num-cols :initform 1 :initarg :num-cols  :accessor num-cols :documentation "number of fields (a.k.a columns)")
    (col-names :initform nil :initarg :col-names :accessor col-names :documentation "name of fields")
    (data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

(defmethod get-col ((self OMArray) (col integer))
  (nth col (data self)))

(defmethod get-col ((self OMArray) (col string))
  (let ((pos (position col (col-names self) :test 'string-equal)))
    (if pos (get-col self pos)
      (om-beep-msg "Field '~A' not found in '~A'" col self))))


