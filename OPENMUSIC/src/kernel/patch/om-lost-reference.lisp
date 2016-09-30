

(in-package :om)

;-------------------------------------------
; BOX FOR LOST FUNCTIONS
;-------------------------------------------

(defclass LostReferenceBox (OMBoxcall) 
  ((reference-type :accessor reference-type :initform nil)
   (reference-name :accessor reference-name :initform nil))
  (:metaclass omstandardclass))
