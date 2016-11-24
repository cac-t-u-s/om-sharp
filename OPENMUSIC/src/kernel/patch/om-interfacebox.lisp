

(in-package :om)

(defclass InterfaceBox (OMBox) ())

(defvar *interfaceboxes* 
  (omNG-make-package "Interface Boxes"
                     :container-pack *om-package-tree*
                     :doc "This package contains special interface boxes and widgets to use in OM patches (sliders, buttons, etc.)"))


(defmethod create-box-outputs ((self InterfaceBox))
  (list (make-instance 'box-output :box self :name "value")))

(defclass SliderBox (InterfaceBox)
  ((min-value :accessor min-value :initarg :min-value :initform 0)
   (max-value :accessor max-value :initarg :max-value :initform 100)
   (increment :accessor increment :initarg :increment :initform 1)
   (orientation :accessor orientation :initarg :orientation :initform :vertical)
   (action :accessor action :initarg :action :initform nil)))

(AddSpecialItem2Pack 'slider *interfaceboxes*)

(defmethod special-box-p ((self (eql 'slider))) t)

(defmethod get-all-keywords ((self SliderBox))
  '((:min-value :max-value :increment :orientation :action)))
 
(defmethod omNG-make-special-box ((reference (eql 'slider)) pos &optional init-args)
  (let* ((box (make-instance 'SliderBox
                             :name "slider"
                             :reference 'slider)))
    (print init-args)
    (setf (value box) 50   
          (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (inputs box) nil)
    (let ((size (om-max-point (minimum-size box) (default-size box))))
      (setf (box-w box) 30
            (box-h box) 50)
      )
    box))