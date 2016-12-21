(in-package :om)

(defmethod om-copy ((self list))
  (mapcar 'om-copy self))

(defmethod om-copy ((self hash-table))
  (let ((hashtable (make-hash-table))
        keylist vallist)
    (maphash #'(lambda (key val)
                 (push key keylist)
                 (push val vallist)) self)
    (loop for key in (om-copy (reverse keylist))
          for val in (om-copy (reverse vallist)) do
          (setf (gethash key hashtable) val))
    hashtable))


(defmethod om-copy ((self oa::ompoint))
  (om-make-point (om-point-x self) (om-point-y self)))

(defmethod om-copy ((self oa::omcolor))
  (om-make-color (om-color-r self) 
                 (om-color-g self) 
                 (om-color-b self)))

(defmethod om-copy ((self oa::om-pointer))
  (oa::om-retain self)
  self)

(defmethod om-copy ((self t)) self)

(defmethod om-copy ((self standard-object)) 
  (clone-object self))

;;; the slot must exit in the target object
(defmethod condition-for-copy-slot ((from t) (to t) slot)
  (slot-exists-p to (slot-definition-name slot)))

;;; OMObject copy/save only the slot with initargs
(defmethod condition-for-copy-slot ((from OMObject) (to t) slot)
  (and (call-next-method)
       (slot-definition-initargs slot)))

(defmethod clone-object ((object standard-object) &optional clone)
  (let ((new-object (or clone (make-instance (type-of object)))))
    (loop for slot in (class-instance-slots (class-of object))
          when (condition-for-copy-slot object new-object slot)
          do (setf (slot-value new-object (slot-definition-name slot)) 
                   (om-copy (slot-value object (slot-definition-name slot)))))
    (initialize-instance new-object)))


#|
(defmethod clone-object ((object omobject) &optional clone)
  (let (initargs new-obj)
    (setf initargs 
          (loop for slot in (class-slots (class-of object))
                when (and (slot-definition-initargs slot)
                          (or (not clone) (slot-exists-p clone (slot-definition-name slot))))
                append (list (car (slot-definition-initargs slot))
                             (om-copy (slot-value object (slot-definition-name slot)))
                             )))
    (setf new-obj (apply 'make-instance (cons (if clone (type-of clone) (type-of object)) initargs)))
    (mapcar #'(lambda (att) 
                (when (and (slot-exists-p object att) (slot-exists-p new-obj att))
                  (setf (slot-value new-obj att) (slot-value object att))))
            (additional-class-attributes object))
    (om-init-instance new-obj nil)))

(defmethod clone-object ((self t) &optional clone)
  (om-beep-msg "Can not clone objects of type ~A !" (type-of self)))
|#


;;============================================================================
;; VERY BASIC CLIPBOARD (INTERNAL FOR OM OBJECTS)
;;============================================================================
(defparameter *om-clipboard* nil)
(defparameter *om-clip-pos* nil)
(defun set-om-clipboard (value) (setf *om-clipboard* value))
(defun get-om-clipboard () *om-clipboard*)
(defun set-paste-position (position &optional panel) (setf *om-clip-pos* (if position (list panel position) nil)))
(defun get-paste-position (panel) (when (equal panel (car *om-clip-pos*)) (cadr *om-clip-pos*)))


