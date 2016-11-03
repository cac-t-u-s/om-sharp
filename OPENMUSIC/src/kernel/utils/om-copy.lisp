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

(defmethod om-copy ((self t)) self)

(defmethod om-copy ((self standard-object)) 
  (clone-object self))

(defmethod clone-object ((object standard-object) &optional clone)
  (let ((new-obj (or clone (make-instance (type-of object)))))
    (loop for slot-name in (mapcar 'slot-definition-name (class-instance-slots (class-of object)))
          when (slot-exists-p new-obj slot-name)
          do (setf (slot-value new-obj slot-name) (om-copy (slot-value object slot-name))))
    new-obj))

;;; OM object copy/save only the slot with initargs and additional class attributes    
(defmethod clone-object ((object omobject) &optional clone)
  (let (initargs new-obj)
    (setf initargs 
          (loop for slot in (class-slots (class-of object))
                when (and (slot-definition-initargs slot)
                          (or (not clone) (slot-exists-p clone (slot-definition-name slot))))
                append (list (car (slot-definition-initargs slot))
                             ;(om-copy (slot-value self (slot-definition-name slot)))
                             (om-copy (funcall (slot-definition-name slot) object))
                             )))
    (setf new-obj (apply 'make-instance (cons (if clone (type-of clone) (type-of object)) initargs)))

    (mapcar #'(lambda (att) 
                (when (and (slot-exists-p object att) (slot-exists-p new-obj att))
                  (setf (slot-value new-obj att) (slot-value object att))))
            (additional-class-attributes object))
    (om-init-instance new-obj nil)))

(defmethod clone-object ((self t) &optional clone)
  (om-beep-msg "Can not clone objects of type ~A !" (type-of self)))
 


