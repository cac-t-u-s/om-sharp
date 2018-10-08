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
                 (om-color-b self)
                 (om-color-a self)
                 ))

(defmethod om-copy ((self oa::om-pointer))
  (oa::om-retain self)
  self)

(defmethod om-copy ((self t)) self)

;;; clone-object doesn't om-init the object
;;; om-copy does
(defmethod om-copy ((self standard-object)) 
  (om-init-instance (clone-object self)))

(defmethod excluded-slots-from-copy ((from t)) nil)

(defmethod additional-slots-to-copy ((from t)) 
  (additional-class-attributes from))

;;; the slot must exit in the target object
;;; .. and not be excluded!
(defmethod condition-for-copy-slot ((from t) (to t) slot)
  (and (slot-exists-p to (slot-definition-name slot))
       (or (slot-definition-initargs slot)
           (member (slot-definition-name slot) (additional-slots-to-copy from)))
       (not (member (slot-definition-name slot) (excluded-slots-from-copy from)))
       ))

;;; OMObject copy/save only the slot with initargs
(defmethod condition-for-copy-slot ((from OMObject) (to t) slot)
  (and (call-next-method) (slot-definition-initargs slot)))

;;; clone-object doesn't om-init the object
;;; om-copy does
(defmethod clone-object ((object standard-object) &optional clone)
  ;(om-print-dbg "=================== OBJECT ~A" (list object))
  (let ((new-object (or clone (clos::allocate-instance (class-of object)))))
    (loop for slot in (class-instance-slots (class-of object))
          when (condition-for-copy-slot object new-object slot)
          do ; (om-print-dbg "SLOT ~A" (list (slot-definition-name slot)))
          (setf (slot-value new-object (slot-definition-name slot)) 
                (om-copy (slot-value object (slot-definition-name slot)))))
    (initialize-instance new-object)
    new-object))


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


