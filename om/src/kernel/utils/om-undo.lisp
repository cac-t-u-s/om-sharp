;============================================================================
; o7: visual programming language for computer-aided music composition
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

;============================================================================
; UNDO/REDO MANAGEMENT
;============================================================================

(in-package :om)

;;; each undo-able object shoudl be a subclass of this 
(defclass undoable-object-mixin ()
 ((level :initarg :level :initform 5 :accessor level)
  (undo-stack :initform nil :accessor undo-stack)
  (redo-stack :initform nil :accessor redo-stack)
  (last-action :initform nil :accessor last-action)
  (last-item :initform nil :accessor last-item)
  ))

;;; call this before any action which might require undo
(defmethod notify-state-before-action ((self undoable-object-mixin) &key action item)
  (unless (and action
               (equal action (last-action self))
               (equal item (last-item self)))
    ;;; this is a new 'key state' we want to store
    (push-undo-state self))
  (setf (last-action self) action
        (last-item self) item))
               
(defmethod push-undo-state ((self undoable-object-mixin))
  (let ((state (get-undoable-object-state self)))
    (push state (undo-stack self)) 
    (when (> (length (undo-stack self)) (level self))
      (setf (undo-stack self) (butlast (undo-stack self))))
    ))

(defmethod push-redo-state ((self undoable-object-mixin))
  (let ((state (get-undoable-object-state self)))
    (push state (redo-stack self))
    (when (> (length (redo-stack self)) (level self))
      (setf (redo-stack self) (butlast (redo-stack self))))
    ))

;;; call this to undo
(defmethod do-undo ((self undoable-object-mixin))
  (setf (last-action self) nil
        (last-item self) nil)
  (if (undo-stack self)
    (progn 
      ;;; push state in redo-stack
      (push-redo-state self)
      ;;; restore state from undo-stack
      (let ((restored-state (pop (undo-stack self))))
        (print restored-state)
        (restore-undoable-object-state self restored-state))
      )
    (om-beep)))

;;; call this to redo
(defmethod do-redo ((self undoable-object-mixin))
  (setf (last-action self) nil
        (last-item self) nil)
  (if (redo-stack self)
    (progn 
      ;;; push state in undo-stack
      (push-undo-state self)
      ;;; restore state from redo-stack
      (let ((restored-state (pop (redo-stack self))))
        (restore-undoable-object-state self restored-state))
      )
    (om-beep)))

;;; all objects can implement this method (not only undoable-object-mixin)
;;; e.g. slots or deep-contained objects
(defmethod get-object-slots-for-undo ((self t)) nil)
(defmethod get-object-slots-for-undo ((self OMEditor)) '(object))
(defmethod get-object-slots-for-undo ((self OMPatch)) '(boxes connections))

(defmethod update-after-state-change ((self t)) nil)
(defmethod update-after-state-change ((self OMEditor)) (om-invalidate-view (main-view self)))

(defmethod update-after-state-change ((self patch-editor))
  (let* ((patch (object self))
         (view (main-view self)))
    (om-remove-all-subviews view)
    (put-patch-boxes-in-editor-view patch view)))

;(defmethod update-after-state-change ((self OMBox))
;  (update-frame-to-box-size self)
;  (update-frame-to-box-position self))

(defmethod get-object-slots-for-undo ((self standard-object)) 
  (loop for slot in (class-instance-slots (class-of self))
        when (slot-definition-initargs slot)
        collect (slot-definition-name slot)))


;;; GENERAL CASE
(defmethod get-undoable-object-state ((self t)) (om-copy self))
(defmethod restore-undoable-object-state ((self t) state) state)

;;; OBJECTS
;;; collect/restore state...
(defmethod get-undoable-object-state ((self standard-object)) 
  (om-print-dbg "collecting state of ~A" (list self) "UNDO")
  (loop for slot in (get-object-slots-for-undo self)
        collect (list slot (get-undoable-object-state (slot-value self slot)))))

(defmethod restore-undoable-object-state ((self standard-object) (state list)) 
  (om-print-dbg "restoring state of ~A" (list self) "UNDO")
  (loop for slot in (get-object-slots-for-undo self)
        do (setf (slot-value self slot) 
                 (restore-undoable-object-state (slot-value self slot)
                                                (cadr (find slot state :key 'car)))))
  self)

;;; LISTS
(defmethod get-undoable-object-state ((self list)) 
  (loop for item in self
        collect (list item (get-undoable-object-state item))))

;;; restore the list as stored, restore each object
(defmethod restore-undoable-object-state ((self cons) (state list)) 
  (loop for item in state 
        do (restore-undoable-object-state (car item) (cadr item))
        collect (car item)))

;;; PATCHES    
(defmethod get-undoable-object-state ((self OMPatch)) 
  `((boxes ,(get-undoable-object-state (boxes self)))
    (connections ,(save-connections-from-boxes (boxes self)))))

(defmethod restore-undoable-object-state ((self OMPatch) (state list)) 
  (let* ((boxes (restore-undoable-object-state 
                 (slot-value self 'boxes)
                 (cadr (find 'boxes state :key 'car))))
         (connections (restore-connections-to-boxes 
                       (cadr (find 'connections state :key 'car)) 
                       boxes)))
    (setf (boxes self) boxes)
    (setf (connections self) connections)
    self))
    
(defmethod restore-undoable-object-state ((self OMEDitor) (state list))
  (call-next-method)
  (update-after-state-change self)
  self)


