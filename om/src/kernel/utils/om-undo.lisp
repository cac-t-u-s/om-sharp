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

;============================================================================
; UNDO/REDO MANAGEMENT
;============================================================================

(in-package :om)

;;; => each undo-able object should be a subclass of this 
(defclass undoable-editor-mixin ()
 ((level :initarg :level :initform 10 :accessor level)
  (undo-stack :initform nil :accessor undo-stack)
  (redo-stack :initform nil :accessor redo-stack)
  (last-action :initform nil :accessor last-action)
  (last-item :initform nil :accessor last-item)
  ))

(defmethod undoable-object ((self undoable-editor-mixin)) (object self))

(defmethod undo-command ((self undoable-editor-mixin)) 
  (when (undo-stack self) 
    #'(lambda () (do-undo self))))

(defmethod redo-command ((self undoable-editor-mixin)) 
  (when (redo-stack self)
    #'(lambda () (do-redo self))))

(defmethod get-undoable-editor-state ((self undoable-editor-mixin))
  (get-undoable-object-state (undoable-object self)))

(defmethod restore-undoable-editor-state ((self undoable-editor-mixin) (state list))
  (restore-undoable-object-state (undoable-object self) state)
  (update-after-state-change self)
  )

(defmethod reset-undoable-editor-action ((self undoable-editor-mixin))
  (setf (last-action self) nil
        (last-item self) nil))

(defmethod cleanup-undoable-editor-stack-elements ((self undoable-editor-mixin) deleted-states)
  (cleanup-undoable-object-elements (undoable-object self)
                                    deleted-states 
                                    (append (undo-stack self) (redo-stack self))))

(defmethod cleanup-undoable-object-elements ((self t) deleted-states stacked-states) nil)
  

(defmethod update-after-state-change ((self t)) nil)
(defmethod update-after-state-change ((self OMEditor)) 
  (om-invalidate-view (main-view self))
  (report-modifications self))


(defmethod push-undo-state ((self undoable-editor-mixin))
  (let ((state (get-undoable-editor-state self)))
    (push state (undo-stack self)) 
    (when (> (length (undo-stack self)) (level self))
      (let ((deleted-states (last (undo-stack self))))
       (setf (undo-stack self) (butlast (undo-stack self)))
       (cleanup-undoable-editor-stack-elements self deleted-states))
      )
    )
  ;(om-print-dbg "UNDO STACK:")
  ;(write (undo-stack self) :stream om-lisp::*om-stream* :pretty t)
  ;(terpri om-lisp::*om-stream*)
  )

(defmethod push-redo-state ((self undoable-editor-mixin))
  (let ((state (get-undoable-editor-state self)))
    (push state (redo-stack self)))
    
    ; in principle this is not necessary: 
    ; the redo-stack size will never be longer than the undo stack
    ;(when (> (length (redo-stack self)) (level self))
    ;  (setf (redo-stack self) (butlast (redo-stack self))))
  
    ;(om-print-dbg "REDO STACK:")
    ;(write (redo-stack self) :stream om-lisp::*om-stream* :pretty t)
    ;(terpri om-lisp::*om-stream*)
  )


;;; => call this before any action which might require undo
;;; => action and item allow to prevent multiple-undo storage for sequences of similar actions (to do: add also a timer?)
(defmethod store-current-state-for-undo ((self undoable-editor-mixin) &key action item)
  (unless (and action
               (equal action (last-action self))
               (equal item (last-item self)))
    ;;; this is a new 'key state' we want to store
    (push-undo-state self)
    ;;; when we push a new undo state, the redo is reinitialized
    (let ((deleted-states (copy-list (redo-stack self))))
      (setf (redo-stack self) nil)
      (cleanup-undoable-editor-stack-elements self deleted-states)
      ))
  (setf (last-action self) action
        (last-item self) item))
  
;;; => call this to undo
(defmethod do-undo ((self undoable-editor-mixin))
  (setf (last-action self) nil
        (last-item self) nil)
  (if (undo-stack self)
    (progn 
      ;;; push state in redo-stack
      (push-redo-state self)
      ;;; restore state from undo-stack
      (let ((restored-state (pop (undo-stack self))))
        ;(print restored-state)
        (restore-undoable-editor-state self restored-state)
        )
      )
    (om-beep)))

;;; call this to redo
(defmethod do-redo ((self undoable-editor-mixin))
  (setf (last-action self) nil
        (last-item self) nil)
  (if (redo-stack self)
    (progn 
      ;;; push state in undo-stack
      (push-undo-state self)
      ;;; restore state from redo-stack
      (let ((restored-state (pop (redo-stack self))))
        (restore-undoable-editor-state self restored-state))
      )
    (om-beep)))


;;;=====================================
;;; COLLECT/RESTORE STATES FOR UNDO/REDO
;;;=====================================

;;; GENERAL CASE: two methods to get/restore the state of an object
(defmethod get-undoable-object-state ((self t)) (om-copy self))
(defmethod restore-undoable-object-state ((self t) state) (setf self state))

;;; POINTS
(defmethod get-undoable-object-state ((self ompoint)) (om-copy self))
(defmethod restore-undoable-object-state ((self ompoint) state) 
  (om-point-set-values-from-point self state)
  self)


;;; STANDARD-OBJECTS
;;; all objects can implement this method (not only undoable-editor-mixin)
;;; e.g. slots or deep-contained objects
(defmethod get-object-slots-for-undo ((self t)) nil)
(defmethod get-object-slots-for-undo ((self standard-object)) 
  (loop for slot in (class-instance-slots (class-of self))
        when (slot-definition-initargs slot)
        collect (slot-definition-name slot)))

; check classes here :)
; (get-object-slots-for-undo (make-instance 'omboxabstraction))

(defmethod get-undoable-object-state ((self standard-object)) 
  ;(om-print-dbg "collecting state of ~A" (list self) "UNDO")
  (loop for slot in (get-object-slots-for-undo self)
        collect (list slot (get-undoable-object-state (slot-value self slot)))))

(defmethod restore-undoable-object-state ((self standard-object) (state list)) 
  ;(om-print-dbg "restoring state of ~A" (list self) "UNDO")
  (loop for slot in (get-object-slots-for-undo self)
        do (setf (slot-value self slot) 
                 (restore-undoable-object-state (slot-value self slot)
                                                (cadr (find slot state :key 'car)))))
  self)

;;; LISTS
(defmethod get-undoable-object-state ((self list)) 
  (loop for item in self
        collect (list item (get-undoable-object-state item))))

;;; restore a new list, restore each object in it
(defmethod restore-undoable-object-state ((self list) (state list)) 
  (loop for item in state 
        do (restore-undoable-object-state (car item) (cadr item))
        collect (car item)))

;;; BOX
(defmethod get-undoable-object-state ((self OMBox)) 
  (list (call-next-method)
        (mapcar 'save-state (inputs self))
        (mapcar 'save-state (outputs self))
        ))

;;; restore a new list, restore each object in it
(defmethod restore-undoable-object-state ((self OMBox) (state list)) 
  
  (call-next-method self (car state))
  
  (setf (inputs self) (create-box-inputs self))
  (setf (outputs self) (create-box-outputs self))
  
  (restore-inputs self (nth 1 state))
  (restore-outputs self (nth 2 state))
  
  self)

(defmethod get-object-slots-for-undo ((self OMBox)) 
  (remove 'reference (call-next-method)))


;;; PATCHES 
;;; need special care because connectiosn are restored from the box list
(defmethod get-undoable-object-state ((self OMPatch)) 
  `((boxes ,(get-undoable-object-state (boxes self)))
    (connections ,(save-connections-from-boxes (boxes self)))))


(defmethod restore-undoable-object-state ((self OMPatch) (state list)) 
  
  ;;; need to save/restore the connections of referencing boxes...
  (let* ((reference-containers (remove-duplicates (mapcar 'container (references-to self))))
         (reference-containers-connections 
          (loop for p in reference-containers collect (save-connections-from-boxes (boxes p)))))
  
    (loop for element in (append (boxes self) (connections self))
          do (omng-remove-element self element))
    ;;; => must be properly removed !!!
    
    (let* ((boxes-in-state (cadr (find 'boxes state :key 'car)))
           (connections-in-state (cadr (find 'connections state :key 'car))))
      
      (loop for b-state in boxes-in-state do
            (let ((b (car b-state)))
              (when b 
                (restore-undoable-object-state b (cadr b-state))
                (loop for box-io in (append (inputs b) (outputs b)) do
                      (setf (connections box-io) nil))
                (omng-add-element self b)
                )))

      (loop for c in (restore-connections-to-boxes connections-in-state (boxes self))
            do (omng-add-element self c))
      )
    
    ;;; restore the connections of referencing boxes
    (loop for p in reference-containers
          for c-list in reference-containers-connections
          do 
          (let ()
            (loop for c in (restore-connections-to-boxes c-list (boxes p))
                  do (omng-add-element p c))
            (when (editor p) (update-after-state-change (editor p)))
            ))
    
    self))
    


(defmethod update-after-state-change ((self patch-editor))
  (let* ((patch (object self))
         (view (main-view self)))
    (om-remove-all-subviews view)
    (put-patch-boxes-in-editor-view patch view)
    (add-lock-item self view)
    (om-invalidate-view view)))

(defmethod cleanup-undoable-object-elements ((self OMPatch) deleted-states stacked-states) 
  
  (let ((removed-boxes (remove-duplicates 
                        (loop for state in deleted-states
                              append (mapcar 'car (cadr (find 'boxes state :key 'car))))))
        (stacked-boxes (remove-duplicates 
                        (loop for state in stacked-states
                              append (mapcar 'car (cadr (find 'boxes state :key 'car)))))))
    
    ;(om-print-dbg "Delete from undo stacks: ~A" (list removed-boxes) "UNDO")
    
    (loop for box in removed-boxes do
          (unless (find box stacked-boxes)
            ;(om-print-dbg "CLEANUP: ~A" (list box) "UNDO") 
            (omng-delete box)))
  ))


#|
;;; BPF
;;; just for debug
(defmethod get-undoable-object-state ((self bpf)) 
  (let ((rep (call-next-method)))
    (om-print-dbg "COLLECT: ~A" (list (point-pairs self)))
    rep))

;;; just for debug
(defmethod restore-undoable-object-state ((self bpf) (state list)) 
  (let ((rep (call-next-method)))
    (om-print-dbg "RESTORE: ~A" (list (point-pairs self)))
    rep))
|#
