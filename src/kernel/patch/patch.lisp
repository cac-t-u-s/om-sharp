;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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
  
;;;==========================================
;;; PATCH OBJECT (VISUAL PROGRAM)
;;;==========================================

(defclass OMPatch (OMProgrammingObject)         
  (; main contents
   (boxes :initform nil :initarg :boxes)
   (connections :initform nil :accessor connections)
   ; patch editing properties
   (grid :accessor grid :initarg :grid :initform nil)
   (lock :accessor lock :initarg :lock :initform nil)
   ; internal lock for asynchronous accesses by the scheduler
   ;(let-list-stack :accessor let-list-stack :initform nil)
   (content-mp-lock :initform (mp:make-lock :name "patch content lock") :accessor content-mp-lock)))

(defmethod default-compiled-gensym  ((self OMPatch)) (gensym "patch-"))

(defmethod boxes ((self OMPatch))
  (mp:with-lock ((content-mp-lock self))
    (slot-value self 'boxes)))

(defmethod (setf boxes) (new-boxes (self OMPatch))
  (mp:with-lock ((content-mp-lock self))
    (setf (slot-value self 'boxes) new-boxes)))

(defmethod ompatch-p ((self OMPatch)) t)
(defmethod ompatch-p ((self t)) nil)

(defclass OMPatchInternal (OMPatch) ()
  (:default-initargs :icon :patch-0)
  (:metaclass omstandardclass))

(defclass OMPatchFile (OMPersistantObject OMPatch) ()
  (:default-initargs :icon :patch-file) 
  (:metaclass omstandardclass))

(defmethod object-doctype ((self OMPatch)) :patch)
(defmethod get-object-type-name ((self OMPatch)) "Patch")
  
(defmethod allowed-element ((self OMPatch) (elem OMBox)) t)
(defmethod allowed-element ((self OMPatch) (elem OMConnection)) t)

(defmethod initialize-instance :after ((self OMPatch) &rest args)
  (mapcar #'(lambda (box) (setf (container box) self)) (boxes self)))

;; e.g. dead box
(defmethod omNG-add-element ((self OMPatch) (elem null)) nil)

(defmethod omNG-add-element ((self OMPatch) (elem OMBox))
  (setf (container elem) self)
  (setf (boxes self) (append (boxes self) (list elem))))

(defmethod omng-remove-element ((self OMPatch) (elem OMBox))
  (when (equal (container elem) self)
    (setf (container elem) nil))
  (setf (boxes self) (remove elem (boxes self) :test 'equal))
  (close-inspector-for-box elem))

;;;========================================
;;; LOADING...
;;;========================================

(defmethod index ((self t)) -1)

;;; ompatchio has an index accessor
;;; otherwise, special types of boxes can organize themselves by redefining the method
;;; (eg. to set priority between loop iterator boxes, see om-loop.lisp)
(defun sort-boxes (boxes)
  (sort boxes '< :key #'(lambda (b) (index (reference b)))))

(defmethod copy-contents ((from OMPatch) (to OMPatch))  
  (let* ((sorted-boxes (sort-boxes (boxes from)))
         (connections (save-connections-from-boxes sorted-boxes))
         (boxes (mapcar 'om-copy sorted-boxes)))

    (mapc #'(lambda (b) 
              (omng-add-element to b)) boxes)
    
    (mapc #'(lambda (c) (omng-add-element to c))
          (restore-connections-to-boxes connections (boxes to)))
    
    to))

(defmethod om-copy ((self OMPatch))  
  (let* ((new-patch (make-instance (type-of self) :name (name self))))
    (copy-contents self new-patch)
    new-patch))

(defmethod get-boxes-of-type ((self OMPatch) type)
  (loop for b in (boxes self) when (subtypep (type-of b) type) collect b))

(defmethod load-contents ((self OMPatchFile)) 
  (if (mypathname self)
      (let ((tmppatch (load-doc-from-file (mypathname self) :patch)))
        (copy-contents tmppatch self))
    (om-beep-msg "CAN NOT LOAD PATCH '~A'" (name self))))


;;;========================================
;;; CLOSING / RELEASING REFERENCES...
;;;========================================

(defmethod get-internal-elements ((self OMPatch))
  (append (boxes self) (connections self)))

(defmethod delete-internal-elements ((self OMPatch))
  (let ((not-closed-elements 
         (loop for element in (get-internal-elements self)
               when (null (omng-delete element))
               collect element
               )))
    
    (when not-closed-elements
      (om-print (format nil "~%The following elements were not closed:~{~%~A~}~%------" not-closed-elements) "ERROR"))
    t))




;;;========================================
;;; CAN BE CALLED FROM OMPATCHES / SEQUENCER
;;;========================================

(defmethod* get-box-by-name ((self OMPatch) (name string))
   (find name (boxes self) :key 'name :test 'string-equal))


;;;=============================
;;; CONNECTIONS
;;;=============================

(defmethod omng-add-element ((self OMPatch) (elem OMConnection))
  (omng-connect elem)
  (setf (connections self) (append (connections self) (list elem))))

(defmethod omng-remove-element ((self OMPatch) (elem OMConnection))
  (omng-unconnect elem)
  (setf (connections self) (remove elem (connections self)))
  (close-inspector-for-box elem))


;;;==========================================
;;; PATCH DOCUMENT (ATTACHED TO A FILE)
;;;==========================================


(defmethod make-new-om-doc ((type (eql :patch)) name)
  (make-instance 'OMPatchFile :name name))


;; For conversions
(defmethod internalized-type ((self OMPatchFile)) 'OMPatchInternal)
(defmethod externalized-type ((self OMPatch)) 'OMPatchFile)
(defmethod externalized-icon ((self OMPatch)) :patch-file)

;;;==========================================
;;; DIFFERENCES BETWEEN INTERNAL AND NOT INTERNAL :
;;;==========================================

;(defmethod update-from-editor ((self OMPatchInternal))
;  (mapcar 'update-from-editor (references-to self)))

;(defmethod update-from-editor ((self OMPatchFile)) 
;  (mapcar 'update-from-editor (references-to self))
;  (touch self))

;;;==========================================
;;; META: TOOLS FOR SEQUENCER ETC.
;;;==========================================

(defmethod* get-boxes ((self OMPatch))
  (boxes self))

(defmethod* get-box-values ((self OMPatch))
  (loop for b in (boxes self) collect (get-box-value b)))





