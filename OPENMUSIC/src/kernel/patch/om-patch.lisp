;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;=========================================================================

(in-package :om)
  
;;;==========================================
;;; PATCH OBJECT (VISUAL PROGRAM)
;;;==========================================

(defclass OMPatch (OMProgrammingObject)         
   ((inputs :initform nil :accessor inputs)
    (outputs :initform nil :accessor outputs)
    (boxes :initform nil :initarg :boxes :accessor boxes)
    (lock :initform (mp:make-lock :name "content lock") :accessor lock)
    (connections :initform nil :accessor connections)))

(defmethod boxes ((self OMPatch))
  (mp:with-lock ((lock self))
    (slot-value self 'boxes)))

(defmethod (setf boxes) (new-boxes (self OMPatch))
  (mp:with-lock ((lock self))
    (setf (slot-value self 'boxes) new-boxes)))

(defmethod ompatch-p ((self OMPatch)) t)
(defmethod ompatch-p ((self t)) nil)

(defclass OMPatchInternal (OMPatch) ()
  (:default-initargs :icon 'patch-0)
  (:metaclass omstandardclass))

(defclass OMPatchFile (OMPersistantObject OMPatch) ()
  (:default-initargs :icon 'patch-file) 
  (:metaclass omstandardclass))


(defmethod object-doctype ((self OMPatch)) :patch)
(defmethod obj-file-extension ((self OMPatch)) "opat")
(defmethod get-object-type-name ((self OMPatch)) "Patch")
  
(defmethod allowed-element ((self OMPatch) (elem OMBox)) t)
(defmethod allowed-element ((self OMPatch) (elem OMConnection)) t)

(defmethod initialize-instance :after ((self OMPatch) &rest args)
  (mapcar #'(lambda (box) (setf (container box) self)) (boxes self)))

;; e.g. dead box
(defmethod omNG-add-element ((self OMPatch) (elem null)) nil)

(defmethod omNG-add-element ((self OMPatch) (elem OMBox))
  (setf (container elem) self)
  ;(print "================================================")
  ;(print (boxes self))
  ;(print elem)
  ;(print "================================================")
  (setf (boxes self) (append (boxes self) (list elem))))

(defmethod omng-remove-element ((self OMPatch) (elem OMBox))
  (when (equal (container elem) self)
    (setf (container elem) nil))
  (setf (boxes self) (remove elem (boxes self) :test 'equal)))

(defmethod omng-delete ((self OMPatch)) 
  (mapc #'omng-delete (boxes self))
  (call-next-method))

(defmethod index ((self t)) -1)
(defun sort-boxes (boxes)
  (sort boxes '< :key #'(lambda (b) (index (reference b)))))

(defmethod om-copy ((self OMPatch))  
  (let* ((sorted-boxes (sort-boxes (boxes self)))
         (connections (save-connections-from-boxes sorted-boxes))
         (boxes (mapcar 'om-copy sorted-boxes))
         (new-patch (make-instance (type-of self) :name (name self))))
    (mapc #'(lambda (b) (omng-add-element new-patch b)) boxes)
    (mapc #'(lambda (c) (omng-add-element new-patch c))
          (restore-connections-to-boxes connections (boxes new-patch)))
    ;(list new-patch (connections new-patch))
    new-patch))

(defmethod get-boxes-of-type ((self OMPatch) type)
  (loop for b in (boxes self) when (subtypep (type-of b) type) collect b))

;;;=============================
;;; CONNECTIONS

(defmethod omng-add-element ((self OMPatch) (elem OMConnection))
  (omng-connect elem)
  (setf (connections self) (append (connections self) (list elem))))

(defmethod omng-remove-element ((self OMPatch) (elem OMConnection))
  (omng-unconnect elem)
  (setf (connections self) (remove elem (connections self))))


;;;==========================================
;;; PATCH DOCUMENT (ATTACHED TO A FILE)
;;;==========================================


(defmethod make-new-om-doc ((type (eql :patch)) name)
  (make-instance 'OMPatchFile :name name))


;;; DOES NOT COPY !!
(defmethod om-copy ((self OMPatchFile)) self)  

;;;==========================================
;;; DIFFERENCES BETWEEN INTERNAL AND NOT INTERNAL :
;;;==========================================


(defmethod update-from-editor ((self OMPatchInternal))
  (mapcar 'update-from-editor (references-to self)))

(defmethod update-from-editor ((self OMPatchFile)) 
  (mapcar 'update-from-editor (references-to self))
  (touch self))





