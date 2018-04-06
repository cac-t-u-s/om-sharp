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

;===============================================================================
; PATCH CONTROL COMPONENTS
; TOP-LEVEL PATCH-COMPOENNT CLASS
; Superclass for OMPatchIO, OMPatchInit, OMPatchIterator...
; Special boxes that do not represent a function or class, but a special patch behaviour
;================================

(in-package :om)


(defclass OMPatchComponent (OMBasicObject) ())
(defclass OMPatchComponentBox (OMBoxCall) ())

(defmethod omNG-make-new-boxcall ((reference OMPatchComponent) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                             :name (name reference)
                             :reference reference
                             :color (make-color-or-nil :color (om-make-color 0.82 0.85 0.7)
                                                       :t-or-nil t)
                             :icon-pos :top
                             :text-align :center))
         (size (minimum-size box)))
    
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    box))


(defmethod h-resizable ((self OMPatchComponentBox)) t)
(defmethod v-resizable ((self OMPatchComponentBox)) nil)

(defmethod get-properties-list ((self OMPatchComponentBox))
  (add-properties (hide-properties 
                   (call-next-method) 
                   '(:icon :lock :lambda :group-id))
                  "Appearance" 
                  '((:icon "Icon position" (:left :top) icon-pos))))


(defmethod object-name-in-inspector ((self OMPatchComponentBox)) (format nil "~A box" (type-of (reference self))))

(defmethod valid-property-p ((self OMPatchComponentBox) (prop-id (eql :lock))) nil)
(defmethod valid-property-p ((self OMPatchComponentBox) (prop-id (eql :lambda))) nil)

(defmethod minimum-size ((self OMPatchComponentBox))
  (om-make-point (max 40
                      (+ 22 (om-string-size (name self) (font-font (text-font self)))
                         (if (equal (icon-pos self) :left) 22 0))
                      (+ 20 (* (length (inputs self)) 10)))
                 (+ (if (equal (icon-pos self) :top) 14 0) 28)))


(defmethod maximum-size ((self OMPatchComponentBox))
  (om-make-point 500 (+ (if (equal (icon-pos self) :top) 14 0) 28)))


;;;==================================
;;; (INTERNAL) PATCH-COMPONENT EVALUATION
;;; most patch components are just used as control and do not 'evaluate' themselves.
;;;==================================

(defmethod omNG-box-value ((self OMPatchComponentBox) &optional (numout 0))
  (setf (value self) (mapcar #'omNG-box-value (inputs self)))
  (return-value self numout))


;;;==================================
;;; BOXES WITH MEMORY
;;;==================================

(defclass OMPatchComponentWithMemory (OMPatchComponent) 
  ((mem-var :initform (gensym "MEM") :accessor mem-var)))

(defmethod get-icon-id ((self OMPatchComponentWithMemory)) 'm-mem)

;;;------------------
;;; DELAY: 'mem'
;;; returns previous eveluation(s) on the right output
;;;------------------

(defmethod special-box-p ((name (eql 'mem))) t)

(defclass OMMemory (OMPatchComponentWithMemory) ())
(defclass OMMemoryBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMMemory)) 'OMMemoryBox)

(defmethod get-icon-id ((self OMMemoryBox)) 'm-mem)
(defmethod object-name-in-inspector ((self OMMemoryBox)) "memory/delay box")

(defmethod omNG-make-special-box ((reference (eql 'mem)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMMemory :name (if name (string name) "mem"))
     pos)))

(defmethod create-box-inputs ((self OMMemoryBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "data to record in memory")
   (make-instance 
    'box-input :box self :value NIL
    :name "size of memory")))

(defmethod create-box-outputs ((self OMMemoryBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "current value")
   (make-instance 
    'box-output :box self :value NIL
    :name "past value(s)")))


(defmethod omNG-box-value ((self OMMemoryBox) &optional (numout 0))
    
    (unless (ev-once-flag self)
      
       (let ((inval (omng-box-value (car (inputs self))))
             (size (omng-box-value (cadr (inputs self)))))
 
         (setf (ev-once-flag self) t)
         
         (setf (value self)
               (if size
                   (list inval 
                         (first-n (cons (car (value self)) 
                                        (list! (cadr (value self))))
                                  size))
                 (list inval (car (value self)))))
         ))

    (return-value self numout))




;;;------------------
;;; COLLECT
;;;------------------

;;;------------------
;;; DELAY: 'mem'
;;; returns previous eveluation(s) on the right output
;;;------------------

(defmethod special-box-p ((name (eql 'collect))) t)

(defclass OMCollect (OMPatchComponentWithMemory) ())
(defclass OMCollectBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMCollect)) 'OMCollectBox)

(defmethod get-icon-id ((self OMCollectBox)) 'm-mem)
(defmethod object-name-in-inspector ((self OMCollectBox)) "collector box")

(defmethod omNG-make-special-box ((reference (eql 'collect)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMCollect :name (if name (string name) "collect"))
     pos)))

(defmethod create-box-inputs ((self OMCollectBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "data in (collected in memory)")
   (make-instance 
    'box-input :box self :value NIL
    :name "push: propagates reactive notification to the data-out outlet")
   (make-instance 
    'box-input :box self :value NIL
    :name "init: reinitializes memory")))

(defmethod create-box-outputs ((self OMCollectBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "collect and output data-in")
   (make-instance 
    'box-output :box self :value NIL
    :name "data-out (collected)")
   (make-instance 
    'box-output :box self :value NIL
    :name "init: reinitializes memory")))


(defmethod omNG-box-value ((self OMCollectBox) &optional (numout 0))
    
    (unless (ev-once-flag self)
      
       (let ((inval (omng-box-value (car (inputs self))))
             (size (omng-box-value (cadr (inputs self)))))
 
         (setf (ev-once-flag self) t)
         
         (setf (value self)
               (if size
                   (list inval 
                         (first-n (cons (car (value self)) 
                                        (list! (cadr (value self))))
                                  size))
                 (list inval (car (value self)))))
         ))

    (return-value self numout))

