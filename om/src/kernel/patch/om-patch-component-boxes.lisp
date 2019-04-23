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
                             :color (make-color-or-nil :color (get-patch-component-box-def-color reference) 
                                                       :t-or-nil t)
                             :icon-pos (or (getf init-args :icon-pos) :top)
                             :text-align :center))
         (size (minimum-size box)))
    
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    box))


(defmethod get-patch-component-box-def-color ((self OMPatchComponent)) (om-make-color 0.82 0.85 0.7))

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
;;; PATCH-COMPONENT EVALUATION
;;; most patch components are just used as control and do not 'evaluate' themselves.
;;;==================================

(defmethod boxcall-value ((self OMPatchComponentBox))
  (values-list (mapcar #'omNG-box-value (inputs self))))


(defmethod gen-code-for-call ((self OMPatchComponentBox) &optional args)
  (declare (ignore args)) ;; only for lambda generation
  (mapcar 'gen-code (inputs self)))



