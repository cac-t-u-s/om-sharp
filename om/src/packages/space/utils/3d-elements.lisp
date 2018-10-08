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

;;;=============================================================
(defclass speaker (background-element) 
  ((pos :accessor pos :initarg :pos :initform nil)
   (size :accessor size :initarg :size :initform 1)))

(defun make-bg-speakers (speakers)
  (mapcar #'(lambda (spk)
              (make-instance 'speaker :pos (make-3dpoint :x (car spk) :y (cadr spk) :z (or (caddr spk) 0))
                             :size .08))
          speakers))

(defmethod draw-background-element ((self speaker) (view bpf-bpc-panel) editor &optional x1 y1 x2 y2)
  (om-draw-rect (x-to-pix view (- (editor-point-x editor (pos self)) (* (size self) .5)))
                (y-to-pix view (- (editor-point-y editor (pos self)) (* (size self) .5)))
                (max (dx-to-dpix view (size self)) 10) 
                (min (dy-to-dpix view (size self)) -10)
                :color (om-def-color :light-gray) :fill t)
  (om-draw-rect (x-to-pix view (- (editor-point-x editor (pos self)) (* (size self) .5)))
                (y-to-pix view (- (editor-point-y editor (pos self)) (* (size self) .5)))
                (max (dx-to-dpix view (size self)) 10) 
                (min (dy-to-dpix view (size self)) -10)
                :line 2 :style :dash :color (om-def-color :gray) :fill nil))

(defmethod make-3D-background-element ((self speaker)) 
  (make-instance 
   '3d-cube :size (size self) 
   :center (list (om-point-x (pos self)) (om-point-y (pos self)) (om-point-z (pos self)))
   :color (om-def-color :gray)))
  

;;;=============================================================
(defclass project-room (background-element) 
  ((center :accessor center :initarg :center :initform (make-3dpoint :x 0 :y 0 :z 0))
   (width :accessor width :initarg :width :initform 1)
   (depth :accessor depth :initarg :depth :initform 1)
   (height :accessor height :initarg :height :initform 1)
   (show-floor :accessor show-floor :initarg :show-floor :initform nil)))

(defun make-bg-room (width depth height)
  (let ((room (make-instance 'project-room :width width :depth depth :height height
                             :show-floor t)))
    (setf (center room) 
          (make-3dpoint :x 0 :y 0 :z (* height 0.5)))
    room))

(defmethod draw-background-element ((self project-room) (view bpf-bpc-panel) editor &optional x1 y1 x2 y2)  
  (let ((size-point (make-3dpoint :x (width self) :y (depth self) :z (height self))))
    (om-draw-rect (x-to-pix view (- (editor-point-x editor (center self)) (* (editor-point-x editor size-point) .5)))
                  (y-to-pix view (- (editor-point-y editor (center self)) (* (editor-point-y editor size-point) .5)))
                  (max (dx-to-dpix view (editor-point-x editor size-point)) 10)
                  (min (dy-to-dpix view (editor-point-y editor size-point)) -10)
                  :line 2 :style :dash :color (om-def-color :dark-red) :fill nil)
    ))

(defmethod make-3D-background-element ((self project-room))
  (make-instance 
   '3d-cube :size (list (width self) (depth self) (height self)) 
   :center (list (om-point-x (center self)) (om-point-y (center self)) (om-point-z (center self)))
   :color (om-def-color :gray)
   :filled nil))
