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

;===========================
;FRAMES
;===========================

(defclass OMFrame (OMObject) 
   ((object :initform nil :initarg :object :accessor object)
    (state :initform nil :accessor state)
    (areas :initform nil :initarg :areas :accessor areas)
    (active-area :initform nil :initarg :active-area :accessor active-area)
    )
   (:documentation "This is the class of frames which visualize the OMBasicObjects. 
Frames can be simple frames (icons, boxes, etc.) or container frames (patch editor, folder editor, etc.)"
    ))

(defmethod get-help ((self OMFrame)) nil)


;;;=================================
;;; OMFrame implements a system of virtual subviews (or 'picking views')
;;;=================================

(defclass frame-area ()
  ((active :initform nil :initarg :active :accessor active)
   (object :initform nil :initarg :object :accessor object)
   (frame :initform nil :initarg :frame :accessor frame)
   (pos :initform nil :initarg :pos :accessor pos)
   (pick :initform '(0 0 0 0) :initarg :pick :accessor pick)))

(defmethod get-position ((self frame-area))
  (if (functionp (pos self))
      (funcall (pos self) (frame self))
    (pos self)))

(defmethod get-pick ((self frame-area))
  (if (functionp (pick self))
      (funcall (pick self) (frame self))
    (pick self)))

(defmethod disabled-area ((area frame-area)) nil)

(defmethod apply-in-area ((self OMFrame) function position)
  (let ((aa (active-area-at-pos self position)))
    
    (when aa (unless (disabled-area aa) (funcall function aa self))
      ;;; the funcall must return T or the click will be reported to the frame
      )))

(defmethod active-area-at-pos ((self OMFrame) pos)
  (let ((aa nil))
    (loop for area in (areas self)
          while (not aa) do
          (setf aa (and (enabled-area area)
                        (area-at-pos? area (om-point-x pos) (om-point-y pos)))))
    aa))

(defmethod enabled-area ((self frame-area)) self)

(defmethod area-at-pos? ((self frame-area) x y)
  (let ((p (get-position self))
        (pick (get-pick self)))
    (and (>= x (+ (om-point-x p) (car pick)))
         (<= x (+ (om-point-x p) (caddr pick)))
         (>= y (+ (om-point-y p) (cadr pick)))
         (<= y (+ (om-point-y p) (cadddr pick)))
         self)))
         
;;; by default frame areas are invisible
(defmethod om-draw-area ((area frame-area)) nil)

(defmethod area-tt-text ((self frame-area)) nil)
(defmethod area-tt-pos ((self frame-area)) 
  (om-add-points
   (om-convert-coordinates (get-position self) (frame self) (om-view-container (frame self)))
   (om-make-point -20 (* -12 (1+ (length (list! (area-tt-text self))))))))


(defmethod om-enter-area ((area frame-area)) 
  (unless (disabled-area area)
    (setf (active area) t) 
    (when (area-tt-text area)
      (om-show-tooltip (om-view-container (frame area)) 
                       (area-tt-text area)
                       (area-tt-pos area) 0.04))
    (om-invalidate-view (frame area))))

(defmethod om-leave-area ((area frame-area)) 
  (setf (active area) nil)
  (when (area-tt-text area)
    (om-hide-tooltip (om-view-container (frame area))))
  (om-invalidate-view (frame area)))

(defmethod om-view-cursor ((area frame-area)) nil)


(defmethod om-view-mouse-motion-handler ((self OMFrame) position)
  (let ((aa (active-area-at-pos self position)))
    (when (and (active-area self) (not (equal aa (active-area self))))
      (om-leave-area (active-area self))
      (om-set-view-cursor self (om-view-cursor self)))
    (if (and aa (not (equal aa (active-area self))))
      (om-enter-area aa) 
      (om-set-view-cursor self (om-view-cursor self)))
    (setf (active-area self) aa)))

(defmethod om-view-mouse-enter-handler ((self OMFrame))
  ;(om-print-dbg "ENTER ~A" (list self))
  (let ((helptext (get-help self)))
    (when (and helptext (om-command-key-p))
      (om-show-tooltip (om-view-container self) helptext nil))))

(defmethod om-view-mouse-leave-handler ((self OMFrame))
  ;(om-print-dbg "LEAVE ~A" (list self))
  (when (active-area self)
    (om-leave-area (active-area self))
    (setf (active-area self) nil))
  (om-hide-tooltip (om-view-container self)))
                                                                  

;;;=================================
;;; OMFrame subclasses
;;;=================================

;;; OMSimpleFrame and OMCompoundFrame are identical in OM 
;;; but they are implemented differently in the underlying API
(defclass OMSimpleFrame (OMFrame om-item-view om-drag-view) ())
(defclass OMCompoundFrame (OMFrame om-internal-view) ())


(defclass OMContainerFrame (om-view OMFrame) ()
   (:documentation "Container frames are frames which can containt OMSimpleFrames within.
In general EditorFrames are instances of the class metaobj-panel, this class inherites from the 'View' class 
and the 'OMcontainerFrame' class.#enddoc#
#seealso# (metaobj-panel) #seealso#"))
