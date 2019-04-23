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
; File author: D. Bouche
;============================================================================

;;===========================================================================
;  Clock
;;===========================================================================

(in-package :om)

(defclass clock (schedulable-object timed-object)
 ((action :initform nil :initarg :action :accessor action :documentation "A function to call periodically")
  (period :initform 1000 :initarg :period :accessor period :documentation "A time period in ms")
  (duration :initform nil :initarg :duration :accessor duration :documentation "A duration in ms or nil if infinite")
  (outval :initform nil :accessor outval :documentation "Slot to store the action's output")
  (parentbox :initform nil :accessor parentbox :documentation "Slot to store the box to propagate the result")
  (tick-n :initform 0 :accessor tick-n :documentation "Internal tick-n if no action is provided")))

(defmethod display-modes-for-object ((self clock))
  '(:hidden :text :mini-view))

;;;Accessor redefinition so the action output is accessible
(defmethod action ((self clock))
  (outval self))

(defmethod get-action ((self clock))
  (or (slot-value self 'action) 'identity))

(defmethod set-period ((self clock) new-period)
  (setf (period self) new-period))

;(defmethod clone-object ((object clock) &optional clone)
;  (let ((new-obj (or clone (make-instance (type-of object)))))
;    (setf (action new-obj) (slot-value object 'action)
;          (period new-obj) (slot-value object 'period)
;          (duration new-obj) (slot-value object 'duration))
;    new-obj))


(defmethod initialize-instance :after ((self clock) &rest initargs)
  (set-object-time-window self (period self))
  (loop-object self)
  self)

(defmethod get-action-list-for-play ((object clock) time-interval &optional parent)
  (list
   (list 0
         #'(lambda () 
             (setf (outval object) (funcall (get-action object) 
                                            (progn 
                                              (incf (tick-n object))
                                              (1- (tick-n object)))))
             (if (parentbox object)
                 (self-notify (parentbox object)))))))

(defmethod player-play-object ((self scheduler) (object clock) caller &key parent interval)
  (setf (parentbox object) caller)
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object clock))
  (setf (tick-n object) 0)
  (call-next-method))

(defmethod get-obj-dur ((self clock)) (or (duration self) (period self)))

(defmethod get-obj-time ((self clock) &key internal-time)
  (if (duration self)
      (call-next-method)
    (mod (call-next-method) (period self))))

(defmethod draw-mini-view ((self clock) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset 0 (get-obj-dur self) w x)
      (multiple-value-bind (fy oy) 
          (conversion-factor-and-offset h 0 (- h 20) (+ y 10))
        (om-with-font (om-def-font :font2b) 
                      (om-draw-string (+ ox (* fx 10)) 
                                      (+ oy (* fy (+ (/ h 2) 10))) (format nil "~A ms Loop" (period self))))
        (om-draw-dashed-line (+ ox (* fx x)) (+ oy (* fy (/ h 2)))
                      (+ ox (* fx (+ x 10000))) (+ oy (* fy (/ h 2))))))))



;;;
(defun gen-notes (p)
  (if p
      (loop for i from 0 to 19
            collect
            (make-midinote :onset (* i 50)
                           :pitch (+ (- (mod p 40) 20) 60)
                           :vel 100
                           :dur 45
                           :channel 1))))
