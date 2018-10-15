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

;(defclass container ()
;  ((inside :accessor inside :initarg :inside :initform nil :documentation "the contents of the container")))

(defclass score-object (schedulable-object)
  (
   ;;; symbolic date and symbolic-dur make sense only if the object is in a context with tempo
   (symbolic-date :accessor symbolic-date :initarg :symbolic-date :initform nil 
                  :documentation "date in symbolic musical time (ratio of beat)")
   (symbolic-dur :accessor symbolic-dur :initarg :symbolic-dur :initform nil 
                 :documentation "duration in symbolic musical time (ratio of beat)")
   
   ;;; used for rhythmic computation
   (extent :accessor extent :initarg :extent :initform 0 :documentation "a duration in the units of its own container")
   
   ;;; bounding-box is a cached graphic information for score display 
   (b-box :accessor b-box :initform nil) 
   ))

;;; this method to be defined according to the different objects' slot names etc.
;;; also allows compat with OM6 naming
(defmethod inside ((self score-object)) nil)

(defstruct b-box (x1) (x2) (y1) (y2))
(defmethod b-box-w (b) (- (b-box-x2 b) (b-box-x1 b)))
(defmethod b-box-h (b) (- (b-box-y2 b) (b-box-y1 b)))


(defmethod display-modes-for-object ((self score-object))
  '(:mini-view :hidden :text))

(defmethod additional-box-attributes ((self score-object)) 
  `((:font-size "a font size for score display" nil)
    (:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    ))


(defmethod initialize-instance :after ((self score-object) &rest initargs)
  (setf (autostop self) t) ;;; ??? wtf 
  )

;;;============ 
;;; BOX
;;;============


(defmethod draw-mini-view ((self score-object) box x y w h &optional time)
  
  ;(om-draw-rect x y w h :fill t :color (om-def-color :white))

  (let ((fontsize 18)
        (staff (get-edit-param box :staff)))
         
    (let* ((staff-lines (apply 'append (mapcar 'staff-lines (staff-split staff))))
           (unit (font-size-to-unit fontsize))
           (n-lines (+ (- (car (last staff-lines)) (car staff-lines)) 8)) ;;; range of the staff lines + 10-margin
           (draw-box-h (* n-lines unit))
           (y-in-units (/ y unit)))
     
      (if (< draw-box-h h)
          ;;; there's space: draw more in the middle
          (setf y-in-units (+ y-in-units (/ (round (- h draw-box-h) 2) unit)))
        ;;; there's no space: reduce font ?
        (progn 
          (setf unit (- unit (/ (- draw-box-h h) n-lines)))
          (setf fontsize (unit-to-font-size unit)))
        )
      
      (let* ((x-in-units (/ x unit)))
        
        (om-with-fg-color (om-make-color 0.0 0.2 0.2)
          (draw-staff x-in-units y-in-units w h fontsize staff :margin-l 1 :margin-r 1 :keys t)
          (score-object-mini-view self x-in-units y-in-units w h staff fontsize)
          )
        ))))





