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
; File authors: J. Bresson, D. Bouche, J. Garcia
;============================================================================


;This class facillitates time manipulations in Maquette and the use of time markers with time rulers.

(in-package :om)

(defclass timed-object () 
  ((onset :accessor onset :initform 0 :initarg :onset)))

;;; some objects just have NO time markers.. ?
(defmethod get-time-markers ((self t)) nil)

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-time-markers ((self timed-object))
  "returns a list of time markers"
  (list 0))


;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-elements-for-marker ((self timed-object) marker)
  "returns a list of elements matching the marker"
  (list nil))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod translate-elements-from-time-marker ((self timed-object) elems dt)
  "translates elements from a time marker with dt"
  (setf (onset self) (max 0 (+ (onset self) dt))))

(defmethod set-object-onset ((self timed-object) onset)
  (setf (onset self) onset))

(defmethod set-object-onset ((self t) onset) 
  ;(om-beep-msg "~A has no onset attribute !" (type-of self))
  onset)



