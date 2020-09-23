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
; File authors: J. Bresson, D. Bouche, J. Garcia
;============================================================================


;This class facillitates time manipulations in Maquette and the use of time markers with time rulers.

(in-package :om)

(defclass timed-object () 
  ((onset :accessor onset :initform 0 
          :initarg :onset :initarg :date  ;;; two possible initargs (for compatibility)
          :documentation "date/time of the object")))

;;; redefine for subclasses
(defmethod get-obj-dur ((self timed-object)) 1)

(defmethod set-object-onset ((self timed-object) onset)
  (setf (onset self) onset))

(defmethod set-object-onset ((self t) onset) 
  ;(om-beep-msg "~A has no onset attribute !" (type-of self))
  onset)



