;============================================================
; o7 -- an implementation of the OpenMusic visual programming
; language and computer-aided composition environment.
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; 
; For information on usage and redistribution, 
; and for a DISCLAIMER OF ALL WARRANTIES, 
; see the file "LICENSE.txt" in this distribution.
;============================================================

(in-package :om)

;;; SIMPLE 2D-ARRAY

(defclass* 2D-array () 
   ((num-elts :initform 1  :accessor num-elts :documentation "number of elements (a.k.a lines)")
    (num-fields :initform 1  :accessor num-fields :documentation "number of fields (a.k.a columns)")
    (data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

(defmethod object-box-label ((self 2D-array))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (num-fields self)) "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod update-info ((self 2D-array))
  "Updates array dimensions according to <data>"
  (if (data self)
      (setf (num-fields self) (length (data self))
            (num-elts self) (funcall 'max (mapcar 'length (data self))))
      (setf (num-fields self) 0)
      ))

(defmethod initialize-instance :after ((self 2D-array) &rest args)
  (update-info self))

(defmethod (setf data) :after (data (self 2D-array))
  (update-info self))

(defmethod get-col ((self 2D-array) (col integer))
  (nth col (data self)))

;;; CLASS-ARRAY / OM6-LIKE

(defclass* class-array (2D-array) 
  ((num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements (a.k.a lines)")
   (field-names :initform nil :accessor field-names :documentation "name of fields")))



(defmethod get-col ((self class-array) (col string))
  (let ((pos (position col (field-names self) :test 'string-equal)))
    (if pos (get-col self pos)
      (om-beep-msg "Field '~A' not found in '~A'" col self))))


