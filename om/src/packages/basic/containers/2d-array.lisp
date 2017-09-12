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
   ((num-elts :initform 0 :accessor num-elts :documentation "number of elements (a.k.a lines)")
    (num-fields :initform 0  :accessor num-fields :documentation "number of fields (a.k.a columns)")
    (data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

(defmethod object-box-label ((self 2D-array))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (num-fields self)) "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod update-contents ((self 2D-array))
  "Updates array dimensions according to <data>"
  (if (data self)
      (setf (num-fields self) (length (data self))
            (num-elts self) (apply 'max (mapcar 'length (data self))))
      (setf (num-fields self) 0)
      ))

(defmethod initialize-instance :after ((self 2D-array) &rest args)
  (update-contents self))

(defmethod (setf data) :after (data (self 2D-array))
  (update-contents self))

(defmethod get-col ((self 2D-array) (col integer))
  (nth col (data self)))

(defmethod display-modes-for-object ((self 2D-array)) '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self 2D-array) (box t) x y w h &optional time)
  (let* ((display-cache (get-display-draw box))
         (font (om-def-font :font1 :size 10))
         (n-lines (length (data self)))
         (inter-line 3)
         (margin 8)
         (line-h (/ (- h margin (* inter-line (1- n-lines))) n-lines)))
    (om-draw-rect (+ x 2) (+ y 2) w h :color (om-def-color :red))
    (loop for n from 0 to (1- n-lines)
          for yy = margin then (+ yy line-h inter-line) do
          (om-draw-rect 2 yy (- w 4) line-h :color (om-def-color :white) :fill t))
    ))

    (om-with-font 
     font 
     (loop for line in (list! (contents self)) 
           for y = 18 then (+ y 12) 
           do (if (< y (- h 8)) 
                  (let ((line (format nil "~A" line)))
                    (if (> (om-string-size line font) (- w 10))
                        (om-draw-string 
                         5 y 
                         (concatenate 
                          'string 
                          (subseq line 0 (min (length line) (- (round w (om-string-size "a" font)) 3)))
                          " ..."))
                      (om-draw-string 5 y line)))
                (progn 
                  (om-draw-string (- (round w 2) 10) (- h 10) "...") 
                  (return)))))))



;;;============================================================
;;; CLASS-ARRAY / OM6-LIKE
;;; A 2D-ARRAY with more explicit semantics assigned to each field.
;;; data are matrix-fields (not just data)
;;; inits declares a number of main matrix-fields + defaults 
;;;============================================================


(defstruct matrix-field (name) (doc) (type) (default) (data))

(defclass* class-array (2D-array) 
  ((inits :initform nil :initarg :inits :accessor inits :documentation "field names and defaults")
   (num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements (a.k.a lines)")
   (data :initform nil :accessor data :documentation "main data (as defined by inits)")
   (additional-data :initform nil :accessor additional-data :documentation "more dynamically, user defined data")
   ))

(defmethod object-box-label ((self class-array))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (+ (length (data self)) (length (additional-data self))))
           "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod get-col ((self class-array) (col string))
  (let ((pos (position col (fields self) 
                       :test 'string-equal 
                       :key #'(lambda (elt) (if (stringp elt) elt (car elt))))))
    (if pos (get-col self pos)
      (om-beep-msg "Field '~A' not found in '~A'" col self))))


(defclass class-array-box (OMBoxEditCall) ())
(defmethod special-box-type ((self (eql 'class-array))) 'class-array-box)

;;; in class array the meta-data determine the contenst of the actual data
(defmethod update-contents ((self class-array))
  (let ((matrix-data 
         (loop for elem in (inits self) collect
               (let ((field (make-matrix-field :name elem)))
                 (if find elem)
              
                 ))))))

;;; class-array-box can as many keyword as we want
(defmethod allow-add-inputs ((self class-array-box)) t)

;;; the default value will be the first unused declared field-name of the object, 
;;; or a default name ('p')
(defmethod next-keyword-input ((self class-array-box)) 
  (let ((usedkeywords (mapcar #'(lambda (in) (name in)) (get-keyword-inputs self)))) 
    (or (and (field-names (get-box-value self)) 
             (find-if-not #'(lambda (elt) (member elt usedkeywords :test 'string-equal)) 
                          (field-names (get-box-value self))))
        "p")))

;;; list of proposed keywords are the declared names
;;; or a free name to choose by the user
(defmethod get-all-keywords ((self class-array-box))
  (list (mapcar 'intern-k (field-names (get-box-value self)))
        '(:-]) ;;; a special symbol to allow user to add a custom name -- see 'change-keyword'
        ))

(defmethod add-keyword-input ((self class-array-box) &key key (value nil val-supplied-p) doc reactive)
  (call-next-method)
  (setf (num-fields (get-box-value self))
        (length (get-keyword-inputs self))))

(defmethod remove-one-keyword-input ((self OMBoxRelatedWClass))
  (call-next-method)
  (setf (data self) nil))


