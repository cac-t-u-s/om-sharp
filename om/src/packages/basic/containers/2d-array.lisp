;============================================================
; o7 -- an implementation of the OpenMusic visual programming
; language and computer-aided composition environment.
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; 
; For information on usage and redistribution, 
; and for a DISCLAIMER OF ALL WARRANTIES, 
; see the file "LICENSE.txt" in this distribution.
;============================================================

;============================================================
; ARRAY OBJECTS
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

(defmethod get-col ((self 2D-array) (col integer))
  (nth col (data self)))

(defmethod get-column-name ((self 2D-array) (col integer)) (format nil "c_~D" col))

(defmethod display-modes-for-object ((self 2D-array)) '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self 2D-array) (box t) x y w h &optional time)
  (let* ((display-cache (get-display-draw box))
         (font (om-def-font :font1 :size 10))
         (n-lines (length (data self)))
         (inter-line 3)
         (margin 8)
         (line-h (if (data self) 
                     (/ (- h margin (* inter-line (1- n-lines))) n-lines)
                   (- h margin))))
    ;(om-draw-rect (+ x 2) (+ y 2) (- w 4) (- h 4) :color (om-def-color :red))
    (when (data self) 
      (loop for n from 0 to (1- n-lines)
            for yy = margin then (+ yy line-h inter-line) do
            (om-draw-rect 2 yy (- w 4) line-h :color (om-def-color :white) :fill t)
            (om-with-fg-color (om-def-color :black)
              (om-draw-string 4 (+ yy 8) (get-column-name self n)))
      ))))


;;;============================================================
;;; CLASS-ARRAY / OM6-LIKE
;;; A 2D-ARRAY with more explicit semantics assigned to each field.
;;; data are matrix-fields (not just data)
;;; fields declares a number of main matrix-fields + defaults 
;;;============================================================


;(defstruct matrix-field (name) (doc) (type) (default) (data))

(defclass* class-array (2D-array) 
  ((fields :initform nil :initarg :fields :accessor fields :documentation "field names and defaults")
   (num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements (a.k.a lines)")
   (data :initform nil :accessor data :documentation "main data (as defined by fields)")
   ))

(defmethod object-box-label ((self class-array))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (length (data self)))
           "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod get-col ((self class-array) (col string))
  (let ((pos (position col (fields self) 
                       :test 'string-equal 
                       :key #'(lambda (elt) (if (stringp elt) elt (car elt))))))
    (if pos (get-col self pos)
      (om-beep-msg "Field '~A' not found in '~A'" col self))))

(defmethod get-column-name ((self class-array) (col integer)) 
  (or (nth col (fields self))
      (format nil "c_~D" col)))

;;; methods for init

(defmethod init-array-data ((init string) n)
  (make-list n :initial-element nil))

;;; methods for filling data

(defmethod fill-array-data-from-input ((input number) n)
  (make-list n :initial-element input))

(defmethod fill-array-data-from-input ((input cons) n)
  (cond ((= (length input) n) 
         (copy-list input))
        ((< (length input) n) 
         (make-array-data-from-input (append input input) n))
        ((> (length input) n)
         (first-n input n))))
        

(defclass class-array-box (OMBoxEditCall) 
  ((keywords :initform nil :accessor keywords :initarg :keywords)))

(defmethod special-box-type ((self (eql 'class-array))) 'class-array-box)

;;; in class array the meta-data determine the contenst of the actual data
(defmethod update-contents ((self class-array))
  (let ((matrix-data 
         (loop for elem in (fields self) collect
               (init-array-data elem (num-elts self)))))
    (setf (data self) matrix-data)))

(defmethod omng-box-value :after ((self class-array-box) &optional numout)  
  (setf (keywords self) (mapcar 'intern-k (fields (get-box-value self))))
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

;;; list of proposed keywords are the declared names
(defmethod get-all-keywords ((self class-array-box)) (list (keywords self)))



;;; class-array-box can as many keyword as we want
;;; (defmethod allow-add-inputs ((self class-array-box)) t)

;(defmethod add-keyword-input ((self class-array-box) &key key (value nil val-supplied-p) doc reactive)
;  ;;; DO SOMETHING HERE !!
;  (call-next-method)
;  (setf (num-fields (get-box-value self))
;        (+ (length (data (get-box-value self))) (length (additional-data (get-box-value self))))))

;(defmethod remove-one-keyword-input ((self class-array-box))
;  ;;; DO SOMETHING HERE !!
;  (call-next-method))

