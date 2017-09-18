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
         (v-margin 8)
         (line-h (if (data self) 
                     (/ (- h v-margin (* inter-line (1- n-lines))) n-lines)
                   (- h v-margin)))
         (h-margin 2)
         (line-w (- w 4)))
    ;(om-draw-rect (+ x 2) (+ y 2) (- w 4) (- h 4) :color (om-def-color :red))
    (when (data self) 
      (loop for n from 0 to (1- n-lines)
            for yy = v-margin then (+ yy line-h inter-line) do
            (om-draw-rect h-margin yy line-w line-h :color (om-def-color :white) :fill t)
            (om-with-fg-color (om-def-color :gray)
              (om-draw-string (- line-w (om-string-size (get-column-name self n))) (+ yy 8) (get-column-name self n)))
            (draw-field-on-box self (nth n (data self)) h-margin yy line-w line-h)
      ))))


(defmethod draw-field-on-box ((self 2D-array) (field list) x y w h)
  (let ((x-space (/ w (num-elts self)))
        (mid-y (+ y (/ h 2)))
        (margin-y (min 8 (/ h 2))))
  
    (flet ((nth-x-pos (n) (* x-space (+ n 0.5)))
           (draw-cross(x y) 
             (om-draw-line (- x 1) y (+ x 1) y :color (om-def-color :light-gray))
             (om-draw-line x (- y 1) x (+ y 1) :color (om-def-color :light-gray))))
    
      (om-with-font 
       (om-def-font :font1 :size 8)
     
       (if (numberp (car field))
        
           ;; Numbers: draw bpf-kind
           (let* ((min-y-val (list-min field))
                  (max-y-val (list-max field))
                  (y-values-range (- max-y-val min-y-val))
                  (y-factor (if (zerop y-values-range) 1 (float (/ (- h (* 2 margin-y)) y-values-range)))))
          
             (let* ((x0 (nth-x-pos 0))
                    (val (car field))
                    (y0 (+ y (- h margin-y (* (- val min-y-val) y-factor))))
                    (step (ceiling (/ (length field) w)))) 
                        
               ;;; draw the first element 
               (draw-cross x0 mid-y)
               (om-draw-circle x0 y0 2 :fill t)
               (om-draw-string (- x0 6) (- y0 6) (format nil "~A" val)) 
               (if (= y-values-range 0) (setf max-y-val nil)) ;; just to prevent drawing teh value again

               (loop for n from 1 to (1- (length field)) 
                     by step do
                     ;(if (> step 1) (setf n (min (1- (length field)) (+ n (om-random 0 step)))))
                     (let* ((elt (nth n field))
                            (xx (nth-x-pos n))
                            (val (nth n field))
                            (yy (+ y (- h margin-y (* (- val min-y-val) y-factor)))))
                       (if (< (length field) 200) (draw-cross xx mid-y))
                       (when (< (length field) 100) (om-draw-circle xx yy 2 :fill t))
                       (when (and max-y-val (= val max-y-val)) 
                         (om-draw-string xx yy (format nil "~A" val))
                         (setf max-y-val nil))
                       (om-draw-line x0 y0 xx yy) 
                       (setf x0 xx y0 yy))
                     )
               (when (> step 1) 
                 (om-draw-rect (- (* w 0.5) 60) (- mid-y 10) 125 15 :color (om-make-color-alpha (om-def-color :white) 0.8)  :fill t)
                 (om-draw-string (- (* w 0.5) 58) mid-y (format nil "[display down-sampled x 1/~D]" step)))
               ))

         ;;; NaN
          (if (< (length field) 200)
            (loop for elt in field
               for n = 0 then (+ n 1) do
               (let* ((xx (nth-x-pos n))
                      (val (nth n field))
                      (yy (+ y (* h .5))))
                 (draw-cross xx mid-y)
                 (om-draw-string xx yy (format nil "~A" val))
                 ))
            (om-draw-string (- (* w 0.5) 40) mid-y "[...(list too long)...]"))
         )))))


;;;============================================================
;;; CLASS-ARRAY / OM6-LIKE
;;; A 2D-ARRAY with more explicit semantics assigned to each field.
;;; data are matrix-fields (not just data)
;;; fields declares a number of main matrix-fields + defaults 
;;;============================================================

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

(defmethod get-slot-val ((self class-array) slot-name)
  (let ((pos (position slot-name (fields self) :test 'string-equal)))
    (if pos (nth pos (data self))
      (call-next-method))))

(defmethod get-cache-display-for-text ((self class-array))
  (append (call-next-method)
          (loop for field in (fields self) collect 
                (list (intern-k field)
                      (get-slot-val self field)))
          ))

;;; methods for init

(defmethod init-array-data ((init string) n)
  (make-list n :initial-element nil))

;;; methods for filling data

(defmethod fill-array-data-from-input ((input t) n)
  (make-list n :initial-element input))

(defmethod fill-array-data-from-input ((input cons) n)
  (cond ((= (length input) n) 
         (copy-list input))
        ((< (length input) n) 
         (fill-array-data-from-input (append input input) n))
        ((> (length input) n)
         (first-n input n))))


;;;=============
;;; Special box
;;;=============  

(defclass class-array-box (OMBoxEditCall) 
  ((keywords :initform nil :accessor keywords :initarg :keywords)))

(defmethod special-box-type ((self (eql 'class-array))) 'class-array-box)

;;; list of proposed keywords are the declared names
(defmethod get-all-keywords ((self class-array-box)) (list (keywords self)))

;;; in class array the meta-data determine the contents of the actual data
(defmethod update-contents ((self class-array))
  (let ((matrix-data 
         (loop for elem in (fields self) collect
               (init-array-data elem (num-elts self)))))
    (setf (data self) matrix-data)))

(defmethod update-key-inputs ((self class-array-box))
  (when(get-box-value self)
    (setf (keywords self) (mapcar 'intern-k (fields (get-box-value self)))))
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod initialize-instance :after ((self class-array-box) &rest args)  
  (declare (ignore args))
  (update-key-inputs self))

(defmethod (setf value) :after (val (self class-array-box))  
  (update-key-inputs self))

(defmethod set-column ((self class-array) n input-val)
  (setf (nth n (data self))
        (fill-array-data-from-input input-val (num-elts self))))

(defmethod omng-box-value :after ((self class-array-box) &optional numout)  
  (update-key-inputs self)
  (loop for inp in (cdr (inputs self)) do
        (when (keyword-input-p inp) 
          (let ((pos (position (name inp) (keywords self) :test 'string-equal))
                (val (omng-box-value inp)))
            (set-column (get-box-value self) pos val)
            ))))





