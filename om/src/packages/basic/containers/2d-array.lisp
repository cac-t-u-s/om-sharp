;============================================================================
; o7: visual programming language for computer-aided music composition
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

;============================================================
; ARRAY OBJECTS
;============================================================

(in-package :om)

;============================================================
;;; SUPERCLASS DEFINING COMMON ARRAY FEATURES
;============================================================

(defclass OMArray ()
  ((num-elts :initform 0 :accessor num-elts :documentation "number of elements (a.k.a lines)")
   (num-fields :initform 0 :accessor num-fields :documentation "number of fields (a.k.a columns)")
   (data :initform nil :accessor data :documentation "data matrix")))

;(defmethod initialize-instance :after ((self OMArray) &rest args)
;  (update-contents self))

(defmethod object-box-label ((self OMArray))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (num-fields self)) "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod get-field-name ((self OMArray) (col integer)) (format nil "c_~D" col))

;;; deprecated - use get-field
(defmethod get-col ((self OMArray) col &key (warn-if-not-found t))
  (get-field self col :warn-if-not-found warn-if-not-found))

(defmethod display-modes-for-object ((self OMArray)) '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self OMArray) (box t) x y w h &optional time)
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
    (when (data self) 
      (loop for n from 0 to (1- n-lines)
            for yy = v-margin then (+ yy line-h inter-line) do
            (om-draw-rect h-margin yy line-w line-h :color (om-def-color :white) :fill t)
            (om-with-fg-color (om-def-color :gray)
              (om-draw-string (- line-w (om-string-size (get-field-name self n))) (+ yy 8) (get-field-name self n)))
            (draw-field-on-box self (nth n (data self)) h-margin yy line-w line-h)
      ))))

;;; general case
;;; redefined for class-array
(defmethod get-array-field-data ((field list)) field)

(defmethod draw-field-on-box ((self OMArray) field x y w h)
  (when (> (num-elts self) 0)
    (let ((x-space (/ w (num-elts self)))
          (mid-y (+ y (/ h 2)))
          (margin-y (min 8 (/ h 2)))
          (field-data (get-array-field-data field)))
  
      (flet ((nth-x-pos (n) (* x-space (+ n 0.5)))
             (draw-cross(x y) 
               (om-draw-line (- x 1) y (+ x 1) y :color (om-def-color :light-gray))
               (om-draw-line x (- y 1) x (+ y 1) :color (om-def-color :light-gray))))
    
        (om-with-font 
         (om-def-font :font1 :size 8)
     
         (if (numberp (car field-data))
        
             ;; Numbers: draw bpf-kind
             (let* ((min-y-val (list-min field-data))
                    (max-y-val (list-max field-data))
                    (y-values-range (- max-y-val min-y-val))
                    (y-factor (if (zerop y-values-range) 1 (float (/ (- h (* 2 margin-y)) y-values-range)))))
          
               (let* ((x0 (nth-x-pos 0))
                      (val (car field-data))
                      (y0 (+ y (- h margin-y (* (- val min-y-val) y-factor))))
                      (step (ceiling (/ (length field-data) w)))) 
                        
                 ;;; draw the first element 
                 (draw-cross x0 mid-y)
                 (om-draw-circle x0 y0 2 :fill t)
                 (om-draw-string (- x0 6) (- y0 6) (format nil "~A" val)) 
                 (if (= y-values-range 0) (setf max-y-val nil)) ;; just to prevent drawing teh value again

                 (loop for n from 1 to (1- (length field-data)) 
                       by step do
                     ;(if (> step 1) (setf n (min (1- (length field-data)) (+ n (om-random 0 step)))))
                       (let* ((elt (nth n field-data))
                              (xx (nth-x-pos n))
                              (val (nth n field-data))
                              (yy (+ y (- h margin-y (* (- val min-y-val) y-factor)))))
                         (if (< (length field-data) 200) (draw-cross xx mid-y))
                         (when (< (length field-data) 100) (om-draw-circle xx yy 2 :fill t))
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
           (if (< (length field-data) 200)
               (loop for elt in field-data
                     for n = 0 then (+ n 1) do
                     (let* ((xx (nth-x-pos n))
                            (val (nth n field-data))
                            (yy (+ y (* h .5))))
                       (draw-cross xx mid-y)  
                       (draw-element-in-array-field val xx yy x-space h)
                       ))
             (om-draw-string (- (* w 0.5) 40) mid-y "[...(list too long)...]"))
           ))))))


(defmethod draw-element-in-array-field ((self t) center-x center-y w h)
  (om-draw-string center-x center-y (format nil "~A" self)))

(defmethod draw-element-in-array-field ((self bpf) center-x center-y w h)
  (let ((dummy-cache (get-cache-display-for-draw self))) ;; dummy because it is not cached actually (bad)
    
    (om-draw-line (+ center-x (* w .5)) (- center-y (* h .48)) 
                  (+ center-x (* w .5)) (+ center-y (* h .48))
                  :style '(2 2) :color (om-def-color :dark-blue))
    
    (draw-bpf-points-in-rect (cadr dummy-cache)
                             (color self) 
                             (car dummy-cache)
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                             (- center-x (* w .45)) 
                             (- center-y (* h .45)) 
                             (* w .9) (* h .9)
                             :lines)))

;============================================================
; SIMPLE 2D-ARRAY
; Initialized mainly with the data (a nested list)
;============================================================

;; <data> is redefined with :initarg so as to appear as a box input
(defclass* 2D-array (OMArray) 
  ((data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

;; array dimensions are set according to <data>
(defmethod om-init-instance ((self 2D-array) &optional initargs)
  (call-next-method)
  (if (data self)
      (setf (num-fields self) (length (data self))
            (num-elts self) (apply 'max (mapcar 'length (data self))))
      (setf (num-fields self) 0)
      )
  self)


(defmethod get-field ((self 2D-array) (col integer) &key (warn-if-not-found t))
  (if (< col (length (data self)))
      (nth col (data self))
    (if warn-if-not-found (om-beep-msg "Field #~D not found in '~A'" col self))))

(defmethod get-cache-display-for-text ((self 2D-array))
  (loop for field in (data self) 
        for i = 0 then (1+ i) 
        collect (list (format nil "[~D]" i) field)))


;;;============================================================
;;; CLASS-ARRAY / OM6-LIKE
;;; A 2D-ARRAY with more explicit semantics assigned to each field.
;;; data are matrix-fields (not just data)
;;; fields declares a number of main matrix-fields + defaults 
;;;============================================================

(defstruct array-field (name) (doc) (type) (default) (data) (decimals))

(defmethod om-copy ((self array-field))
  (make-array-field 
   :name (array-field-name self)
   :doc (array-field-doc self)
   :type (array-field-type self)
   :decimals (array-field-decimals self)
   :default (array-field-default self)
   :data (array-field-data self)))

(defmethod omng-save ((self array-field))  
  `(:array-field 
    (:name ,(array-field-name self))
    (:doc ,(array-field-doc self))
    (:type ,(omng-save (array-field-type self)))
    (:decimals ,(array-field-decimals self))
    (:default ,(omng-save (array-field-default self)))
    (:data ,(omng-save (array-field-data self)))))

(defmethod om-load-from-id ((id (eql :array-field)) data)
  (make-array-field :name (find-value-in-kv-list data :name) 
                    :doc (find-value-in-kv-list data :doc)
                    :type (find-value-in-kv-list data :type)
                    :decimals (find-value-in-kv-list data :decimals)
                    :default (omng-load (find-value-in-kv-list data :default))
                    :data (omng-load (find-value-in-kv-list data :data))))
   
(defmethod get-array-field-data ((field array-field)) 
  (array-field-data field))

;;; the <data> slot is not visible and set according to the meta-info + optional additionl box inputs 
;;; (requires a dedicated box type)
(defclass* class-array (OMArray) 
  ((fields :initform nil :initarg :fields :accessor fields :documentation "field names and defaults")
   (num-elts :initform 1 :initarg :num-elts  :accessor num-elts :documentation "number of elements (a.k.a lines)")))

(defmethod object-box-label ((self class-array))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (length (data self)))
           "x" 
           (number-to-string (num-elts self)) "]"))

(defmethod additional-slots-to-save ((self class-array)) '(data))


(defmethod om-init-instance ((self class-array) &optional initargs)
  
  (call-next-method) 
  
  ;;; in class array some 'meta-data' determine the contents of the actual data
  (setf (num-fields self) (length (fields self)))
  (unless (num-elts self) (setf (num-elts self) 0))

  (when initargs ;; INITARGS = NIL means we are loading a saved object (data is already in)
    (setf (data self)
          (loop for field in (fields self) collect
                
                (let* ((input-data (find-value-in-kv-list initargs (intern-k field)))
                       
                       ;; the field can already be in the data
                       ;; if this data was copied or initialized from a subclass (e.g. cs-evt in OMChroma)
                       (existing-field (find field (data self) :test 'string-equal :key 'array-field-name))
                       
                       (final-field (or existing-field (make-array-field :name field :decimals 4))))
                  
                  (if input-data 
                      ;; the field is to set from specified data
                      (setf (array-field-data final-field)
                            (get-array-data-from-input input-data (num-elts self)))
                    
                    ;; the field will be filled from the default value (if any) or NIL
                    (setf (array-field-data final-field)
                          (get-array-data-from-input (array-field-default final-field) (num-elts self)))
                    )
                  
                  final-field)
                ))
    )               
  self)



  
(defmethod get-field ((self class-array) (col integer) &key (warn-if-not-found t))
  (if (< col (length (data self)))
      (array-field-data (nth col (data self)))
    (if warn-if-not-found (om-beep-msg "Field #~D not found in '~A'" col self))))

(defmethod get-field ((self class-array) (col string) &key (warn-if-not-found t))
  (let ((array-field (find col (data self) 
                       :test 'string-equal 
                       :key #'array-field-name)))
    (if array-field (array-field-data array-field)
      (if warn-if-not-found (om-beep-msg "Field '~A' not found in '~A'" col self)))))

(defmethod get-field-name ((self class-array) (col integer)) 
  (or                        
   (and (< col (length (data self)))              ;;; v
        (array-field-name (nth col (data self)))) ;;; in principle these are the same
   (nth col (fields self))                        ;;; ^ 
   (format nil "c_~D" col)))

(defmethod get-field-type ((self class-array) (col integer)) 
  (and (< col (length (data self)))
       (array-field-type (nth col (data self)))))


;;; redefinition from OM methods
(defmethod get-slot-val ((self class-array) slot-name)
  (or (get-field self (string slot-name) :warn-if-not-found nil)
      (call-next-method)))
 
(defmethod get-cache-display-for-text ((self class-array))
  (append (call-next-method)
          (loop for array-field in (data self) collect 
                (list (intern-k (array-field-name array-field))
                      (array-field-data array-field)))
          ))


;;; collect the raw internal data
(defmethod get-data ((self class-array))
  (mapcar #'array-field-data (data self)))

;;; methods for filling data
(defmethod get-array-data-from-input ((input t) n)
  (make-list n :initial-element (om-copy input)))

(defmethod get-array-data-from-input ((input cons) n)
  (cond ((= (length input) n) 
         (om-copy input))
        ((< (length input) n) 
         (get-array-data-from-input (append (om-copy input) (om-copy input)) n))
        ((> (length input) n)
         (first-n (om-copy input) n))))


(defmethod get-array-data-from-input ((input function) n)
  (case (length (function-arg-list input))
    (1 (mapcar input (loop for i from 0 to (1- n) collect i)))
    (0 (loop for i from 1 to n collect (funcall input)))
    (otherwise (om-beep-msg "functions as array input must have 1 or 0 arguments!"))
    ))

(defmethod get-array-data-from-input ((input symbol) n)
  (if (fboundp input) 
      (get-array-data-from-input (fdefinition input) n) 
    (call-next-method)))

(defmethod get-array-data-from-input ((input bpf) n)
  (multiple-value-bind (bpf xx yy) (om-sample input n) yy))




;;;============================
;;; Special box for class-array
;;;============================

(defclass ClassArrayBox (OMBoxEditCall) 
  ((keywords :initform nil :accessor keywords :initarg :keywords)))

(defmethod special-box-type ((self (eql 'class-array))) 'ClassArrayBox)

;;; list of proposed keywords are the declared names
(defmethod get-all-keywords ((self ClassArrayBox)) (list (keywords self)))

(defmethod update-key-inputs ((self ClassArrayBox))
  (when(get-box-value self)
    (setf (keywords self) 
          (mapcar 
           #'(lambda (f) (intern-k (array-field-name f)))
           (data (get-box-value self)))))
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod initialize-instance :after ((self ClassArrayBox) &rest args)  
  (declare (ignore args))
  (update-key-inputs self))

(defmethod omng-box-value :after ((self ClassArrayBox) &optional numout)
  (update-key-inputs self))

(defmethod (setf value) :after (val (self ClassArrayBox))  
  (update-key-inputs self))

(defmethod rep-editor ((box ClassArrayBox) num)
  (if (or (null num) (<= num 2)) (call-next-method)
    (let* ((field-name (name (nth num (outputs box)))))
      (get-field (get-box-value box) field-name))))  



;;;========================================
;;; A superclass for 'playable' class-array
;;;========================================

(defclass SynthesisEvt (class-array schedulable-object) ())


