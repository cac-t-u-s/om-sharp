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

;============================================================
; ARRAY OBJECTS
;============================================================

(in-package :om)

;============================================================
;;; SUPERCLASS DEFINING COMMON ARRAY FEATURES
;============================================================

(defclass OMArray ()
  ((elts :initform 0 :accessor elts :documentation "number of elements (a.k.a lines)")
   (fields :initform 0 :accessor fields :documentation "number of fields (a.k.a columns)")
   (field-names :initform nil :accessor field-names :initarg :field-names :documentation "field (column) names ")
   (data :initform nil :accessor data :documentation "data matrix")))

;(defmethod initialize-instance :after ((self OMArray) &rest args)
;  (update-contents self))

(defmethod object-box-label ((self OMArray))
  (string+ (string-upcase (type-of self)) " ["
           (number-to-string (fields self)) "x" 
           (number-to-string (elts self)) "]"))

(defmethod get-field-name ((self OMArray) (col integer)) 
  (or (nth col (field-names self))
      (format nil "c_~D" col)))


;;; deprecated - use get-field
(defmethod get-col ((self OMArray) col &key (warn-if-not-found t))
  (get-field self col :warn-if-not-found warn-if-not-found))

(defmethod display-modes-for-object ((self OMArray)) '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self OMArray) (box t) x y w h &optional time)
  (let* ((display-cache (ensure-cache-display-draw box self))
         (font (om-def-font :font1 :size 10))
         (n-lines (length (data self)))
         (inter-line 3)
         (v-margin 8)
         (line-h (if (data self) 
                     (/ (- h v-margin (* inter-line (1- n-lines))) n-lines)
                   (- h v-margin)))
         (h-margin (+ x 2))
         (line-w (- w 4)))
    (when (data self) 
      (loop for n from 0 to (1- n-lines)
            for yy = v-margin then (+ yy line-h inter-line) do
            (om-draw-rect h-margin yy line-w line-h :color (om-def-color :white) :fill t)
            (om-draw-string (+ x (- line-w (om-string-size (get-field-name self n) font)))
                            (+ yy 10) (get-field-name self n) 
                            :font font :color (om-make-color .6 .6 .7))
            (draw-field-on-box self (nth n (data self)) h-margin yy line-w line-h)
      ))))


;;; general case
;;; redefined for class-array
(defmethod get-array-field-data ((field list)) field)

(defmethod draw-field-on-box ((self OMArray) field x y w h)
  (when (> (elts self) 0)
    (let ((x-space (/ w (elts self)))
          (mid-y (+ y (/ h 2)))
          (margin-y (min 8 (/ h 2)))
          (field-data (get-array-field-data field)))
  
      (flet ((nth-x-pos (n) (+ x (* x-space (+ n 0.5))))
             (draw-cross (x y) 
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
                 (om-draw-string x0 (- y0 6) (format nil "~A" val)) 
                 (if (= y-values-range 0) (setf max-y-val nil)) ;; just to prevent drawing the value again

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
                   (om-draw-string (- (* w 0.5) 58) mid-y (format nil "[display down-sampled x 1/~D]" step)
                                   :color (om-make-color .7 .6 .7)))
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
                             :lines-only)))

;============================================================
; SIMPLE 2D-ARRAY
; Initialized mainly with the data (a nested list)
;============================================================

;; <data> is redefined with :initarg so as to appear as a box input
(defclass* 2D-array (OMArray) 
  ((data :initform nil :initarg :data :accessor data :documentation "data matrix / list of lists : (col1 col2 ...)")))

(defmethod additional-class-attributes ((self 2D-array)) '(field-names))

;;; collect the raw internal data / redefined in class-array
(defmethod get-array-data ((self 2D-array)) (data self))

;; array dimensions are set according to <data>
(defmethod om-init-instance ((self 2D-array) &optional initargs)

  (call-next-method)

  (if (data self)
      (setf (fields self) (length (data self))
            (elts self) (loop for field in (get-array-data self) maximize (length (get-array-field-data field))))
      (setf (fields self) 0 
            (elts self) 0)
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
                    :type (omng-load (find-value-in-kv-list data :type))
                    :decimals (find-value-in-kv-list data :decimals)
                    :default (omng-load (find-value-in-kv-list data :default))
                    :data (omng-load (find-value-in-kv-list data :data))))
   
(defmethod get-array-field-data ((field array-field)) 
  (array-field-data field))

;;; the <data> slot is not visible and set according to the meta-info + optional additionl box inputs 
;;; (requires a dedicated box type)
(defclass* class-array (OMArray) 
  ((field-names :initform nil :initarg :field-names :accessor field-names :documentation "field (column) names ")
   (elts :initform 1 :initarg :elts  :accessor elts :documentation "number of elements (lines)")
   (attached-components :accessor attached-components :initform nil) ;; a temporary list of 'active' accessed components
   )
  (:documentation "
CLASS-ARRAY is a special implementation of a 2D-array where specific semantics is given to the different fields (columns).

<field-names> is a list of strings. Each name in it initializes a column which becomes accessible through the additional/optional inputs of the CLASS-ARRAY box.

<elts> is the number of lines/elements in the array.

Data instanciation in a column is done according to the specified number of lines/elements (<elts>), either by

- Repeating a single value
- Applying list values
- Looping through the list of value (if shorter than <elts>)
- Sampling a BPF
- Evaluating a lambda function of 0 or 1 argument (if 1, the argument is the element index in the array).
"))


(defmethod additional-slots-to-save ((self class-array)) '(data))
(defmethod additional-slots-to-copy ((self class-array)) '(data))

(defmethod om-init-instance ((self class-array) &optional initargs)
   
  ;;; no next-method actually (T)
  (call-next-method) 
  
  ;;; find the potential extra-controls and add them as field names
  (setf (om::field-names self)
        (append (om::field-names self)
                (remove nil (loop for initarg in initargs
                                  when (car initarg)  ;; sometimes initargs = (nil)
                                  collect 
                                  (let ((arg-name (symbol-name (car initarg))))
                                    (unless (or ;; already there from previous initialization (e.g. from Csound orc)
                                                (find arg-name (om::field-names self) :test 'string-equal) 
                                                ;; member of the 'invisible' slots
                                                (find arg-name (om::class-slots (class-of self)) :key 'om::slot-name
                                                      :test 'string-equal)
                                                )
                                      arg-name))))
                ))


  ;;; in class-array some 'meta-data' determine the contents of the actual data
  (setf (fields self) (length (field-names self)))
  (unless (elts self) (setf (elts self) 0))
  
  (when t ;; initargs ;; INITARGS = NIL means we are loading a saved object (data is already in)
    (setf (data self)
          (loop for field in (field-names self) collect
                
                (let* ((input-data (find-value-in-kv-list initargs (intern-k field)))
                       
                       ;; the field can already be in the data
                       ;; if this data was copied or initialized from a subclass (e.g. cs-evt in OMChroma)
                       (existing-field (find field (data self) :test 'string-equal :key 'array-field-name)))
                  
                  (cond (input-data 
                         ;; the field is to be set from specified data, whatever existed before
                         (make-array-field :name field :decimals 4
                                           :default (and existing-field (array-field-default existing-field))
                                           :type (and existing-field (array-field-type existing-field))
                                           :doc (and existing-field (array-field-doc existing-field))
                                           :data (get-array-data-from-input input-data (elts self))))
                        
                        (existing-field
                         ;; the field already exists
                         (make-array-field :name field :decimals 4
                                           :default (array-field-default existing-field)
                                           :type (array-field-type existing-field)
                                           :doc (array-field-doc existing-field)
                                           :data (get-array-data-from-input 
                                                  (or (array-field-data existing-field)
                                                      (array-field-default existing-field))
                                                  (elts self))))
                        (t
                         ;; the field will be filled from the default value (if any) or NIL
                         (make-array-field :name field :decimals 4
                                           :data (get-array-data-from-input nil (elts self))))
                        )
                  )))
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

(defmethod get-field-default ((self class-array) (col integer)) 
  (and (< col (length (data self)))
       (array-field-default (nth col (data self)))))

(defmethod get-field-id ((self class-array) (field string))
  (position field (field-names self) :test 'string-equal))

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

(defmethod default-size ((self ClassArrayBox)) (om-make-point 100 100))

(defmethod box-free-keyword-name ((self ClassArrayBox)) 'add-field)

;;; list of proposed keywords are the declared names
(defmethod get-all-keywords ((self ClassArrayBox)) 
  (append (list (keywords self))
          (call-next-method) ;; additional-class-attributes of the reference
          (when (and (get-box-value self) (allow-extra-controls (get-box-value self)))
            `((,(box-free-keyword-name self))))
          ))


(defmethod allow-extra-controls ((self class-array)) nil)

(defmethod update-key-inputs ((self ClassArrayBox))
  (when (get-box-value self)
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


(defmethod get-input-doc ((self ClassArrayBox) name)
  (or (call-next-method)
      (when (get-box-value self) ;;; sometimes at the very beginning of box creation there is not yet a value...
        (let ((field (find name (data (get-box-value self)) :test 'string-equal :key 'array-field-name)))
          (when field 
            (array-field-doc field))))))

(defmethod get-input-def-value ((self ClassArrayBox) name)
  (or (call-next-method)
      (when (get-box-value self) ;;; sometimes at the very beginning of box creation there is not yet a value...
        (let ((field (find name (data (get-box-value self)) :test 'string-equal :key 'array-field-name)))
          (when field 
            (array-field-default field))))))


#|
;;; this will eventually call (setf value) anyway... => removed
(defmethod omng-box-value :after ((self ClassArrayBox) &optional numout)
  (update-key-inputs self))
|#

(defmethod (setf value) :after (val (self ClassArrayBox))  
  (update-key-inputs self))

(defmethod rep-editor ((box ClassArrayBox) num)
  (if (or (null num) (<= num 2)) (call-next-method)
    (let* ((field-name (name (nth num (outputs box)))))
      (get-field (get-box-value box) field-name))))  



;;;==============================================
;;; Components are temporary structures 
;;; used internally by OMChroma
;;; to parse the contents of arrays by column
;;;============================================== 

;;; currently 'used' components of a same array 
;;; are stored in a temporary slot of the array (attached-components)
;;; in order to maintain the indexes when components are added or removed.

(defstruct component  
  (array nil)
  (vals nil)
  (index 0))

;-----------Tools--------------

(defmethod om-copy ((self component))
  (make-component :array (component-array self)
                  :vals (om-copy (component-vals self))
                  :index nil))

(defmethod get-field-id ((self component) (field string))
   (get-field-id (component-array self) field))


(defmethod get-array-element ((self class-array) n)
  (loop for array-field in (data self) collect
        (nth n (array-field-data array-field))))

(defmethod add-array-element ((self class-array) pos val-list)
  (loop for array-field in (data self) 
        for val in val-list do
        (setf (array-field-data array-field)
              (insert-in-list (array-field-data array-field) val pos)))
  (setf (elts self) (1+ (elts self))))

(defmethod set-array-element ((self class-array) pos val-list)
  (loop for array-field in (data self) 
        for val in val-list do
        (setf (nth pos (array-field-data array-field)) val)))

(defmethod remove-array-element ((self class-array) pos)
  (loop for array-field in (data self) do 
        (setf (array-field-data array-field)
              (remove-nth pos (array-field-data array-field))))
  (setf (elts self) (1- (elts self))))


;-----------Interface-----------

(defmethod* new-comp (vals)
   :initvals '(nil)
   :indoc '("component values")
   :doc "Creates a new component filled with <vals>."
   :icon 'array-comp
   (make-component :val-list vals))


(defmethod* get-comp ((self class-array) (n integer))
   :initvals '(nil 0)
   :indoc '("a class-array instance"  "component number")
   :doc "Returns the <n>th component in <self> (a class-array object).

Components are returned as instances of the class 'component' and can be accessed using the functions comp-field, comp-list, fill-comp."
   :icon 'array-comp
   (or (find n (attached-components self) :key 'component-index :test '=)
       (when (< n (elts self))
         (let ((comp (make-component
                      :array self
                      :vals (get-array-element self n)
                      :index n)))
           (pushr comp (attached-components self))
           comp))))

(defmethod* add-comp ((self class-array) (comp component) &optional position)
   :initvals '(nil nil)
   :indoc '("a class-array instance"  "a component" "position in the array")
   :doc "Adds <comp> in <self> at <pos>.
If <pos> is not specified, the component is added at the end of the array."
   :icon 'array-comp
   (let* ((pos (or position (elts self))))
     (setf (component-array comp) self)
     (setf (component-index comp) pos)
     (add-array-element self pos (component-vals comp))
     (loop for cmp in (attached-components self) do
           (when (>= (component-index cmp) pos)
             (setf (component-index cmp) (1+ (component-index cmp)))))
     (pushr comp (attached-components self))
     comp))


(defmethod* remove-comp ((self component)) 
   :initvals '(nil)
   :indoc '("a class-array component")
   :doc 
"Remove component <self> from its associated array.

The modification is immediately applied in the original array."
   :icon 'array-comp
   (remove-array-element (component-array self) (component-index self))
   (remove (component-index self) (attached-components (component-array self)) :key 'index :test '=)
   ;;; !!! comp-array is still "self" for other removed components of same index
   (loop for cmp in (attached-components (component-array self)) do
         (when (> (component-index cmp) (component-index self))
           (setf (component-index cmp) (1- (component-index cmp)))))        
   (setf (component-array self) nil)
   self)


(defmethod! comp-list ((self component) &optional val-list) 
   :initvals '(nil)
   :indoc '("a class-array component")
   :doc "Sets (if <val-list> is supplied) or returns (if not) the values in <self> (a class-array component).

If <val-list> is supplied, the component itself is returned.

If the component is attached to an array, i.e. previously passed through 'get-comp' or 'add-comp',
then the modifications are immediately stored in the original array."
   :icon 323
    (if val-list
        (progn 
          (setf (component-vals self) val-list)
          (when (and (component-array self) (component-index self))
            (set-array-element (component-array self) (component-index self) (component-vals self)))
          self)
      (component-vals self)))


(defmethod* comp-field ((self component) (LineId integer) &optional val) 
   :initvals '(nil 0 nil)
   :indoc '("a class-array component" "a line identifier" "a value")
   :doc 
"Sets (if <val> is supplied) or returns (if not) the value of line <lineid> for component <self>.

If <val>, the component itself is returned.

If the component is attached to an array, i.e. previously passed through 'get-comp' or 'add-comp',
then the modifications are immediately stored in the original array.
<lineid> can be a number (index of the line) or a string (name of the line)."
   :icon 'array-comp
   (if val 
     (progn 
       (setf (nth LineId (component-vals self)) val)
       (when (and (component-array self) (component-index self))
         (set-array-element (component-array self) (component-index self) (component-vals self)))
       self)
     (nth LineId (component-vals self))))

(defmethod* comp-field ((self component) (LineId string) &optional val)
   (comp-field self (get-field-id self LineId) val))


