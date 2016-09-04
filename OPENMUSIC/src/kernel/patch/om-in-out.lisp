;--------------------------------------------------------
; Managing inouts and outputs in OM patches
;--------------------------------------------------------

(in-package :om)

;;;==========================
;;; CLASSES
;;;==========================

(defclass OMPatchIO (OMObject) 
  ((doc :initform "" :accessor doc :initarg :doc)
   (index :initform nil :accessor index)))

(defclass OMIn (OMPatchIO) 
  ((defval :initform nil :accessor defval :initarg :defval)
   (in-symbol :initform nil :accessor in-symbol)))

(defclass OMOut (OMPatchIO) ())

;;;==========================
;;; BOX
;;;==========================

;;; GENERAL SUPERCLASS
(defclass OMInOutBox (OMBoxCall) ())

(defmethod box-color ((self t)) (om-def-color :black))

(defmethod omNG-make-new-boxcall ((reference OMPatchIO) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                             :name (name reference)
                             :reference reference
                             :icon-pos :top
                             :text-align :center
                             :color nil
                             :border nil))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    box))

(defmethod get-properties-list ((self OMInOutBox))
  (add-properties (hide-property (call-next-method) '(:icon))
                  "Appearance" 
                  '((:icon "Icon position" (:left :top) icon-pos))))

(defmethod valid-property-p ((self OMInOutBox) (prop-id (eql :lock))) nil)
(defmethod valid-property-p ((self OMInOutBox) (prop-id (eql :lambda))) nil)

(defmethod minimum-size ((self OMInOutBox))
  (om-make-point (+ 8 (om-string-size (name self) (text-font self)) (if (equal (icon-pos self) :left) 22 0))
                 (+ (if (equal (icon-pos self) :top) 14 0) 28)))

(defmethod maximum-size ((self OMInOutBox))
  (om-make-point 500 (+ (if (equal (icon-pos self) :top) 14 0) 28)))


(defmethod related-patchbox-slot ((self OMInOutBox)) nil)

(defmethod set-name ((self OMInOutBox) name)
  (setf (name (reference self)) name)
  (when (container self)
    (let ((patchboxes (references-to (container self))))
      (loop for box in patchboxes do
            (setf (name 
                   (nth (1- (index (reference self))) 
                        (funcall (related-patchbox-slot self) box))
                   ) name))))
  (call-next-method))
 
(defmethod allow-text-input ((self OMInOutBox)) 
  (values 
   (name self)
   #'(lambda (box text) (set-name box text))))

(defmethod box-draw ((self OMInOutBox) frame)
  (let* ((size (om-make-point 20 16))
         (pos (if (equal (icon-pos self) :left)
                  (om-make-point 0 (- (h frame) 24))
               (om-make-point (round (- (w frame) (om-point-x size)) 2) 6))))
    (om-with-fg-color (box-color self)
      (om-draw-rect (+ (om-point-x pos) 3) (om-point-y pos)
                  (- (om-point-x size) 6) (- (om-point-y size) 6)
                  :fill t)
      (om-draw-polygon (list (om-point-x pos) (+ (om-point-y pos) (- (om-point-y size) 8))
                             (+ (om-point-x pos) (om-point-x size)) (+ (om-point-y pos) (- (om-point-y size) 8))
                             (+ (om-point-x pos) (/ (om-point-x size) 2)) (+ (om-point-y pos) (om-point-y size)))
                       :fill t)
      )
    (om-with-fg-color (om-def-color :white)
      (om-with-font (om-def-font :font1b)
                    (om-draw-string (- (+ (om-point-x pos) (/ (om-point-x size) 2)) 4) (if (equal (icon-pos self) :left) 14 16) 
                                    (number-to-string (index (reference self))))
                    ))
    t))



;;;====================================
;; SPECIFIC BOXES
;;;====================================

;;;====================================
;;; IN (GENERAL)
(defclass OMInBox (OMInOutBox) ())
(defmethod box-n-outs ((self OMInBox)) 1)
(defmethod box-color ((self OMInBox)) (om-make-color 0.2 0.6 0.2))

(defmethod special-box-p ((name (eql 'in))) t)
(defmethod get-box-class ((self OMIn)) 'OMInBox)
(defmethod related-patchbox-slot ((self OMInBox)) 'inputs)

(defmethod omNG-make-new-boxcall ((reference (eql 'in)) pos &optional init-args)
  (let ((name (car (list! init-args)))
        (val (cadr (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMIn :name (if name (string name) "in") :defval val)
     pos init-args)))

(defmethod current-box-value ((self OMInBox) &optional (numout nil))
  (if numout (defval (reference self)) (list (defval (reference self)))))

;;;===================================
;;; OUT
(defclass OMOutBox (OMInOutBox) ())
(defmethod box-n-outs ((self OMOutBox)) 0)
(defmethod box-color ((self OMOutBox)) (om-make-color 0.3 0.6 0.8))

(defmethod create-box-inputs ((self OMOutBox)) 
  (list 
   (make-instance 'box-input 
                  :name "out-value"
                  :box self
                  :value NIL
                  :doc-string "Connect here")))

(defmethod special-box-p ((name (eql 'out))) t)
(defmethod get-box-class ((self OMOut)) 'OMOutBox)
(defmethod related-patchbox-slot ((self OMOutBox)) 'outputs)

(defmethod omNG-make-new-boxcall ((reference (eql 'out)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMOut :name (if name (string name) "out"))
     pos init-args)))

;;;====================================
;; PATCH INTEGRATION
;;;====================================

;(defmethod nth-elem-in-patch ((self OMPatch) elem)
;  (count (type-of elem) (boxes self) :test 'subtypep :key 'type-of))
;
;(defmethod verify-name ((elem OMInOutBox) (container OMPatch))
;  (when (find (name elem) (remove elem (get-boxes-of-type container (type-of elem))) :test 'string-equal :key 'name)
;    (setf (name elem) (format nil (concatenate 'string (name elem) "~D") (index elem)))
;    (verify-name elem container)))
;
;(defmethod register-patch-io ((Self OMPatch) (elem OMPatchIO))
;  (setf (index elem) (nth-elem-in-patch self elem))
;  (verify-name elem self))
 
(defmethod register-patch-io ((self OMPatch) (elem OMIn))
  (setf (inputs self) (append (inputs self) (list elem)))
  (unless (index elem) ;; for instance when the input is loaded, the index is already set
    (setf (index elem) (length (inputs self)))))
 
(defmethod register-patch-io ((self OMPatch) (elem OMOut))
  (setf (outputs self) (append (outputs self) (list elem)))
  (unless (index elem) (setf (index elem) (length (outputs self)))))
  
(defmethod unregister-patch-io ((self OMPatch) (elem OMIn))
  (setf (inputs self) (remove elem (inputs self)))
  (loop for inp in (inputs self) do
          (when (> (index inp) (index elem))
            (setf (index inp) (1- (index inp))))))

(defmethod unregister-patch-io ((self OMPatch) (elem OMOut))
  (setf (outputs self) (remove elem (outputs self)))
  (loop for out in (outputs self) do
          (when (> (index out) (index elem))
            (setf (index out) (1- (index out))))))

(defmethod omNG-add-element ((self OMPatch) (box OMInOutBox))
  (call-next-method)
  (register-patch-io self (reference box))
  ;(unless *loaading-stack* ;;; do not update if patch is being loaded: inputs are already OK
  (loop for item in (references-to self) do
        (update-from-reference item))
  t)

(defvar *erased-io* nil)
(defmethod omng-remove-element ((self OMPatch) (box OMInOutBox))
  (call-next-method)
  (unregister-patch-io self (reference box))  
  (setf *erased-io* (reference box))
  (loop for item in (references-to self) do (update-from-reference item))
  (setf *erased-io* nil))




;;;====================================
;;; META
;;; ATTENTION THESE BOXES MUST BE UPDATED BEFORE EVALUATION DEPENDING ON CONTEXT
;;; THE META INPUTS DO NOT APPEAR OUSIDE THE PATCH
(defclass OMSelfIn (OMIn) ())
(defclass OMSelfInBox (OMInBox) ())
(defmethod box-color ((self OMSelfInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod special-box-p ((name (eql 'mybox))) t)
(defmethod get-box-class ((self OMSelfIn)) 'OMSelfInBox)

(defmethod omNG-make-new-boxcall ((reference (eql 'mybox)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMSelfIn :name "BOX")
   pos init-args))

;;;====================================
;;; Maquette accessor for control patch
(defclass OMMaqIn (OMIn) ())
(defclass OMMaqInBox (OMInBox) ())
(defmethod box-color ((self OMMaqInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod special-box-p ((name (eql 'mymaq))) t)
(defmethod get-box-class ((self OMMaqIn)) 'OMMaqInBox)

(defmethod omNG-make-new-boxcall ((reference (eql 'mymaq)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMMaqIn :name "MAQUETTE")
   pos init-args))
