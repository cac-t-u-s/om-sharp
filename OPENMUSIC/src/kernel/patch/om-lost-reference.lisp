

(in-package :om)

;-------------------------------------------
; BOX FOR LOST FUNCTIONS
;-------------------------------------------
; THIS BOX IS CREATED IF THE BOX IS NOT FOUND
; IN PRINCIPLE IT SHOULD NEVER BE SAVED BUT 
; RE-SAVE THE ORIGINAL REFERENCE BOX

(defclass LostReferenceBox (OMBoxCall) 
  ((reference-type :accessor reference-type :initform nil :initarg :reference-type)
   (lost-reference :accessor lost-reference :initform nil :initarg :lost-reference))
  (:default-initargs :reference :missing-reference)
  (:metaclass omstandardclass))

(defmethod get-icon-id-from-reference ((self LostReferenceBox)) 'dead)
(defmethod object-name-in-inspector ((self LostReferenceBox)) 
  (format nil "Missing reference box [~A ~A]" 
          (reference-type self)
          (lost-reference self)))

;; hacks the accessors
(defmethod color ((self LostReferenceBox)) (om-make-color 1 0.6 0.5))
(defmethod text-color ((self LostReferenceBox)) (om-make-color .8 0. 0.))
(defmethod border ((self LostReferenceBox)) nil)

;; for object boxes
(defmethod (setf window-pos) (pos (self LostReferenceBox)) nil)
(defmethod (setf window-size) (pos (self LostReferenceBox)) nil)


;(defmethod draw-border ((self LostReferenceBox) x y w h style)
;  (om-draw-rect x y w h :line (if (numberp style) style 3) :color (om-make-color 0.9 0.4 0.4) :angles :round))

;;; RE-SAVE AS IF EVERYTHING OK...
(defmethod save-box-reference ((self LostReferenceBox)) (lost-reference self))
(defmethod box-type ((self LostReferenceBox)) (reference-type self))

;;; EVAL/GEN-CODE
(defmethod omNG-box-value ((self LostReferenceBox) &optional (numout 0)) 
  (om-beep-msg "MISSING REFERENCE FOR BOX [~A ~A]" 
               (reference-type self)
               (lost-reference self))
  (om-abort))

(defmethod gen-code ((self LostReferenceBox) &optional numout)
  (om-beep-msg "MISSING REFERENCE FOR BOX [~A ~A]" 
               (reference-type self)
               (lost-reference self))
  (om-abort))

;;; we have to expliciely copy the inputs and outputs 
;;; and not le the default mechanism work
(defmethod update-from-reference ((self LostReferenceBox)) nil)
(defmethod smart-copy-additional-inputs ((self LostReferenceBox) newbox) nil)

(defmethod om-copy ((self LostReferenceBox)) 
  (let ((newbox (call-next-method)))
    ;;; add the in/outs
    (setf (inputs newbox) 
          (mapcar #'(lambda (i)
                      (make-instance (type-of i)
                                     :value (om-copy (value i))
                                     :reference (reference i)
                                     :name (name i)
                                     :box newbox
                                     :doc-string (doc-string i)))
                  (inputs self)))
    (setf (outputs newbox) 
          (mapcar 
           #'(lambda (o)
               (make-instance (type-of o)
                              :value (om-copy (value o))
                              :reference (reference o)
                              :name (name o)
                              :box newbox
                              :doc-string (doc-string o)))
           (outputs self)))
    newbox))

(defmethod restore-inputs ((self LostReferenceBox) inputs)
  (ignore-errors 
    (loop for input-desc in inputs do
          (let ((type (find-value-in-kv-list (cdr input-desc) :type))
                (name (find-value-in-kv-list (cdr input-desc) :name))
                (val (find-value-in-kv-list (cdr input-desc) :value))
                (reac (find-value-in-kv-list (cdr input-desc) :reactive)))
            (case type
              (:standard 
               (setf (inputs self) 
                     (append (inputs self) 
                             (list (make-instance 'box-input :box self
                                                  :name name :reference (intern name)
                                                  :value (omng-load val) :reactive reac)))))
               (:optional 
                (add-optional-input self :name name
                                    :value (omng-load val)
                                    :reactive reac))
               (:key 
                (add-keyword-input self :key name
                                   :value (omng-load val)
                                   :reactive reac))
              )))))

(defmethod restore-outputs ((self LostReferenceBox) outputs)
  (setf (outputs self)
        (loop for output-desc in outputs
              for i from 0 collect
              (let* ((name (find-value-in-kv-list (cdr output-desc) :name))
                     (reac (find-value-in-kv-list (cdr output-desc) :reactive)))
                (make-instance 'box-output :box self
                               :name name :reference i
                               :reactive reac))
              )))


;;;===================
;;; LOST FUNCTION
;;;===================
(defmethod omng-make-lost-fun-box (reference pos &optional init-args)
  (let* ((box (make-instance 'LostReferenceBox
                             :lost-reference reference
                             :reference-type :function))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))


;;;===================
;;; LOST CLASS
;;;===================
(defmethod omng-make-lost-class-box (reference pos &optional init-args)
  (let* ((box (make-instance 'LostReferenceBox
                             :lost-reference reference
                             :reference-type :object))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))

(defmethod omng-make-lost-slots-box (reference pos &optional init-args)
  (let* ((box (make-instance 'LostReferenceBox
                             :lost-reference reference
                             :reference-type :slots))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    box))


#|

(defclass testclass () 
  ((a :accessor a :initarg :a :initform nil)
   (b :accessor b :initarg :b :initform nil)))

(make-instance 'testclass)
(find-class 'testclass nil)
(clos::remove-class-internal (find-class 'testclass nil))

|#
