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

(defmethod get-icon-id ((self LostReferenceBox)) :dead)
(defmethod object-name-in-inspector ((self LostReferenceBox)) 
  (format nil "Missing reference box [~A ~A]" 
          (reference-type self)
          (lost-reference self)))

;; hacks the accessors
(defmethod box-draw-color ((self LostReferenceBox)) (om-make-color 1 0.6 0.5))
(defmethod box-draw-text-color ((self LostReferenceBox)) (om-make-color .8 0. 0.))
(defmethod border ((self LostReferenceBox)) nil)

;; for object boxes
(defmethod (setf window-pos) (pos (self LostReferenceBox)) nil)
(defmethod (setf window-size) (pos (self LostReferenceBox)) nil)

(defmethod maximum-size ((self LostReferenceBox)) nil)

;(defmethod draw-border ((self LostReferenceBox) x y w h style)
;  (om-draw-rect x y w h :line (if (numberp style) style 3) :color (om-make-color 0.9 0.4 0.4) :angles :round))

;;; RE-SAVE AS IF EVERYTHING OK...
(defmethod save-box-reference ((self LostReferenceBox)) (omng-save (lost-reference self)))
(defmethod box-type ((self LostReferenceBox)) (reference-type self))

;;; EVAL/GEN-CODE
(defmethod omNG-box-value ((self LostReferenceBox) &optional (numout 0)) 
  (om-beep-msg "MISSING REFERENCE FOR BOX [~A ~A]" 
               (reference-type self)
               (lost-reference self))
  (om-abort))

(defmethod gen-code ((self LostReferenceBox) &optional numout)
  (error (format nil "MISSING REFERENCE FOR BOX [~A ~A]" 
                 (reference-type self)
                 (lost-reference self))))

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


(defun restore-inputs-from-saved-desc (box desc)
  (loop for input-desc in desc do
        (let ((type (find-value-in-kv-list (cdr input-desc) :type))
              (name (find-value-in-kv-list (cdr input-desc) :name))
              (val (find-value-in-kv-list (cdr input-desc) :value))
              (reac (find-value-in-kv-list (cdr input-desc) :reactive)))
          (case type
            (:standard 
             (setf (inputs box) 
                   (append (inputs box) 
                           (list (make-instance 'box-input :box box
                                                :name name :reference (intern name)
                                                :value (omng-load val) :reactive reac)))))
            (:optional 
             (add-optional-input box :name name
                                 :value (omng-load val)
                                 :reactive reac))
            (:key 
             (add-keyword-input box :key name
                                :value (omng-load val)
                                :reactive reac))
            ))))

(defmethod restore-inputs ((self LostReferenceBox) inputs)
  (restore-inputs-from-saved-desc self inputs))

(defun restore-outputs-from-saved-desc (box desc)
  (setf (outputs box)
        (loop for output-desc in desc
              for i from 0 collect
              (let* ((name (find-value-in-kv-list (cdr output-desc) :name))
                     (reac (find-value-in-kv-list (cdr output-desc) :reactive)))
                (make-instance 'box-output :box box
                               :name name :reference i
                               :reactive reac))
              )))

(defmethod restore-outputs ((self LostReferenceBox) outputs)
  (restore-outputs-from-saved-desc self outputs))



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
    (setf (name box) (string (lost-reference box)))
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
    (setf (name box) (string (lost-reference box)))
    box))

(defmethod omng-make-lost-slots-box (reference pos &optional init-args)
  (let* ((box (make-instance 'LostReferenceBox
                             :lost-reference reference
                             :reference-type :slots))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos))
    (setf (name box) (string (lost-reference box)))
    box))


#|

;;; test with function
(defun testfun (a b &key c) (+ a b))
(fmakunbound 'testfun)

;;; test with class
(defclass testclass () 
  ((a :accessor a :initarg :a :initform nil)
   (b :accessor b :initarg :b :initform nil)))
;(make-instance 'testclass)
;(find-class 'testclass nil)
(clos::remove-class-internal (find-class 'testclass nil))

|#

;;;===============================
;;; MISSING ABSTRACTIONS 
;;;===============================
;;; For persistant abstraction boxes we play it differently:
;;; The box remaions but it highlighted until the refence is missing (i.e., file not found)
;;; The user can also go and look for the file by himself
(defmethod lost-reference? ((box OMBoxAbstraction))
  (and (is-persistant (reference box))
       (not (probe-file (mypathname (reference box))))))

(defmethod restore-inputs ((self OMBoxAbstraction) inputs)
  (if (lost-reference? self)
      (restore-inputs-from-saved-desc self inputs)
    (call-next-method)))

(defmethod restore-outputs ((self OMBoxAbstraction) outputs)
  (if (lost-reference? self)
      (restore-outputs-from-saved-desc self outputs)
    (call-next-method)))

(defmethod box-draw-text-color ((self OMBoxAbstraction))
  (if (lost-reference? self) (om-make-color .8 0. 0.) (call-next-method)))

(defmethod box-draw-color ((self OMBoxAbstraction)) 
  (if (lost-reference? self) (om-make-color 1 0.6 0.5) (call-next-method)))

(defmethod draw-patch-icon :after ((self OMBoxAbstraction) &optional (offset-x 0) (offset-y 0))
  (when (lost-reference? self)
    (let ((x1 (+ offset-x 8)) (x2 (+ offset-x (- 24 4)))
          (y1 (+ offset-y 10)) (y2 (+ offset-y (- 24 0))))
      (om-with-fg-color (om-make-color-alpha (om-def-color :dark-red) .7)
        (om-with-line-size 3
          (om-draw-line x1 y1 x2 y2)
          (om-draw-line x1 y2 x2 y1)
          )))))

(defmethod open-editor ((self OMBoxAbstraction)) 
  (if (lost-reference? self) 
      (progn 
        (om-beep-msg "MISSING REFERENCE FOR BOX '~A'.~%[=> File '~s' not found]" 
                     (name self)
                     (mypathname (reference self)))
        (setf (loaded? (reference self)) nil) ;; is it not ?
        )
    (call-next-method)))

(defmethod boxcall-function :before ((self OMBoxAbstraction))
  (when (lost-reference? self)
    (om-beep-msg "MISSING REFERENCE FOR BOX '~A'.~%[=> File '~s' not found]" 
                 (name self)
                 (mypathname (reference self)))
    (om-abort)))

(defmethod gen-code :before ((self OMBoxAbstraction) &optional numout)
   (when (lost-reference? self)
    (om-beep-msg "MISSING REFERENCE FOR BOX '~A'.~%[=> File '~s' not found]" 
                 (name self)
                 (mypathname (reference self)))
    (om-abort)))