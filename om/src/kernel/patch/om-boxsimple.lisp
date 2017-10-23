;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed; in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;=========================================================================
;;; SIMPLE VALUE BOX
;;;====================================================

(in-package :om)

(defclass OMValueBox (OMBoxCall) ()
  (:metaclass omstandardclass)
  (:default-initargs :icon-pos :noicon))

(defmethod get-object-type-name ((self OMValueBox)) "Simple value")

(defmethod get-properties-list ((self OMValueBox))
  (list '("Appearance" ;;; category
                       (:color "Color" 'color-or-nil color)
                       (:border "Border" :bool border)
                       (:font "Text font" :font text-font) ;;; id text type slot-name
                       (:align "Text align" (:left :center :right) text-align)
                       )
        '("Execution" ;;; category
                      (:lock "Lock state" (nil :locked :eval-once) lock-state) ;;; id text type 
                      (:reactive "Reactive (r)" :bool reactive))
        ))


(defmethod create-box-outputs ((self OMValueBox))
  (list (make-instance 'box-output :box self :name "value")))

(defmethod next-optional-input ((self OMValueBox)) t)

;;; can add only one ?
(defmethod more-optional-input ((self OMValueBox) &key name (value nil val-supplied-p) doc reactive)
  (unless nil ; (inputs self)
    (add-optional-input self :name "in" :value (if val-supplied-p value nil) :doc "set box value" :reactive reactive)))

(defmethod omNG-make-new-boxcall ((reference (eql 'value)) pos &optional init-args)
  (let* ((box (make-instance 'OMValueBox
                            :name "value box"
                            :reference (type-of init-args))))
    ;(print (list "new box" reference))
    (setf (value box) (list init-args)
          (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (inputs box) nil)
    (let ((size (om-max-point (minimum-size box) (default-size box))))
      (setf (box-w box) (om-point-x size)
            (box-h box) (om-point-y size)))
    box))

(defmethod print-value ((self OMValueBox)) 
  (format nil "~s" (car (value self))))

(defmethod default-size ((self OMValueBox))
  (minimum-size self))

(defmethod minimum-size ((self OMValueBox))
  (let ((text-size (om-string-size (print-value self) (text-font self))))
    (om-make-point (max text-size
                        (+ 20 (* (length (inputs self)) 10))
                        32)
                   28)))

(defmethod allow-text-input ((self OMValueBox)) 
  (values (format nil "~s" (car (value self)))
          #'(lambda (box text)
                   (handler-bind ((error #'(lambda (error) (om-beep) (om-abort)))) 
                     ;;(setf (name box) text)
                     (let ((val (read-from-string text)))
                       (set-value box (list (if (quoted-form-p val) (eval val) val)))
                       )))
          ))

(defmethod om-copy ((self OMValueBox)) 
  (let ((newbox (call-next-method)))
    ;;; add the optional/keywords
    (setf (value newbox) (om-copy (value self)))
    newbox))


;;; BOX FRAME

(defclass OMValueBoxFrame (OMBoxFrame) ())

(defmethod get-box-frame-class ((self OMValueBox)) 'OMValueBoxFrame)

(defmethod display-text-and-area ((self OMValueBoxFrame))
  (let ((font (or (text-font (object self)) (om-get-font self))))
    (multiple-value-bind (w h) (om-string-size (print-value (object self)) font)
      (values (print-value (object self)) 3 8 w h))))

(defmethod om-view-doubleclick-handler ((self OMValueBoxFrame) position)
  (or (apply-in-area self 'click-in-area position)
      (multiple-value-bind (edittext action)
          (allow-text-input (object self))
        (let* ((container-view (om-view-container self)))
          (edit-text-in-patch edittext self container-view action (omp 0 0) (omp (w self) (h self)))
          t))))

