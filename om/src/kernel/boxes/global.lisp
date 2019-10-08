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

;;; A simplified version of teh "global variable" system
;;; Global variables are identified by a unique symbol/name (just like in Lisp!) 

(in-package :om)

;;; OMGlobalVar is actually not really used: 
;;; the name of the box is the same and refeers to a Lisp global. 
(defclass OMGlobalVar (OMPatchComponent) ())

(defmethod initialize-instance :after ((self OMGlobalVar) &rest args)
  (when (name self)
    (let ((sym (intern (string-upcase (name self)))))
      (unless (boundp sym)
        (om-print-format "Defining global variable: ~A" (list sym))
        (eval `(defvar ,sym nil))
        ))))


(defmethod set-name ((self OMGlobalVar) new-name)
  (call-next-method)
  (let ((sym (intern (string-upcase (name self)))))
   (unless (boundp sym)
     (om-print-format "Defining global variable: ~A" (list sym))
     (eval (print `(defvar ,sym nil)))
     )))


(defclass OMGlobalBox (OMPatchComponentBox) ())

(defmethod special-box-p ((ref (eql 'global))) t) 
(defmethod get-box-class ((ref OMGlobalVar)) 'OMGlobalBox)
(defmethod box-symbol ((self OMGlobalVar)) 'global)

(defmethod object-box-label ((self OMGlobalVar)) (string+ "VAR " (name self)))

(defmethod box-draw ((self OMGlobalBox) frame)
  (om-with-clip-rect frame 0 0 (- (w frame) 4) (- (h frame) 8)
    (draw-label self (reference self))))


(defmethod create-box-outputs ((self OMGlobalBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "value")))


(defmethod next-optional-input ((self OMGlobalBox)) 
  (zerop (length (inputs self))))

(defmethod more-optional-input ((self OMGlobalBox) &key name value doc reactive)
  (declare (ignore name doc))
  (add-optional-input self :name "value"
                      :value value 
                      :reactive reactive)
  t)

(defmethod allow-rename ((self OMGlobalBox)) t)

(defmethod set-name ((self OMGlobalBox) new-name)
  (call-next-method)
  (set-name (reference self) new-name))

(defmethod allow-text-input ((self OMGlobalBox)) 
  (unless (is-persistant (reference self))
    (values (name self)
            #'(lambda (box text)
                ;;; the box name shall be updated as well
                (set-name box text)
                ))))


(defmethod omNG-make-special-box ((reference (eql 'global)) pos &optional init-args)
  (let* ((name (car (list! init-args)))
         (var (make-instance 'OMGlobalVar :name (if name (string name)))))
    (omNG-make-new-boxcall var pos)))




(defmethod boxcall-value ((self OMGlobalBox)) 
  
  (let ((sym (intern (string-upcase (name self)))))
    
    ;;; set-value of the global variable if the input is connected
    (when (and (car (inputs self)) (connections (car (inputs self))))
      (let ((val (omNG-box-value (car (inputs self)))))
        ;;; actually in principle,  (name (reference self)) = (name self) 
        (eval `(setf ,sym ,val))
        ))
  
    (eval sym)))
    
    
    