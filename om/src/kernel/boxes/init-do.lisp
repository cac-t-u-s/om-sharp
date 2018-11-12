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

;;;====================
;;; INIT-DO 
;;;====================
;;; A general box that evaluates prior to the execution of the patch
;;; Much of its behaviour is similar to the "OUT" boxes, except that 
;;; it is not returning anything out
;;; it is also useful to use along with iterator to do something fbefore to start a loop 

(defclass OMPatchInit (OMPatchComponent) ())
(defclass OMPatchInitBox (OMPatchComponentBox) ())

(defmethod special-box-p ((name (eql 'init-do))) t)
(defmethod get-box-class ((self OMPatchInit)) 'OMPatchInitBox)
(defmethod box-symbol ((self OMPatchInit)) 'init-do)


(defmethod get-icon-id ((self OMPatchInitBox)) :m-play)

(defmethod object-name-in-inspector ((self OMPatchInitBox)) "init call box")

(defmethod omNG-make-special-box ((reference (eql 'init-do)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMPatchInit :name (if name (string name) "init-do"))
     pos init-args)))

(defmethod create-box-inputs ((self OMPatchInitBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "action")))

(defmethod get-input-doc ((self OMPatchInitBox) name) "to do before to evaluate patch outputs")

(defmethod next-optional-input ((self OMPatchInitBox)) t)

(defmethod more-optional-input ((self OMPatchInitBox) &key name (value nil val-supplied-p) doc reactive)
  (add-optional-input self :name "action to do before to evaluate patch outputs" 
                      :value (if val-supplied-p value nil) 
                      :reactive reactive)
  t)