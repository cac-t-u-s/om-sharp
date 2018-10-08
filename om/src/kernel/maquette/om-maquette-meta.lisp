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
; File author: J. Bresson, D. Bouche
;============================================================================

;=========================================================================
; MAQUETTE CONTROL PATCH
;=========================================================================

(in-package :om)


;;;==========================
;;; THE CONTROL PATCH
;;;==========================
(defclass OMMaqControlPatch (OMPatchInternal) ())

;;; in principle there is only 1 reference (the maquette)
;(defmethod update-from-editor ((self OMMaqControlPatch))
;  (mapc #'(lambda (ref) (report-modifications (editor ref)))
;        (references-to self)))


(defmethod find-persistant-container ((self OMMaqControlPatch))
  (find-persistant-container (car (references-to self))))

(defmethod set-control-patch ((self OMMaquette) (patch OMPatch))
  (setf (ctrlpatch self) patch)
  (setf (references-to (ctrlpatch self)) (list self)))

(defmethod initialize-instance :after ((self OMMaquette) &rest args)
  ;;;--put this somewhere else ??
  (set-object-autostop self nil)
  ;;;------
  (unless (ctrlpatch self)
    (let* ((patch (make-instance 'OMMaqControlPatch :name "Control Patch"))
           (inbox (omng-make-special-box 'mymaquette (omp 150 12)))
           (comment (omng-make-new-comment "This patch is a general controller for the maquette..." (omp 10 40))))
      (setf (index (reference inbox)) 0
            (defval (reference inbox)) self)
      (omng-add-element patch inbox)
      (omng-resize comment (omp 120 45))
      (omng-add-element patch comment)
      (set-control-patch self patch)
      ))
  self)

(defmethod omng-delete ((self OMMaquette)) 
  (release-reference (ctrlpatch self) self)
  (omng-delete (ctrlpatch self))
  (call-next-method))

(defmethod close-internal-elements ((self OMMaquette))
  (close-internal-elements (ctrlpatch self))
  (call-next-method))

;;; FOR THE META INPUTS
;;; the references-to a control patch is just the maquette
(defmethod maquette-container ((self OMMaqControlPatch)) (car (references-to self)))
;;; if there are severa references (maquetteFile) we assume that the first in the list is the current caller
(defmethod box-container ((self OMMaqControlPatch)) (car (references (car (references-to self)))))


;;;====================================
;;; Maquette accessor for control patch or temporal boxes
;;;====================================

(defclass OMMaqIn (OMIn) ())
(defclass OMMaqInBox (OMInBox) ())
(defmethod io-box-icon-color ((self OMMaqInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod next-optional-input ((self OMMaqInBox)) nil)

(defmethod special-box-p ((name (eql 'mymaquette))) t)
(defmethod get-box-class ((self OMMaqIn)) 'OMMaqInBox)
(defmethod box-symbol ((self OMMaqIn)) 'mymaquette)


(defmethod related-patchbox-slot ((self OMMaqInBox)) nil)
(defmethod allow-text-input ((self OMMaqInBox)) nil)

(defmethod omNG-make-special-box ((reference (eql 'mymaquette)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMMaqIn :name "MAQUETTE")
   pos init-args))

(defmethod register-patch-io ((self OMPatch) (elem OMMaqIn))
  (setf (index elem) 0)
  (setf (defval elem) nil))

;;; check the container: can be a patch, a controlpatch or a maquette
(defmethod maquette-container ((self OMBox)) (maquette-container (container self)))
(defmethod maquette-container ((self OMMaquette)) self)
;;; by convention the first of the references-to is the one that is being evaluated
(defmethod maquette-container ((self OMPatch)) (maquette-container (car (references-to self))))


;;; BOX VALUE
(defmethod omNG-box-value ((self OMMaqInBox) &optional (numout 0)) 
  (set-value self (list (maquette-container self)))
  (return-value self numout))

(defmethod gen-code ((self OMMaqInBox) &optional numout)
  (set-value self (list (maquette-container self)))
  (nth numout (value self)))

(defmethod current-box-value ((self OMMaqInBox) &optional (numout nil))
  (if numout (return-value self numout) (value self)))



#|
;;; note : maybe this is all not useful and I should set the meta just at eval

;;; TRY TO SET THE DEFVAL AS THE CONTAINER MAQUETTE
(defmethod register-patch-io ((self OMMaqControlPatch) (elem OMMaqIn))
  (call-next-method)
  ;;; For OMMaqControlPatch the only references-to is the maquette
  (setf (defval elem) (car (references-to self))))

(defmethod register-patch-io ((self OMPatchInternal) (elem OMMaqIn))
  (call-next-method)
  ;;; For OMPatchInternal the only references-to is the box
  ;;; => just check if it is in a maquette...
  (when (subtypep (type-of (container (car (references-to self)))) 'OMMaquette)
    (setf (defval elem) (container (car (references-to self))))))

(defmethod register-patch-io ((self OMPatchFile) (elem OMMaqIn))
  (call-next-method)
  ;;; For OMPatchFile the only references-to can be multiples !
  ;;; In this case, it will be set only before eval
  (if (and (= 1 (length (references-to self)))
           (subtypep (type-of (container (car (references-to self)))) 'OMMaquette))
      (setf (defval elem) (container (car (references-to self))))))
|#

