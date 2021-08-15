;============================================================================
; om#: visual programming language for computer-assisted music composition
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

;;; META INPUTS:
;;; These boxes nust be updated before evaluation, depending on their call context
;;; The 'meta' inputs DO NOT APPEAR as box inlets outside the patch

;;;====================================
;;; THIS BOX
;;;====================================

(defclass OMSelfIn (OMIn) ()
  (:documentation "Returns the box containing this patch."))

(defclass OMSelfInBox (OMInBox) ())
(defmethod io-box-icon-color ((self OMSelfInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod special-box-p ((name (eql 'thisbox))) t)
(defmethod get-box-class ((self OMSelfIn)) 'OMSelfInBox)
(defmethod box-symbol ((self OMSelfIn)) 'thisbox)
(defmethod special-item-reference-class ((item (eql 'thisbox))) 'OMSelfIn)

(defmethod related-patchbox-slot ((self OMSelfInBox)) nil)
(defmethod allow-text-input ((self OMSelfInBox)) nil)


(defmethod next-optional-input ((self OMSelfInBox)) nil)

(defmethod omNG-make-special-box ((reference (eql 'thisbox)) pos &optional init-args)
  (omNG-make-new-boxcall
   (make-instance 'OMSelfIn :name "THIS BOX")
   pos init-args))

(defmethod register-patch-io ((self OMPatch) (elem OMSelfIn))
  (setf (index elem) 0)
  (setf (defval elem) nil))

;;; doesn't impact other inputs
(defmethod unregister-patch-io ((self OMPatch) (elem OMSelfIn)) nil)


(defmethod box-container ((self OMBox)) (box-container (container self)))

;;; if there are several references (OMPatchFile)
;;; we assume that the first in the list is the current caller
;;; this is set by omng-box-value :before
(defmethod box-container ((self OMPatch)) (car (box-references-to self)))

;;; BOX VALUE
(defmethod omNG-box-value ((self OMSelfInBox) &optional (numout 0))
  (set-value self (list (box-container self)))
  (return-value self numout))

(defmethod gen-code ((self OMSelfInBox) &optional (numout 0))
  (set-value self (list (box-container self)))
  (nth numout (value self)))


;;;====================================
;;; THIS PATCH
;;;====================================

(defclass OMPatchIn (OMIn) ()
  (:documentation "Returns the Patch containing this patch/subpatch."))

(defclass OMPatchInBox (OMInBox) ())
(defmethod io-box-icon-color ((self OMPatchInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod next-optional-input ((self OMPatchInBox)) nil)

(defmethod special-box-p ((name (eql 'thispatch))) t)
(defmethod get-box-class ((self OMPatchIn)) 'OMPatchInBox)
(defmethod box-symbol ((self OMPatchIn)) 'thispatch)
(defmethod special-item-reference-class ((item (eql 'thispatch))) 'OMPatchIn)

(defmethod related-patchbox-slot ((self OMPatchInBox)) nil)
(defmethod allow-text-input ((self OMPatchInBox)) nil)

(defmethod omNG-make-special-box ((reference (eql 'thispatch)) pos &optional init-args)
  (omNG-make-new-boxcall
   (make-instance 'OMPatchIn :name "THIS PATCH")
   pos init-args))

(defmethod register-patch-io ((self OMPatch) (elem OMPatchIn))
  (setf (index elem) 0)
  (setf (defval elem) nil))

(defmethod unregister-patch-io ((self OMPatch) (elem OMPatchIn)) nil)


(defmethod omNG-box-value ((self OMPatchInBox) &optional (numout 0))
  (set-value self (list (container self)))
  (return-value self numout))

(defmethod gen-code ((self OMPatchInBox) &optional (numout 0))
  (set-value self (list (container self)))
  (nth numout (value self)))
