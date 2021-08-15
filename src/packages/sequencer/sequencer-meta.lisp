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
; File author: J. Bresson, D. Bouche
;============================================================================

;=========================================================================
; SEQUENCER CONTROL PATCH
;=========================================================================

(in-package :om)


;;;==========================
;;; THE CONTROL PATCH
;;;==========================
(defclass OMControlPatch (OMPatchInternal) ())

;;; in principle there is only 1 reference (the sequencer)
;(defmethod update-from-editor ((self OMControlPatch))
;  (mapc #'(lambda (ref) (report-modifications (editor ref)))
;        (references-to self)))


(defmethod set-control-patch ((self OMSequencer) (patch OMPatch))
  (change-class patch (find-class 'OMControlPatch))
  (setf (ctrlpatch self) patch)
  (setf (references-to (ctrlpatch self)) (list self)))

;;; not used... (?)
(defmethod sequencer-reference ((self t)) nil)
(defmethod sequencer-reference ((self OMControlPatch))
  (car (references-to self))) ;;; in principle this is the only one !

(defmethod find-persistant-container ((self OMControlPatch))
  (find-persistant-container (car (references-to self))))

(defmethod get-internal-elements ((self OMSequencer))
  (append (call-next-method)
          (get-internal-elements (ctrlpatch self))))


(defparameter *control-patch-help-comment*
  "This patch is a general controller for the sequencer.

Additional inputs/outputs will appear on the sequencer box.
")

(defmethod initialize-instance :after ((self OMSequencer) &rest args)

  ;;; put this somewhere else ??
  ;; the sequencer doesn't auto-stop when its duration is passed
  (set-object-autostop self nil)

  (unless (ctrlpatch self)
    (let* ((patch (make-instance 'OMControlPatch :name "Control Patch"))
           (inbox (omng-make-special-box 'thissequencer (omp 30 12)))
           (outbox (omng-make-special-box 'out (omp 52 140)))
           (connection (omng-make-new-connection (car (outputs inbox)) (car (inputs outbox))))
           ;(comment (omng-make-new-comment *control-patch-help-comment* (omp 60 40)))
           )
      (setf (index (reference inbox)) 0
            (defval (reference inbox)) self)
      (omng-add-element patch inbox)
      (omng-add-element patch outbox)
      (omng-add-element patch connection)
      ;(omng-resize comment (omp 120 120))
      ;(omng-add-element patch comment)
      (set-control-patch self patch)
      ))
  self)



;;; called when some change is made in the sequencer or in the control-patch
(defmethod update-from-reference  ((self OMSequencer))
  (loop for item in (references-to self) do (update-from-reference item)))

(defmethod get-inputs ((self OMSequencer))
  (get-inputs (ctrlpatch self)))

(defmethod get-outputs ((self OMSequencer))
  (get-outputs (ctrlpatch self)))




;;;====================================
;;; Sequencer accessor for control patch or temporal boxes
;;;====================================

(defclass OMSequencerIn (OMIn) ()
  (:documentation "Returns the Sequencer containing this patch/subpatch."))

;; for compatibility / remove me in a little while:)
(defclass OMSequenceIn (OMSequencerIn) ())

(defclass OMSequencerInBox (OMInBox) ())
(defmethod io-box-icon-color ((self OMSequencerInBox)) (om-make-color 0.6 0.2 0.2))

(defmethod next-optional-input ((self OMSequencerInBox)) nil)

(defmethod special-box-p ((name (eql 'thissequencer))) t)
(defmethod get-box-class ((self OMSequencerIn)) 'OMSequencerInBox)
(defmethod box-symbol ((self OMSequencerIn)) 'thissequencer)
(defmethod special-item-reference-class ((item (eql 'thissequencer))) 'OMSequencerIn)

(defmethod related-patchbox-slot ((self OMSequencerInBox)) nil)
(defmethod allow-text-input ((self OMSequencerInBox)) nil)

(defmethod omNG-make-special-box ((reference (eql 'thissequencer)) pos &optional init-args)
  (omNG-make-new-boxcall
   (make-instance 'OMSequencerIn :name "THIS SEQUENCER")
   pos init-args))

(defmethod register-patch-io ((self OMPatch) (elem OMSequencerIn))
  (setf (index elem) 0)
  (setf (defval elem) nil))

(defmethod unregister-patch-io ((self OMPatch) (elem OMSequencerIn)) nil)


;;; FOR THE META INPUTS
;;; if there are several references (OMSequencerFile) we assume that the first in the list is the current caller
(defmethod box-container ((self OMControlPatch))  (car (references-to (car (references-to self)))))

;;; check the container: can be a patch, a controlpatch or a sequencer
(defmethod sequencer-container ((self OMBox)) (sequencer-container (container self)))
;;; the references-to a control patch is just the sequencer
(defmethod sequencer-container ((self OMControlPatch)) (car (references-to self)))
(defmethod sequencer-container ((self OMSequencer)) self)
(defmethod sequencer-container ((self OMPatch)) (sequencer-container (car (box-references-to self))))
(defmethod sequencer-container ((self t)) nil)


;;; BOX VALUE
(defmethod omNG-box-value ((self OMSequencerInBox) &optional (numout 0))
  (set-value self (list (sequencer-container self)))
  (return-value self numout))

(defmethod gen-code ((self OMSequencerInBox) &optional (numout 0))
  (set-value self (list (sequencer-container self)))
  (nth numout (value self)))

(defmethod current-box-value ((self OMSequencerInBox) &optional (numout nil))
  (if numout (return-value self numout) (value self)))



#|
;;; note : maybe this is all not useful and I should set the meta just at eval

;;; TRY TO SET THE DEFVAL AS THE CONTAINER SEQUENCER
(defmethod register-patch-io ((self OMControlPatch) (elem OMSequencerIn))
  (call-next-method)
  ;;; For OMControlPatch the only references-to is the sequencer
  (setf (defval elem) (car (references-to self))))

(defmethod register-patch-io ((self OMPatchInternal) (elem OMSequencerIn))
  (call-next-method)
  ;;; For OMPatchInternal the only references-to is the box
  ;;; => just check if it is in a sequencer...
  (when (subtypep (type-of (container (car (references-to self)))) 'OMSequencer)
    (setf (defval elem) (container (car (references-to self))))))

(defmethod register-patch-io ((self OMPatchFile) (elem OMSequencerIn))
  (call-next-method)
  ;;; For OMPatchFile the only references-to can be multiples !
  ;;; In this case, it will be set only before eval
  (if (and (= 1 (length (references-to self)))
           (subtypep (type-of (container (car (references-to self)))) 'OMSequencer))
      (setf (defval elem) (container (car (references-to self))))))
|#

