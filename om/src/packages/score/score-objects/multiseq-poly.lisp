;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

;;;===================================================
;;; SUPERIMPOSITION OF VOICES 
;;; POLY / MULTI-SEQ + MIXED-TYPES
;;;===================================================

(defclass* multi-seq (score-element collection) 
  ((obj-list :initarg :obj-list :initarg :chord-seqs
             :accessor obj-list :initform nil)))

(defclass* poly  (multi-seq) 
  ((obj-list :initarg :obj-list :initarg :voices
             :accessor obj-list :initform nil)))

(defmethod chord-seqs ((self multi-seq)) (obj-list self))
(defmethod voices ((self poly)) (obj-list self))

(defmethod voice-type ((self multi-seq)) 'chord-seq)
(defmethod voice-type ((self poly)) 'voice)

(defmethod inside ((self multi-seq)) (obj-list self))
(defmethod num-voices ((self multi-seq)) (length (obj-list self)))
(defmethod get-obj-dur ((self multi-seq)) (apply 'max (cons 0 (mapcar 'get-obj-dur (obj-list self)))))

(defmethod initialize-instance :after ((self multi-seq) &rest args)
  (setf (obj-type self) (voice-type self)))

(defmethod objfromobjs ((model multi-seq) (target multi-seq))
  (setf (obj-list target)
        (loop for obj in (obj-list model) collect
              (objfromobjs obj (make-instance (voice-type target)))))
  target)

;;; will also work with voice/poly
(defmethod objfromobjs ((model chord-seq) (target multi-seq))
  (setf (obj-list target)
        (list
         (objfromobjs model (make-instance (voice-type target)))))
  target)

(defmethod initialize-instance ((self multi-seq) &rest args)
  (call-next-method)
  (setf (obj-list self)
        (loop for v in (list! (obj-list self))
              when (or 
                    (subtypep (type-of v) 'chord-seq)
                    (om-beep-msg "Removing voice of type ~A" (type-of v)))
              collect v))
  self)


(defmethod get-voices ((self multi-seq)) (obj-list self))
(defmethod get-voices ((self chord-seq)) (list self))
(defmethod get-voices ((self t)) nil) ;;; e.g. for a chord
                          
;;;===================================================
;;; TIME-SEQUENCE METHODS APPLIED TO POLYPHONIES
;;;===================================================

(defmethod time-sequence-update-obj-dur ((self multi-seq))
  (loop for voice in (obj-list self) do
        (time-sequence-update-obj-dur voice)))

(defmethod time-sequence-reorder-timed-item-list ((self multi-seq))
  (loop for voice in (obj-list self) do
        (time-sequence-reorder-timed-item-list voice)))

(defmethod time-sequence-update-internal-times ((self multi-seq) &optional (interpol-mode :constant-speed) 
                                                (duration 10000) (modif-time nil))
  (loop for voice in (obj-list self) do
        (time-sequence-update-internal-times voice interpol-mode duration modif-time)))
