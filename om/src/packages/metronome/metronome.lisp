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
; File author: D. Bouche
;============================================================================

;;===========================================================================
;DocFile
;  Metronome
;;===========================================================================
(in-package :om)

;(defconstant *metronome-up-path* (namestring (current-pathname "./Metronome-up.wav")))
;(defconstant *metronome-down-path* (namestring (current-pathname "./Metronome-down.wav")))
;(om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil nil))

(require-om-package "midi")

;;===========================================================================
;;;Metronome
;;===========================================================================
;;;Structure
(defclass metronome (schedulable-object)
  ((editor :initform nil :initarg :editor :accessor editor)
   (time-signature :initform '(4 4) :initarg :time-signature :accessor time-signature)
   (click :initform '(nil nil) :initarg :click :accessor click)))

(defmethod initialize-instance :after ((self metronome) &rest initargs)
  (setf (click self)
        (list (om-midi:make-midi-evt 
                               :type :keyOn
                               :chan 10 :port 0
                               :fields (list 32 127)) ;44
              (om-midi:make-midi-evt 
                               :type :keyOff
                               :chan 10 :port 0
                               :fields (list 32 127)))))

(defmethod get-action-list-for-play ((self metronome) time-interval &optional parent)
  (filter-list (loop for beat in (get-beat-grid (get-tempo-automation (editor self)) (car time-interval) (cadr time-interval))
                     collect
                     (list
                      (car beat)
                      #'(lambda ()
                          (mapcar 'om-midi::midi-send-evt (click self)))))
               (car time-interval) (cadr time-interval)
               :key 'car))

(defmethod get-obj-dur ((self metronome))
  *positive-infinity*)

