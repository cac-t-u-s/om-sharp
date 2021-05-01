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
; File authors: J. Bresson, D. Bouche
;============================================================================

(in-package :om)


;=========================================================================
; METRONOME: An object to use as metronome in play-editor-mixin
;=========================================================================

(defclass metronome (schedulable-object)
  ((editor :initform nil :initarg :editor :accessor editor)
   (tempo :initform 60.0 :initarg :tempo :accessor tempo)
   (time-signature :initform '(4 4) :initarg :time-signature :accessor time-signature)
   (metronome-on :initform nil :initarg :metronome-on :accessor metronome-on)
   (click :initform '(nil nil) :initarg :click :accessor click)))

(defmethod get-obj-dur ((self metronome)) *positive-infinity*)

;;; the editor attached to metronome must return a tempo-automation instance from this method
;;; Note: the metronome reports changes to the editor with update-to-editor
(defmethod editor-get-tempo-automation ((editor t)) nil)


(defmethod get-action-list-for-play ((self metronome) time-interval &optional parent)
  (when (editor-get-tempo-automation (editor self))
    (filter-list (loop for beat in (tempo-automation-get-beat-grid
                                    (editor-get-tempo-automation (editor self))
                                    (car time-interval) (cadr time-interval))
                       collect
                       (list
                        (car beat)
                        #'(lambda ()

                            (om-midi::midi-send-evt
                             (om-midi:make-midi-evt
                              :type :keyOn
                              :chan 10 :port 0
                              :fields (list 32 127)))

                            (om-midi::midi-send-evt
                             (om-midi:make-midi-evt
                              :type :keyOff
                              :chan 10 :port 0
                              :fields (list 32 127)))
                            )))
                 (car time-interval) (cadr time-interval)
                 :key 'car)))


(defmethod metronome-on/off ((self metronome) on)
  (setf (metronome-on self) on)
  (when (editor self)
    (if on
        ;;; activate:
        (if (eq (state (get-obj-to-play (editor self))) :play)
            (player-play-object (player (editor self)) self nil
                                :interval (list (get-obj-time (get-obj-to-play (editor self))) nil)))
      ;;; deactivate
      (player-stop-object (player (editor self)) self))))


(defmethod set-tempo ((self metronome) new-tempo)
  (setf (tempo self) new-tempo)
  (with-schedulable-object
   self
   (when (and (editor self) (get-g-component (editor self) :tempo-box)) ;;; see below...
     (set-value (cadr (om-subviews (get-g-component (editor self) :tempo-box)))
                new-tempo))
   (update-to-editor (editor self) self)))

(defmethod set-time-signature ((self metronome) new-signature)
  (with-schedulable-object self (setf (time-signature self) new-signature)))


;;;=======================================
;;; add controlers in editors:
;;;=======================================

(defmethod player-play-object ((self scheduler) (object metronome) caller &key parent interval)
  (declare (ignore parent interval))
  (when (metronome-on object)
    (call-next-method)))

(defmethod player-continue-object ((self scheduler) (object metronome))
  (when (metronome-on object)
    (call-next-method)))

(defmethod player-stop-object ((self scheduler) (object metronome))
  (call-next-method))
(defmethod player-pause-object ((self scheduler) (object metronome))
  (call-next-method))
