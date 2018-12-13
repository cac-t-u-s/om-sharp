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

(in-package :om)

(declaim (optimize (speed 3) (safety 0) (debug 1)))

;;;=========================================================
;;; BUFFER PLAYER (Here using LAS)
;;;=========================================================
(defstruct (buffer-player
            (:conc-name bp-))
  (pointer nil)
  ;(volume-effect nil)
  (buffer nil)
  (size 0)
  (channels 2)
  (sample-rate 44100)
  (state :stop)
  (:documentation ""))

(defun get-bp-buffer (bp) (bp-buffer bp))
(defun get-bp-pointer (bp) (bp-pointer bp))

(defun make-player-from-buffer (buffer size channels sample-rate)
  (let ((bp (make-buffer-player :buffer buffer
                                :size size
                                :channels channels
                                :sample-rate sample-rate))
        (ptr (juce::makeAudioSourceFromBuffer buffer channels size sample-rate)))
    (setf (bp-pointer bp) ptr)
    bp))

(defun free-buffer-player (bp)
  (audio-io::om-free-audio-buffer (bp-buffer bp) (bp-channels bp))
  (juce::freeAudioSource (bp-pointer bp)))


(defmethod set-buffer-player-volume ((self buffer-player) volume)
  (print (list "TODO: set volume" volume)))

(defun make-player-from-file (path &optional (buffered nil))
  (if (not buffered)
      (make-buffer-player :pointer (juce::makeAudioSourceFromFile path))
    (progn
      (multiple-value-bind (buffer format channels sr ss size)
          (audio-io::om-get-audio-buffer path *default-audio-type* nil)
        (make-player-from-buffer buffer size channels sr)))))

(defmethod start-buffer-player ((self buffer-player) &key (start-frame 0))
  (when (eq (bp-state self) :stop)
    (setf (bp-state self) :play)
    (jump-to-frame self start-frame)
    (juce::startAudioSource *juce-player* (bp-pointer self))))

(defmethod pause-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :play)
    (setf (bp-state self) :pause)
    (juce::pauseAudioSource *juce-player* (bp-pointer self))))

(defmethod continue-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :pause)
    (setf (bp-state self) :play)
    (juce::startAudioSource *juce-player* (bp-pointer self))))

(defmethod stop-buffer-player ((self buffer-player))
  (when (not (eq (bp-state self) :stop))
    (setf (bp-state self) :stop)
    (juce::stopAudioSource *juce-player* (bp-pointer self))))

(defmethod jump-to-frame ((self buffer-player) frame)
  (juce::setAudioSourcePos (bp-pointer self) frame))

(defmethod jump-to-time ((self buffer-player) time)
  (juce::setAudioSourcePos (bp-pointer self) (min (max 0 (round (* time (/ (bp-sample-rate self) 1000.0)))) (bp-size self))))

(defmethod get-buffer-player-frame ((self buffer-player))
  (juce::getAudioSourcePos (bp-pointer self)))

;(defmethod restore-buffer-player-pointer ((self buffer-player))
;  ;(print "setpos error: restoring buffer-player")
;  (setf (bp-pointer self) (las::makebuffersound (bp-buffer self) (bp-size self) (bp-channels self))))