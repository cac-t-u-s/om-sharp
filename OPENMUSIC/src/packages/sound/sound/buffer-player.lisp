(in-package :om)

(declaim (optimize (speed 3) (safety 0) (debug 1)))

(defvar *default-audio-channels* 2)
(defvar *default-audio-rate* 44100)
(defvar *default-audio-type* :float)
(defvar *default-audio-format* :aiff)
(defvar *default-audio-resolution* 24)

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
        (ptr (juce::makedatareader buffer channels size sample-rate)))
    (setf (bp-pointer bp) ptr)
    bp))

(defun free-buffer-player (bp)
  (om-audio::om-free-sound-buffer (bp-buffer bp) (bp-channels bp))
  (juce::freereader (bp-pointer bp)))


(defmethod set-buffer-player-volume ((self buffer-player) volume)
  (print (list "TODO : set volume" volume)))

(defun make-player-from-file (path &optional (buffered nil))
  (if (not buffered)
      (make-buffer-player :pointer (juce::makefilereader path))
    (progn
      (multiple-value-bind (buffer format channels sr ss size skip)
          (om-audio::om-get-sound-buffer path *default-audio-type* nil)
        (make-player-from-buffer buffer size channels sr)))))

(defmethod start-buffer-player ((self buffer-player) &key (start-frame 0))
  (when (eq (bp-state self) :stop)
    (setf (bp-state self) :play)
    (jump-to-frame self start-frame)
    (juce::startreader *juce-player* (bp-pointer self))))

(defmethod pause-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :play)
    (setf (bp-state self) :pause)
    (juce::pausereader *juce-player* (bp-pointer self))))

(defmethod continue-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :pause)
    (setf (bp-state self) :play)
    (juce::startreader *juce-player* (bp-pointer self))))

(defmethod stop-buffer-player ((self buffer-player))
  (when (not (eq (bp-state self) :stop))
    (setf (bp-state self) :stop)
    (juce::stopreader *juce-player* (bp-pointer self))))

(defmethod jump-to-frame ((self buffer-player) frame)
  (juce::setposreader (bp-pointer self) frame))

(defmethod jump-to-time ((self buffer-player) time)
  (juce::setposreader (bp-pointer self) (min (max 0 (round (* time (/ (bp-sample-rate self) 1000.0)))) (bp-size self))))

(defmethod get-buffer-player-frame ((self buffer-player))
  (juce::getposreader (bp-pointer self)))

;(defmethod restore-buffer-player-pointer ((self buffer-player))
;  ;(print "setpos error: restoring buffer-player")
;  (setf (bp-pointer self) (las::makebuffersound (bp-buffer self) (bp-size self) (bp-channels self))))