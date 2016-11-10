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

(defun make-player-from-buffer (buffer size channels sample-rate)
  (let ((bp (make-buffer-player :buffer buffer
                                :size size
                                :channels channels
                                :sample-rate sample-rate))
        (ptr (las::makebuffersound buffer size channels))
        ;(volumewrap (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume\",1,0,2,0.01));" channels) "" ""))
        )
    (setf (bp-pointer bp) (if (> channels *las-player-n-channels*)
                              (las::makeselectsound ptr
                                                    *las-channels-selector*
                                                    *las-player-n-channels*)
                            ptr))
    ;(setf (bp-volume-effect bp) volumewrap)
    ;;;QUICKFIX of the size issue of LAS buffer sound
    (loop while (< (jump-to-frame bp 0) 0)
          do
          (restore-buffer-player-pointer bp))
    ;(setf (bp-pointer bp) (las::makeeffectsound (bp-pointer bp) (bp-volume-effect bp) 0 0))
    bp))

(defun free-buffer-player (bp)
  (om-audio::om-free-sound-buffer (bp-buffer bp) (bp-channels bp)))

(defmethod set-buffer-player-volume ((self buffer-player) volume)
  (las::setcontrolvalueeffect (bp-volume-effect self) 0 (float (min (max volume 0) 2))))

(defun make-player-from-file (path &optional (buffered nil))
  (if (not buffered)
      (let ((ptr (las::makereadsound path)))
        (make-buffer-player :pointer (if (> (las::getchannelssound ptr) *las-player-n-channels*)
                                         (las::makeselectsound ptr
                                                               *las-channels-selector*
                                                               *las-player-n-channels*)
                                       ptr)))
    (progn
      (multiple-value-bind (buffer format channels sr ss size skip)
          (om-audio::om-get-sound-buffer path *default-audio-type* nil)
        (make-player-from-buffer buffer size channels sr)))))

(defmethod start-buffer-player ((self buffer-player) &key (start-frame 0))
  (when (eq (bp-state self) :stop)
    (setf (bp-state self) :play)
    (jump-to-frame self start-frame)
    (las::startsound *las-player* (bp-pointer self) (gen-current-date))))

(defmethod pause-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :play)
    (setf (bp-state self) :pause)
    (las::stopsound *las-player* (bp-pointer self) (gen-current-date))))

(defmethod continue-buffer-player ((self buffer-player))
  (when (eq (bp-state self) :pause)
    (setf (bp-state self) :play)
    (las::startsound *las-player* (bp-pointer self) (gen-current-date))))

(defmethod stop-buffer-player ((self buffer-player))
  (when (not (eq (bp-state self) :stop))
    (setf (bp-state self) :stop)
    (las::stopsound *las-player* (bp-pointer self) (gen-current-date))
    ;(jump-to-time self 0)
    (las::resetsound (bp-pointer self))
    ))

(defmethod jump-to-frame ((self buffer-player) frame)
  (las::setpossound (bp-pointer self) frame))

(defmethod jump-to-time ((self buffer-player) time)
  (las::setpossound (bp-pointer self) (min (max 0 (round (* time (/ (bp-sample-rate self) 1000.0)))) (bp-size self))))

(defmethod get-buffer-player-frame ((self buffer-player))
  (las::getpossound (bp-pointer self)))

(defmethod restore-buffer-player-pointer ((self buffer-player))
  ;(print "setpos error: restoring buffer-player")
  (setf (bp-pointer self) (las::makebuffersound (bp-buffer self) (bp-size self) (bp-channels self))))