(in-package :om)

;;;======================================
;;; BASIC MIDI PIANO ROLL
;;; (just a data-stram with frames = midi-notes)
;;; (om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil))
;;;======================================

(defclass* midi-note (data-frame) 
  ((date :accessor date :initarg :date :initform 0 :documentation "date of the note")
   (pitch :accessor pitch :initarg :pitch :initform nil :documentation "type of event")
   (vel :accessor vel :initarg :vel :initform 1 :documentation "MIDI channel (1-16)")
   (dur :accessor dur :initarg :dur :initform 0 :documentation "value(s)")
   (channel :accessor channel :initarg :channel :initform 0 :documentation "Target MIDI port")))
           

(defun midinote-onset (midinote) (date midinote))
(defun midinote-pitch (midinote) (pitch midinote))
(defun midinote-vel (midinote) (vel midinote))
(defun midinote-dur (midinote) (dur midinote))
(defun midinote-channel (midinote) (channel midinote))

(defun midinote-end (midinote) (+ (midinote-onset midinote) (midinote-dur midinote)))

;;; redefined from timed-item
(defmethod item-duration ((self midi-note)) (midinote-dur self))

(defun make-midinote (&key (onset 0) (pitch 60) (vel 100) (dur 1000) (channel 1))
  (make-instance 'midi-note :date onset :pitch pitch :vel vel :dur dur :channel channel))

(defmethod get-text-description ((self midi-note))
  (list (format nil "MIDI NOTE (channel ~A)" (channel self))
        (format nil "~A - ~A" (pitch self) (vel self))))

;;; when midi-note is in a data-stream (= not good in principle)
;;; otherwise see get-action-list-for-play (piano-roll)
(defmethod get-frame-action ((note midi-note)) 
  #'(lambda () 
      (om-midi::midi-send-evt 
       (om-midi:make-midi-evt 
        :type :keyOn
        :chan (or (midinote-channel note) 1) :port 0
        :fields (list (midinote-pitch note) (midinote-vel note))))
      (mp:schedule-timer-relative-milliseconds
       (mp::make-timer #'(lambda ()                         (om-midi::midi-send-evt 
                            (om-midi:make-midi-evt 
                             :type :keyOff
                             :chan (or (midinote-channel note) 1) :port 0
                             :fields (list (midinote-pitch note) 0)))))
       (midinote-dur note))))

;;;===================================================
;;; PIANO-ROLL IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

(defclass* piano-roll (data-stream)
  ((midi-notes :accessor midi-notes :initarg :midi-notes :initform '() :documentation "a list of midi-note"))
  (:default-initargs :default-frame-type 'midi-note))

(defmethod (setf midi-notes) (notes (self piano-roll)) (print notes)
  (setf (slot-value self 'midi-notes) (sort notes '< :key 'midinote-onset)))

(defmethod data-stream-frames-slot ((self piano-roll)) 'midi-notes)

;;; midi-notes is just a short-hand to the frame slot of data-stream
;(defmethod midi-notes ((self piano-roll)) (frames self))
;(defmethod (setf midi-notes) (notes (self piano-roll)) (setf (frames self) notes))
;(defmethod set-midi-notes ((self piano-roll) notes) (setf (midi-notes self) notes))

(defmethod initialize-instance ((self piano-roll) &rest initargs)
  (call-next-method)
  (setf (midi-notes self) (sort (slot-value self 'midi-notes) '< :key 'midinote-onset))
  self)

(defmethod initialize-instance :after ((self piano-roll) &rest initargs)
  (setf (autostop self) t) ;;; ??? wtf 
  )

;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self piano-roll)) 1000)
       
;;;======================================
;;; EDITOR
;;;======================================

(defmethod frame-display-modes-for-object ((self stream-editor) (object piano-roll))
  '((:blocks "blocks")))

(defmethod y-range-for-object ((self piano-roll)) '(30 90))

(defparameter +midi-colors+ (loop for i from 1 to 16 collect (om-random-color)))

(defmethod set-frame-attributes ((f midi-note) editor) 
  (setf (getf (attributes f) :color) (nth (channel f) +midi-colors+)
        (getf (attributes f) :posy) (pitch f)
        (getf (attributes f) :sizey) 1
        ))

;;;======================================
;;; PLAY
;;;======================================
(defmethod get-action-list-for-play ((object piano-roll) interval &optional parent)
  (sort 
   (mapcan #'(lambda (n)
               (remove nil (list 
                            (if (in-interval (midinote-onset n) interval :exclude-high-bound t) 
                                (list (midinote-onset n)
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOn
                                                         :chan (or (midinote-channel note) 1) :port 0
                                                         :fields (list (midinote-pitch note) (midinote-vel note)))))
                                      (list n)))

                            (if (in-interval (midinote-end n) interval :exclude-high-bound t)
                                (list (midinote-end n)
                                      #'(lambda (note) (om-midi::midi-send-evt 
                                                        (om-midi:make-midi-evt 
                                                         :type :keyOff
                                                         :chan (or (midinote-channel note) 1) :port 0
                                                         :fields (list (midinote-pitch note) 0))))
                                      (list n)))
                      
                            )))
           
           (remove-if #'(lambda (note) (or (< (midinote-end note) (car interval))
                                           (> (midinote-onset note) (cadr interval))))
                      (midi-notes object)))
   '< :key 'car))


;;; not good
(defmethod player-stop-object ((self scheduler) (object piano-roll))
  (call-next-method)
  (om-midi::midi-stop))

;;;======================================
;;; DRAW
;;;======================================
(defmethod display-modes-for-object ((self piano-roll))
  '(:hidden :text :mini-view))

(defmethod get-cache-display-for-draw ((self piano-roll)) 
  ;(list (loop for c from 1 to 16 collect (om-random-color 0.8))
  ;`((,(om-def-color :green) ,(om-def-color :gray) ,(om-def-color :red))))
`((,(om-make-color (/ 248 256.0) (/ 87 256.0) (/ 95 256.0))
   ,(om-make-color (/ 118 256.0) (/ 169 256.0) (/ 234 256.0))
   ,(om-make-color (/ 135 256.0) (/ 113 256.0) (/ 99 256.0)))))

(defmethod draw-mini-view ((self piano-roll) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset 0 (get-obj-dur self) w x)
      (multiple-value-bind (fy oy) 
          (conversion-factor-and-offset 100 30 (- h 20) (+ y 10))
        (om-with-line-size 8
          (loop for n in (midi-notes self) do
                (om-with-fg-color (nth (midinote-channel n) +midi-colors+)
                  (om-draw-line (+ ox (* fx (midinote-onset n))) (+ oy (* fy (midinote-pitch n)))
                                (+ ox (* fx (midinote-end n))) (+ oy (* fy (midinote-pitch n))))
                  )))))
    t))


;;;======================================
;;; MOVE THESE GUYS SOMEWHERE ELSE ??
;;;======================================

(defmethod add-note ((object piano-roll) note)
  (with-schedulable-object object
                           (if (midi-notes object)
                               (insert-in-order note (midi-notes object) :key 'car :test <)
                             (setf (midi-notes object) (list note))))
  (om-invalidate-view object))

(defmethod prune-object ((self t) t1-ms t2-ms) nil)

(defmethod prune-object ((self piano-roll) t1-ms t2-ms)
  (let* ((t1 (max 0 (or t1-ms 0)))
         (t2 (min (get-obj-dur self) (or t2-ms *positive-infinity*)))
         (new-notes (filter-list (midi-notes self)
                                 (max 0 (or t1-ms 0))
                                 (min (get-obj-dur self) (or t2-ms *positive-infinity*))
                                 :key 'midinote-onset)))
    (loop for note in new-notes
          do
          (decf (car note) t1))
    (setf (midi-notes self) new-notes
          (slice-duration self) (- t2 t1))
    (om-invalidate-view self)))

;; not used (?)
(defmethod tile-notes ((object piano-roll) (notes list) &key (preserve-overlap t))
  (when notes
    (with-schedulable-object (object)
                             (if (midi-notes object)
                                 (let* ((start-date (midinote-onset (car notes)))
                                        (p (position start-date (midi-notes object) :test '<= :key 'car)))
                                   (if p
                                       (if (= p 0)
                                           (setf (midi-notes object) notes)
                                         (setf (nthcdr p (midi-notes object)) notes))
                                     (nconc (midi-notes object) notes)))
                               (setf (midi-notes object) notes)))
    (om-invalidate-view object)))

(defun gentest ()
  (let* ((channel (om-random 1 2))
         (pitchlist (om+ 2 '(50 52 53 55 55 56 58 60))))
    (if (= channel 1)
        (loop for i from 0 to 14 collect
              (make-midinote :onset (abs (+ (* i 333) (om-random -40 40)))
                             :pitch (om+ (nth-random pitchlist) (nth-random '(0 12))) 
                             :vel 100 ; (om-random 45 110) 
                             :dur (if (= channel 1) (om-random 200 500) (om-random 100 200)) 
                             :channel 1))
      (loop for i from 0 to 4 collect
            (make-midinote :onset (abs (+ (* i 1000) (om-random -40 40)))
                           :pitch (nth-random '(52 53))
                           :vel (om-random 45 90)
                           :dur (om-random 800 1000) 
                           :channel 2)))))





