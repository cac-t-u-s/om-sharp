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

(in-package :om)

;;;======================================
;;; BASIC MIDI NOTES / PIANO ROLL
;;; (just a data-stram with frames = midi-notes)
;;; (om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil))
;;;======================================

;;; MIDI Note is a special kind of MIDI-Events representing two events (KeyOn/KeyOff)
;;; The MIDI 'values' are also split in two different slots (pitch / vel)
;;; <dur> determines the delay between the two events

(defclass* midi-note (MIDIEvent)
  ((onset :accessor onset :initform 0
          :initarg :onset :initarg :date  ;;; two possible initargs (for compatibility)
          :documentation "date/time of the object")
   ;;; Note-specific slots
   (pitch :accessor pitch :initarg :pitch :initform 60 :documentation "pitch (MIDI)")
   (vel :accessor vel :initarg :vel :initform 100 :documentation "velocity (0-127)")
   (dur :accessor dur :initarg :dur :initform 500 :documentation "duration (ms)")
   ;;; these two slots are repeated from MIDIEvent:
   (ev-chan :accessor ev-chan :initarg :ev-chan :initform 1 :documentation "MIDI channel (1-16)")
   (ev-port :accessor ev-port :initarg :ev-port :initform 0 :documentation "MIDI port (0-...)"))

  (:default-initargs :ev-type :note) ;; <= this is how we differenciate it from a "real" event

  (:documentation "An extension of MIDIEVENT suitable for use in a MIDI-TRACK container.

Internally a MIDI-NOTE is converted into two MIDIEVENTs (KeyOn and KeyOff). MIDI-NOTE manipulates them as a single entity.")
  )


(defun midinote-onset (midinote) (onset midinote))

;;; round pitch: during drag operations, pitch can have float values
;;; (it is rounded only when drag terminates)
(defun midinote-pitch (midinote) (round (pitch midinote)))
(defun midinote-vel (midinote) (vel midinote))
(defun midinote-dur (midinote) (dur midinote))
(defun midinote-channel (midinote) (ev-chan midinote))
(defun midinote-port (midinote) (ev-port midinote))
(defun midinote-track (midinote) (ev-track midinote))
(defun midinote-end (midinote) (+ (midinote-onset midinote) (midinote-dur midinote)))


;;; redefined from timed-item
(defmethod item-get-duration ((self midi-note)) (midinote-dur self))
(defmethod item-set-duration ((self midi-note) dur) (setf (dur self) dur))


(defun make-midinote (&key (onset 0) (pitch 60) (vel 100) (dur 1000) (chan 1) (port 0) (track 0))
  (let ((n (make-instance 'midi-note :onset onset :pitch pitch :vel vel :dur dur :ev-chan chan :ev-port port)))
    (when track (setf (ev-track n) track))
    n))

(defmethod initialize-instance ((self midi-note) &rest args)
  (call-next-method)
  (setf (ev-values self) (list (pitch self) (vel self))))

(defmethod data-frame-text-description ((self midi-note))
  (list (format nil "MIDI NOTE (channel ~A)" (ev-chan self))
        (format nil "~A - ~A" (pitch self) (vel self))))

;;; when midi-note is in a data-track (= not good in principle)
;;; otherwise see get-action-list-for-play (midi-track)
(defmethod get-frame-action ((note midi-note))
  #'(lambda ()
      (om-midi::midi-send-evt
       (om-midi:make-midi-evt
        :type :keyOn
        :chan (or (midinote-channel note) 1) :port 0
        :fields (list (midinote-pitch note) (midinote-vel note))))
      (mp:schedule-timer-relative-milliseconds
       (mp::make-timer #'(lambda ()
                           (om-midi::midi-send-evt
                            (om-midi:make-midi-evt
                             :type :keyOff
                             :chan (or (midinote-channel note) 1)
                             :port (or (midinote-port note) (get-pref-value :midi :out-port))
                             :fields (list (midinote-pitch note) 0)))))
       (midinote-dur note))))


;;; utility (for tests)
(defun gen-random-midi-notes (n &optional (tmax 10000) (channel 1))
  (loop for i from 0 to (1- n) collect
        (make-midinote :onset (om-random 0 tmax)
                       :pitch (om-random 50 90)
                       :vel 100
                       :dur (om-random 200 500)
                       :chan (or channel (om-random 1 16)))))


;;;===================================================
;;; MIDI-TRACK IS JUST A SPECIAL KIND OF DATA-TRACK
;;;===================================================

(defclass* midi-track (data-track)
  ((midi-events :initarg :midi-events :initform '() :documentation "a list of MIDI-NOTEs or MIDIEVENTs"))
  (:default-initargs :default-frame-type 'midi-note)
  (:documentation "A container for MIDI-NOTE and other MIDIEVENT objects.

MIDI-TRACK is a special kind of DATA-TRACK with adapted visualization and editing of MIDI events.

Can import MIDI files by connecting a pathname to the <self> input, or using the keyword :file to open a file chooser."))


(defmethod midi-events ((self midi-track))
  (data-track-get-frames self))

(defmethod initialize-instance ((self midi-track) &rest initargs)
  (call-next-method)
  (data-track-set-frames self (midievents-to-midinotes (slot-value self 'midi-events) :collect-other-events t))
  (setf (slot-value self 'midi-events) nil)
  self)


;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self midi-track)) 1000)


;;;===================================
;;; IMPORT NOTES FROM MIDI
;;;===================================

(defmethod midi-key-evt-pitch ((evt MIDIEvent))
  (car (ev-values evt)))

(defmethod midi-key-evt-vel ((evt MIDIEvent))
  (cadr (ev-values evt)))

(defun close-note-on (notelist chan pitch date)
  (flet ((match (x) (and (equal (midinote-pitch x) pitch)
                         (equal (midinote-channel x) chan)
                         (not (plusp (midinote-dur x))) ;;; note is still "open"
                         ;;; (equal (sixth x) track) (equal (seventh x) port) ;;; not used (yet)
                         )))
    (let ((pos (position-if #'match notelist :from-end t)))
      (if pos
          (setf (dur (nth pos notelist)) (- date (* -1 (midinote-dur (nth pos notelist)))))
        (om-print-format "Warning: this MIDI sequence has orphan KeyOff messages in channel ~D: ~D (t=~Dms)." (list chan pitch date)))
      )))


(defun midievents-to-midinotes (evtlist &key collect-other-events)

  (let ((notelist nil)
        (other-events nil))

    (loop for event in (sort evtlist #'< :key #'onset) do

          (case (ev-type event)

            (:KeyOn

             (if (= (midi-key-evt-vel event) 0) ;;; actually it's a KeyOff

                 (close-note-on notelist (ev-chan event) (midi-key-evt-pitch event) (onset event))

               ;;; put a note on with duration open in the list
               (push (make-midinote :onset (onset event)
                                    :pitch (midi-key-evt-pitch event)
                                    :dur (* -1 (onset event))
                                    :vel (midi-key-evt-vel event)
                                    :chan (ev-chan event)
                                    :port (ev-port event)
                                    :track (ev-track event)
                                    )
                     ; (ev-track event)    ;;; not used
                     notelist))
             )

            (:KeyOff (close-note-on notelist
                                    (ev-chan event)
                                    (midi-key-evt-pitch event)
                                    (onset event)))

            (otherwise
             (when collect-other-events
               (push (om-copy event) other-events)
               ))
            )
          )

    (when (find-if 'minusp notelist :key 'midinote-dur)
      (om-print (format nil "Warning: this MIDI sequence has unterminated notes!")))

    (sort (append (reverse notelist) (reverse other-events)) #'< :key #'onset)
    ))


(defun import-midi-notes (&optional file)
  (midievents-to-midinotes
   (get-midievents
    (import-midi-events file))  ;; #'(lambda (evt) (test-midi-type evt '(:keyon :keyoff))))
   :collect-other-events t))


(defmethod objFromObjs ((model pathname) (target midi-track))
  (data-track-set-frames target (import-midi-notes model))
  target)


;;; :choose-file ?
(defmethod box-def-self-in ((self (eql 'midi-track))) nil)

(defmethod objFromObjs ((model (eql (or :file :choose-file))) (target midi-track))
  (let ((file (om-choose-file-dialog :prompt "Choose a MIDI file..."
                                     :types '("MIDI files" "*.mid;*.midi"))))
    (if file
        (objFromObjs file target)
      (abort-eval))))


;;; DATA-TRACK=>MIDI-TRACK
;;; Converts KeyOn/KeyOff events to MIDI-NOTEs

(defmethod objfromobjs ((model data-track) (target midi-track))
  (data-track-set-frames
   target
   (midievents-to-midinotes (frames model) :collect-other-events t))
  target)


;;; Drop a MIDI file in patch
(pushr '(:midi ("mid" "midi") "MIDI files") *doctypes*)

(defmethod omNG-make-new-box-from-file ((type (eql :midi)) file pos)
  (let* ((miditrack (objfromobjs file (make-instance 'midi-track)))
         (box (make-new-box-with-instance (om-init-instance miditrack) pos)))
    (set-lock-state box :locked)
    box))


;;;===================================
;;; TO MIDIEVENT
;;;===================================

(defmethod* get-midievents ((self midi-track) &optional test)
  (let ((evtlist
         (sort
          (remove nil
                  (loop for n in (midi-events self) append

                        (if (equal (ev-type n) :note)

                            (list  (make-MIDIEvent
                                    :ev-date (midinote-onset n)
                                    :ev-type :keyon
                                    :ev-chan (midinote-channel n)
                                    :ev-values (list (midinote-pitch n) (midinote-vel n))
                                    :ev-port (midinote-port n)
                                    :ev-track (midinote-track n))
                                   (make-MIDIEvent
                                    :ev-date (midinote-end n)
                                    :ev-type :keyoff
                                    :ev-chan (midinote-channel n)
                                    :ev-values (list (midinote-pitch n) 0)
                                    :ev-port (midinote-port n)
                                    :ev-track (midinote-track n)))
                          ;;; normal event
                          (list (om-copy n))))
                  )
          #'< :key #'onset)))
    (if test
        (get-midievents evtlist test)
      evtlist)))


;;;======================================
;;; EDITOR
;;;======================================
(defclass midi-track-editor (data-track-editor)
  ((recording-notes :accessor recording-notes :initform nil)))

(defmethod get-editor-class ((self midi-track)) 'midi-track-editor)

(defmethod frame-display-modes-for-object ((self data-track-editor) (object midi-track)) '(:blocks))

(defparameter *MIN-MIDI-KEY* 36)
(defparameter *MAX-MIDI-KEY* 96)

(defmethod object-default-edition-params ((self midi-track))
  `((:display-mode :blocks)
    (:grid t)
    (:x1 0) (:x2 nil)
    (:y1 ,*MIN-MIDI-KEY*) (:y2 ,*MAX-MIDI-KEY*)))


(defmethod resizable-frame ((self midi-note)) t)
(defmethod resizable-frame ((self midievent)) nil)

(defmethod get-frame-color ((self midievent))
  (if (ev-chan self)
      (get-midi-channel-color (ev-chan self))
    (om-make-color 0 0 0)))

(defmethod get-frame-color ((self midi-note)) (get-midi-channel-color (ev-chan self)))


(defmethod get-frame-posy ((self midievent)) *MAX-MIDI-KEY* - 4) ;;; MIDIevents are just displayed at the top of the roll
(defmethod get-frame-posy ((self midi-note)) (pitch self))

(defmethod get-frame-sizey ((self midi-note)) 1)
(defmethod get-frame-sizey ((self midievent)) 4)

(defmethod get-frame-area ((frame midievent) editor)
  (let ((panel (active-panel editor)))
    (values (x-to-pix panel (date frame))
            (y-to-pix panel (get-frame-posy frame))
            (max 3 (dx-to-dpix panel (get-frame-graphic-duration frame)))
            (min -3 (dy-to-dpix panel (get-frame-sizey frame)))  ;; !! upwards
            )))


(defmethod move-editor-selection ((self midi-track-editor) &key (dx 0) (dy 0))

  (let* ((midi-track (object-value self))
         (frames (loop for fp in (selection self)
                       collect (nth fp (data-track-get-frames midi-track)))))

    (unless (equal (editor-play-state self) :stop)
      (close-open-midinotes-at-time frames (get-obj-time midi-track)))

    (loop for frame in frames do
          (when (equal (ev-type frame) :note)
            (setf (pitch frame)
                  (min (1- *MAX-MIDI-KEY*)
                       (max *MIN-MIDI-KEY*
                            (if (equal dy :round)
                                (round (pitch frame))
                              (+ (pitch frame) dy)))))

            (when (equal dx :round)
              (item-set-time frame (round (item-get-time frame))))
            ))

    (call-next-method)))


(defmethod resize-editor-selection ((self midi-track-editor) &key (dx 0) (dy 0))
  (declare (ignore dy))

  (unless (or (equal (editor-play-state self) :stop) (zerop dx))
    (let* ((midi-track (object-value self))
           (frames (loop for fp in (selection self)
                         collect (nth fp (data-track-get-frames midi-track)))))
      (close-open-midinotes-at-time frames (get-obj-time midi-track))))

  (call-next-method))


(defmethod delete-editor-selection ((self midi-track-editor))

  (unless (equal (editor-play-state self) :stop)
    (let* ((midi-track (object-value self))
           (frames (loop for fp in (selection self)
                         collect (nth fp (data-track-get-frames midi-track)))))
      (close-open-midinotes-at-time frames (get-obj-time midi-track))))

  (call-next-method))


(defmethod finalize-data-frame ((frame midi-note) &rest args)
  (when (equal (ev-type frame) :note)
    (let ((posy (getf args :posy)))
      (when posy
        (setf (pitch frame) (round posy))))))


;;;==================
;; Keyborad on the left
;;;==================

(defclass keyboard-view (om-view)
  ((pitch-min :accessor pitch-min :initarg :pitch-min :initform 36)
   (pitch-max :accessor pitch-max :initarg :pitch-max :initform 96)))

(defmethod make-left-panel-for-object ((editor data-track-editor) (object midi-track) view)
  (declare (ignore view))
  (om-make-view 'keyboard-view :size (omp 20 nil)))

;;; the small view at the left of teh timeline should be sized according to the editor's layout
(defmethod make-timeline-left-item ((self midi-track-editor) id)
  (om-make-view 'om-view :size (omp 20 15)))

(defun draw-keyboard-octave (i x y w h &optional (alpha 1) (borders nil) (octaves nil))
  (let ((unit (/ h 12))
        (blackpos '(1 3 6 8 10))
        (whitepos '(0 1.5 3.5 5 6.5 8.5 10.5 12)))
    (loop for wk on whitepos when (cadr wk) do
          (let* ((y1 (- y (* (car wk) unit)))
                 (y2 (- y (* (cadr wk) unit))))
            (om-draw-rect x y1 w (- y2 y1) :fill t :color (om-make-color 1 1 1 alpha))
            (when borders (om-draw-rect x y1 w (- y2 y1) :fill nil :color (om-make-color 0 0 0 alpha)))
            ))
    (om-with-fg-color (om-make-color 0.2 0.2 0.2)
      (loop for bp in blackpos do
            (om-draw-rect x (- y (* unit bp)) (/ w 1.8) (- unit) :fill t :color (om-make-color 0 0 0 alpha)))
      (when octaves
        (om-with-font
         (om-def-font :tiny)
         (om-draw-string (+ x (/ w 2))  (- y 2) (format nil "C~D" i))))
      )))

(defun draw-keyboard (x y w h pitch-min pitch-max &optional (alpha 1) borders octaves)
  (let* ((n-oct (round (- pitch-max pitch-min) 12))
         (oct-h (/ h n-oct)))
    (loop for i from y to (1- n-oct) do
          (draw-keyboard-octave (1+ i) x (- h (* i oct-h)) w oct-h alpha borders octaves)
          )
    ))

(defmethod om-draw-contents ((self keyboard-view))
  (draw-keyboard 0 0 (om-width self) (om-height self)
                 (pitch-min self) (pitch-max self)
                 1 nil t))


(defmethod draw-background ((editor midi-track-editor) (view data-track-panel))
  (when (om-add-key-down)
    (draw-keyboard (- (om-point-x (om-mouse-position view)) 40)
                   0
                   40 (h view)
                   *MIN-MIDI-KEY* *MAX-MIDI-KEY*
                   0.1 t nil)
    ))


(defmethod position-display ((self midi-track-editor) position)
  (when (om-add-key-down)
    (om-invalidate-view (active-panel self)))
  (call-next-method))


(defun select-channel-dialog (&key (default 1))
  (let ((win (om-make-window 'om-dialog :title "MIDI channel"))
        (list (om-make-di 'om-popup-list :size (omp 80 30)
                          :items '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
                          :value default)))
    (om-add-subviews
     win
     (om-make-layout
      'om-column-layout :align :right
      :subviews
      (list (om-make-di 'om-simple-text :text "Select a MIDI channel for the note selection" :size (omp 260 20))
            list
            (om-make-layout
             'om-row-layout
             :subviews (list
                        (om-make-di 'om-button :text "Cancel" :size (omp 80 30)
                                    :di-action #'(lambda (b) (declare (ignore b))
                                                   (om-return-from-modal-dialog win nil)))
                        (om-make-di 'om-button :text "OK" :size (omp 80 30)
                                    :di-action #'(lambda (b) (declare (ignore b))
                                                   (om-return-from-modal-dialog
                                                    win
                                                    (om-get-selected-item list)))))
             ))))
    (om-modal-dialog win)
    ))

(defmethod editor-key-action ((editor midi-track-editor) key)
  (let* ((panel (active-panel editor))
         (pr (object-value editor)))
    (case key
      (:om-key-up
       (store-current-state-for-undo editor)
       (move-editor-selection editor :dy (if (om-shift-key-p) 12 1))
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor))
      (:om-key-down
       (store-current-state-for-undo editor)
       (move-editor-selection editor :dy (if (om-shift-key-p) -12 -1))
       (om-invalidate-view panel)
       (report-modifications editor))
      (#\c
       (when (selection editor)
         (let ((c (select-channel-dialog :default (chan (nth (car (selection editor)) (data-track-get-frames pr))))))
           (when c
             (loop for notep in (selection editor) do
                   (let ((note (nth notep (data-track-get-frames pr))))
                     (setf (chan note) c)
                     )))
           ))
       (om-invalidate-view panel)
       (report-modifications editor))
      (otherwise
       (call-next-method))
      )))


;;;======================================
;;; PLAY
;;;======================================

(defmethod get-action-list-for-play ((object midi-track) interval &optional parent)
  (sort
   (loop for evt in (midi-events object)
         when (or (in-interval (onset evt) interval :exclude-high-bound t)
                  (in-interval (+ (onset evt) (get-obj-dur evt)) interval :exclude-high-bound t))
         append
         (case (ev-type evt)

           (:note

            (remove nil

                    (list

                     (when (in-interval (midinote-onset evt) interval :exclude-high-bound t)

                       (list (midinote-onset evt)
                             #'(lambda (note)
                                 (om-midi::midi-send-evt
                                  (om-midi:make-midi-evt
                                   :type :keyOn
                                   :chan (or (midinote-channel note) 1)
                                   :port (or (midinote-port note) (get-pref-value :midi :out-port))
                                   :fields (list (midinote-pitch note) (midinote-vel note)))))
                             (list evt)))

                     (when (in-interval (midinote-end evt) interval :exclude-high-bound t)

                       (list (midinote-end evt)
                             #'(lambda (note)
                                 (om-midi::midi-send-evt
                                  (om-midi:make-midi-evt
                                   :type :keyOff
                                   :chan (or (midinote-channel note) 1)
                                   :port (or (midinote-port note) (get-pref-value :midi :out-port))
                                   :fields (list (midinote-pitch note) 0))))
                             (list evt)))

                     )))

           (:tempo nil)

           (otherwise
            (list
             (list (onset evt)
                   #'(lambda (e) (send-midievent e))
                   (list evt)))
            )))
   '< :key 'car)
  )


(defun close-open-midinotes-at-time (notes time)
  (loop for note in notes
        do
        (when (or (zerop time)
                  (and (<= (midinote-onset note) time)
                       (>= (midinote-end note) time)))
          (om-midi::midi-send-evt
           (om-midi:make-midi-evt
            :type :keyOff
            :chan (or (midinote-channel note) 1)
            :port (or (midinote-port note) (get-pref-value :midi :out-port))
            :fields (list (midinote-pitch note) 0)))
          )))


(defmethod send-current-midi-key-offs ((self midi-track))
  (close-open-midinotes-at-time
   (remove-if-not
    #'(lambda (n) (typep n 'midi-note))
    (data-track-get-frames self))
   (get-obj-time self)))


(defmethod player-stop-object ((self scheduler) (object midi-track))
  (send-current-midi-key-offs object)
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object midi-track))
  (send-current-midi-key-offs object)
  (call-next-method))

(defmethod player-loop-object ((self scheduler) (object midi-track))
  (send-current-midi-key-offs object)
  (call-next-method))

(defmethod set-object-current-time ((self midi-track) time)
  (declare (ignore time))
  (send-current-midi-key-offs self)
  (call-next-method))


;;;======================================
;;; RECORD
;;;======================================

(defmethod can-record ((self midi-track-editor)) t)


(defmethod close-recording-notes ((self midi-track-editor))

  (let ((obj (get-obj-to-play self)))

    (when (recording-notes self)

      (let ((time-ms (player-get-object-time (player self) obj))
            (max-time (or (cadr (play-interval self)) (get-obj-dur obj))))

        (maphash
         #'(lambda (pitch note)
             (declare (ignore pitch))
             (setf (dur note)
                   (- (if (> time-ms (midinote-onset note)) time-ms max-time)
                      (midinote-onset note))))
         (recording-notes self))

        (clrhash (recording-notes self))))

    (time-sequence-update-obj-dur obj)
    ))


(defmethod editor-record-on ((self midi-track-editor))

  (let ((midi-track (get-obj-to-play self))
        (in-port (get-pref-value :midi :in-port)))

    (setf (recording-notes self) (make-hash-table))

    (setf (record-process self)
          (om-midi::portmidi-in-start
           in-port

           #'(lambda (message time)
               (declare (ignore time))

               (when (equal :play (editor-play-state self))

                 (let ((time-ms (player-get-object-time (player self) midi-track))
                       (max-time (or (cadr (play-interval self)) (get-obj-dur midi-track)))
                       (pitch (car (om-midi:midi-evt-fields message))))

                   (case (om-midi::midi-evt-type message)

                     (:KeyOn
                      (let ((note (make-midinote :onset time-ms
                                                 :dur 100
                                                 :pitch (car (om-midi:midi-evt-fields message))
                                                 :vel (cadr (om-midi:midi-evt-fields message))
                                                 :chan (om-midi:midi-evt-chan message)
                                                 :port (om-midi:midi-evt-port message)
                                                 :track (om-midi:midi-evt-ref message)
                                                 )))

                        (setf (gethash pitch (recording-notes self)) note)
                        (time-sequence-insert-timed-item-and-update midi-track note)

                        (report-modifications self)
                        (update-timeline-editor self)
                        (editor-invalidate-views self)
                        ))

                     (:KeyOff
                      (let ((note (gethash pitch (recording-notes self))))
                        (when note
                          (setf (dur note)
                                (- (if (> time-ms (midinote-onset note)) time-ms max-time)
                                   (midinote-onset note)))
                          (remhash pitch (recording-notes self)))
                        (editor-invalidate-views self)))

                     (otherwise nil)))))

           1
           (and (get-pref-value :midi :thru)
                (get-pref-value :midi :thru-port))
           ))

    (push self *running-midi-recorders*)

    (om-print-format "Start recording in ~A (port ~D)"
                     (list (or (name (object self)) (type-of (get-obj-to-play self))) in-port)
                     "MIDI")
    ))


(defmethod editor-record-off ((self midi-track-editor))

  (when (record-process self)

    (om-print-format "Stop recording in ~A"
                     (list (or (name (object self)) (type-of (get-obj-to-play self))))
                     "MIDI")

    (om-midi::portmidi-in-stop (record-process self)))

  (close-recording-notes self)

  (setf *running-midi-recorders* (remove self *running-midi-recorders*)))


(defmethod editor-stop ((self midi-track-editor))
  (close-recording-notes self)
  (call-next-method))

(defmethod editor-pause ((self midi-track-editor))
  (close-recording-notes self)
  (call-next-method))


;;;======================================
;;; DRAW
;;;======================================

(defmethod display-modes-for-object ((self midi-track))
  '(:mini-view :text :hidden))

;; (defmethod get-cache-display-for-draw ((self midi-track) box) (list 30 100))

(defmethod draw-mini-view ((self midi-track) (box t) x y w h &optional time)

  (multiple-value-bind (fx ox)
      (conversion-factor-and-offset 0 (get-obj-dur (get-box-value box)) w x)
    (multiple-value-bind (fy oy)
        (conversion-factor-and-offset 96 36 (- h 20) (+ y 10))
      (om-with-line-size 2
        (loop for evt in (midi-events self) do
              (if (subtypep (type-of evt) 'midi-note)
                  (om-with-fg-color (get-midi-channel-color (midinote-channel evt))
                    (om-draw-line (round (+ ox (* fx (midinote-onset evt))))
                                  (round (+ oy (* fy (midinote-pitch evt))))
                                  (round (+ ox (* fx (midinote-end evt))))
                                  (round(+ oy (* fy (midinote-pitch evt)))))
                    )
                (om-with-fg-color (or (get-midi-channel-color (ev-chan evt))
                                      (om-make-color 0 0 0))
                  (om-draw-line (round (+ ox (* fx (onset evt))))
                                y
                                (round (+ ox (* fx (onset evt))))
                                (+ y 10))
                  )
                )
              ))
      t)))
