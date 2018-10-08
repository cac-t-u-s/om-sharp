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
; File author: J. Bresson
;============================================================================

(in-package :om)

;;;======================================
;;; BASIC MIDI NOTES / PIANO ROLL
;;; (just a data-stram with frames = midi-notes)
;;; (om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil))
;;;======================================

(defclass* midi-note (data-frame) 
  ((date :accessor date :initarg :date :initform 0 :documentation "date of the note")
   (pitch :accessor pitch :initarg :pitch :initform 60 :documentation "pitch (MIDI)")
   (vel :accessor vel :initarg :vel :initform 100 :documentation "velocity (0-127)")
   (dur :accessor dur :initarg :dur :initform 500 :documentation "duration(ms)")
   (channel :accessor channel :initarg :channel :initform 1 :documentation "MIDI channel (1-16)")))
           

(defun midinote-onset (midinote) (date midinote))
(defun midinote-pitch (midinote) (pitch midinote))
(defun midinote-vel (midinote) (vel midinote))
(defun midinote-dur (midinote) (dur midinote))
(defun midinote-channel (midinote) (channel midinote))

(defun midinote-end (midinote) (+ (midinote-onset midinote) (midinote-dur midinote)))

;;; redefined from timed-item
(defmethod item-get-duration ((self midi-note)) (midinote-dur self))
(defmethod item-set-duration ((self midi-note) dur) (setf (dur self) dur))

(defun make-midinote (&key (onset 0) (pitch 60) (vel 100) (dur 1000) (channel 1))
  (make-instance 'midi-note :date onset :pitch pitch :vel vel :dur dur :channel channel))

(defmethod data-frame-text-description ((self midi-note))
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


;;;===================================
;;; IMPORT NOTES
;;;===================================
;;; Main function:
(defun import-midi-events (&optional file)
  (multiple-value-bind (evt-lists ntracks unit format)
      (om-midi::midi-import file)
    (when evt-lists
      (midievents-to-milliseconds (sort (flat evt-lists) '< :key 'om-midi::midi-evt-date) unit))
      ))

(defun import-midi-notes (&optional file)
  (midievents-to-midinotes (import-midi-events file)))

(defun midi-key-evt-pitch (evt)
  (car (om-midi:midi-evt-fields evt)))

(defun midi-key-evt-vel (evt)
  (cadr (om-midi:midi-evt-fields evt)))

(defun close-note-on (notelist chan pitch date) 
  (flet ((match (x) (and (equal (midinote-pitch x) pitch) 
                         (equal (midinote-channel x) chan) 
                         (not (plusp (midinote-dur x))) ;;; note is still "open" 
                         ;;; (equal (sixth x) track) (equal (seventh x) port) ;;; not used (yet)
                         )))
    (let ((pos (position-if #'match notelist :from-end t)))
      (if pos
          (setf (dur (nth pos notelist)) (- date (* -1 (midinote-dur (nth pos notelist)))))
        (om-print-format "Warning: this MIDI sequence has orphan KeyOff messages in channel ~D: ~D (t=~Dms)." chan pitch date))
      )))


; OM MIDI tempo
; 1000000 microseconds / beat
; i.e. tempo = 60
(defvar *midi-init-tempo* 1000000)   

;;; from OM6
(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 1000.0 unit/sec) 
               (* (- abstract-time tempo-change-abst-time)
                  (/ cur-tempo *midi-init-tempo*))))))


(defun midievents-to-milliseconds (evtseq units/sec)

  (let ((rep nil)
        (cur-tempo *midi-init-tempo*)
        (tempo-change-abst-time 0)
        (tempo-change-log-time 0) date 
        (initdate (om-midi::midi-evt-date (car evtseq))))
    
    (loop for event in evtseq do
	  
          (if (equal :Tempo (om-midi::midi-evt-type event))
            
              (let ((date (- (om-midi::midi-evt-date event) initdate)))
                (setq tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time units/sec))
                (setq cur-tempo (car (om-midi:midi-evt-fields event)))
                (setq tempo-change-abst-time date))
          
            (let ((date-ms (logical-time (om-midi::midi-evt-date event)  
                                         cur-tempo tempo-change-abst-time 
                                         tempo-change-log-time units/sec)))
            
              (push (om-midi::make-midi-evt :date date-ms
                                        :type (om-midi::midi-evt-type event) 
                                        :chan (om-midi::midi-evt-chan event)
                                        :ref (om-midi::midi-evt-ref event)
                                        :port (om-midi::midi-evt-port event)
                                        :fields (om-midi::midi-evt-fields event))
                    rep)
              )
            )
          )
    
    (reverse rep)))

(defun midievents-to-midinotes (evtseq)

  (let ((notelist nil))
        
    (loop for event in evtseq do
	  
          (case (om-midi::midi-evt-type event)   
            
            (:KeyOn 
             
             (if (= (midi-key-evt-vel event) 0) ;;; actually it's a KeyOff
                 (close-note-on notelist (om-midi::midi-evt-chan event) (midi-key-evt-pitch event) (om-midi::midi-evt-date event))
               
               ;;; put a note on with duration open in the list
               (push (make-midinote :onset (om-midi::midi-evt-date event)
                                    :pitch (midi-key-evt-pitch event) 
                                    :dur (* -1 (om-midi::midi-evt-date event))
                                    :vel (midi-key-evt-vel event) 
                                    :channel (om-midi::midi-evt-chan event))
                     ;(om-midi::midi-evt-ref event)    ;;; not used (yet)
                     ;(om-midi::midi-evt-port event))  ;;; not used (yet)
                     notelist))
             )
            
            (:KeyOff (close-note-on notelist (om-midi::midi-evt-chan event) (midi-key-evt-pitch event) (om-midi::midi-evt-date event)))
            
            )
          )
    
    (when (find-if 'minusp notelist :key 'midinote-dur) 
      (om-print (format nil "Warning: this MIDI sequence has unterminated notes!")))

    (reverse notelist)))



;;;===================================================
;;; PIANO-ROLL IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

(defclass* piano-roll (data-stream)
  ((midi-notes :accessor midi-notes :initarg :midi-notes :initform '() :documentation "a list of midi-note"))
  (:default-initargs :default-frame-type 'midi-note))

(defmethod (setf midi-notes) (notes (self piano-roll))
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
(defclass piano-roll-editor (data-stream-editor) ())

(defmethod get-editor-class ((self piano-roll)) 'piano-roll-editor)

(defmethod frame-display-modes-for-object ((self data-stream-editor) (object piano-roll)) '(:blocks))

(defmethod y-range-for-object ((self piano-roll)) '(36 96))

(defmethod resizable-frame ((self midi-note)) t)

(defmethod get-frame-color ((self midi-note)) (get-midi-channel-color (channel self)))
(defmethod get-frame-posy ((self midi-note)) (pitch self))
(defmethod get-frame-sizey ((self midi-note)) 1)

(defmethod get-frame-area ((frame midi-note) editor)
  (let ((panel (active-panel editor)))
    (values (x-to-pix panel (date frame))
            (- (h panel)
               (y-to-pix panel (+ (get-frame-posy frame ))))
            (max 3 (dx-to-dpix panel (get-frame-graphic-duration frame)))
            (min -3 (dy-to-dpix panel (- (get-frame-sizey frame))))  ;; !! upwards
            )))

(defmethod move-editor-selection ((self piano-roll-editor) &key (dx 0) (dy 0))
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          (setf (pitch frame) 
                (min 95 (max 36
                             (if (equal dy :round) 
                                 (round (pitch frame))
                               (+ (pitch frame) dy))
                             )))
          (when (equal dx :round)
            (item-set-time frame (round (item-get-time frame))))
          ))
  (call-next-method))

(defmethod finalize-data-frame ((frame midi-note) &rest args) 
  (let ((posy (getf args :posy)))
    (when posy 
      (setf (pitch frame) (round posy)))))


;;;==================
;; Keyborad on the left
;;;==================

(defclass keyboard-view (om-view)
  ((pitch-min :accessor pitch-min :initarg :pitch-min :initform 36)
   (pitch-max :accessor pitch-max :initarg :pitch-max :initform 96)))

(defmethod make-left-panel-for-object ((editor data-stream-editor) (object piano-roll))
  (om-make-view 'keyboard-view :size (omp 20 nil)))

;;; the small view at the left of teh timeline should be sized according to the editor's layout
(defmethod make-timeline-left-item ((self piano-roll-editor) id) 
  (om-make-view 'om-view :size (omp 20 15)))

(defun draw-keyboard-octave (i x y w h &optional (alpha 1) (borders nil) (octaves nil))
  (let ((unit (/ h 12))
        (white-h (/ h 7))
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
       (om-def-font :font1 :size 7)
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


(defmethod draw-background ((editor piano-roll-editor) (view stream-panel))
  (when (om-add-key-down)
    (draw-keyboard (- (om-point-x (om-mouse-position view)) 40)
                   0
                   40 (h view)
                   36 96
                   0.1 t nil)
    ))


(defmethod position-display ((self piano-roll-editor) position)
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

(defmethod editor-key-action ((editor piano-roll-editor) key)
  (let* ((panel (active-panel editor))
         (pr (object-value editor)))
    (case key
      (:om-key-up
       (move-editor-selection editor :dy (if (om-shift-key-p) 12 1))
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor))
      (:om-key-down
       (move-editor-selection editor :dy (if (om-shift-key-p) -12 -1))
       (om-invalidate-view panel)
       (report-modifications editor))
      (#\c 
       (when (selection editor)
         (let ((c (select-channel-dialog :default (channel (nth (car (selection editor)) (data-stream-get-frames pr))))))
           (when c 
             (loop for notep in (selection editor) do
                   (let ((note (nth notep (data-stream-get-frames pr))))
                     (setf (channel (nth notep (data-stream-get-frames pr))) c)
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
(defmethod get-action-list-for-play ((object piano-roll) interval &optional parent)
  (sort 
   (mapcan #'(lambda (n)
               (remove nil (list 
                            (if (in-interval (midinote-onset n) interval :exclude-high-bound t) 
                                (list (midinote-onset n)
                                      #'(lambda (note) 
                                          (om-midi::midi-send-evt 
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

;; (defmethod get-cache-display-for-draw ((self piano-roll)) (list 30 100)) 

(defmethod draw-mini-view ((self piano-roll) (box t) x y w h &optional time)
  (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset 0 (get-obj-dur self) w x)
      (multiple-value-bind (fy oy) 
          (conversion-factor-and-offset 36 96 (- h 20) (+ y 10))
        (om-with-line-size 2
          (loop for n in (midi-notes self) do
                (om-with-fg-color (get-midi-channel-color (midinote-channel n))
                  (om-draw-line  (round (+ ox (* fx (midinote-onset n))))
                                 (round (+ (- oy) (- h (* fy (midinote-pitch n)))))
                                 (round (+ ox (* fx (midinote-end n))))
                                 (round(+ (- oy) (-  h (* fy (midinote-pitch n))))))
                  ))))
      t))


;;;======================================
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
          do (decf (car note) t1))
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



(defun gen-midi-notes (n &optional (tmax 10000) (channel 1))
  (loop for i from 0 to (1- n) collect
        (make-midinote :onset (om-random 0 tmax)
                       :pitch (om-random 50 90) 
                       :vel 100
                       :dur (om-random 200 500) 
                       :channel (or channel (om-random 1 16)))))

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





