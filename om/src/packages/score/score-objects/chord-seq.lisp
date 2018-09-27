;============================================================================
; o7: visual programming language for computer-aided music composition
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

;;;===================================================
;;; CHORD-SEQ IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

;;; some of the slots :initargs of INTERNAL-CHORD-SEQ are hidden in the graphical interface

(defclass internal-chord-seq (data-stream container score-object)   
  ((Lmidic :initform (list 6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform (list  0 1000) :accessor LOnset :initarg :LOnset :type list :documentation "onsets (ms): list")
   (Ldur :initform (list 1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform (list 100) :accessor Lvel :initarg :Lvel :type list :documentation "velocities (0-127): list or list of lists")
   (Loffset :initform (list 0) :accessor Loffset  :initarg :Loffset :type list :documentation "offsets (ms): list or list of lists")
   (Lchan :initform (list 1) :accessor Lchan  :initarg :Lchan :type list :documentation "MIDI channels (1-16): list or list of lists")
   (Lport :initform (list 0) :accessor Lport  :initarg :Lport :type list :documentation "MIDI ports: list or list of lists")
   (Llegato :initform nil :accessor Llegato  :initarg :Llegato :documentation "relative chords duration (0.0-... or NIL)")
  ))


;;; redefines only visible :initargs
(defclass* chord-seq (internal-chord-seq)   
 
  ((Lmidic :initform (list 6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform (list  0 1000) :accessor LOnset :initarg :LOnset :type list :documentation "onsets (ms): list")
   (Ldur :initform (list 1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform (list 100) :accessor LVel :initarg :LVel :type list :documentation "velocities (0-127): list or list of lists"))
 
  (:documentation "
A sequence of chords.

Time is expressed in absolute dates and durations. (For rhytmic structures see the VOICE object.)

CHORD-SEQ is defined with:

- <lmidic>: a list of list of pitches (midicents: 100 = 1 half-tone - 6000 = C3); e.g. '((6000 6500) (6100 6700 7100)). Each pitch list is considered as a chord in teh sequence. Single-note sequences can be specified with simple list of pitches, e.g. (6000 61000 6800) ; in this case each pitch will be considered as a single-note chord.
- <lonsets>: a list of onsets in milliseconds (one for each chord). If the list is shorter than the list of pitch, the last interval is repeated.
- <ldur>: a list or list of lists values (in milliseconds) expressing chords durations or note durations inside each chord.
- <lvel>: a list or list of lists of values (MIDI velocity from 0 to 127) expressing chords velocities or note durations inside each chord.
- <loffset>: a list or list of lists of values (milliseconds) expressing note offsets for the notes inside each chord.
- <lchan>: a list or list of lists of values (1-16) expressing MIDI channels for each chord or notes inside each chord.
- <lport>: a list or list of lists of values expressing MIDI ports for each chord or notes inside each chord.
- <llegato>: list of numbers > 0.0 (or NIL), indicating the duration of chords as a ratio of inter-onsets time intervals. When applied, the ldur and loffset inputs are ignored, and note durations are set with regard to the legato value.

All values (excepted lonsets and legato) are returned (in the box outputs) as list of lists (one value for each note, missing values computed from previous notes or chords).

Note: the last value of <lonset> is the end of the sequence (end-time of the longest note). 

Internally most of these values are just used to build a list of CHORD objects, accessible with GET-CHORDS.

")
  )


(defmethod additional-class-attributes ((self chord-seq)) '(Loffset Lchan Lport Llegato))


(defmethod data-stream-frames-slot ((self chord-seq)) 'inside)

;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self chord-seq)) 1000)

(defmethod om-init-instance ((self chord-seq) &optional initargs)
  (call-next-method)
  (setf (inside self) (sort (slot-value self 'inside) '< :key 'date))
  self)

(defmethod get-chords ((self chord-seq))
  (om-copy (inside slef))) 


(defmethod do-initialize ((self chord-seq) &key Lmidic Lvel Loffset Ldur Lonset Lchan Lport Llegato)
  
  (let ((defdelay (if (>= (length LOnset) 2)
                      (- (car (last LOnset)) (car (last LOnset 2)))
                    1000)) ;;; the default delay between two chords if not specified otherwise
        (defstart (or (pop LOnset) 0)))
    
    (cond 
     
     ;;; special cases.. 
     ((list-subtypep Lmidic '(chord)) ;;; this is probably a mistake but we can deal with it
      (om-print "chord-seq <lmidic> slot initialized with a list of chords." "Warning")
      (setf (inside self) (om-copy Lmidi))) 
     
     ((list-subtypep LMidic '(note))
      (om-print "chord-seq <lmidic> slot initialized with a list of notes." "Warning")
      (setf (inside self) 
            (mapcar #'(lambda (n) (ObjfromObjs n (make-instance 'chord))) LMidic)))

     (t
      (let ((midics (list! LMidic))
            (vels (list! LVel))
            (durs (list! LDur))
            (offsets (list! LOffset))
            (chans (list! LChan))
            (ports (list! LPort))
            (legatos (list! Llegato)))
        
        (let ((chords (loop while (or midics vels durs offsets ports)
                            for midic = (or (pop midics) midic)
                            for vel = (or (pop vels) vel)
                            for dur = (or (pop durs) dur)
                            for offset = (or (pop offsets) offset)
                            for chan = (or (pop chans) chan)
                            for port = (or (pop ports) port) 
                            collect (make-instance 'chord 
                                                   :Lmidic (list! midic) 
                                                   :Lvel (list! vel)
                                                   :Ldur (list! dur)
                                                   :Loffset (list! offset)
                                                   :Lchan (list! chan)
                                                   :Lport (list! port)
                                  ))))
              
              (loop for chord in chords
                    for onset = defstart then (or (pop Lonset)  (+ onset defdelay))
                    for next-ontset = (or (first Lonset) (+ onset defdelay))
                    for legato = (or (pop legatos) legato) 
                    do (setf (date chord) onset)
                    do (when (and legato (> legato 0))
                         (let ((dur (round (* (- next-ontset onset) legato))))
                           (loop for note in (inside self) 
                                 do (setf (offset note) 0 
                                          (dur note) dur))))
                    )
              
              (time-sequence-set-timed-item-list self chords)
                      
              )))
     )
    self
    ))


(defmethod initialize-instance ((self chord-seq) &rest initargs  &key empty)
  (declare (ignore initargs)) 
  (call-next-method)
  (unless empty
    (do-initialize self 
                   :Lmidic (slot-value self 'Lmidic)
                   :Lvel (slot-value self 'Lvel)  
                   :Ldur (slot-value self 'Ldur) 
                   :Lonset (slot-value self 'Lonset) 
                   :Loffset (slot-value self 'Loffset) 
                   :Lchan (slot-value self 'Lchan) 
                   :Lport (slot-value self 'Lport) 
                   :Llegato (slot-value self 'Llegato) 
                   ))
   (setf (slot-value self 'Lmidic) nil  (slot-value self 'Lvel) nil 
         (slot-value self 'Loffset) nil  (slot-value self 'Ldur) nil
         (slot-value self 'Lonset) nil (slot-value self 'Lchan) nil
         (slot-value self 'Lport) nil (slot-value self 'Llegato) nil)
   self)


;========================
; GET/SET ACCESSORS
;========================

(defmethod Lmidic ((self chord-seq))
  (loop for chord in (inside self)
        collect (Lmidic chord)))
(defmethod Lvel ((self chord-seq))
   (loop for chord in (inside self)
         collect (Lvel chord)))
(defmethod Ldur ((self chord-seq))
   (loop for chord in (inside self)
         collect (Ldur chord)))
(defmethod Loffset ((self chord-seq))
   (loop for chord in (inside self)
         collect (Loffset chord)))
(defmethod Lchan ((self chord-seq))
   (loop for chord in (inside self)
         collect (Lchan chord)))
(defmethod Lport ((self chord-seq))
   (loop for chord in (inside self)
         collect (Lport chord)))

(defmethod Lonset ((self chord-seq))
  (nconc (loop for chord in (inside self)
               collect (item-get-time chord))
         (list (get-obj-dur self))))





(defmethod (setf Lmidic) ((LMidic list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic LMidic
                  :LVel (LVel self)
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)))

(defmethod (setf LVel) ((LVel list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel LVel
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)))

(defmethod (setf LOffset) ((LOffset list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset LOffset
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)))

(defmethod (setf LDur) ((Ldur list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOnset (LOnset self)
                  :LOffset (LOffset self)
                  :LDur lDur
                  :LChan (LChan self)
                  :Legato (legato self)))

(defmethod (setf LOnset) ((LOnset list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset LOnset
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato (legato self)))

(defmethod (setf LChan) ((LChan list) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan LChan
                  :Legato (legato self)))

(defmethod (setf Legato) ((Legato number) (self chord-seq))
  (do-initialize self 
               :LPort (LPort self)
                  :LMidic (LMidic self)
                  :LVel (LVel self)
                  :LOffset (LOffset self)
                  :LOnset (LOnset self)
                  :LDur (LDur self)
                  :LChan (LChan self)
                  :Legato legato))

(defmethod (setf LPort) ((LPort list) (self chord-seq))
  (loop for ports in LPort
        for chord in (inside self)
        do (setf (Lport chord)  ports))
  self)



#|
;;;======================================
;;; PLAY
;;;======================================
(defmethod get-action-list-for-play ((object score) interval &optional parent)
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
                      (sort (midi-notes object) '< :key 'midinote-onset)))
   '< :key 'car))


;;; not good
(defmethod player-stop-object ((self scheduler) (object score))
  (call-next-method)
  (om-midi::midi-stop))
|#
