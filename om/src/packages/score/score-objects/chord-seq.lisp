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

;;;===================================================
;;; CHORD-SEQ IS JUST A SPECIAL KIND OF DATA-STREAM
;;;===================================================

;;; some of the slots :initargs of INTERNAL-CHORD-SEQ are hidden in the graphical interface

;;; (almost) all slot accessors are redefined below in this file

(defclass internal-chord-seq (score-object data-stream)   
  ((Lmidic :initform '((6000)) :initarg :Lmidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform '(0 1000) :initarg :Lonset :type list :documentation "onsets (ms): list")
   (Ldur :initform '((1000)) :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform 100 :initarg :Lvel :type list :documentation "velocities (0-127): list or list of lists")
   (Loffset :initform 0 :initarg :Loffset :type list :documentation "offsets (ms): list or list of lists")
   (Lchan :initform 1 :initarg :Lchan :type list :documentation "MIDI channels (1-16): list or list of lists")
   (Lport :initform nil :initarg :Lport :type list :documentation "MIDI ports: list or list of lists")
   (Llegato :accessor Llegato :initform nil :initarg :Llegato :documentation "relative chords duration (0.0-... or NIL)")   ;;; this one has no redefined accessor (do it?)
   )
  (:default-initargs :default-frame-type 'chord))


;;; redefines only visible :initargs
(defclass* chord-seq (internal-chord-seq)   
 
  ((Lmidic :initform '((6000)) :initarg :LMidic :type list :documentation "pitches (mc): list or list of lists")
   (Lonset :initform '(0 1000) :initarg :LOnset :type list :documentation "onsets (ms): list")
   (Ldur :initform '((1000)) :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (Lvel :initform 100 :initarg :LVel :type list :documentation "velocities (0-127): list or list of lists"))
 
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

"))

(defmethod chords ((self chord-seq)) (time-sequence-get-timed-item-list self))

(defmethod inside ((self chord-seq)) (chords self))


(defmethod additional-class-attributes ((self chord-seq)) '(Loffset Lchan Lport Llegato))

;; (defmethod data-stream-frames-slot ((self chord-seq)) 'inside)

;;; redefined from time-sequence
(defmethod time-sequence-default-duration ((self chord-seq)) 1000)

(defmethod get-chords ((self chord-seq))
  (om-copy (frames self))) 


(defmethod do-initialize ((self chord-seq) &key Lmidic Lvel Loffset Ldur Lonset Lchan Lport Llegato)
  
  (let ((defdelay (if (>= (length Lonset) 2)
                      (- (car (last Lonset)) (car (last Lonset 2)))
                    1000)) ;;; the default delay between two chords if not specified otherwise
        (defstart (or (pop Lonset) 0)))
    
    (cond 
     
     ;;; special cases.. 
     ((list-subtypep Lmidic '(chord)) ;;; this is probably a mistake but we can deal with it
      (om-print "chord-seq <lmidic> slot initialized with a list of chords." "Warning")
      (time-sequence-set-timed-item-list self (om-copy Lmidic))) 
     
     ((list-subtypep LMidic '(note))
      (om-print "chord-seq <lmidic> slot initialized with a list of notes." "Warning")
      (time-sequence-set-timed-item-list self 
                                         (mapcar #'(lambda (n) (ObjfromObjs n (make-instance 'chord))) Lmidic)))

     (t
      (let ((midics (list! Lmidic))
            (vels (list! Lvel))
            (durs (list! Ldur))
            (offsets (list! Loffset))
            (chans (list! Lchan))
            (ports (list! Lport))
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
                       (loop for note in (notes chord) 
                             do (setf (offset note) 0 
                                      (dur note) dur))))
                )
              
              (time-sequence-set-timed-item-list self chords)
              
              )))
     )
    self
    ))


(defmethod initialize-instance ((self chord-seq) &rest initargs)
  
  (call-next-method)
  
  (when initargs
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
  (loop for chord in (chords self)
        collect (Lmidic chord)))

(defmethod Lvel ((self chord-seq))
   (loop for chord in (chords self)
         collect (Lvel chord)))

(defmethod Ldur ((self chord-seq))
   (loop for chord in (chords self)
         collect (Ldur chord)))

(defmethod Loffset ((self chord-seq))
   (loop for chord in (chords self)
         collect (Loffset chord)))

(defmethod Lchan ((self chord-seq))
   (loop for chord in (chords self)
         collect (Lchan chord)))

(defmethod Lport ((self chord-seq))
   (loop for chord in (chords self)
         collect (Lport chord)))

(defmethod Lonset ((self chord-seq))
  (nconc (loop for chord in (chords self)
               collect (item-get-time chord))
         (list (get-obj-dur self))))



(defmethod (setf Lmidic) ((Lmidic list) (self chord-seq))
  (do-initialize self 
                 :Lmidic Lmidic
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Lvel) ((Lvel list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel Lvel
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Loffset) ((Loffset list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset Loffset
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Ldur) ((Ldur list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur ldur
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Lonset) ((Lonset list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset Lonset
                 ))

(defmethod (setf Lchan) ((Lchan list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan Lchan
                 :Lport (Lport self)
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

(defmethod (setf Llegato) ((Llegato list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)
                 :Llegato Llegato
                 :Lonset (Lonset self)
                 ))

(defmethod (setf LPort) ((LPort list) (self chord-seq))
  (do-initialize self 
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Ldur (Ldur self)
                 :Loffset (Loffset self)
                 :Lchan (Lchan self)
                 :Lport Lport
                 :Llegato (Llegato self)
                 :Lonset (Lonset self)
                 ))

;;;============ 
;;; BOX
;;;============



(defmethod score-object-mini-view ((self chord-seq) x-u y-u w h staff fontsize)
  
  (when (chords self)
    
    (let* ((unit (font-size-to-unit fontsize))
           (shift-x-u 7) ;;  
           (w-u (- (/ w unit) shift-x-u 2));; +1 for margin each side 
           (x-ratio (/ w-u (get-obj-dur self))))
    
      (loop for chord in (chords self) do
            (draw-chord (notes chord) 
                        (+ shift-x-u (* (date chord) x-ratio)) 
                        y-u 
                        w h fontsize :scale nil :staff staff)
            ))))




;;;======================================
;;; PLAY
;;;======================================
(defmethod get-action-list-for-play ((object chord-seq) interval &optional parent)
  (sort 
   (loop for c in (remove-if #'(lambda (chord) (or (< (+ (date chord) (get-obj-dur chord)) (car interval))
                                                   (> (date chord) (cadr interval))))
                             (chords object))
    
         append 
         (loop for n in (notes c) append
               (remove nil 
                       (list 
                        (if (in-interval (+ (date c) (offset n)) interval :exclude-high-bound t) 
                                  
                            (list (+ (date c) (offset n))
                                        
                                  #'(lambda (note) (om-midi::midi-send-evt 
                                                    (om-midi:make-midi-evt 
                                                     :type :keyOn
                                                     :chan (or (chan note) 1) :port 0
                                                     :fields (list (round (midic note) 100) (vel note)))))
                                  (list n)))

                        (if (in-interval (+ (date c) (offset n) (dur n)) interval :exclude-high-bound t)
                                
                            (list (+ (date c) (offset n) (dur n))
                                      
                                  #'(lambda (note) (om-midi::midi-send-evt 
                                                    (om-midi:make-midi-evt 
                                                     :type :keyOff
                                                     :chan (or (chan note) 1) :port 0
                                                     :fields (list (round (midic note) 100) 0))))
                                  (list n)))
                      
                        )))
         )
   '< :key 'car))


(defmethod player-stop-object ((self scheduler) (object score-object))
  (call-next-method)
  (om-midi::midi-stop))
