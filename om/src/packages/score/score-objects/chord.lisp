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

;;;=============
;;; NOTE
;;;=============

;;; some of the slots :initargs of INTERNAL-NOTE are hidden in the graphical interface
(defclass internal-note (score-object)
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 80 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
   (dur :initform 1000 :accessor dur :initarg :dur :type number :documentation "duration (ms)")
   (chan :initform 1 :accessor chan :initarg :chan :type integer :documentation "MIDI channel (1-16)")
   (port :initform nil :accessor port :initarg :port)
   (offset :initform 0 :accessor offset :initarg :offset) ;;; offset makes sense only if the note is inside a chord 
   (tie :initform nil :accessor tie)
   ))

;;; redefines only visible :initargs
(defclass* note (internal-note)
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 80 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
   (dur :initform 1000 :accessor dur :initarg :dur :type number :documentation "duration (ms)")
   (chan :initform 1 :accessor chan :initarg :chan :type integer :documentation "MIDI channel (1-16)")
   )
  
  (:documentation "
A simple NOTE defined with :

- pitch (midicents: 100 = 1 half-tone - 6000 = C3)
- velocity (MIDI velocity from 0 to 127)
- duration in milliseconds
- MIDI channel 
")
  )

;;; allow as additional slot
(defmethod additional-class-attributes ((self note)) '(port))



;;;=============
;;; CHORD
;;;=============

;;; some of the slots :initargs of INTERNAL-CHORD are hidden in the graphical interface
(defclass internal-chord (container data-frame score-object)  
  ((Lmidic :initform '(6000) :accessor Lmidic :initarg :Lmidic :type list :documentation "pitches (list of midicents)")
   (Lvel :initform '(80) :accessor Lvel :initarg :Lvel :type list :documentation "velocities (list of values 0-127)")
   (Loffset :initform '(0) :accessor Loffset :initarg :Loffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform '(1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (list of values in ms)")
   (Lchan :initform '(1) :accessor Lchan :initarg :Lchan :type list :documentation "MIDI channels (list of values 0-16)")
   (Lport :initform nil :accessor Lport :initarg :Lport :type list :documentation "MIDI ports (list of values 0-16)")
   ))

;;; redefines only visible :initargs
(defclass* chord (internal-chord)  
  ((Lmidic :initform '(6000) :accessor Lmidic :initarg :Lmidic :type list :documentation "pitches (list of midicents)")
   (Lvel :initform '(80) :accessor Lvel :initarg :Lvel :type list :documentation "velocities (list of values 0-127)")
   (Loffset :initform '(0) :accessor Loffset :initarg :Loffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform '(1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (list of values in ms)")
   )
  
  (:documentation "
A CHORD object (set of simultaneous notes) defined with 

- <lmidic>: list of pitches (midicents: 100 = 1 half-tone - 6000 = C3)
- <lvel>: velocities (MIDI velocity from 0 to 127)
- <loffset>: offsets (delay of notes after the actual chord onset)
- <ldur> durations in milliseconds
- <lchan> MIDI channels for each note
- <lport> (additional/optional) MIDI port for each note (defaults to the value defined in MIDI preferences)
These slots are simpel accessor for initialization. In reality the CHORD contains a list of NOTE instance.
"))

;;; allow as additional slots
(defmethod additional-class-attributes ((self chord)) '(date Lchan Lport))

(defmethod LMidic ((self chord))
  (loop for note in (inside self)
        collect (midic note)))

(defmethod LChan ((self chord))
  (loop for note in (inside self)
        collect (chan note)))

(defmethod Lvel ((self chord))
  (loop for note in (inside self)
        collect (vel note)))

(defmethod LDur ((self chord))
  (loop for note in (inside self)
        collect (dur note)))

(defmethod LOffset ((self chord))
  (loop for note in (inside self)
        collect (offset note)))


(defmethod (setf Lmidic) ((LMidic list) (self chord))
  (do-initialize self 
                 :LMidic LMidic
                 :LVel (LVel self)
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LChan) ((LChan list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel (LVel self)
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan LChan
                 :LPort (LPort self)))

(defmethod (setf LVel) ((LVel list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel LVel
                 :LOffset (LOffset self)
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LOffset) ((LOffset list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self)
                 :LVel (LVel self)
                 :LOffset LOffset
                 :LDur (LDur self)
                 :LChan (LChan self)
                 :LPort (LPort self)))

(defmethod (setf LDur) ((Ldur list) (self chord))
  (do-initialize self 
                 :LMidic (LMidic self) 
                 :LVel  (LVel self) 
                 :LOffset (LOffset self)
                 :LDur LDur
                 :LChan (LChan self)
                 :LPort (LPort self)))


;;  (NoteType 'note))
(defmethod initialize-instance ((self chord) &rest initargs) 
   
  (call-next-method)
  
  (when initargs
    (do-initialize self 
                   :Lmidic (slot-value self 'Lmidic) 
                   :Lvel (slot-value self 'Lvel)
                   :Loffset (slot-value self 'Loffset)
                   :Ldur (slot-value self 'Ldur)
                   :Lchan (slot-value self 'Lchan)
                   :Lport (slot-value self 'Lport)
                   ))

  ;;; better to remove these values... ? 
  (setf (slot-value self 'Lmidic) nil 
        (slot-value self 'Lvel) nil 
        (slot-value self 'Loffset) nil  
        (slot-value self 'Ldur) nil 
        (slot-value self 'Lchan) nil
        (slot-value self 'Lport) nil)
  self)


(defmethod do-initialize ((self chord) &key LMidic LVel Loffset LDur LChan LPort)
  (setf (inside self)
        (loop while Lmidic 
              for midic = (or (pop Lmidic) midic)
              for vel = (or (pop Lvel) vel)
              for offset = (or (pop Loffset) offset)
              for dur = (or (pop Ldur) dur)
              for chan = (or (pop Lchan) chan)
              for port = (or (pop Lport) port)  
              collect (make-instance 'note 
                                     :midic (round midic) 
                                     :vel (round vel) 
                                     :dur (round dur) 
                                     :offset (round offset) 
                                     :chan chan
                                     :port port 
                                     )))
  self)


(defmethod objfromobjs ((model note) (target Chord)) 
  (objfromobjs (list model) target))


(defmethod objfromobjs ((model list) (target Chord))

  (cond
   
   ;;; a list of chords (=> merge)
   ((list-subtypep model 'chord)
    (let ((notes (flat (mapcar 'inside model))))
      (objfromobjs notes target)))
   
   ;;; a list of number (probably a patching mistake, but consider it a list of pitches..)
   ((list-subtypep model 'number)
    (make-instance (type-of type) :lmidic model))

   ;;; a list of notes
   ((list-subtypep model 'note)
    (let ((chord (make-instance (type-of target))))
      (setf (inside chord) (mapcar 'clone model))
      chord))
   
   (t nil)))


(defmethod get-notes ((self chord)) (inside self))
(defmethod get-notes ((self note)) (list self))
(defmethod get-notes ((self list))
  (loop for item in self append (get-notes item)))

(defmethod item-get-duration ((self chord)) 
  (if (inside self)
      (apply 'max (mapcar 'dur (inside self)))
    0))


;;;============ 
;;; BOX
;;;============


(defmethod score-object-mini-view ((self note) x-u y-u w h staff fontsize)
  
  (let* ((unit (font-size-to-unit fontsize))
         (middle-in-units (/ w 2 unit)))
    
    (draw-chord (list self) (+ 2 middle-in-units) y-u w h fontsize :scale nil :staff staff)
    
    ))
  

(defmethod score-object-mini-view ((self chord) x-u y-u w h staff fontsize)
  
  (let* ((unit (font-size-to-unit fontsize))
         (middle-in-units (/ w 2 unit)))
    
    (when (inside self)
      (draw-chord (inside self) (+ 2 middle-in-units) y-u w h fontsize :scale nil :staff staff))
      
    ))

