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

;;;=============
;;; NOTE
;;;=============

;;; some of the slots :initargs of INTERNAL-NOTE are hidden in the graphical interface
(defclass internal-note (score-element)
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 100 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
   (dur :initform 1000 :accessor dur :initarg :dur :type number :documentation "duration (ms)")
   (chan :initform 1 :accessor chan :initarg :chan :type integer :documentation "MIDI channel (1-16)")
   (port :initform nil :accessor port :initarg :port)
   (offset :initform 0 :accessor offset :initarg :offset) ;;; offset makes sense only if the note is inside a chord
   ))

;;; redefines only visible :initargs
(defclass* note (internal-note)
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 100 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
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


(defmethod initialize-instance ((self note) &rest args)
  (when (or (consp (getf args :midic))
            (consp (getf args :vel))
            (consp (getf args :dur))
            (consp (getf args :chan))
            (consp (getf args :port)))
    (error "NOTE attributes can not be lists!"))
  (call-next-method))



;;; allow as additional slot
(defmethod additional-class-attributes ((self note)) '(port))

(defmethod get-obj-dur ((self note)) (dur self))

;;;=============
;;; CHORD
;;;=============

;;; some of the slots :initargs of INTERNAL-CHORD are hidden in the graphical interface
(defclass internal-chord (data-frame score-element)
  ((Lmidic :initform '(6000) :initarg :Lmidic :type list :documentation "pitches (list of midicents)")
   (Lvel :initform '(80) :initarg :Lvel :type list :documentation "velocities (list of values 0-127)")
   (Loffset :initform '(0) :initarg :Loffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform '(1000) :initarg :Ldur :type list :documentation "durations (list of values in ms)")
   (Lchan :initform '(1) :initarg :Lchan :type list :documentation "MIDI channels (list of values 0-16)")

   (Lport :initform nil :accessor Lport :initarg :Lport :type list :documentation "MIDI ports (list of values 0-16)")

   (notes :initform nil :initarg :notes :accessor notes :type list :documentation "the actual list of notes")
   ))

;;; redefines only visible :initargs
(defclass* chord (internal-chord)
  ((Lmidic :initform '(6000) :initarg :Lmidic :type list :documentation "pitches (list of midicents)")
   (Lvel :initform '(80) :initarg :Lvel :type list :documentation "velocities (list of values 0-127)")
   (Loffset :initform '(0) :initarg :Loffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform '(1000) :initarg :Ldur :type list :documentation "durations (list of values in ms)")
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
(defmethod additional-class-attributes ((self chord)) '(onset Lchan Lport extras))

(defmethod excluded-slots-from-save ((self chord)) '(notes)) ;;; or just remove its :initarg ?


(defmethod Lmidic ((self chord))
  (loop for note in (notes self)
        collect (midic note)))

(defmethod Lchan ((self chord))
  (loop for note in (notes self)
        collect (chan note)))

(defmethod Lvel ((self chord))
  (loop for note in (notes self)
        collect (vel note)))

(defmethod Ldur ((self chord))
  (loop for note in (notes self)
        collect (dur note)))

(defmethod Loffset ((self chord))
  (loop for note in (notes self)
        collect (offset note)))

(defmethod Lport ((self chord))
  (loop for note in (notes self)
        collect (port note)))


(defmethod (setf Lmidic) ((Lmidic list) (self chord))
  (do-initialize self
                 :Lmidic Lmidic
                 :Lvel (Lvel self)
                 :Loffset (Loffset self)
                 :Ldur (Ldur self)
                 :Lchan (Lchan self)
                 :Lport (Lport self)))

(defmethod (setf Lchan) ((Lchan list) (self chord))
  (do-initialize self
                 :Lmidic (Lmidic self)
                 :Lvel (Lvel self)
                 :Loffset (Loffset self)
                 :Ldur (Ldur self)
                 :Lchan Lchan
                 :Lport (Lport self)))

(defmethod (setf Lvel) ((Lvel list) (self chord))
  (do-initialize self
                 :LMidic (Lmidic self)
                 :LVel Lvel
                 :LOffset (Loffset self)
                 :LDur (Ldur self)
                 :LChan (Lchan self)
                 :LPort (Lport self)))

(defmethod (setf Loffset) ((Loffset list) (self chord))
  (do-initialize self
                 :LMidic (Lmidic self)
                 :LVel (Lvel self)
                 :LOffset Loffset
                 :LDur (Ldur self)
                 :LChan (Lchan self)
                 :LPort (Lport self)))

(defmethod (setf Ldur) ((Ldur list) (self chord))
  (do-initialize self
                 :LMidic (Lmidic self)
                 :LVel  (lvel self)
                 :LOffset (loffset self)
                 :LDur ldur
                 :LChan (lchan self)
                 :LPort (lport self)))

(defmethod (setf Lport) ((Lport list) (self chord))
  (do-initialize self
                 :LMidic (Lmidic self)
                 :LVel  (lvel self)
                 :LOffset (loffset self)
                 :LDur (ldur self)
                 :LChan (lchan self)
                 :LPort Lport))


;;  (NoteType 'note))
(defmethod initialize-instance ((self chord) &rest initargs)

  (call-next-method)

  (unless (listp (slot-value self 'Lmidic))
    (om-print "CHORD object initialized with single value for pitch-list (converting to list)." "Warning")
    (setf (slot-value self 'Lmidic) (list (slot-value self 'Lmidic))))

  ;;; don't do-initialize if initargs was :notes
  (when (intersection initargs '(:LMidic :LVel :Loffset :LDur :LChan :LPort))

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

  (let ((lmidic (list! lmidic))
        (lvel (list! lvel))
        (loffset (list! loffset))
        (ldur (list! ldur))
        (lchan (list! lchan))
        (lport (list! lport)))

    (setf (notes self)
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
    self))

(defmethod objfromobjs ((model note) (target Chord))
  (objfromobjs (list model) target))


(defmethod objfromobjs ((model list) (target Chord))

  (cond

   ;;; a list of chords (=> merge)
   ((list-subtypep model 'chord)
    (let ((notes (flat (mapcar 'notes model))))
      (objfromobjs notes target)))

   ;;; a list of number (probably a patching mistake, but consider it a list of pitches..)
   ((list-subtypep model 'number)
    (make-instance (type-of target) :lmidic model))

   ;;; a list of notes
   ((list-subtypep model 'note)
    (let ((chord (make-instance (type-of target))))
      (setf (notes chord) (mapcar 'clone model))
      chord))

   (t nil)))


;;; used in score utils and editor hierarchical/recursive calls
(defmethod inside ((self chord)) (notes self))

(defmethod chords ((self chord)) (list self))

(defmethod get-notes ((self chord)) (notes self))
(defmethod get-notes ((self note)) (list self))
(defmethod get-notes ((self list))
  (loop for item in self append (get-notes item)))

(defmethod item-get-duration ((self chord))
  (if (notes self)
      (apply 'max (mapcar 'dur (notes self)))
    0))


