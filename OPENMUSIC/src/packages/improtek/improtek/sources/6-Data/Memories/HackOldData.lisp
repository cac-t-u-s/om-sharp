(in-package :om)


(defclass exbeat (event)
  (
   (midiset :initform () :initarg :midiset :accessor midiset)   
   (HarmLabel :initform () :initarg :HarmLabel :accessor HarmLabel)
   (RelHarmLabel :initform () :initarg :RelHarmLabel :accessor RelHarmLabel)
   (NumBeat :initform 1 :initarg :NumBeat :accessor NumBeat)
   (RefHarmLabel :initform 1 :initarg :RefHarmLabel :accessor RefHarmLabel)
   (StartPhrase :initform () :initarg :StartPhrase :accessor StartPhrase)
   (QMidiSet :initform () :initarg :QMidiSet :accessor QMidiSet)
   (Qdivision :initform 1 :initarg :Qdivision :accessor Qdivision)
   (Density :initform 1 :initarg :Density :accessor Density)
   (feature :initform nil :initarg :feature :accessor feature)             
   ))

(setf (find-class 'beat nil) (find-class 'exbeat))


;(defmethod make-instance ((class-name symbol) &rest initargs) 
;  (apply #'make-instance (find-class class-name) initargs))


#|
(make-instance (quote exbeat) :midiset (list (list 67 218 106 105 1)) :duration 330 :data nil :harmlabel (list (quote g) (quote m7)) :relharmlabel nil :numbeat 1 :refharmlabel 1 :startphrase nil :qmidiset nil :qdivision 1 :density 1 :feature nil)

(make-instance (quote beat) :midiset (list (list 67 218 106 105 1)) :duration 330 :data nil :harmlabel (list (quote g) (quote m7)) :relharmlabel nil :numbeat 1 :refharmlabel 1 :startphrase nil :qmidiset nil :qdivision 1 :density 1 :feature nil)
|#


(defmethod ExBeat->Midiharmbeat ((b exbeat))
  (NewMidiharmbeat (nth 0 (harmlabel b)) (nth 1 (harmlabel b)) (midiset b) (duration b)))

(defmethod ImprovizerExBeat->MidiHarmBeat ((self improvizer))
  (loop for i from 1 to (maxetat self) do
        (setf (otext self i) (ExBeat->Midiharmbeat (otext self i))))
  self)

(defun load-old-improvizer (path)
  (ImprovizerExBeat->MidiHarmBeat (load-improvizer path)))


#|
(setf memory_solo 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or")
      memory_accomp 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or"))

(om-inspect memory_solo)

(om-inspect (otext memory_solo 33))

(om-inspect (ExBeat->Midiharmbeat (otext memory_solo 33)))

(setf memory_solo 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or")
      memory_accomp 
      (load-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Oracles/AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or"))

(om-inspect (otext (ImprovizerExBeat->MidiHarmBeat memory_solo) 33))

|#
