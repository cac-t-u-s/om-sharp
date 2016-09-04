 (in-package :om)

(defclass* mididata (data)
  (
   (MidiSet :initform () :initarg :MidiSet :accessor MidiSet) ; liste 5uples (midi onset dur vel can)
   (QMidiSet :initform () :initarg :QMidiSet :accessor QMidiSet) ; quantized midiset
   (Qdivision :initform 1 :initarg :Qdivision :accessor Qdivision)
   (Density :initform 1 :initarg :Density :accessor Density)
   ))


(defmacro MEPitch (midi5up)
  `(first ,midi5up))

(defmacro MEOnset (midi5up)
  `(second ,midi5up))

(defmacro MEDur (midi5up)
  `(third ,midi5up))

(defmacro MEVel (midi5up)
  `(fourth ,midi5up))

(defmacro MEChannel (midi5up)
  `(fifth ,midi5up))

(defun timestretch (midi5up coef)
  (loop for x in midi5up for y = (clone x)
        when (= (length y) 5) do (setf (MEOnset y) (om-round (* coef (MEOnset y))) 
                                       (MEDur y) (if (clock-event-from-midi? y) (MEDur y) (om-round (* coef (MEDur y)))))
        collect y))


(defun NewMididata (midiset)
  (make-instance 'mididata :Midiset midiset))

(defmethod TransposeData ((self mididata) delta) ; /!\ Modifies and returns the object !
  (loop for midi5up in (MidiSet self)  ; only for notes, does not  apply if it remains other kind of event like clicks (onset 248)
           when (= (length midi5up) 5)          ; but in newer version (MidiSet Event) only contains real notes (not other events like clocks)
           do (let ((note (+ (abs (MEPitch midi5up)) delta)))
              (if (< note 0) (incf note 12))       ; pb MIDI pitch  limits 0-127
              (if (> note 127) (decf note 12))
              ;AAAAA RRREEEEEVVVVOOOOIIIIRRR
              ;;;;;;(setf (MEPitch midi5up) (* note (if (>= (MEPitch evt) 0) 1  -1))))))  ;BUG 2011/5/29: transposition error for negative pitch (prolongation)
              (setf (MEPitch midi5up) (* note (if (>= (MEPitch midi5up) 0) 1  -1))))) self)  ;BUG 2011/5/29: transposition error for negative pitch (prolongation)


(defmethod TimeStretchData ((self mididata) coef)
  (setf (MidiSet self) (timestretch (MidiSet self) coef)))



(defclass* audiodata (data)
  (
   (IdxInBuffer :initform -1 :initarg :IdxInBuffer :accessor IdxInBuffer)
   (DatesInBuffer :initform '(-1 -1) :initarg :DatesInBuffer :accessor DatesInBuffer)
   (CurrentTransfo :initform 0 :initarg :CurrentTransfo :accessor CurrentTransfo)
   (CurrentTimeStretchCoef :initform 1 :initarg :CurrentTimeStretchCoef :accessor CurrentTimeStretchCoef)
   (InitDuration :initform 500 :initarg :InitDuration :accessor InitDuration)
   (PathAudioFile :initform "unknown path" :initarg :PathAudioFile :accessor PathAudioFile)
   ))

(defun NewAudioData (idx duration &optional dates)
  (let* ((newdata (make-instance 'audiodata
                                   :IdxInBuffer idx
                                   :InitDuration duration
                                   )))
    (when dates (setf (DatesInBuffer newdata) dates))
    newdata))

(defmethod TransposeData ((self audiodata) delta)
  (setf (CurrentTransfo self) (+ (CurrentTransfo self) delta))
  self)

(defmethod TimeStretchData ((self audiodata) coef)
  (setf (CurrentTimeStretchCoef self) (* (CurrentTimeStretchCoef self) coef)))







;====================================================================================================================================================================================
;====================================================================================================================================================================================
;MARC 10/2/2012 generic function that works with 'Events', but also with objects of specific classes ('meloEvents', 'relativechords', ...)
;you need to redefine the following functions: 
;- TransposeClonedEvent ((self Event) int)
;- eligible-Event? ((self Event) (label list))
;- CompareEvents ((Event1 Event) (event2 Event))
;- clone-object ((self Event))
;when using label objects instead of simple lists ('garnerlabel'...):
;- TransposeLabel ((label list) int)  
;- FormatLabel ((label list))
;- undefined-label? ((label list))

;--> continuations-by-suppleance, find-Event-label-match, choose-factor-link uses these functions only
;====================================================================================================================================================================================
;====================================================================================================================================================================================


#|
(defmethod CompareEvents ((event1 Event) (event2 Event)) 
   (or (equal (label event1) (label event2)) 
       (equalLabel (label event1) (label event2))))

(defmethod TransposeClonedEvent ((self Event) int)
   (let ((ClonedEvent (clone-object self))) 
     (setf (label ClonedEvent) (TransposeLabel (label ClonedEvent) int))
     (setf (data ClonedEvent) (TransposeData (data ClonedEvent) int))
     ClonedEvent))

(defmethod empty-Event? ((self Event))
  (null (data self)))

(defmethod eligible-Event? ((self Event) (label list)) 
  (and ;(not (empty-Event? self))                     
       (or (null label) (equalLabel label (label self)))))

(defmethod clone-object ((self Event))
  (let ((cEvent (clone self)))
    (setf (MidiSet cEvent) 
          (copy-tree (MidiSet  self)))
    cEvent))

(defmethod eligible-feature? ((self event) (o improvizer))
  (if (null (feature o)) t 
    (if (integerp (feature self))
        (member (abs (feature self)) (feature o))  ;'features' are MIDI codes, thus 'abs' is needed for prolongation
        nil)))     ;'feature' = nil when the midiharmbeat has no feature, thus it should be rejected if the oracle looks for features
;(defmethod eligible-feature? ((self t) (o improvizer)) t)      ;;;;;;;;;;for genericity


|#









