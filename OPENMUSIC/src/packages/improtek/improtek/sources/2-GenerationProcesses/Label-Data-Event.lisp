;ISSU DU DÉCOUPAGE DE IMPROVIZER.LISP ET LABELS.LISP 


;Ex Labels.lisp : définition d'une classe --> macros deviennent méthodes
;EventLabelData.lisp
;JN 23/02/15 after M.C.
(in-package :om)


(defclass label () ())

(defmethod NormalizeLabel ((self label)) self)
(defmethod equalLabel ((l1 label) (l2 label)) (equal l1 l2))
(defmethod TransposeLabel ((self label) delta) self); /!\ Modifies and returns the object !
(defmethod TransformLabel ((self label) delta) (TransposeLabel self delta)); /!\ Modifies and returns the object !
(defmethod TransposeCloneLabel ((self label) delta) 
    (TransposeLabel (clone-object self) delta))

(defmethod undefined-label? ((self label)) (null self))
(defmethod FormatLabel  ((self label)) self)

(defmethod TransposeLabelList ((labellist list) delta) (loop for l in labellist collect (TransposeLabel l delta)))
(defmethod TransformLabelList ((labellist list) delta) (TransposeLabelList labellist delta))
(defmethod TransposeCloneLabelList ((labellist list) delta) (loop for l in labellist collect (TransposeCloneLabel l delta)))

(defmethod FormatLabelList ((labellist list)) (loop for l in labellist collect (FormatLabel l)))

(defmethod MakeLabelsFromList ((l list) (spec (eql 'label))) l)


(defclass data () ())

(defmethod TransposeData ((self data) delta) self); /!\ Modifies and returns the object !
(defmethod TransformData ((self data) delta) (TransposeData self delta)); /!\ Modifies and returns the object !
(defmethod TransposeCloneData ((self data) delta) (TransposeData (clone-object self) delta))
(defmethod TimeStretchData ((self t) coef) self)


(defclass event ()
  (
   (label :initform (make-instance 'label) :initarg :label :accessor label :type label)
   (data :initform (make-instance 'data) :initarg :data :accessor data :type data)
   (duration :initform 0 :initarg :duration :accessor duration)
   (feature :initform nil :initarg :feature :accessor feature)
   ))

(defmethod CompareEvents ((event1 Event) (event2 Event)) 
   (or (equal (label event1) (label event2)) 
       (equalLabel (label event1) (label event2))))
(defmethod CompareEvents ((Event1 T) (event2 T))
  (equal event1 event2))
(defmethod CompareEvents ((l1 list) (l2 list))
  (equallabel l1 l2))

(defmethod TransposeClonedEvent ((self Event) int)
   (let ((ClonedEvent (clone-object self)))           ; cloned Event needed not to modify Events in the Oracle
     (progn 
       (TransposeLabel (label ClonedEvent) int)
       (TransposeData (data ClonedEvent) int))
     ClonedEvent))
;(defmethod TransposeClonedEvent ((self t) int) self)   ; for genericity, when self is a non 'Event' event (t is for other types)

(defmethod TimeStretchEvent ((self event) coef)
  (setf (duration self) (* (duration self) coef))
  (setf (data self) (TimeStretchData (data self) coef))
  )

(defmethod empty-event? ((self Event))
  (null (data self)))

;ICI RAJOUTER DES CHOSES POUR FEATURES
(defmethod eligible-event? ((self Event) (label label)) 
  (and ;(not (empty-event? self))                     
       (or (null label) (equalLabel label (label self)))))
;(defmethod eligible-event? ((self t) (label list)) (null label))   ; for genericity, when self is a non 'Event' event (t is for other types)

(defmethod clone-object ((self t) &optional clone)
  (let ((cloned-object (make-instance (class-of self))))
    (loop for sl in 
          (mapcar #'slot-definition-name (class-slots (class-of self))) do
          (setf (slot-value cloned-object sl) 
                (slot-value self sl)))      
    cloned-object))


;(defmethod clone-object ((object t) &optional clone)
;  (clone object))


;------------------------------------------------------------
; Format and send

(defmethod osc-send-sequence-fragment ((sequence list) beatIdxInImpro hostsend portsend adresssend numVoice)
    (osc-send-sequence-fragment-of sequence (car sequence) beatIdxInImpro hostsend portsend adresssend numVoice))
;THIS FUNCTION HAS TO BE OVERLOADED FOR EVERY SUBCLASS OF "EVENT"
(defmethod osc-send-sequence-fragment-of ((sequence list) (whencontent event) beatIdxInImpro hostsend portsend adresssend numVoice)
  sequence)

(defmethod FormatOutputSequence ((sequence list) &optional beatduration)
  (when beatduration
      (FormatOutputSequenceOf sequence (car sequence) beatduration)
    (FormatOutputSequenceOf sequence (car sequence))))
;THIS FUNCTION HAS TO BE OVERLOADED FOR EVERY SUBCLASS OF "EVENT"
(defmethod FormatOutputSequenceOf ((sequence list) (whencontent t) &optional beatduration) sequence)
