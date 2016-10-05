;Ex Labels.lisp : définition d'une classe --> macros deviennent méthodes
;HarmLabels.lisp
;JN 23/02/15 after M.C.
(in-package :om)


;====================================================
; HARMONIC LABELS
;====================================================

;-----------------------------
;Class
;-----------------------------
(defclass* harmlabel (label)
           ((root :initform 'c :initarg :root :accessor root) 
            (chordtype :initform 'maj7 :initarg :chordtype :accessor chordtype)))

(defmethod NewHarmLabel ((root symbol) (chordtype t))
  (make-instance 'harmlabel :root root :chordtype chordtype))
;(setf hl1 (NewHarmLabel 'd 'm7) hl2 (NewHarmLabel 'g 7))

(defmethod MakeLabelsFromList ((l list) (spec (eql 'harmlabel)))
  (loop for couple in l collect 
        (NewHarmLabel (nth 0 couple) (nth 1 couple))
        ))
;(defmethod NewHarmLabelList ((l list))
;  (loop for couple in l collect 
;        (NewHarmLabel (nth 0 couple) (nth 1 couple))
;        ))
;(setf chordprogression (NewHarmLabelList '((d m7) (g 7) (c maj7))))

;-----------------------------
;Specific funtions & methods
;-----------------------------
(defmacro NormalizeNote (note) `(case ,note (db 'c#) (d# 'eb) (gb 'f#) (ab 'g#) (a# 'bb) (t ,note)))
(defmacro TransposeNote (root int) `(nth (mod ,int 12) (member (NormalizeNote ,root) '(c c# d eb e f f# g g# a bb b c c# d eb e f f# g g# a bb b))))
(defmacro MidiRoot (root) `(- 12 (length (member (NormalizeNote ,root) '(c c# d eb e f f# g g# a bb b)))))
(defmacro DeltaNote (root1 root2)
  `(- (mod (+ 5 (- (length (member (NormalizeNote ,root1) '(c c# d eb e f f# g g# a bb b)))
                   (length (member (NormalizeNote ,root2) '(c c# d eb e f f# g g# a bb b))))) 12)
      5))

(defmethod NormalizeLabel ((self harmlabel)) (setf (root self) (NormalizeNote (root self))) self)

;-----------------------------
;"Label" overloaded methods
;-----------------------------

(defmethod equalLabel ((h1 harmlabel) (h2 harmlabel))
  (let ((nh1 (NormalizeLabel h1)) (nh2 (NormalizeLabel h2)))
    (and 
     (equal (root nh1) (root nh2)) 
     (equal (chordtype nh1) (chordtype nh2))
     ))) 

(defmethod TransposeLabel ((self harmlabel) delta); /!\ Modifies the object !
  (cond ((null self) nil)
        (t (progn (setf (root self) (TransposeNote (root self) delta)) self))))
(defmethod TransformLabel ((self harmlabel) delta) (TransposeLabel self delta)); /!\ Modifies the object !

(defmethod undefined-label? ((self harmlabel)) 
  (not (member (root self) (append '(c c# d eb e f f# g g# a bb b) '(db d# gb ab a#)))))

(defmethod FormatLabel  ((self harmlabel)) (list (root self) (chordtype self)))
;(FormatLabel  (NewHarmLabel 'a 'm7))

(defmethod FormatLabelList ((harmlabellist list)) 
  (loop for hl in harmlabellist collect 
        (FormatLabel hl)
        ))
;(FormatLabelList (NewHarmLabelList '((d m7) (g 7) (c maj7))))


;====================================================
; AUDIO DESCRIPTORS CLASSES
; Transposition / transformation only on descriptor 1.
;====================================================

;-----------------------------
;Class
;-----------------------------
(defclass* AudioDescr (label)
           ((IdxClusterDesc1 :initform 0 :initarg :IdxClusterDesc1 :accessor IdxClusterDesc1) 
            (IdxClusterDesc2 :initform 0 :initarg :IdxClusterDesc2 :accessor IdxClusterDesc2) 
            (ClusterMeanValuesDesc1 :initform nil :initarg :ClusterMeanValuesDesc1  :accessor ClusterMeanValuesDesc1 )
            (ClusterMeanValuesDesc2 :initform nil :initarg :ClusterMeanValuesDesc2  :accessor ClusterMeanValuesDesc2)))

(defmethod NewAudioDescrLabel (IdxClusterDesc1 IdxClusterDesc2 &optional ClusterMeanValuesDesc1 ClusterMeanValuesDesc2)
  (let ((ClusterMeanVals1 nil) (ClusterMeanVals2 nil))
    (when ClusterMeanValuesDesc1 (setf ClusterMeanVals1 ClusterMeanValuesDesc1))
    (when ClusterMeanValuesDesc2 (setf ClusterMeanVals2 ClusterMeanValuesDesc2))
    (make-instance 'AudioDescr :IdxClusterDesc1 IdxClusterDesc1 :IdxClusterDesc2 IdxClusterDesc2 :ClusterMeanValuesDesc1 ClusterMeanValuesDesc1 :ClusterMeanValuesDesc2 ClusterMeanValuesDesc2)))
;(FormatLabel (NewAudioDescrLabel 1 2))

(defmethod MakeLabelsFromList ((l list) (spec (eql 'AudioDescr)))
  (loop for el in l
        collect (NewAudioDescrLabel (nth 0 el) (nth 1 el) (nth 2 el) (nth 3 el))))

;-----------------------------
;"Label" overloaded methods
;-----------------------------

(defmethod equalLabel ((d1 AudioDescr) (d2 AudioDescr))
  (and 
   (equal (IdxClusterDesc1 d1) (IdxClusterDesc1 d2))
   (equal (IdxClusterDesc2 d1) (IdxClusterDesc2 d2))
   )
)

(defmethod TransposeLabel ((self AudioDescr) delta)
  (cond ((null self) nil)
        (t 
         (let ((newIndex (+ (IdxClusterDesc1 self) delta)))
           (setf newIndex (max newIndex 0))
           (if (ClusterMeanValuesDesc1 self) 
               (setf newIndex (min newIndex (- (list-length (ClusterMeanValuesDesc1 self)) 1))))
           (setf (IdxClusterDesc1 self) newIndex)
           self))
        ))
(defmethod TransformLabel ((self AudioDescr) delta) (TransposeLabel self delta))

(defmethod undefined-label? ((self AudioDescr)) 
  (or (< (IdxClusterDesc1 self) 0)
      (< (IdxClusterDesc2 self) 0)
      (and (ClusterMeanValuesDesc1 self) 
           (> (IdxClusterDesc1 self) (- (list-length (ClusterMeanValuesDesc1 self)) 1)))
      (and (ClusterMeanValuesDesc2 self) 
           (> (IdxClusterDesc2 self) (- (list-length (ClusterMeanValuesDesc2 self)) 1)))))
   

(defmethod FormatLabel  ((self AudioDescr)) (list (IdxClusterDesc1 self) (IdxClusterDesc2 self)))



