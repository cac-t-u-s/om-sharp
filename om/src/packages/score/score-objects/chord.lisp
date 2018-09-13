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


;(defclass chord (data-frame)
;  ((onset :accessor onset :initarg :onset :initform 0 :documentation "onset of the chord (ms)")))

(defclass* note ()
  ((midic :initform 6000 :accessor midic :initarg :midic :type number :documentation "pitch (midicents)")
   (vel :initform 80 :accessor vel :initarg :vel :type number :documentation "velocity (0-127)")
   (dur :initform 1000 :accessor dur :initarg :dur :type number :documentation "duration (ms)")
   (chan :initform 1 :accessor chan :initarg :chan :type integer :documentation "MIDI channel (1-16)")
   (port :initform 0 :accessor port)
   (offset :initform 0 :accessor offset)
   (tie :initform nil :accessor tie))
  
  (:documentation "
A simple NOTE defined with :

- pitch (midicents: 100 = 1 half-tone - 6000 = C3)
- velocity (MIDI velocity from 0 to 127)
- duration in milliseconds
- MIDI channel 
")
  )


(defclass* chord (container score-object)  
  ((Lmidic :initform '(6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (list of midicents)")
   (LVel :initform '(100) :accessor LVel :initarg :LVel :type list :documentation "velocities (list of values 0-127)")
   (LOffset :initform '(0) :accessor LOffset :initarg :LOffset :type list :documentation "offsets (list of values in ms)")
   (Ldur :initform '(1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (list of values in ms)")
   (LChan :initform '(1) :accessor LChan :initarg :LChan :type list :documentation "MIDI channels (list of values 0-16)")
   (Lport :initform '(0) :accessor LPort :type list :documentation "MIDI ports (list of values 0-16)")
   )
  
  (:documentation "
A CHORD object (set of simultaneous notes) defined with 

- list of pitches (midicents: 100 = 1 half-tone - 6000 = C3)
- velocities (MIDI velocity from 0 to 127)
- offsets (delay of notes after the actual chord onset)
- durations in milliseconds
- MIDI channels for each note

"))

(defmethod additional-class-attributes ((self chord)) '(date Lport))

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


(defmethod initialize-instance ((self chord) &rest initargs  &key (Empty nil) (NoteType 'note))
  (declare (ignore initargs)) 
  (call-next-method)
  (unless Empty
    (do-initialize self 
                   :LMidic (slot-value self 'LMidic) 
                   :LVel (slot-value self 'LVel)
                   :LOffset (slot-value self 'LOffset)
                   :LDur (slot-value self 'LDur)
                   :LChan (slot-value self 'LChan)
                   :LChan (slot-value self 'LPort)
                   ))
  ;(setf (slot-value self 'LMidic) nil 
  ;      (slot-value self 'LVel) nil 
  ;      (slot-value self 'LOffset) nil  
  ;      (slot-value self 'LDur) nil 
  ;      (slot-value self 'LChan) nil)
  self
  )


(defmethod do-initialize ((self chord) &key LMidic LVel Loffset LDur LChan LPort)
  (setf (inside self)
        (loop while LMidic 
              for midic = (or (pop LMidic) midic)
              for vel = (or (pop LVel) vel)
              for offset = (or (pop LOffset) offset)
              for dur = (or (pop LDur) dur)
              for chan = (or (pop LChan) chan)
              for port = (or (pop LPort) 0)   ;;; now port can be nil.. 
              for note = (make-instance 'note :midic (round midic) :vel (round vel) :dur (round dur) :chan chan)
              do (setf (offset note)  (round offset))
              (setf (port note) port)
              collect note))
  self)


(defmethod Objfromobjs ((self note) (Type Chord)) 
  (ObjFromObjs (list self) Type))

#|
(defmethod* Objfromobjs ((self list) (Type Chord))
  (cond
   ((list-subtypep self 'chord)
    (let ((notes (flat (mapcar 'inside self))))
      (objfromobjs notes type)))
   ((chord-p (car self)) (Clone (car self)))
   ((list-subtypep self 'number)
    (mki (type-of type) :LMidic self))
   ((list-subtypep self 'note)
    (let ((chord (make-instance (type-of type) :empty t)))
      (setQValue chord 1000 :recursive nil)
      (setf (inside chord) (mapcar 'clone self))
      (QNormalize chord)
      (setf (slot-value chord 'LMidic) nil  (slot-value chord 'LVel) nil 
            (slot-value chord 'LOffset) nil  (slot-value chord 'LDur) nil 
            (slot-value chord 'LChan) nil) 
      chord))
   (t nil)))

|#



;;;============ 
;;; BOX
;;;============

(defmethod object-default-edition-params ((self chord))
  '((:font-size 24)
    (:staff :gf)))

(defmethod additional-box-attributes ((self chord)) 
  '((:font-size "a default size for score display" nil)
    (:staff "default staff configuration" 
     (("G" :g) ("F" :f) ("GF" :gf) ("GG" :gg) ("FF" :ff) ("GGF" :ggf) ("GFF" :gff) ("GGFF" :ggff)))
    ))

(defmethod display-modes-for-object ((self chord))
  '(:hidden :text :mini-view))


(defmethod draw-mini-view ((self chord) (box t) x y w h &optional time)

  (om-draw-rect x y w h :fill t :color (om-def-color :white))
  
  (let ((fontsize 24)
        (staff (get-edit-param box :staff)))
         
    (let* ((staff-lines (apply 'append (mapcar 'staff-lines (staff-split staff))))
           (unit (font-size-to-unit fontsize))
           (n-lines (+ (- (car (last staff-lines)) (car staff-lines)) 10)) ;;; range of the staff lines + 10-margin
           (draw-box-h (* n-lines unit)))
     
      (if (< draw-box-h h)
          ;;; there's space: draw more in the middle
          (setf y (+ y (round (- h draw-box-h) 2)))
        ;;; there's no space: reduce font ?
        (progn 
          (setf unit (- unit (/ (- draw-box-h h) n-lines)))
          (setf fontsize (unit-to-font-size unit)))
        )
      
      (om-with-fg-color (om-make-color 0.0 0.2 0.2)
        (score-draw self x y w h fontsize nil staff))
      )))


;;;============ 
;;; EDITOR
;;;============

(defclass score-panel (OMEditorView) ())

(defclass chord-editor (OMEditor) ())

(defmethod editor-view-drawable ((self chord-editor)) t)

(defmethod object-has-editor ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chord-editor)
(defmethod editor-view-class ((self chord-editor)) 'score-panel)

(defmethod om-draw-contents ((self score-panel))
  (let* ((editor (editor self))
         (chord (object-value editor)))
    (om-trap-errors 
     (om-with-fg-color (om-make-color 0.0 0.2 0.2)
       (score-draw chord 0 0 (w self) (h self) (editor-get-edit-param editor :font-size) nil :gf))
     )))

(defmethod update-to-editor ((editor chord-editor) (from t))
  (call-next-method)
  (om-invalidate-view (main-view editor)))

;(defmethod get-properties-list ((self chord)) (call-next-method))
;  '((""
;     (:name "Name" :text name)
;     )))

