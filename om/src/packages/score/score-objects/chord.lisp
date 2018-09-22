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

(defclass* note (score-object)
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

(defmethod additional-class-attributes ((self note)) '(port offset))


(defclass* chord (container data-frame score-object)  
  ((Lmidic :initform '(6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (list of midicents)")
   (LVel :initform '(80) :accessor LVel :initarg :LVel :type list :documentation "velocities (list of values 0-127)")
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



;;;============ 
;;; BOXES
;;;============

(defmethod additional-box-attributes ((self note)) 
  `((:font-size "a font size for score display" nil)
    (:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    ))

(defmethod additional-box-attributes ((self chord)) 
  `((:font-size "a font size for score display" nil)
    (:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    ))

(defmethod display-modes-for-object ((self note))
  '(:hidden :text :mini-view))

(defmethod display-modes-for-object ((self chord))
  '(:hidden :text :mini-view))


(defun chord-mini-view (notes box x y w h)
  
  (om-draw-rect x y w h :fill t :color (om-def-color :white))
  
  (let ((fontsize 24)
        (staff (get-edit-param box :staff)))
         
    (let* ((staff-lines (apply 'append (mapcar 'staff-lines (staff-split staff))))
           (unit (font-size-to-unit fontsize))
           (n-lines (+ (- (car (last staff-lines)) (car staff-lines)) 8)) ;;; range of the staff lines + 10-margin
           (draw-box-h (* n-lines unit))
           (y-in-units (/ y unit)))
     
      (if (< draw-box-h h)
          ;;; there's space: draw more in the middle
          (setf y-in-units (+ y-in-units (/ (round (- h draw-box-h) 2) unit)))
        ;;; there's no space: reduce font ?
        (progn 
          (setf unit (- unit (/ (- draw-box-h h) n-lines)))
          (setf fontsize (unit-to-font-size unit)))
        )
      
      (let* ((margin 1)
             (x-in-units (+ margin (/ x unit)))
             (middle-in-units (/ w 2 unit)))
        
      (om-with-fg-color (om-make-color 0.0 0.2 0.2)
        (draw-staff x-in-units y-in-units (- w (* margin unit)) h fontsize staff)
        (draw-chord notes (+ 2 middle-in-units) y-in-units w h fontsize :scale nil :staff staff))
      ))))

(defmethod draw-mini-view ((self chord) box x y w h &optional time)
  (chord-mini-view (inside self) box x y w h))

(defmethod draw-mini-view ((self note) box x y w h &optional time)
  (chord-mini-view (list self) box x y w h))


;;;============ 
;;; EDITOR
;;;============

(defmethod object-default-edition-params ((self chord))
  '((:font-size 24)
    (:staff :gf)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    ))

(defmethod object-default-edition-params ((self note))
  '((:font-size 24)
    (:staff :gf)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    ))



(defclass chord-editor (OMEditor undoable-editor-mixin) ())
(defmethod object-has-editor ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chord-editor)

;; (defmethod editor-view-drawable ((self chord-editor)) t)

(defmethod make-editor-window-contents ((editor chord-editor))
  (let* ((object (object-value editor))
         (panel (om-make-view 'score-view 
                              :size (omp 50 100) :direct-draw t :bg-color (om-def-color :white) :scrollbars nil
                              :editor editor))
         
         (bottom-area 
          (om-make-layout 
           'om-row-layout 
           :align :center ; :size (omp 100 100)
           ;:ratios '(1 1 nil) 
           :subviews
           (list 
            (om-make-layout 
             'om-column-layout
             :subviews (list 
                        
                        (om-make-di 'om-simple-text :text "Score params" 
                               :size (omp 140 22) ;; :bg-color (om-def-color :red)
                               :font (om-def-font :font1b))

                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews 
                         (list 
                          (om-make-di 'om-simple-text :text "Staff" 
                                      :size (omp 60 20) 
                                      :font (om-def-font :font1))
                          (om-make-di 'om-popup-list :items *score-staff-options* 
                                      :size (omp 60 24) :font (om-def-font :font1)
                                      :value (editor-get-edit-param editor :staff)
                                      :di-action #'(lambda (list) 
                                                     (editor-set-edit-param editor :staff (om-get-selected-item list))
                                                     (report-modifications editor) ;; to update the box drisplay as well...
                                                     ))
                          ))
                             
                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews
                         (list 
                          (om-make-di 'om-simple-text :text "Font size" 
                                      :font (om-def-font :font1)
                                      :size (omp 60 20))
                          (om-make-graphic-object 'numbox 
                                                  :value (editor-get-edit-param editor :font-size)
                                                  :min-val 8 :max-val 120 
                                                  :size (omp 40 18)
                                                  :font (om-def-font :font1)
                                                  :bg-color (om-def-color :white)
                                                  :after-fun #'(lambda (numbox) 
                                                                 (editor-set-edit-param editor :font-size (value numbox))))
                          ))
                        ))


            (om-make-layout 
             'om-column-layout
             :subviews (list 
                        
                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews 
                         (list 
                          (om-make-di 'om-simple-text :text "duration" 
                                      :size (omp 68 20) 
                                      :font (om-def-font :font1))
                          (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                                      :checked-p (editor-get-edit-param editor :duration-display)
                                      :di-action #'(lambda (item) 
                                                     (editor-set-edit-param editor :duration-display (om-checked-p item))))
                          ))

                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews 
                         (list 
                          (om-make-di 'om-simple-text :text "velocity" 
                                      :size (omp 68 20) 
                                      :font (om-def-font :font1))
                          (om-make-di 'om-popup-list :items '(:hidden :value :symbol :size :alpha) 
                                      :size (omp 80 24) :font (om-def-font :font1)
                                      :value (editor-get-edit-param editor :velocity-display)
                                      :di-action #'(lambda (list) 
                                                     (editor-set-edit-param editor :velocity-display (om-get-selected-item list))))
                          ))
                        
                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews 
                         (list 
                          (om-make-di 'om-simple-text :text "MIDI channel" 
                                      :size (omp 68 20) 
                                      :font (om-def-font :font1))
                          (om-make-di 'om-popup-list :items '(:hidden :number :color :color-and-number) 
                                      :size (omp 80 24) :font (om-def-font :font1)
                                      :value (editor-get-edit-param editor :channel-display)
                                      :di-action #'(lambda (list) 
                                                     (editor-set-edit-param editor :channel-display (om-get-selected-item list))))
                          ))
                        
                        (om-make-layout 
                         'om-row-layout :align :center
                         :subviews 
                         (list 
                          (om-make-di 'om-simple-text :text "MIDI port" 
                                      :size (omp 68 20) 
                                      :font (om-def-font :font1))
                          (om-make-di 'om-check-box :text "" :font (om-def-font :font1)
                                      :checked-p (editor-get-edit-param editor :port-display)
                                      :di-action #'(lambda (item) 
                                                     (editor-set-edit-param editor :port-display (om-checked-p item))))
                          ))

                        
                        ))
            ;nil
            )
           
            )))

    (set-g-component editor :main-panel panel)
    (om-make-layout 'om-row-layout :ratios '(99 1) 
                    :subviews 
                    (list 
                     (om-make-layout 'om-column-layout 
                                     :ratios '(99 1)
                                     :subviews (list panel bottom-area))
                     (call-next-method)))
    ))



(defmethod update-to-editor ((editor chord-editor) (from t))
  (call-next-method)
  (editor-invalidate-views editor))

(defmethod editor-invalidate-views ((self chord-editor))
  (call-next-method)
  (om-invalidate-view (get-g-component self :main-panel)))



;;;============ 
;;; DRAW
;;;============

(defclass score-view (OMEditorView) ())

(defmethod om-draw-contents ((self score-view))
  
  (let* ((editor (editor self))
         
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size))))
    
    (om-trap-errors 
     (om-with-fg-color (om-make-color 0.0 0.2 0.2)
       
       (draw-staff 1 0 (- (w self) unit) (h self) 
                   (editor-get-edit-param editor :font-size) 
                   (editor-get-edit-param editor :staff))
       
       (draw-score-object-in-editor-view editor self unit)
       
       ))
    ))


(defmethod draw-score-object-in-editor-view ((editor chord-editor) view unit)

  (let ((chord (object-value editor))
        (middle-in-units (/ (w view) 2 unit)))
    
    (setf 
     (b-box chord)
     (draw-chord (inside chord) middle-in-units 0 
                 (w view) (h view) 
                 (editor-get-edit-param editor :font-size) 
                 :staff (editor-get-edit-param editor :staff)
                 :draw-chans (editor-get-edit-param editor :channel-display)
                 :draw-vels (editor-get-edit-param editor :velocity-display)
                 :draw-ports (editor-get-edit-param editor :port-display)
                 :draw-durs (editor-get-edit-param editor :duration-display)
                 :selection (if (find chord (selection editor)) T 
                              (selection editor))
                 ))
    
    ;(draw-b-box chord)
    ))


;;;============ 
;;; INTERACTION
;;;============

(defun point-in-bbox (p bbox)
  (and (>= (om-point-x p) (car bbox))
       (<= (om-point-x p) (cadr bbox))
       (>= (om-point-y p) (caddr bbox))
       (<= (om-point-y p) (cadddr bbox))))
       
;;; supposing that sub-bounding-boxes are always included
(defun find-score-element-at-pos (editor object pos)
  (when (point-in-bbox pos (b-box object))
    (or (find pos (inside object) :test 'point-in-bbox :key 'b-box)
        object)))

(defmethod om-view-click-handler ((self score-view) position)
  
  (let* ((editor (editor self))
         (obj (object-value editor))
         (shift (calculate-staff-line-shift (editor-get-edit-param editor :staff)))
         (score-unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (click-y-in-units (- shift (/ (om-point-y position) score-unit)))
         (pitch (line-to-pitch click-y-in-units))) ;;; <= scale here ??? )
    
    (cond ((om-add-key-down)  ;;; add a note
           (store-current-state-for-undo editor)
           
           (let* ((new-note (make-instance 'note :midic pitch)))
             
             (setf (inside obj)
                   (sort (cons new-note
                               (inside obj))
                         '< :key 'midic))
                  
             (report-modifications editor)
             (om-invalidate-view self)
             
             (setf (selection editor) (list new-note))   ; (position p (point-list obj))

             (om-init-temp-graphics-motion 
              self position nil :min-move 10
              :motion #'(lambda (view pos)
                          (let* ((new-y-in-units (- shift (/ (om-point-y pos) score-unit)))
                                 (new-pitch (line-to-pitch new-y-in-units))
                                 (diff (- new-pitch pitch)))
                            (store-current-state-for-undo editor :action :move :item (selection editor))
                            (loop for n in (selection editor) do
                                  (setf (midic n) (+ (midic n) diff)))
                            (setf pitch new-pitch)
                            (editor-invalidate-views editor)
                            ))
              :release #'(lambda (view pos) 
                           (reset-undoable-editor-action editor)
                           (report-modifications editor))
              )
             ))
          
          ;; select
          (t (let ((selection (find-score-element-at-pos editor obj position)))
               
               (set-selection editor selection)
               (om-invalidate-view self)
               ;;; move the selection or select rectangle
               
               (if selection
                   
                   ;;; move it
                   (om-init-temp-graphics-motion 
                    self position nil :min-move 10
                    :motion #'(lambda (view pos)
                                (let* ((new-y-in-units (- shift (/ (om-point-y pos) score-unit)))
                                       (new-pitch (line-to-pitch new-y-in-units))
                                       (diff (- new-pitch pitch)))
                                  (store-current-state-for-undo editor :action :move :item (selection editor))
                                  (loop for n in (get-notes (selection editor)) do
                                        (setf (midic n) (+ (midic n) diff)))
                                  (setf pitch new-pitch)
                                  (editor-invalidate-views editor)
                                  ))
                      :release #'(lambda (view pos) 
                                   (reset-undoable-editor-action editor)
                                   (report-modifications editor))
                      )

                 ;;; no selection: start selection-rectangle
                 (om-init-temp-graphics-motion 
                  self position 
                  (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                  :min-move 10
                  :release #'(lambda (view position)
                               ;;; => selection IN RECTANGLE !!
                               (set-selection editor (find-score-element-at-pos editor obj position))                         
                               (om-invalidate-view view)
                               )
                  ))
               ))
          )
    ))

