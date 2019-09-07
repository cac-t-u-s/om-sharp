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


;;;===========================================
;;; SCORE EDITORS (GENERAL/SHARED FEATURES)
;;;===========================================

(defclass score-editor (OMEditor undoable-editor-mixin) ())

;;; these params are shared between the editor and the box
(defmethod object-default-edition-params ((self score-object))
  '((:font-size 24)
    (:staff :g)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    ;; (:time-map ((-1 -1) (0 0)))    ;;; not necessary to store it as a persistent param....
    (:h-stretch 1)
    (:y-shift 4)))


;;; only chord-seq-editor allows to edit time
(defmethod edit-time-? ((self score-editor)) nil)

;;;============ 
;;; SCORE VIEW
;;;============

;;; A specialized view for the display of SCORE-OBJECTs
(defclass score-view (OMEditorView) 
  ((margin-l :accessor margin-l :initarg :margin-l :initform 1)
   (margin-r :accessor margin-r :initarg :margin-r :initform 1)
   (keys :accessor keys :initarg :keys :initform t)
   (contents :accessor contents :initarg :contents :initform t)))


;;;======================== 
;;; TIME/SPACE CONVERSIONS
;;;========================

(defmethod time-to-pixel ((self score-view) time) 
  (let* ((ed (editor self))
         (stretch (editor-get-edit-param ed :h-stretch))
         (time-map (editor-get-edit-param ed :time-map)))
    
    ;(print (list ed time-map stretch))
    (if (and time-map (numberp stretch)) ;;; in principle only VOICEs have a time-map
        
        (let ((adjusted-stretch-factor (* stretch (font-size-to-unit (editor-get-edit-param ed :font-size)))))
          
          (* adjusted-stretch-factor
             (- 
              (score-time-to-units time-map time)
              (score-time-to-units time-map (x1 self)))))

      (call-next-method))))


;;; used afaik only when we click on the view to re-position the play cursor
;;; DOES NOT WORK !!
(defmethod pixel-to-time ((self score-view) pixel)
 (let* ((ed (editor self))
        (stretch (editor-get-edit-param ed :h-stretch))
        (time-map (editor-get-edit-param ed :time-map)))  
   
   ;; (print time-map)

   (if (and time-map (numberp stretch))
       
        (let* ((unit (font-size-to-unit (editor-get-edit-param ed :font-size)))
               (pixels-from-0 (- pixel (time-to-pixel self 0)))
               (0-in-units (/ (time-to-pixel self 0) (* stretch unit)))
               (pos-in-units (/ pixels-from-0 (* stretch unit))))
          
          ;; (print pos-in-units) ;;; <= THIS VALUE IS WRONG !!
          
          (score-units-to-time time-map pos-in-units))
          
     (call-next-method))))

;;; for simple views that are not x-graduated (e.g. chord)
(defmethod pixel-to-time ((self om-view) x) 0)
   
;;;======================== 
;;; DISPLAY
;;;========================


(defmethod om-draw-contents ((self score-view))
  
  (let* ((editor (editor self))
         (obj (object-value editor)) 
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size))))
    
    (om-trap-errors 
     
     ;(om-with-fg-color (if (find obj (selection editor)) *score-selection-color*)  ; (om-make-color 0.0 0.2 0.2)
       
     (draw-staff 0 0 
                 (editor-get-edit-param editor :y-shift)
                 (w self) (h self) 
                 (editor-get-edit-param editor :font-size) 
                 (editor-get-edit-param editor :staff)
                 :margin-l (margin-l self) 
                 :margin-r (margin-r self)
                 :keys (keys self))
     ;)
     
       (when (contents self)
         (draw-score-object-in-editor-view editor self unit))
       
       )
    ))




;;;============ 
;;; INTERACTION
;;;============

(defun point-in-bbox (p bbox)
  (and (>= (om-point-x p) (b-box-x1 bbox))
       (<= (om-point-x p) (b-box-x2 bbox))
       (>= (om-point-y p) (b-box-y1 bbox))
       (<= (om-point-y p) (b-box-y2 bbox))))

(defun bbox-in-rect (bbox x1 y1 x2 y2)
  (and (<= x1 (b-box-x1 bbox))
       (>= x2 (b-box-x2 bbox))
       (<= y1 (b-box-y1 bbox))
       (>= y2 (b-box-y2 bbox))))
       
       
;;; supposing that sub-bounding-boxes are always included
(defmethod find-score-element-at-pos ((object note) pos)
  (and (b-box object) 
       (point-in-bbox pos (b-box object))
       object))

;;; in measure the "selectable" bounding box does not contain the internal element's bounding boxes
(defmethod find-score-element-at-pos ((object measure) pos)
  (if (and (b-box object) (point-in-bbox pos (b-box object)))
      object 
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      found)))

(defmethod find-score-element-at-pos ((object score-object) pos)
  
  (cond 
   ((null (b-box object)) ;;; the object itself has no bounding box (yet?) or, we have clicked outside
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      found))
   
   ((point-in-bbox pos (b-box object)) ;; the object has one, and we're inside 
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      (or found object)))
  
   (t NIL) ;;; not here...
   ))


(defmethod find-score-elements-in-area ((object score-object) x1 y1 x2 y2)
        
   (if (and (b-box object) 
            (bbox-in-rect (b-box object) x1 y1 x2 y2)) ;; the object has one, and it's inside 
       object ;;; the whole object is selected
     ;;; else: go check in the children
     (remove nil 
             (flat 
              (loop for elem in (inside object) ;; check its children..
                    collect (find-score-elements-in-area elem x1 y1 x2 y2)))) 
     ))

;;; measure is special: it is selected only by clickking on the bar/signature
(defmethod find-score-elements-in-area ((object measure) x1 y1 x2 y2)      
  (flat 
   (loop for elem in (inside object) ;; check its children..
         collect (find-score-elements-in-area elem x1 y1 x2 y2))))



;;;======================
;;; MOUSE ACTIONS
;;;======================

(defmethod score-object-update ((self t)) nil)

(defmethod score-object-update ((self time-sequence))  
  (time-sequence-reorder-timed-item-list self)
  (time-sequence-update-obj-dur self))


;;; overrides data-stream-panel in chord-seq-editor
(defmethod om-view-mouse-motion-handler ((self score-view) position)
  (position-display (editor self) position))


(defmethod om-view-click-handler ((self score-view) position)
  
  (let* ((editor (editor self))
         (obj (object-value editor))
         (staff (editor-get-edit-param editor :staff))
         (y-shift (editor-get-edit-param editor :y-shift))
         (shift (+ (calculate-staff-line-shift staff) y-shift))
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (clicked-pos position)
         (click-y-in-units (- shift (/ (om-point-y position) unit)))
         (clicked-pitch (line-to-pitch click-y-in-units)) ;;; <= scale here ??? )
         (clicked-time (pixel-to-time self (om-point-x position))))
    
    (cond ((om-add-key-down)  ;;; add a note
           (store-current-state-for-undo editor)
           
           (let ((container-chord (get-chord-from-editor-click editor position)))
             
             (when container-chord
               (let ((new-note (make-instance 'note :midic clicked-pitch)))
                 
                 ;;; set to the same dur as others (important in voices)
                 (when (notes container-chord)
                   (setf (dur new-note) (list-max (ldur container-chord))))
                 
                 (setf (notes container-chord)
                       (sort (cons new-note
                                   (notes container-chord))
                             '< :key 'midic))
             
                 ;;; some updates of the time-sequence required here
                 (score-object-update obj)
                 (report-modifications editor)
                 (editor-invalidate-views editor)
             
                 (if (or (= 1 (length (notes container-chord)))
                         (find container-chord (selection editor)))
                     (setf (selection editor) (list container-chord))
                   (setf (selection editor) (list new-note)))
             
                 (om-init-temp-graphics-motion 
                  self position nil :min-move 1
                  :motion #'(lambda (view pos)
                              (declare (ignore view))
                              (let* ((new-y-in-units (- shift (/ (om-point-y pos) unit)))
                                     (new-pitch (line-to-pitch new-y-in-units))
                                     (diff (- new-pitch clicked-pitch)))
                                (unless (zerop diff)
                                  (store-current-state-for-undo editor :action :move :item (selection editor))
                                  (loop for n in (get-notes (selection editor)) do
                                        (setf (midic n) (+ (midic n) diff)))
                                  (setf clicked-pitch new-pitch)
                                  (om-invalidate-view self))
                                ))
                  :release #'(lambda (view pos) 
                               (declare (ignore view pos))
                               (reset-undoable-editor-action editor)
                               (report-modifications editor))
                  )
                 ))))
          
          ;; select
          (t (let ((selection (find-score-element-at-pos obj position)))
               
               (set-selection editor selection)
               (editor-invalidate-views editor)
               
               ;;; move the selection or select rectangle...
               
               (if selection
                   
                   ;;; move it
                   (let ((modif nil))
                     (om-init-temp-graphics-motion 
                      self position nil :min-move 1
                      :motion #'(lambda (view pos)
                                  (declare (ignore view))
                                  
                                  (store-current-state-for-undo editor :action :move)
                                  
                                  (let ((x-move (- (om-point-x pos) (om-point-x clicked-pos)))
                                        (y-move (- (om-point-y pos) (om-point-y clicked-pos))))

                                    (if (and (edit-time-? editor)
                                             (> (abs x-move) (abs y-move)))
                                        
                                        (let* ((new-time (pixel-to-time self (om-point-x pos)))
                                               (diff (- new-time clicked-time)))
                                          (unless (zerop diff)
                                            (setf modif t)
                                            (store-current-state-for-undo editor :action :move :item (selection editor))
                                            ;;; remove-duplicates: continuation chords refer to existing notes !
                                            (loop for c in (remove-duplicates 
                                                            (remove-if-not #'(lambda (obj) (typep obj 'chord))
                                                                           (selection editor)))
                                                  do (item-set-time c (+ (item-get-time c) diff)))
                                            (setf clicked-time new-time)
                                            (editor-invalidate-views editor)
                                            )
                                          )

                                      (let* ((new-y-in-units (- shift (/ (om-point-y pos) unit)))
                                             (new-pitch (line-to-pitch new-y-in-units))
                                             (diff (- new-pitch clicked-pitch)))
                                        (unless (zerop diff)
                                          (setf modif t)
                                          (store-current-state-for-undo editor :action :move :item (selection editor))
                                          ;;; remove-duplicates: continuation chords refer to existing notes !
                                          (loop for n in (remove-duplicates (get-notes (selection editor))) 
                                                do (setf (midic n) (+ (midic n) diff)))
                                          (setf clicked-pitch new-pitch)
                                          (editor-invalidate-views editor)
                                          )))
                                    (setf clicked-pos pos)
                                    ))

                      :release #'(lambda (view pos) 
                                   (declare (ignore view pos))
                                   (when modif
                                     (score-object-update obj)
                                     (reset-undoable-editor-action editor)
                                     (report-modifications editor)))
                      ))

                 ;;; no selection: start selection-rectangle
                 (om-init-temp-graphics-motion 
                  self position 
                  (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                  :min-move 10
                  :release #'(lambda (view end-pos)
                               (let ((x1 (min (om-point-x position) (om-point-x end-pos)))
                                     (x2 (max (om-point-x position) (om-point-x end-pos)))
                                     (y1 (min (om-point-y position) (om-point-y end-pos)))
                                     (y2 (max (om-point-y position) (om-point-y end-pos))))
                                 (set-selection editor (find-score-elements-in-area obj x1 y1 x2 y2))                         
                                 (om-invalidate-view view)
                                 ))
                  )
                 )
               ))
          )
    ))


(defmethod om-view-zoom-handler ((self score-view) position zoom)
  (let ((editor (editor self))
        (d-size (if (> zoom 1) 1 -1))) 
    (set-font-size editor (+ d-size (editor-get-edit-param editor :font-size)))
    ))

;;;======================
;;; KEYBOARD ACTIONS
;;;======================

;;; to be specialized for specific types of editor/objects 
(defmethod score-editor-delete ((self score-editor) element) nil) 
(defmethod score-editor-change-selection-durs ((self score-editor) delta) nil)


(defmethod editor-key-action ((editor score-editor) key)
  
  (case key
    
    (:om-key-left 
     (if (om-option-key-p)
         (progn 
           (store-current-state-for-undo editor)
           (score-editor-change-selection-durs editor (if (om-shift-key-p) -1000 -100))
           (editor-invalidate-views editor)
           (report-modifications editor))
       (call-next-method)))
         
    (:om-key-right 
     (if (om-option-key-p)
         (progn 
           (store-current-state-for-undo editor)
           (score-editor-change-selection-durs editor (if (om-shift-key-p) 1000 100))
           (editor-invalidate-views editor)
           (report-modifications editor))
       (call-next-method)))
    
    ;;; if not with "option", left/right for chord-seq are handled in data-stream-editor only
    
    (:om-key-up
     (store-current-state-for-undo editor)
     (move-editor-selection editor :dy (if (om-shift-key-p) 12 1))
     (editor-invalidate-views editor)
     (report-modifications editor))

    (:om-key-down
     (store-current-state-for-undo editor)
     (move-editor-selection editor :dy (if (om-shift-key-p) -12 -1))
     (editor-invalidate-views editor)
     (report-modifications editor))

    (:om-key-delete
     (when (selection editor)
       (store-current-state-for-undo editor)
       (loop for element in (selection editor) do 
             (score-editor-delete editor element))
       (setf (selection editor) nil)
       (editor-invalidate-views editor)
       (report-modifications editor)))
   
    (otherwise 
     (call-next-method))
    ))




;;;====================== 
;;; CONTROL PANEL
;;;======================

(defmethod set-font-size ((self score-editor) size)
  (let ((v (min 120 (max 8 size))))
    (editor-set-edit-param self :font-size v)
    (when (get-g-component self :font-size-box)
      (om-set-selected-item (get-g-component self :font-size-box) v))))


(defmethod make-score-display-params-controls ((editor score-editor)) 
  
  (om-make-layout 
   'om-row-layout 
   :align :center ; :size (omp 100 100)
           ;:ratios '(1 1 nil) 
   :subviews
   (list 
    (om-make-layout 
     'om-column-layout
     :subviews (list 
                        
                (om-make-di 'om-simple-text :text "Display params" 
                            :size (omp 140 22) ;; :bg-color (om-def-color :red)
                            :font (om-def-font :font2b))

                (om-make-layout 
                 'om-row-layout :align :center
                 :subviews 
                 (list 
                  (om-make-di 'om-simple-text :text "staff:" 
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
                  (om-make-di 'om-simple-text :text "font-size:" 
                              :font (om-def-font :font1)
                              :size (omp 60 20))

                  (set-g-component editor :font-size-box
                                   (om-make-di 'om-popup-list :items *score-fontsize-options* 
                                               :size (omp 60 24) :font (om-def-font :font1)
                                               :value (editor-get-edit-param editor :font-size)
                                               :di-action #'(lambda (list) 
                                                              (set-font-size editor (om-get-selected-item list))
                                                              )))
                  ))
                ))


    (om-make-layout 
     'om-column-layout
     :subviews (list 
                        
                nil

                (om-make-layout 
                 'om-row-layout :align :center
                 :subviews 
                 (list 
                  (om-make-di 'om-simple-text :text "velocity:" 
                              :size (omp 50 20) 
                              :font (om-def-font :font1))
                  (om-make-di 'om-popup-list :items '(:hidden :value :symbol :size :alpha) 
                              :size (omp 80 24) :font (om-def-font :font1)
                              :value (editor-get-edit-param editor :velocity-display)
                              :di-action #'(lambda (list) 
                                             (editor-set-edit-param editor :velocity-display (om-get-selected-item list))))
                  ))
                        
                (om-make-di 'om-check-box :text "duration" :font (om-def-font :font1)
                            :size (omp 68 20) 
                            :checked-p (editor-get-edit-param editor :duration-display)
                            :di-action #'(lambda (item) 
                                           (editor-set-edit-param editor :duration-display (om-checked-p item))))
                        
                ))
    (om-make-layout 
     'om-column-layout
     :subviews (list 

                nil
                        
                (om-make-layout 
                 'om-row-layout :align :center
                 :subviews 
                 (list 
                  (om-make-di 'om-simple-text :text "MIDI channel:" 
                              :size (omp 68 20) 
                              :font (om-def-font :font1))
                  (om-make-di 'om-popup-list :items '(:hidden :number :color :color-and-number) 
                              :size (omp 80 24) :font (om-def-font :font1)
                              :value (editor-get-edit-param editor :channel-display)
                              :di-action #'(lambda (list) 
                                             (editor-set-edit-param editor :channel-display (om-get-selected-item list))))
                  ))
                        
                (om-make-di 'om-check-box :text "MIDI port" :font (om-def-font :font1)
                            :size (omp 68 20) 
                            :checked-p (editor-get-edit-param editor :port-display)
                            :di-action #'(lambda (item) 
                                           (editor-set-edit-param editor :port-display (om-checked-p item))))

                        
                ))

    )
           
   ))





                