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

(defmethod object-default-edition-params ((self score-object))
  '((:font-size 24)
    (:staff :gf)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    ))



;;;============ 
;;; SCORE VIEW
;;;============

;;; A specialized view for the display of SCORE-OBJECTs
(defclass score-view (OMEditorView) 
  ((margin-l :accessor margin-l :initarg :margin-l :initform 1)
   (margin-r :accessor margin-r :initarg :margin-r :initform 1)
   (keys :accessor keys :initarg :keys :initform t)
   (contents :accessor contents :initarg :contents :initform t)))



(defmethod om-draw-contents ((self score-view))
  
  (let* ((editor (editor self))
         (unit (font-size-to-unit (editor-get-edit-param editor :font-size))))
    
    (om-trap-errors 
     ;(om-with-fg-color (om-make-color 0.0 0.2 0.2)
       
       (draw-staff 0 0 
                   (w self) (h self) 
                   (editor-get-edit-param editor :font-size) 
                   (editor-get-edit-param editor :staff)
                   :margin-l (margin-l self) :margin-r (margin-r self)
                   :keys (keys self))
       
       (when (contents self)
         (draw-score-object-in-editor-view editor self unit))
       
       ;)
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
       
;;; supposing that sub-bounding-boxes are always included
(defmethod find-score-element-at-pos ((object note) pos)
  (and (b-box object) 
       (point-in-bbox pos (b-box object))
       object))

(defmethod find-score-element-at-pos ((object score-object) pos)
  
  (cond 
  
   ((null (b-box object)) ;;; the object itself has no bounding box (yet?)
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      found))
          
   ((point-in-bbox pos (b-box object))  ;; the object has one, and we're inside 
    (let ((found nil))
      (loop for elem in (inside object) ;; check its children..
            while (not found)
            do (setf found (find-score-element-at-pos elem pos)))
      (or found object)))
   
   (t NIL) ;;; not here...
   ))

;;; overrides data-stream-panel
(defmethod om-view-mouse-motion-handler ((self score-view) position)
  (position-display (editor self) position))


(defmethod om-view-click-handler ((self score-view) position)
  
  (let* ((editor (editor self))
         (obj (object-value editor))
         (shift (calculate-staff-line-shift (editor-get-edit-param editor :staff)))
         (score-unit (font-size-to-unit (editor-get-edit-param editor :font-size)))
         (click-y-in-units (- shift (/ (om-point-y position) score-unit)))
         (pitch (line-to-pitch click-y-in-units))) ;;; <= scale here ??? )
    
    (cond ((om-add-key-down)  ;;; add a note
           (store-current-state-for-undo editor)
           
           (let* ((new-note (make-instance 'note :midic pitch))
                  (container-chord (get-chord-from-editor-click editor position)))
             
             (setf (notes container-chord)
                   (sort (cons new-note
                               (notes container-chord))
                         '< :key 'midic))
             ;;; probably some updates of the time-sequence required here.. (?)
             
             (report-modifications editor)
             (editor-invalidate-views editor)
             
             (if (or (= 1 (length (notes container-chord)))
                     (find container-chord (selection editor)))
                 (setf (selection editor) (list container-chord))
               (setf (selection editor) (list new-note)))

             (om-init-temp-graphics-motion 
              self position nil :min-move 1
              :motion #'(lambda (view pos)
                          (let* ((new-y-in-units (- shift (/ (om-point-y pos) score-unit)))
                                 (new-pitch (line-to-pitch new-y-in-units))
                                 (diff (- new-pitch pitch)))
                            (store-current-state-for-undo editor :action :move :item (selection editor))
                            (loop for n in (get-notes (selection editor)) do
                                  (setf (midic n) (+ (midic n) diff)))
                            (setf pitch new-pitch)
                            (om-invalidate-view self)
                            ))
              :release #'(lambda (view pos) 
                           (reset-undoable-editor-action editor)
                           (report-modifications editor))
              )
             ))
          
          ;; select
          (t (let ((selection (find-score-element-at-pos obj position)))
               
               (set-selection editor selection)
               (om-invalidate-view self)
               ;;; move the selection or select rectangle
               
               (if selection
                   
                   ;;; move it
                   (om-init-temp-graphics-motion 
                    self position nil :min-move 1
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
                               (set-selection editor (find-score-element-at-pos obj position))                         
                               (om-invalidate-view view)
                               )
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
;;; CONTROL PANEL
;;;======================

(defmethod set-font-size ((self score-editor) size)
  (let ((v (min 120 (max 8 size))))
    (editor-set-edit-param self :font-size v)
    (when (get-g-component self :font-size-box)
      (om-set-selected-item (get-g-component self :font-size-box) v))))

(defun make-score-control-panel (editor) 
  
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
                  ;(set-g-component editor :font-size-box
                  ;                 (om-make-graphic-object 'numbox 
                  ;                                         :value (editor-get-edit-param editor :font-size)
                  ;                                         :min-val 8 :max-val 120 
                  ;                                         :size (omp 40 18)
                  ;                                         :font (om-def-font :font1)
                  ;                                         :bg-color (om-def-color :white)
                  ;                                         :after-fun #'(lambda (numbox) 
                  ;                                                        (set-font-size editor (value numbox))
                  ;                                                        )))
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





                