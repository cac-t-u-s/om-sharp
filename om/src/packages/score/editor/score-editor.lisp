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


;;;=====================================
;;; SCORE EDITORS
;;;=====================================

;;; A specialized view for the display of SCORE-OBJECTs
(defclass score-view (OMEditorView) ())


(defmethod object-default-edition-params ((self score-object))
  '((:font-size 24)
    (:staff :gf)
    (:duration-display nil)
    (:velocity-display :hidden)
    (:channel-display :hidden)
    (:midiport-display nil)
    ))

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






;;;============ 
;;; CHORD EDITOR
;;;============

(defclass chord-editor (OMEditor undoable-editor-mixin) ())
(defmethod object-has-editor ((self chord)) t)
(defmethod get-editor-class ((self chord)) 'chord-editor)


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


;;; SPECIAL/SIMPLE CASE FOR CHORD-EDITOR
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





                