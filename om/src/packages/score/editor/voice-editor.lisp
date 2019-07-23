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

;;;======================================================================== 
;;; VOICE EDITOR
;;;========================================================================

(defclass voice-editor (chord-seq-editor) ())
(defmethod get-editor-class ((self voice)) 'voice-editor)

(defmethod edit-time-? ((self voice-editor)) nil)
(defmethod add-chords-allowed ((self voice-editor)) nil)

(defparameter *stretch-options*
  '(.25 .5 .75 1 1.5 2 4 :proportional))

(defmethod make-score-display-params-controls ((editor voice-editor)) 
  
  (om-make-layout  
   'om-row-layout 
   :subviews
   (list 

    (call-next-method)
    
    (om-make-view 'om-view :size (omp 50 10))

    (om-make-layout 
     'om-column-layout
     :subviews (list 
                
                NIL

                (om-make-layout 
                 'om-row-layout :align :center
                 :subviews 
                 (list 
                 
                  (om-make-di 'om-simple-text :text "h-stretch" 
                              :size (omp 50 20) 
                              :font (om-def-font :font1))

                  (om-make-di 'om-popup-list :items *stretch-options* 
                              :size (omp 80 24) :font (om-def-font :font1)
                              :value (editor-get-edit-param editor :h-stretch)
                              :di-action #'(lambda (list) 
                                             (editor-set-edit-param editor :h-stretch (om-get-selected-item list))))
                  ))
                
                NIL
                ))
    )))
             


;;; voice editor has a different ruler
(defclass voice-ruler (time-ruler) ())

;;; a "fake" negative duration to allow for teh first time-signature to show
(defmethod data-stream-get-x-ruler-vmin ((self voice-editor)) -2000)


(defmethod make-time-ruler ((editor voice-editor) dur)
  (om-make-view 'voice-ruler 
                :related-views (get-g-component editor :data-panel-list)
                :size (omp nil 20) 
                :bg-color (om-def-color :light-gray)
                :vmin (data-stream-get-x-ruler-vmin editor)
                :x1 (data-stream-get-x-ruler-vmin editor) 
                :x2 dur))


(defmethod time-to-pixel ((self voice-ruler) time) 
  (time-to-pixel (car (related-views self)) time))

(defmethod ruler-value-to-pix ((self voice-ruler) v) 
  (time-to-pixel (car (related-views self)) v))

(defmethod ruler-zoom-? ((self voice-ruler)) 
  ;;; if h-stretch is a number, we are not in proportional view
  (not (numberp (editor-get-edit-param (editor (car (related-views self))) :h-stretch))))


(defmethod draw-sequence ((object voice) editor view unit)

  ;;; NOTE: so far we don't build/update a bounding-box for the containers
  
  (let* ((time-map (editor-get-edit-param editor :time-map))
         (font-size (editor-get-edit-param editor :font-size))
         (staff (editor-get-edit-param editor :staff))
         (h-stretch (editor-get-edit-param editor :h-stretch))
         (y-shift (editor-get-edit-param editor :y-shift))
         (unit (font-size-to-unit font-size))
         (selection (selection editor)))
    
    (loop with on-screen = t
          with prev-signature = nil
          for m in (inside object)
          for i from 1
          while on-screen
          do (let* 
                 ((begin (beat-to-time (symbolic-date m) (tempo object)))
                  (end (beat-to-time (+ (symbolic-date m) (symbolic-dur m)) (tempo object)))
                  (x1 (time-to-pixel view begin))
                  (x2 (time-to-pixel view end)))
               
               (if (> x1 (w view)) (setf on-screen nil) ;;; this should also take into account measures before screen
                 ;;; else :
                 (when (> x2 0) 
                   ;;; DRAW THIS MEASURE
                   (draw-measure m (tempo object) (object editor) view 
                                 :position i
                                 :y-shift y-shift
                                 :with-signature (not (equal (car (tree m)) prev-signature))
                                 :selection selection
                                 :staff staff
                                 :stretch h-stretch
                                 :font-size font-size
                                 :time-map time-map)
                   ))
               (setf prev-signature (car (tree m)))
               )
          ))
  )





           