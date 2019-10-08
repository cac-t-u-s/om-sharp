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
                                             (editor-set-edit-param editor :h-stretch (om-get-selected-item list))
                                             (build-editor-window editor)
                                             (init-editor-window editor) ;;; will change the ruler
                                             (editor-update-ruler editor)))
                  ))
                
                NIL
                ))
    nil
    )))
             


;;; no cut from voice
(defmethod cut-command ((self voice-editor)) 
  #'(lambda () (om-beep)))
  

(defmethod score-editor-paste ((self voice-editor) elements)

  (let* ((view (get-g-component self :main-panel))
         (paste-pos (get-paste-position view))
         (target-voice (cond ((typep (car (selection self)) 'voice)
                              (car (selection self)))
                             ((typep (car (selection self)) 'measure)
                              (find-if #'(lambda (v) (find (car (selection self)) (inside v)))
                                       (get-all-voices self)))
                             (t 
                              (get-voice-at-pos self (or paste-pos (omp 0 0)))))))
    
    (cond 
     ;;; all selection are measures
     ((list-subtypep elements 'measure)
      ;;; insert measures before selected measure (if any) or at the end
      (let* (
             (mesure (find 'measure (selection self) :key #'type-of))
             (pos-in-voice (if mesure (position mesure (inside target-voice))
                             (length (inside target-voice)))))
           
        (store-current-state-for-undo self)
        (insert-in-voice target-voice (mapcar #'om-copy elements) pos-in-voice)
        (report-modifications self)
        ))
          
     ;; otherwise collect all chords and apply to selection in voice
     (t
      (let ((copied-chords (sort (loop for item in elements append (get-tpl-elements-of-type item 'chord)) #'< :key #'onset))
            (target-chords (if (selection self) 
                               (sort (loop for item in (selection self) append (get-tpl-elements-of-type item 'chord)) #'< :key #'onset)
                             (get-tpl-elements-of-type target-voice 'chord))))
        
        (store-current-state-for-undo self)
        
        (loop for c in copied-chords
              for target in target-chords do 
              (do-initialize  target 
                             :lmidic (lmidic c)
                             :lvel (lvel c)
                             :lchan (lchan c)
                             :lport (lport c)
                             ;;; dur/offset are kept from original chord
                             :ldur (ldur target)
                             :loffset (loffset target)
                             )
              )
        (report-modifications self)
        t))
     )))


;;; redo time-map at editing the object
(defmethod report-modifications ((self voice-editor))
  (call-next-method)
  (editor-set-edit-param self :time-map (build-time-map (object-value self)))
  (editor-update-ruler self))


(defmethod note-dur-edit-allowed ((self voice-editor)) nil)

;;; voice editor has a different ruler
(defclass voice-ruler (time-ruler) ())

;;; a "fake" negative duration to allow for teh first time-signature to show
(defmethod data-stream-get-x-ruler-vmin ((self voice-editor)) -1400)

(defmethod set-ruler-range ((self voice-ruler) v1 v2) 
  (let* ((panel (car (related-views self))))
    (call-next-method self v1 (pixel-to-time panel 
                                             (+ (time-to-pixel panel v1)
                                                (om-width panel))))))

(defmethod editor-update-ruler ((self voice-editor))
  (let ((ruler (get-g-component self :x-ruler)))
    (set-ruler-range ruler
                     (v1 ruler) 
                     (v2 ruler))
    ))

(defmethod init-editor-window :after ((self voice-editor))
  (editor-update-ruler self))

(defmethod make-time-ruler ((editor voice-editor) dur)
  (om-make-view (if (numberp (editor-get-edit-param editor :h-stretch))
                    'voice-ruler 'time-ruler)
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
  ;;(not (numberp (editor-get-edit-param (editor (car (related-views self))) :h-stretch)))
  nil)


(defmethod move-editor-selection ((self voice-editor) &key (dx 0) (dy 0))
  (declare (ignore dx))
  (call-next-method self :dx 0 :dy dy))

(defmethod score-editor-change-selection-durs ((self voice-editor) delta) nil)

;;; TODO: do something with rhythms ?
(defmethod score-editor-delete ((self voice-editor) element) (call-next-method))



(defmethod draw-tempo-in-editor-view ((editor voice-editor) (self score-view))

  (let* ((y-shift (editor-get-edit-param editor :y-shift))
         (font-size (editor-get-edit-param editor :font-size))
         (unit (font-size-to-unit font-size)))
    
   (draw-tempo (object-value editor) (* 2 unit) (* y-shift unit) font-size)
   ))


(defmethod draw-sequence ((object voice) editor view unit &optional force-y-shift)

  ;;; NOTE: so far we don't build/update a bounding-box for the containers
  
  (let* ((font-size (editor-get-edit-param editor :font-size))
         (staff (editor-get-edit-param editor :staff))
         (h-stretch (editor-get-edit-param editor :h-stretch))
         (y-u (or force-y-shift (editor-get-edit-param editor :y-shift)))
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
                                 :y-shift y-u
                                 :with-signature (not (equal (car (tree m)) prev-signature))
                                 :selection selection
                                 :staff staff
                                 :stretch h-stretch
                                 :font-size font-size
                                 )
                   ))
               (setf prev-signature (car (tree m)))
               )
          ))
  )





           