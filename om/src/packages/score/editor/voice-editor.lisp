;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
;;; the object must have been drawn
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
  (editor-set-edit-param self :time-map (build-time-map (object-value self)))
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



;;;======================
;;; ACTIONS
;;;======================


;;; MOVE / CHANGE DURS:

(defmethod move-editor-selection ((self voice-editor) &key (dx 0) (dy 0))
  (declare (ignore dx))
  (call-next-method self :dx 0 :dy dy))

(defmethod score-editor-change-selection-durs ((self voice-editor) delta) nil)

;;; TAB NAVIGATION:

(defmethod next-element-in-editor ((editor voice-editor) (element rhythmic-object))
  (car (inside element)))

(defmethod next-element-in-editor ((editor voice-editor) (element score-element))
  (let* ((seq (object-value editor))
         (pos (position element (get-all-chords seq))))
    (or (nth (1+ pos) (get-all-chords seq))
        (car (get-all-chords seq)))))


;;; TURN REST INTO A CHORD:

;;; return a new chord if the clicked object is a rest
(defmethod get-chord-from-editor-click ((self voice-editor) position) 
  
  (let ((chord (call-next-method))
        (voice (get-voice-at-pos self position)))
    
    (if chord 
        
        (when (typep chord 'r-rest)
      
          (let* ((time-pos (beat-to-time (symbolic-date chord) (tempo voice))))
        
            ;; make a real chord
            (change-class chord 'chord)
            (setf (onset chord) time-pos)
            (setf (notes chord) nil)
        
            ;; insert in teh actual sequence
            (time-sequence-insert-timed-item-and-update voice chord (find-position-at-time voice time-pos))
        
            ;; compute new tree / rebuild the structure
            (set-tree voice (build-tree voice nil))
            ))
      
      ;;; little hacked hooked-in here: add a measure 
      (let ((time-pos (pixel-to-time (get-g-component self :main-panel) (om-point-x position)))
            (click-on-measure 
             (find-if #'(lambda (element)
                          (and (typep element 'measure)
                               (b-box element)
                               (>= (om-point-x position) (b-box-x1 (b-box element)))
                               (<= (om-point-x position) (b-box-x2 (b-box element)))))
                      (inside voice))))
        
        (let ((pos (cond (click-on-measure (position click-on-measure (inside voice)))
                         ((>= time-pos (get-obj-dur voice)) (length (inside voice)))  ;;; click at the end of the voice: add measure at the end
                         (t nil))))
          (when pos

            (let ((m (make-instance 'measure :tree '((4 4) (-1))
                                    :symbolic-dur 1
                                    :symbolic-date 0
                                    )))
              (setf (inside m) (list (make-instance 'r-rest :symbolic-dur 1 :symbolic-date 0)))
              (insert-in-voice voice m pos) 
              (report-modifications self)
              ))
          )
        )
      )
    chord))


(defmethod tie-untie-selection ((editor voice-editor))
  (let ((selected-chords (loop for elt in (selection editor) append 
                               (get-tpl-elements-of-type elt '(chord continuation-chord)))))
    (when selected-chords
      (store-current-state-for-undo editor)
  
      (loop for v in (get-voices (object-value editor)) do
            (let ((selected-chords-in-v (remove-if-not #'(lambda (c) (find c (get-all-chords v))) selected-chords)))
              (when selected-chords-in-v
                (tie-untie v selected-chords-in-v))))
        
      (set-selection editor nil)
        
      (report-modifications editor))))


(defmethod group-selection ((editor voice-editor))
  (let ((selected-chords (loop for elt in (selection editor) append 
                               (get-tpl-elements-of-type elt '(chord continuation-chord r-rest)))))
    (when selected-chords
      (store-current-state-for-undo editor)
  
      (loop for v in (get-voices (object-value editor)) do
            (let ((selected-chords-in-v (remove-if-not #'(lambda (c) (find c (get-all-chords v))) selected-chords)))
              (when selected-chords-in-v
                (group v selected-chords-in-v))))
        
      (set-selection editor nil)
        
      (report-modifications editor))))


(defmethod subdivide-selection ((editor voice-editor) n)
  (let ((selected-chords (loop for elt in (selection editor) append 
                               (get-tpl-elements-of-type elt '(chord continuation-chord r-rest)))))
    (when selected-chords
      (store-current-state-for-undo editor)
  
      (loop for v in (get-voices (object-value editor)) do
            (let ((selected-chords-in-v (remove-if-not #'(lambda (c) (find c (get-all-chords v))) selected-chords)))
              (when selected-chords-in-v
                (subdivide v selected-chords-in-v n))))
        
      (set-selection editor nil)
        
      (report-modifications editor))))


(defmethod break-selection ((editor voice-editor))
  (let ((selected-groups (loop for elt in (selection editor) append 
                               (get-tpl-elements-of-type elt '(group)))))
    (when selected-groups
      (store-current-state-for-undo editor)
  
      (loop for v in (get-voices (object-value editor)) do
            (let ((selected-groups-in-v (remove-if-not #'(lambda (g) (deep-search v g)) selected-groups)))
              (when selected-groups-in-v
                (break-groups v selected-groups-in-v))))
        
      (set-selection editor nil)
        
      (report-modifications editor))))


(defmethod editor-key-action ((editor voice-editor) key)
  (cond         
   ; tie/untie
   ((equal key #\=) 
    (tie-untie-selection editor))
   
   ; group
   ((equal key #\+) 
    (group-selection editor))
   
   ; break
   ((equal key #\-) 
    (break-selection editor))
   
   ; subdivise
   ((member key '(#\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
    (let ((num (read-from-string (string key))))
      (subdivide-selection editor num)
      ))
    
   ; otherwise
   (t (call-next-method)) ;;; => score-editor
   ))


;;; disabled from chord-seq editor:
(defmethod stems-on-off ((self voice-editor)) nil)
(defmethod align-chords-in-editor ((self voice-editor)) nil)


;;;======================
;;; DRAW SPECIFICS
;;;======================

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
    
    (when (listp staff)
      (setf staff (or (nth (position object (get-voices (object-value editor))) staff)
                      (car staff))))
    
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





           