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

;;;======================================================================== 
;;; CHORD-SEQ EDITOR
;;;========================================================================

(defclass chord-seq-editor (data-stream-editor undoable-editor-mixin) ())
(defmethod get-editor-class ((self chord-seq)) 'chord-seq-editor)

(defclass score-panel (score-view stream-panel) ()
  (:default-initargs :keys nil :margin-l nil :margin-r 1))

(defmethod editor-view-class ((self chord-seq-editor)) 'score-panel)


;;; this will just disable the display-mode menu 
(defmethod frame-display-modes-for-object ((self data-stream-editor) (object score-object)) '(:chords))

(defmethod object-default-edition-params ((self chord-seq))
  (append (call-next-method)
          '((:grid nil))))

(defmethod make-left-panel-for-object ((editor chord-seq-editor) (object score-object))
  (om-make-view 'score-view :size (omp (* 2 (editor-get-edit-param editor :font-size)) nil)
                :direct-draw t 
                :bg-color (om-def-color :white) 
                :scrollbars nil
                :editor editor
                :margin-l 1 :margin-r nil :keys t :contents nil
                ))


;;; leave some space (-200) for the first note...
(defmethod default-editor-x-range ((self chord-seq-editor))
  (let ((play-obj (get-obj-to-play self))) 
    (if play-obj
        (list -200 (+ (get-obj-dur play-obj) (default-editor-min-x-range self)))
      (list (vmin self) (or (vmax self) (default-editor-min-x-range self))))))


(defmethod update-to-editor ((editor chord-seq-editor) (from t))
  (call-next-method)
  (editor-invalidate-views editor))

(defmethod editor-invalidate-views ((self chord-seq-editor))
  (om-invalidate-view (main-view self))) ;; brutal..


;;; called at add-click
(defmethod get-chord-from-editor-click ((self chord-seq-editor) position) 
 
  (let ((time-seq (object-value self))
        (time-pos (pixel-to-time (get-g-component self :main-panel) (om-point-x position))))
    
    (or 
     ;;; there's a selected chord near the click
     (find-if #'(lambda (element)
                  (and (typep element 'chord)
                       (b-box element)
                       (>= (om-point-x position) (b-box-x1 (b-box element)))
                       (<= (om-point-x position) (b-box-x2 (b-box element)))))
              (selection self))
     ;;; make a new chord
     (let ((new-chord (time-sequence-make-timed-item-at time-seq time-pos)))
       (setf (inside new-chord) nil)
       (time-sequence-insert-timed-item-and-update time-seq new-chord (find-position-at-time time-seq time-pos))
       new-chord)
     
     )))


(defmethod make-editor-window-contents ((editor chord-seq-editor))
  
  (let* ((obj (object-value editor))
         (max-dur (get-obj-dur obj))
         (ed-dur (if (zerop max-dur) 10000 (+ max-dur 1000))))

    (set-g-component editor :data-panel-list ;;; compat with data-stream-editor 
                     (list (om-make-view (editor-view-class editor) :stream-id 0
                                         :editor editor :size (omp 50 60) 
                                         :direct-draw t 
                                         :bg-color (om-def-color :white) 
                                         :scrollbars :h)))
                     
    (set-g-component editor :x-ruler (om-make-view 'time-ruler 
                                                   :related-views (get-g-component editor :data-panel-list)
                                                   :size (omp nil 20) 
                                                   :bg-color (om-def-color :white)
                                                   :vmin -200
                                                   :x1 -200 :x2 ed-dur))
    
    (set-g-component (timeline-editor editor) :main-panel (om-make-layout 'om-row-layout)) 
    (set-g-component editor :main-panel (car (get-g-component editor :data-panel-list)))
    (when (editor-get-edit-param editor :show-timeline) (make-timeline-view (timeline-editor editor)))
    
    (om-make-layout 
     'om-column-layout 
     :ratios '(96 2 2)
     :subviews (list 
                ;;; first group with the 'main' editor:
                (om-make-layout 
                 'om-grid-layout 
                 :delta 0
                 :ratios '((nil 100) 
                           (1 98 1))
                 :subviews 
                 (append (list nil (make-control-bar editor))
                         (loop for view in (get-g-component editor :data-panel-list)
                               append (list (make-left-panel-for-object editor obj)
                                            view))
                         (list nil (get-g-component editor :x-ruler)))
                 )
                ;;; the timeline editor:
                (get-g-component (timeline-editor editor) :main-panel)
                ;;; the bottom control bar:
                (om-make-layout 'om-row-layout 
                                :size (omp nil 40) 
                                :subviews (list (make-score-control-panel editor) nil (make-timeline-check-box editor)))
                ))
    ))



(defmethod draw-score-object-in-editor-view ((editor chord-seq-editor) view unit)
  
  (let ((font-size (editor-get-edit-param editor :font-size))
        (staff (editor-get-edit-param editor :staff))
        (chan (editor-get-edit-param editor :channel-display))
        (vel (editor-get-edit-param editor :velocity-display))
        (port (editor-get-edit-param editor :port-display))
        (dur (editor-get-edit-param editor :duration-display)))
    
    ;;; warning: so far we don't build/update a boundng-box for the chord-seq..
    (loop for chord in (inside (object-value editor)) do
          (setf 
           (b-box chord)
           (draw-chord (inside chord) 
                       (/ (time-to-pixel view (date chord)) unit)
                       0 
                       (w view) (h view) 
                       font-size 
                       :staff (editor-get-edit-param editor :staff)
                       :draw-chans chan
                       :draw-vels vel
                       :draw-ports port
                       :draw-durs dur
                       :selection (if (find chord (selection editor)) T 
                                    (selection editor))
                       :build-b-boxes t
                       ))
          ;(draw-b-box chord)
          ;(mapcar 'draw-b-box (inside chord))
    
          )))
