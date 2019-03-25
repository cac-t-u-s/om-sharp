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
;;; CHORD-SEQ EDITOR / GENERAL SCORE EDITOR
;;;========================================================================

(defclass chord-seq-editor (data-stream-editor score-editor) ())
(defmethod get-editor-class ((self chord-seq)) 'chord-seq-editor)

(defclass score-panel (score-view stream-panel) ()
  (:default-initargs :keys nil :margin-l nil :margin-r 1))

(defmethod editor-view-class ((self chord-seq-editor)) 'score-panel)


;;; this will just disable the display-mode menu 
(defmethod frame-display-modes-for-object ((self data-stream-editor) (object score-object)) '(:chords))

(defmethod object-default-edition-params ((self chord-seq))
  (append (call-next-method)
          '((:grid nil))))

(defclass left-score-view (score-view) ())

(defmethod om-draw-contents ((self left-score-view))
  (let* ((editor (editor self))
         (scrolled (> (x1 (get-g-component editor :main-panel)) 0))
         (shift (* 2 (font-size-to-unit (editor-get-edit-param editor :font-size)))))
    
    (draw-staff 0 0 
                (w self) (h self) 
                (editor-get-edit-param editor :font-size) 
                (editor-get-edit-param editor :staff)
                :margin-l (margin-l self) :margin-r (margin-r self)
                :keys (keys self))
    
    (when scrolled 
      (om-draw-rect (- (w self) 20) 0 20 (h self)
                    :fill t :color (om-make-color .9 .9 .9 .5))
      (om-draw-string (- (w self) 15) 10 "...")
      (om-draw-string (- (w self) 15) (- (h self) 10) "..."))

    ))




(defmethod make-left-panel-for-object ((editor chord-seq-editor) (object score-object))
  (om-make-view 'left-score-view :size (omp (* 2 (editor-get-edit-param editor :font-size)) nil)
                :direct-draw t 
                :bg-color (om-def-color :white) 
                :scrollbars nil
                :editor editor
                :margin-l 1 :margin-r nil :keys t :contents nil
                ))

(defmethod make-editor-controls ((editor chord-seq-editor))
  (make-score-display-params-controls editor))

         
(defmethod update-view-from-ruler ((self x-ruler-view) (view score-panel))
  (call-next-method)
  (om-invalidate-view (left-view view)))

(defmethod om-view-zoom-handler ((self score-panel) position zoom)
  (zoom-rulers self :dx (- 1 zoom) :dy 0 :center position))



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


(defmethod set-font-size ((self chord-seq-editor) size)
  (call-next-method)
  (build-editor-window self)
  (init-editor-window self))


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
       (setf (notes new-chord) nil)
       ;;; notes = NIL here so the duration will be 0 at updating the time-sequence
       (time-sequence-insert-timed-item-and-update time-seq new-chord (find-position-at-time time-seq time-pos))
       new-chord)
     
     )))


;;;=========================
;;; WINDOW CONSTRUCTOR
;;;=========================

(defmethod data-stream-get-x-ruler-vmin ((self chord-seq-editor)) -200)


(defmethod make-editor-window-contents ((editor chord-seq-editor)) (call-next-method))
  

(defmethod draw-score-object-in-editor-view ((editor chord-seq-editor) view unit)
  (let ((obj (if (multi-display-p editor)
                 (nth (stream-id view) (multi-obj-list editor))
               (object-value editor))))
    (draw-sequence obj editor view unit)))


(defmethod draw-sequence ((object t) editor view unit) nil)


;;; redefined this for other objects
(defmethod draw-sequence ((object chord-seq) editor view unit)

  (let ((font-size (editor-get-edit-param editor :font-size))
        (staff (editor-get-edit-param editor :staff))
        (chan (editor-get-edit-param editor :channel-display))
        (vel (editor-get-edit-param editor :velocity-display))
        (port (editor-get-edit-param editor :port-display))
        (dur (editor-get-edit-param editor :duration-display)))
    
    ;;; NOTE: so far we don't build/update a bounding-box for the chord-seq itself (might be useful in POLY)..
    (loop for chord in (chords object) do
          (setf 
           (b-box chord)
           (draw-chord chord
                       (time-to-pixel view (date chord))
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

