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

;;;===========================
;;; DRAW SCORE OBJECT BOXES
;;;===========================

(in-package :om)


(defclass ScoreBox (OMBoxEditCall) 
  ((fontsize :accessor fontsize :initform 18)))

(defmethod special-box-type ((self (eql 'score-object))) 'ScoreBox)

(defmethod display-modes-for-object ((self score-object))
  '(:mini-view :hidden :text))

(defmethod additional-box-attributes ((self score-object)) 
  `((:font-size "a font size for score display" nil) 
    (:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    ))

(defmethod miniview-time-to-pixel-proportional ((object score-object) box view time)

  (let* ((fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize))
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) unit)))
    ;(print (list "time" time))
    ;(print 
     (+ shift-x-pix ;; left margin
        *miniview-x-margin*
        (* (- (w view) (* *miniview-x-margin* 2) shift-x-pix)   
           (/ time (if (plusp (get-obj-dur object)) (get-obj-dur object) 1000))))
     ;)
     ))

(defmethod miniview-time-to-pixel-rhythmic ((object score-object) box view time)

  (let* ((fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize))
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) unit))
         (time-map (get-edit-param box :time-map)))
    
    (+ shift-x-pix
       (* unit (score-time-to-units time-map time)))
    ))


;;; all objects (except voice/poly) on a box
(defmethod miniview-time-to-pixel ((object score-object) box (view omobjectboxframe) time) 
  (miniview-time-to-pixel-proportional object box view time))

;;; voice on a box
(defmethod miniview-time-to-pixel ((object voice) box (view omobjectboxframe) time)
  (miniview-time-to-pixel-rhythmic object box view time))

(defmethod miniview-time-to-pixel ((object poly) box (view omobjectboxframe) time)
  (miniview-time-to-pixel-rhythmic object box view time))

(defmethod miniview-time-to-pixel ((object multi-seq) box (view omobjectboxframe) time)
  (miniview-time-to-pixel-proportional object box view time))


;;; an objects in the maquette tracks...
(defmethod miniview-time-to-pixel ((object score-object) box (view sequencer-track-view) time)
  (- (time-to-pixel view (+ (box-x box) time)) 
     (time-to-pixel view (box-x box))
     ))

(defun score-mini-view-left-shift-in-units (box)
  (if (or (equal (get-edit-param box :staff) :line)
          (equal (get-edit-param box :staff) :empty))
      1 5))


(defmethod draw-mini-view ((self score-object) (box ScoreBox) x y w h &optional time)

  (om-draw-rect x y w h :fill t :color (om-def-color :white))
  
  (when (> (num-voices self) 0)
    (let ((staff (get-edit-param box :staff))
          (h-per-voice (/ h (num-voices self))))
    
      (setf (fontsize box) 18)
    
      (let* ((staff-lines (apply 'append (mapcar 'staff-lines (staff-split staff))))
             (unit (font-size-to-unit (fontsize box)))
             (n-lines (+ (- (car (last staff-lines)) (car staff-lines)) 8)) ;;; range of the staff lines + 10-margin
             (draw-box-h (* n-lines unit))
             (y-in-units (/ y unit)))
     
        (if (< draw-box-h h-per-voice)
            ;;; there's space: draw more in the middle
            (setf y-in-units (+ y-in-units (/ (round (- h-per-voice draw-box-h) 2) unit)))
          ;;; there's no space: reduce font ?
          (progn 
            (setf unit (- unit (/ (- draw-box-h h-per-voice) n-lines)))
            (setf (fontsize box) (unit-to-font-size unit)))
          )
      
        (om-with-fg-color (om-make-color 0.0 0.2 0.2)
          (score-object-mini-view self box x y y-in-units w h)
          )
        ))))



;;;===========================
;;; NOTE
;;;===========================

;;; note has no editor (at the moment) so the font-size is not used
(defmethod additional-box-attributes ((self note)) 
  `((:staff "default staff configuration" 
     ,(loop for s in *score-staff-options* collect (list (string-upcase s) s)))
    ))

(defmethod score-object-mini-view ((self note) box x-pix y-pix y-u w h)
  
  (let ((staff (get-edit-param box :staff))
        (font-size (fontsize box))
        (in-sequencer? (typep (frame box) 'sequencer-track-view)))
    
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 1 :margin-r 1 
                :keys (not in-sequencer?))

    (draw-chord (make-instance 'chord :notes (list self)) 
                0 
                0 y-u 0 y-pix w h 
                font-size 
                :scale nil :staff staff
                :stem NIL
                :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                )
    ))
  

;;;===========================
;;; CHORD
;;;===========================
(defmethod score-object-mini-view ((self chord) box x-pix y-pix y-u w h)
  
  (let ((staff (get-edit-param box :staff))
        (in-sequencer? (typep (frame box) 'sequencer-track-view)))
    
    (draw-staff x-pix y-pix y-u w h (fontsize box) staff :margin-l 1 :margin-r 1 
                :keys (not in-sequencer?))

    (when (notes self)
      (draw-chord self 0 0 y-u x-pix y-pix w h (fontsize box) :scale nil :staff staff
                  :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                  ))
    ))


;;;===========================
;;; CHORD-SEQ
;;;===========================
(defmethod score-object-mini-view ((self chord-seq) box x-pix y-pix y-u w h)
  
  (let* ((staff (get-edit-param box :staff))
         (font-size (fontsize box))
         (in-sequencer? (typep (frame box) 'sequencer-track-view)))
        
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 0 :margin-r 0 :keys (not in-sequencer?))

    (loop for chord in (chords self) do
          (draw-chord chord
                      (date chord)
                      0 y-u  
                      x-pix 
                      y-pix 
                      w h
                      font-size :scale nil :staff staff
                      :time-function #'(lambda (time) (miniview-time-to-pixel (get-box-value box) box (frame box) time))
                      )
          )))


;;;===========================
;;; VOICE
;;;===========================

(defmethod score-object-mini-view ((self voice) box x-pix y-pix shift-y w h)
  
  (let* ((staff (get-edit-param box :staff))
         (font-size (fontsize box))
         (unit (font-size-to-unit font-size))
         (x-u (/ x-pix unit))
         (y-u (/ y-pix unit))
         (shift-x x-u) ; (+ (score-mini-view-left-shift-in-units box) x-u))
         (frame (frame box))
         (max-w (w frame))
         
         (in-sequencer? (typep frame 'sequencer-track-view))
         )
    
    (draw-staff x-pix y-pix shift-y w h font-size staff 
                :margin-l 0 :margin-r 0 :keys (not in-sequencer?))
    
    (draw-tempo self (+ x-pix 4) (+ y-pix (* unit (+ shift-y 2))) font-size)

    (loop with on-screen = t 
          with prev-signature = nil
          for m in (inside self)
          for i from 1
          while on-screen
          do 
          (setf on-screen (< (miniview-time-to-pixel (get-box-value box) box frame 
                                                     (beat-to-time (symbolic-date m) (tempo self)))
                             max-w))
          ;;; we draw the measure if it begins on-screen...
          (when on-screen
            (draw-measure m (tempo self) box (frame box) 
                          :position i
                          :with-signature (and (not in-sequencer?)
                                               (not (equal (car (tree m)) prev-signature)))
                          :staff staff
                          :x-shift shift-x
                          :y-shift (+ shift-y y-u) 
                          :font-size font-size 
                          :time-function #'(lambda (time) (miniview-time-to-pixel (get-box-value box) box frame time))
                          ))
          
          ;;; if the end is off-screen we notify it with a little gray area at the end
          (when (> (time-to-pixel frame (beat-to-time (+ (symbolic-date m) (symbolic-dur m)) (tempo self))) max-w)
            (om-draw-rect (- (w frame) 20) 0 20 (h frame) :fill t :color (om-make-color .8 .8 .8 .5))
            (om-draw-string (- (w frame) 16) (- (h frame) 12) "..."))
          (setf prev-signature (car (tree m)))
          )
    ))


;;;===========================
;;; POLY
;;;===========================

(defmethod score-object-mini-view ((self multi-seq) box x-pix y-pix y-u w h)
  (let ((voice-h (if (obj-list self) (/ h (num-voices self)) h))
        (max-dur (loop for o in (obj-list self) maximize (get-obj-dur o))))
    (loop for voice in (obj-list self)
          for i from 0 do
          (score-object-mini-view voice box x-pix (+ y-pix (* i voice-h)) 0 w voice-h))
    ))


