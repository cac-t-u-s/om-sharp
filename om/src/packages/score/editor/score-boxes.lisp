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


(defun score-mini-view-left-shift-in-units (box)
  (if (or (equal (get-edit-param box :staff) :line)
          (equal (get-edit-param box :staff) :empty))
      1 5))


(defmethod miniview-time-to-pixel-proportional ((object score-object) view time)

  (let* ((box (object view))
         (fontsize (or (fontsize box) 24))
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


(defmethod miniview-time-to-pixel-rhythmic ((object score-object) view time)

  (let* ((box (object view))
         (fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize))
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) unit))
         (time-map (get-edit-param box :time-map)))
    
    (+ shift-x-pix
       (* unit (score-time-to-units time-map time)))
    ))

(defmethod miniview-time-to-pixel ((object score-object) view time)
  (miniview-time-to-pixel-proportional object view time))

(defmethod miniview-time-to-pixel ((object voice) view time)
  (miniview-time-to-pixel-rhythmic object view time))





;;;===========================
;;; NOTE
;;;===========================
(defmethod score-object-mini-view ((self note) box x-pix y-pix y-u w h)
  
  (let ((staff (get-edit-param box :staff)))
    
    (draw-staff x-pix y-pix y-u w h (fontsize box) staff :margin-l 1 :margin-r 1 :keys t)

    (draw-chord (make-instance 'chord :notes (list self)) 
                0 
                0 y-u 0 y-pix w h 
                (fontsize box) :scale nil :staff staff
                :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                )
    ))
  

;;;===========================
;;; CHORD
;;;===========================
(defmethod score-object-mini-view ((self chord) box x-pix y-pix y-u w h)
  
  (let* ((staff (get-edit-param box :staff)))
    
    (draw-staff x-pix y-pix y-u w h (fontsize box) staff :margin-l 1 :margin-r 1 :keys t)

    (when (notes self)
      (draw-chord self 0 0 y-u 0 y-pix w h (fontsize box) :scale nil :staff staff
                  :time-function #'(lambda (time) (declare (ignore time)) (/ w 2))
                  ))
    ))


;;;===========================
;;; CHORD-SEQ
;;;===========================
(defmethod score-object-mini-view ((self chord-seq) box x-pix y-pix y-u w h)
  
  (let* ((staff (get-edit-param box :staff))
         (font-size (fontsize box))
         (unit (font-size-to-unit font-size)))
        
    (draw-staff x-pix y-pix y-u w h font-size staff :margin-l 0 :margin-r 0 :keys t)
    
    (loop for chord in (chords self) do
          (draw-chord chord
                      (date chord)
                      0 y-u  
                      x-pix 
                      y-pix 
                      w h
                      font-size :scale nil :staff staff
                      :time-function #'(lambda (time) (miniview-time-to-pixel self (frame box) time))
                      )
          )))


;;;===========================
;;; VOICE
;;;===========================

(defmethod score-object-mini-view ((self voice) box x-pix y-pix shift-y w h)
  
  (let* ((time-map (get-edit-param box :time-map))
         (staff (get-edit-param box :staff))
         (font-size (fontsize box))
         (unit (font-size-to-unit font-size))
         (x-u (/ x-pix unit))
         (y-u (/ y-pix unit))
         (shift-x x-u) ; (+ (score-mini-view-left-shift-in-units box) x-u))
         (frame (frame box))
         (max-w (w frame)))
        
    (draw-staff x-pix y-pix shift-y w h font-size staff 
                :margin-l 0 :margin-r 0 :keys t)
    
    (draw-tempo self 4 (+ shift-y 2.5) font-size) 

    (loop with on-screen = t 
          with prev-signature = nil
          for m in (inside self)
          for i from 1
          while on-screen
          do 
          (setf on-screen (< (time-to-pixel frame (beat-to-time (symbolic-date m) (tempo self))) max-w))
          ;;; we draw the measure if it begins on-screen...
          (when on-screen
            (draw-measure m (tempo self) box (frame box) 
                          :position i
                          :with-signature (not (equal (car (tree m)) prev-signature))
                          :staff staff
                          :x-shift shift-x
                          :y-shift (+ shift-y y-u) 
                          :font-size font-size 
                          :time-map time-map))
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
(defmethod score-object-mini-view ((self poly) box x-pix y-pix y-u w h)
  (let ((voice-h (if (obj-list self) (/ h (num-voices self)) h)))
    (loop for voice in (obj-list self)
          for i from 0 do
          (score-object-mini-view voice box x-pix (+ y-pix (* i voice-h)) 0 w voice-h))
    ))


