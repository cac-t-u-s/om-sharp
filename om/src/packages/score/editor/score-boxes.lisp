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
;;; DRAW SCOIRE OBJECT BOXES
;;;===========================

(in-package :om)


(defun score-mini-view-left-shift-in-units (box)
  (if (or (equal (get-edit-param box :staff) :line)
          (equal (get-edit-param box :staff) :empty))
      2 6))


(defmethod miniview-time-to-pixel ((object score-object) view time)
  (let* ((box (object view))
         (fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize)))
    (+ (* (+ 1 (score-mini-view-left-shift-in-units box)) unit)
       (score-time-to-pixel view time (get-edit-param box :time-map) unit))
    ))

; (+ shift-x-pix (* time (/ (- (w view) shift-x-pix (* 2 unit)) (get-obj-dur object))))




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
  
  (let ((staff (get-edit-param box :staff)))
    
    (draw-staff x-pix y-pix y-u w h (fontsize box) staff :margin-l 1 :margin-r 1 :keys t)

    (loop for chord in (chords self) do
          (draw-chord chord
                      (date chord)
                      0 y-u  
                      x-pix y-pix w h
                      (fontsize box) :scale nil :staff staff
                      :time-function #'(lambda (time) (miniview-time-to-pixel self (frame box) time))
                      )
          )))




(defmethod score-object-mini-view ((self voice) box x-pix y-pix y-u w h)
  
  (let ((time-map (get-edit-param box :time-map)))
    
    (draw-staff x-pix y-pix y-u w h (fontsize box) (get-edit-param box :staff) :margin-l 1 :margin-r 1 :keys t)
  
    (om-with-translation 
        (+ x-pix (* (score-mini-view-left-shift-in-units box) (font-size-to-unit (fontsize box))))
        y-pix
      
      (loop for m in (inside self)
            for i from 1
            do (draw-score-element m (tempo self) box (frame box) 
                                   :position i
                                   :x-shift 0
                                   :y-shift y-u 
                                   :font-size (fontsize box) 
                                   :time-map time-map)
            ))))

;;;===========================
;;; POLY
;;;===========================
(defmethod score-object-mini-view ((self poly) box x-pix y-pix y-u w h)
  (let ((voice-h (if (obj-list self) (/ h (num-voices self)) h)))
    (loop for voice in (obj-list self)
          for i from 0 do
          (score-object-mini-view voice box x-pix (+ y-pix (* i voice-h)) 0 w voice-h))
    ))


