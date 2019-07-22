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
      2 4))


(defmethod miniview-time-to-pixel-proportional ((object score-object) view time)
  (let* ((box (object view))
         (fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize))
         
         (shift-x-pix (* (score-mini-view-left-shift-in-units box) 
                         (font-size-to-unit (fontsize box)))))
    
    (+ unit ;; left margin
       shift-x-pix 
       (* (- (w view) shift-x-pix (* 2 unit)) 
          (/ time (if (plusp (get-obj-dur object)) (get-obj-dur object) 1000))))
    ))


(defmethod miniview-time-to-pixel-rhythmic ((object score-object) view time)

  (let* ((box (object view))
         (fontsize (or (fontsize box) 24))
         (unit (font-size-to-unit fontsize))
         (time-map (get-edit-param box :time-map)))
    
    (score-time-to-pixel view time time-map unit)
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
  
  (let ((staff (get-edit-param box :staff))
        (x-shift (* (score-mini-view-left-shift-in-units box) 
                    (font-size-to-unit (fontsize box)))))
        
    (draw-staff x-pix y-pix y-u w h (fontsize box) staff :margin-l 1 :margin-r 1 :keys t)
    
    (loop for chord in (chords self) do
          (draw-chord chord
                      (date chord)
                      0 y-u  
                      (+ x-pix x-shift) 
                      y-pix 
                      w h
                      (fontsize box) :scale nil :staff staff
                      :time-function #'(lambda (time) (miniview-time-to-pixel self (frame box) time))
                      )
          )))


;;;===========================
;;; VOICE
;;;===========================

(defmethod score-object-mini-view ((self voice) box x-pix y-pix y-u w h)
  
  (let ((time-map (get-edit-param box :time-map))
        (staff (get-edit-param box :staff))
        (font-size (fontsize box)))
    
    (draw-staff x-pix y-pix y-u w h font-size staff 
                :margin-l 1 :margin-r 1 :keys t)
  
    (om-with-translation 
        (+ x-pix (* (score-mini-view-left-shift-in-units box) 
                    (font-size-to-unit font-size)))
        y-pix
      
      (loop with prev-signature = nil
            for m in (inside self)
            for i from 1
            do (draw-measure m (tempo self) box (frame box) 
                             :position i
                             :with-signature (not (equal (car (tree m)) prev-signature))
                             :staff staff
                             :x-shift 0
                             :y-shift y-u 
                             :font-size font-size 
                             :time-map time-map)
            (setf prev-signature (car (tree m)))
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


