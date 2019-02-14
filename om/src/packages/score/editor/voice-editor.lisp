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

;;;===========================
;;; UTILS
;;;===========================

(defun bin-value-below (num)
  (/ (closest-pwr-of-2 num) 2))

(defun closest-bin-value (num)
  (let* ((high (closest-pwr-of-2 num))
         (low (/ high 2)))
    (if (< (- high num) (- num low))
        hight low)))

;;; is (numerator dur) a power of 2
(defun is-binaire? (dur)
  (and (= (numerator dur) 1) 
       (= (denominator dur) (closest-pwr-of-2 (denominator dur)))
       ))

;;; gives number of beams for a given division
(defun get-number-of-beams (val)
  
  (let* ((haut (numerator val))
         (bas (denominator val))
         (bef (bin-value-below haut)))
    
     (cond
      ((= bef haut)
       (note-strict-beams (/ haut bas)))
      
      ((= (* bef 1.5) haut)
       (note-strict-beams (/ bef bas) ))
       
      ((= (* bef 1.75) haut)
       (setf beams (note-strict-beams (/ bef bas) )))

      (t 0))
     ))

(defun find-group-symbol (val)
  (let* ((haut (numerator val))
         (bas (denominator val))
         (bef (car (before&after-bin bas))))
    (list 
     (note-strict-beams (/ 1 bef)) 
     (denominator (/ bef bas)))))

;;; entry in the process of determining the beaming for a given chord.
;;; this code is directlty adapted from OM6 score editors
(defun note-strict-beams (val)
   (cond
    ((or (= val 1/4) (= val 1/2) (>= val 1)) 0)
    ((= val 1/8)  1)
    ((= val 1/16) 2)
    ((= val 1/32) 3)
    ((= val 1/64) 4)
    ((= val 1/128) 5)
    ((= val 1/256) 6)
    ((= val 1/512) 7)
    ((is-binaire? val) (round (- (log (denominator val) 2) 2)))
    (t (find-group-symbol val))))



;;; GROUPS

(defmethod draw-score-element ((object rhythmic-object) tempo editor view unit level)
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end)))
    
    (om-draw-rect x1 10 (- x2 x1) (- (h view) (* (1+ level) 20)) :fill nil :color (om-def-color :light-gray))
    
    (loop for element in (inside object) do
          (draw-score-element element tempo editor view unit (1+ level)))
    ))


;;; CHORDS
(defmethod draw-score-element ((object chord) tempo editor view unit level)
  
  (call-next-method)

  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end))

         (font-size (editor-get-edit-param editor :font-size))
         (staff (editor-get-edit-param editor :staff))
         (chan (editor-get-edit-param editor :channel-display))
         (vel (editor-get-edit-param editor :velocity-display))
         (port (editor-get-edit-param editor :port-display))
         (dur (editor-get-edit-param editor :duration-display)))
    
    (setf 
     (b-box object)
     (draw-chord (inside object) 
                 (/ (time-to-pixel view (date object)) unit)
                 0 
                 (w view) (h view) 
                 font-size 
                 :staff (editor-get-edit-param editor :staff)
                 :draw-chans chan
                 :draw-vels vel
                 :draw-ports port
                 :draw-durs dur
                 :selection (if (find object (selection editor)) T 
                              (selection editor))
                 :build-b-boxes t
                 ))

    ))


;;; OTHER (RESTS)
(defmethod draw-score-element ((object score-object) tempo editor view unit level)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end)))
    
    (om-draw-rect x1 10 (- x2 x1) (- (h view) (* (1+ level) 20)) :fill nil :color (om-def-color :light-red))

    ))





(defmethod draw-sequence ((editor chord-seq-editor) (object voice) view unit)

  ;;; NOTE: so far we don't build/update a bounding-box for the containers
  
  (let ((on-screen t))
    (loop for m in (inside object)
          for i from 0
          while on-screen
          do (let* ((begin (beat-to-time (symbolic-date m) (tempo object)))
                    (end (beat-to-time (+ (symbolic-date m) (symbolic-dur m)) (tempo object)))
                    (x1 (time-to-pixel view begin))
                    (x2 (time-to-pixel view end)))
               
               (if (> x1 (w view)) (setf on-screen nil)
                 ;;; else :
                 (when (> x2 0) 
                   ;;; DRAW THIS MEASURE
                   
                   (unless (zerop i) (draw-measure-bar (/ x1 unit) 0 font-size staff))
                   
                   ;; (om-draw-rect x1 10 (- x2 x1) (- (h view) 20) :fill t :color (om-random-color 0.5)) 
                   
                   (loop for element in (inside m) do
                         (draw-score-element element (tempo object) editor view unit 1))
                   )))
          ))
  )


;;; todo
#|
;;; TIED NOTES
(defun draw-liason-begin (left top right bot direction)
 (if (string-equal direction "down")
     (om-draw-ellipse-arc left top   (- right left)    (round (- bot top) 2) pi  (/ pi 2) )
   (om-draw-ellipse-arc left top   (- right left)    (round (- bot top) 2) (/ pi 2)  (/ pi 2) )))

(defun draw-liason-end (left top right bot direction) 
 (if (string-equal direction "down")
     (om-draw-ellipse-arc left top   (- right left)    (round (- bot top) 2) (* 3 (/ pi 2))  (/ pi 2) )
   (om-draw-ellipse-arc left top   (- right left)    (round (- bot top) 2) 0  (/ pi 2) )))
|#

;;; RESTS

;;; POINTS

;;; TEMPO / MESURE NUMBER / CHIFFRAGE

;;; SPACING !!
