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
         (bef (bin-value-below bas)))
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

;;; get the notehead symbol and eventually points corresponding to a given duration
(defun note-head-and-points (dur)
   (let* ((haut (numerator dur))
          (bas (denominator dur))
          (bef (bin-value-below haut))
          (points 0) (symbol :head-1/4))
     (cond
      ((= bef haut)
       (setf symbol (note-symbol (/ haut bas))))
      ((= (* bef 1.5) haut)
       (setf symbol (note-symbol (/ bef bas)))
       (setf points 1))
      ((= (* bef 1.75) haut)
       (setf symbol (note-symbol (/ bef bas)))
       (setf points 2)))
     (values symbol points)))


(defun rest-head-and-points (dur)
   (let* ((haut (numerator dur))
          (bas (denominator dur))
          (bef (bin-value-below haut))
          (points 0) (symbol :rest-1/4))
     (cond
      ((= bef haut)
       (setf symbol (rest-symbol (/ haut bas))))
      ((= (* bef 1.5) haut)
       (setf symbol (rest-symbol (/ bef bas)))
       (setf points 1))
      ((= (* bef 1.75) haut)
       (setf symbol (rest-symbol (/ bef bas)))
       (setf points 2)))
     (values symbol points)))


(defun note-symbol (val &optional rest)
  (cond 
   ((>= val 8) (list val)) 
   ((= val 8) :head-8)   ;;; will never happen becvause of previous statement: fix that 
   ((= val 4) :head-4)
   ((= val 2) :head-2)
   ((= val 1) :head-1)
   ((= val 1/2) :head-1/2)    
   (t :head-1/4)))

(defun rest-symbol (val)
  (cond
   ((> val 4) (list val)) 
   ((= val 4) :rest-4)
   ((= val 2) :rest-2)
   ((= val 1) :rest-1)
   ((= val 1/2) :rest-1/2)
   ((= val 1/4) :rest-1/4)
   ((= val 1/8) :rest-1/8)
   ((= val 1/16) :rest-1/16)
   ((= val 1/32) :rest-1/32)
   ((= val 1/64) :rest-1/64)
   ((= val 1/128) :rest-1/128)
   (t :rest-1/128)))

;;;===============================================
;;; DRAW
;;;===============================================

;;; GROUP
(defmethod draw-score-element ((object rhythmic-object) tempo editor view unit level)
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end)))
    
    (om-draw-rect x1 10 (- x2 x1) (- (h view) (* (1+ level) 20)) :fill nil :color (om-def-color :light-gray))
    
    (loop for element in (inside object) do
          (draw-score-element element tempo editor view unit (1+ level)))
    ))


;;; CHORD
(defmethod draw-score-element ((object chord) tempo editor view unit level)
  
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
    
    ;; (print (list "chord" (symbolic-dur object)))

    (setf 
     (b-box object)
     (draw-chord object
                 (/ (time-to-pixel view (date object)) unit)
                 0 
                 (w view) (h view) 
                 font-size 
                 :head (multiple-value-list (note-head-and-points (symbolic-dur object)))
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


;;; REST
(defmethod draw-score-element ((object r-rest) tempo editor view unit level)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end))
         (font-size (editor-get-edit-param editor :font-size)))
    
    (om-draw-rect x1 10 (- x2 x1) (- (h view) (* (1+ level) 20)) :fill nil :color (om-def-color :light-red))
    
    ;; (print (list "rest" (symbolic-dur object)))
    
    (setf 
     (b-box object)
     (draw-rest object
                (/ (time-to-pixel view begin) unit)
                0 
                (w view) (h view) 
                font-size 
                :head (multiple-value-list (rest-head-and-points (symbolic-dur object)))
                :staff (editor-get-edit-param editor :staff)
                :selection (if (find object (selection editor)) T 
                             (selection editor))
                :build-b-boxes t
                ))
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
(defmethod get-tie-direction ((self grap-note))
   (let* ((note (midic (reference self)))
          (list (sort (lmidic (parent (reference self))) '<)))
     (if (>= (position note list :test 'equal) (ceiling (/ (length list) 2)))
       "up" "down")))

(defmethod get-next-tied-noted ((self grap-note) )
   (and (parent self)
        (let ((next-chord (next-figure (parent self))))
          (and next-chord (grap-ryth-chord-p next-chord) 
               (find (midic (reference self)) (inside next-chord) :key #'(lambda (x) (midic (reference x))))))))

(defmethod get-last-tied-noted ((self grap-note) )
   (and (parent self)
        (let ((previous (previous-figure (parent self))))
          (and previous (grap-ryth-chord-p previous)
               (find (midic (reference self)) (inside previous) :key #'(lambda (x) (midic (reference x))))))))

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
