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

;;;====================================================
;;; DRAW METHODS FOR SCORE EDITOR / MINIVIEW
;;;====================================================


(defmethod score-object-mini-view ((self voice) box x-pix y-u w h)
  
  (draw-staff x-pix y-u w h (fontsize box) (get-edit-param box :staff) :margin-l 1 :margin-r 1 :keys t)

  (loop for m in (inside self)
        for i from 1
        do (draw-score-element m (tempo self) box (frame box) :y-shift y-u :font-size (fontsize box) :level i)
        ))


;;; EDITOR
(defmethod draw-sequence ((object voice) editor view unit)

  ;;; NOTE: so far we don't build/update a bounding-box for the containers
  
  (let ((on-screen t))
    (loop for m in (inside object)
          for i from 1
          while on-screen
          do (let* ((begin (beat-to-time (symbolic-date m) (tempo object)))
                    (end (beat-to-time (+ (symbolic-date m) (symbolic-dur m)) (tempo object)))
                    (x1 (time-to-pixel view begin))
                    (x2 (time-to-pixel view end)))
               
               (if (> x1 (w view)) (setf on-screen nil)
                 ;;; else :
                 (when (> x2 0) 
                   
                   ;;; DRAW THIS MEASURE
                   (draw-score-element m (tempo object) (object editor) view :level i
                                       :font-size (editor-get-edit-param editor :font-size)
                                       :selection (selection editor))
                   )))
          ))
  )



;;;===============================================
;;; STRUCTURE
;;;===============================================

;;; MEASURE
;;; for measure we use level as measure number
(defmethod draw-score-element ((object measure) tempo param-obj view &key font-size (y-shift 0) (level 1) selection)
  (let ((x-pix (time-to-pixel view (beat-to-time (symbolic-date object) tempo))))
    
    (unless (= level 1) 
      (draw-measure-bar x-pix y-shift font-size (get-edit-param param-obj :staff))
      (om-draw-string x-pix (- y-shift 1) (number-to-string level) 
                      :font (om-def-font :font1 :size (/ font-size 3))))
                   
    (loop for element in (inside object) do
          (draw-score-element element tempo param-obj view 
                              :y-shift y-shift
                              :font-size font-size
                              :selection selection))
    ))


;;; GROUP
(defmethod draw-score-element ((object group) tempo param-obj view &key font-size (y-shift 0) (level 1) selection)
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end)))
    
    (om-draw-rect x1 10 (- x2 x1) (- (h view) (* (1+ level) 20)) :fill nil :color (om-def-color :light-gray))
    
    ;;; IF LEVEL = 1: do someting about beaming

    (loop for element in (inside object) do
          (draw-score-element element tempo param-obj view 
                              :y-shift y-shift
                              :font-size font-size 
                              :level (1+ level) 
                              :selection selection))
    ))


;;;===========================================
;;; draw-stems of same size for group elements

(defmethod group-draw-stems ((self t) dir x y rect zoom size) t)
(defmethod group-draw-stems ((self group) dir x y rect zoom size)
   (loop for item in (inside self) do
         (group-draw-stems item dir x y rect zoom size)))

(defmethod group-draw-stems ((self chord) dir x y rect zoom size)
   (when (and (stem self) (x self))
     (let* ((note-min-max (om+ y (get-min-max self)))
            (ystart (if (string-equal dir "up") (second note-min-max) (first note-min-max)))
            (ygroup (if (string-equal dir "up") (second rect) (fourth rect) )))
       (setf y 0)
       (draw-stem-no-group  (if (string-equal dir "up") 
                              (round (+  x  (/ size 3.5) (* zoom (x self))))
                              (round (+  x   (* zoom (x self))))) 
                            (selected self) 
                            (+ y ystart)
                            (+ y ygroup)))))

(defmethod group-draw-stems ((self r-rest) dir x y rect zoom size)
   ""
   (let* ((ystart (if (string-equal dir "up") (if (>= (beams-num self) 1) 
                                                (+ (second (rectangle self)) (round (* (- (fourth (rectangle self)) 
                                                                                          (second (rectangle self))) 0.4)))
                                                (second (rectangle self))) 
                      (if (>= (beams-num self) 1) 
                        (- (fourth (rectangle self)) (round (* (- (fourth (rectangle self)) 
                                                                  (second (rectangle self))) 0.4)))
                        (fourth (rectangle self)))))
          (ygroup (if (string-equal dir "up") (second rect) (fourth rect) )))
     (draw-stem-no-group  (if (string-equal dir "up") 
                            (round (+  x (/ size 3.5) (* zoom (x self))))
                            (round (+  x  (* zoom (x self))))) 
                          (selected self) 
                          ystart
                          ygroup)))


;;;=======================================================
;;; CHORD
(defmethod draw-score-element ((object chord) 
                               tempo param-obj view 
                               &key font-size (y-shift 0) (level 1) selection)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end))

         (staff (get-edit-param param-obj :staff))
         (chan (get-edit-param param-obj :channel-display))
         (vel (get-edit-param param-obj :velocity-display))
         (port (get-edit-param param-obj :port-display))
         (dur (get-edit-param param-obj :duration-display))

         ;;; from OM6.. 
         (beams (get-number-of-beams (symbolic-dur object))) 
         (beams-num (if (listp beams) (car beams) beams))
         (propre-group (if (listp beams) (cadr beams))))
    
    ;; (print (list "chord" (symbolic-dur object) beams))
    
    ;;; IF LEVEL = 1: draw individual stem and beaming

    (setf 
     (b-box object)
     (draw-chord object
                 (time-to-pixel view begin)
                 y-shift 
                 (w view) (h view) 
                 font-size
                 :head (multiple-value-list (note-head-and-points (symbolic-dur object)))
                 :stem (= level 1)
                 :beams beams-num
                 :staff (get-edit-param param-obj :staff)
                 :draw-chans chan
                 :draw-vels vel
                 :draw-ports port
                 :draw-durs dur
                 :selection (if (find object selection) T selection)
                 :build-b-boxes t
                 ))

    ))

;;; REST
(defmethod draw-score-element ((object r-rest) tempo param-obj view &key font-size (y-shift 0) (level 1) selection)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (end (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
         (x1 (time-to-pixel view begin))
         (x2 (time-to-pixel view end)))
        
    (setf 
     (b-box object)
     (draw-rest object
                (time-to-pixel view begin)
                y-shift 
                (w view) (h view) 
                font-size 
                :head (multiple-value-list (rest-head-and-points (symbolic-dur object)))
                :staff (get-edit-param param-obj :staff)
                :selection (if (find object selection) T selection)
                :build-b-boxes t
                ))
    ))





     







;;; todo

;;; GROUPS
;;; TEMPO CHIFFRAGE
;;; SPACING !!


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


