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

;;; MINIVIEW
(defmethod score-object-mini-view ((self voice) box x-pix y-u w h)
  
  (draw-staff x-pix y-u w h (fontsize box) (get-edit-param box :staff) :margin-l 1 :margin-r 1 :keys t)

  (loop for m in (inside self)
        for i from 1
        do (draw-score-element m (tempo self) box (frame box) :y-shift y-u :font-size (fontsize box) :position i)
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
                   (draw-score-element m (tempo object) (object editor) view :position i
                                       :font-size (editor-get-edit-param editor :font-size)
                                       :selection (selection editor))
                   )))
          ))
  )


;;;===============================================
;;; MEASURE
;;;===============================================

(defmethod draw-score-element ((object measure) tempo param-obj view 
                               &key font-size (y-shift 0) (level 0) position beam-info selection)

  (declare (ignore level beam-info))

  (let ((x-pix (time-to-pixel view (beat-to-time (symbolic-date object) tempo))))
    
    (unless (= position 1) 
      (draw-measure-bar x-pix y-shift font-size (get-edit-param param-obj :staff))
      (om-draw-string x-pix (- y-shift 1) (number-to-string position)
                      :font (om-def-font :font1 :size (/ font-size 3))))
                   
    (loop for element in (inside object) 
          for i from 0 do
          (draw-score-element element tempo param-obj view 
                              :level 1
                              :position i
                              :y-shift y-shift
                              :font-size font-size
                              :selection selection))
    ))

;;;========================
;;; BEAMING / GROUPS
;;;========================

;;; select up or down according to the mean-picth in this group vs. center of the current staff
;;; accordingly, the beam-point is <beam-size> above max pitch, or below min-pitch
(defmethod find-group-beam-line ((self group) staff)
  
  (let* ((medium (staff-medium-pitch staff))
         (chords (get-all-chords self)) ;;; can be nil if only rests !!
         (pitches (apply 'append (mapcar 'lmidic chords)))
         (p-max (list-max pitches)) (p-min (list-min pitches))
         (mean (if pitches 
                   ;(/ (apply '+ pitches) (length pitches)) 
                   (* (+ p-max p-min) .5)
                 7100)) ;;; default = B4
         (max-beams (1- (list-max (mapcar #'(lambda (c) (get-number-of-beams (symbolic-dur c))) chords)))))
    
    (if (<= mean medium) 
        ;;; up
        (values (+ (pitch-to-line p-max) *stem-height* (* max-beams .5)) :up)
      ;;; down
      (values (- (pitch-to-line p-min) *stem-height* (* max-beams .5)) :down)
      )
    ))


(defun find-group-symbol (val)
  (let* ((den (denominator val))
         (bef (bin-value-below den)))
    (list 
     (note-strict-beams (/ 1 bef)) 
     (denominator (/ bef den)))))

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

;;; gives number of beams for a given division
;;; => entry in the process of determining the beaming for a given chord.
;;; this code is directly adapted from OM6 score editors
(defun get-number-of-beams (val)
  
  (let* ((num (numerator val))
         (den (denominator val))
         (bef (bin-value-below num)))
    
     (cond
      ((= bef num)
       (note-strict-beams (/ num den)))
      
      ((= (* bef 1.5) num)
       (note-strict-beams (/ bef den)))
       
      ((= (* bef 1.75) num)
       (note-strict-beams (/ bef den)))

      (t 0))
     ))

;;; Get the depth of num/dem line in a group
(defmethod calcule-chiff-level ((self t)) 0)
(defmethod calcule-chiff-level ((self group))
  (+ (if (numdenom self) 1 0) 
     (loop for item in (inside self)
           maximize (calcule-chiff-level item))))



(defmethod draw-score-element ((object group) tempo param-obj view 
                               &key font-size (y-shift 0) (level 1) position beam-info selection)
  
  (declare (ignore position))

  (let* ((staff (get-edit-param param-obj :staff))
         (beam-n-and-dir (or (first-n beam-info 2) ;; the rest of the list is local info
                             (multiple-value-list (find-group-beam-line object staff))))
         (beams-from-parent (nth 2 beam-info)))
    
    ;; the first group catching a beam information transfers to all descendants 
        
    (let ((beams (loop for c in (get-all-chords object)
                       minimize (let ((bn (get-number-of-beams (symbolic-dur c))))
                                  (if (listp bn) (car bn) bn)) into n 
                       minimize (beat-to-time (symbolic-date c) tempo) into t1
                       maximize (beat-to-time (symbolic-date c) tempo) into t2
                       finally return (list (arithm-ser 1 n 1) t1 t2)
                       )))

      (loop for element in (inside object)
            for i from 0 do
            (draw-score-element element tempo param-obj view 
                                :y-shift y-shift
                                :font-size font-size
                                :level (1+ level) 
                                :beam-info (when beam-n-and-dir (append beam-n-and-dir (list (car beams))))
                                ;;; send the beams already drawn in 3rd position
                                :position i
                                :selection selection))

      ;;; sub-groups or chords wont draw these  
      (draw-beams (time-to-pixel view (nth 1 beams)) 
                  (time-to-pixel view (nth 2 beams))
                  (car beam-n-and-dir)  ;; the beam init line
                  (cadr beam-n-and-dir) ;; the beam direction
                  (set-difference (car beams) beams-from-parent)     ;; the beam numbers 
                  y-shift staff font-size)
      
      )

    ;;; subdivision line and numbers 
    (let* ((numdenom-level (calcule-chiff-level object)))
      ;;; chiflevel tells us how much above or below the beam this should be placed
      ;; (print (list object chiflevel (numdenom object)))
      )
    )
    
  )



#|
;;; passed through groups as "durtot" in OM6:
;;; starting at (* symb-beat-val factor) in measure

 (real-beat-val (/ 1 (fdenominator (first tree))))
 (symb-beat-val (/ 1 (find-beat-symbol (fdenominator (first tree)))))
 (dur-obj-noire (/ (extent item) (qvalue item)))
 (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
 (unite (/ durtot denom))
;;; => 
(if (not group-ratio) 
     (let* ((dur-obj (/ (/ (extent item) (qvalue item)) 
                        (/ (extent self) (qvalue self)))))
       (* dur-obj durtot))
   (let* ((operation (/ (/ (extent item) (qvalue item)) 
                        (/ (extent self) (qvalue self))))
          (dur-obj (numerator operation)))
     (setf dur-obj (* dur-obj (/ num (denominator operation))))
     (* dur-obj unite)))

;;; passed through groups as "ryth"
;;; starting at  (list real-beat-val (nth i (cadr (tree self)))) in measure
;;; => 
(list (/ (car (second ryth)) (first ryth))
      (nth i (cadr (second ryth))))

|#


;;;===================
;;; CHORD
;;;===================

(defmethod draw-score-element ((object chord) 
                               tempo param-obj view 
                               &key font-size (y-shift 0) (level 1) (position 0) beam-info selection)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         
         (staff (get-edit-param param-obj :staff))
         (chan (get-edit-param param-obj :channel-display))
         (vel (get-edit-param param-obj :velocity-display))
         (port (get-edit-param param-obj :port-display))
         (dur (get-edit-param param-obj :duration-display))

         ;;; from OM6.. 
         (beams (get-number-of-beams (symbolic-dur object)))
         (beams-from-parent (nth 2 beam-info))
         (beams-num (if (listp beams) (car beams) beams))
         (beams-to-draw (set-difference (arithm-ser 1 beams-num 1) beams-from-parent))
         ;;(propre-group (if (listp beams) (cadr beams)))
         )
    
    ;; (print (list "chord" (symbolic-dur object) beams))
    ;; in fact propre-group (= when a standalone chord has a small group indication) will never happen (in OM)
    
    (setf 
     (b-box object)
     (draw-chord object
                 (time-to-pixel view begin)
                 y-shift 
                 (w view) (h view) 
                 font-size
                 :head (multiple-value-list (note-head-and-points (symbolic-dur object)))
                 :stem (or (= level 1) (car beam-info))  ;; (car beam-info) is the beam-line 
                 :beams (list beams-to-draw position)
                 :staff staff
                 :draw-chans chan
                 :draw-vels vel
                 :draw-ports port
                 :draw-durs dur
                 :selection (if (find object selection) T selection)
                 :build-b-boxes t
                 ))

    ))

;;;=========
;;; REST
;;;=========

(defmethod draw-score-element ((object r-rest) tempo param-obj view &key font-size (y-shift 0) (level 1) position beam-info selection)
  
  (let* ((begin (beat-to-time (symbolic-date object) tempo)))
        
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
;;; LONG-HEAD (see draw-chord)
;;; RESTS: GROUPS/BEAMING AND Y-POSITIONS
;;; TIES
;;; TEMPO / CHIFFRAGE MESURE
;;; SPACING
;;; TEMPO CHANGE
;;; Grace notes


#|
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


