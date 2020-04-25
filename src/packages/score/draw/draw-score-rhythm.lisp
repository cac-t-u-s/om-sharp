;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

;;; RHYTHMIC ASPECTS OF SCORE DRAWING (IN VOICE/POLY EDITORS)

(in-package :om)

;;;===============================================
;;; MEASURE
;;;===============================================

(defmethod draw-measure ((object measure) tempo param-obj view 
                         &key staff font-size position with-signature selection
                         (stretch 1) (x-shift 0) (y-shift 0)
                         time-function)

  (let* ((unit (font-size-to-unit font-size))
         
         ; (extra-units-for-bar (if (= position 1) 0 4))
         ; (extra-units-for-sig (if with-signature 6 0))
         (extra-units-for-bar (if (numberp stretch) 0 12))
         (extra-units-for-sig (if with-signature 0 0))
         (extra-pixels (if (typep view 'sequencer-track-view) 0   ; in sequencer track we draw everything proportional :-s 
                         (* (+ extra-units-for-bar extra-units-for-sig)
                            ;;; the stretch factor:
                            (if (numberp stretch) 
                                ;;; rhythmic with stretch
                                (* unit stretch) 
                              ;;; proportional:
                              (if (= position 1) (/ unit 1.5) 1)))))
         (bar-x-pix (- (+ (* x-shift unit) 
                          (funcall 
                           (or time-function #'(lambda (time) (time-to-pixel view time)))
                           (list ;; as a concvention we pass measure times as lists to 
                                 ;; the time-function, so we can take adequate decisions to position bars etc.
                            (beat-to-time (symbolic-date object) tempo)))
                          )
                       extra-pixels))
         (sig-x-pix (+ bar-x-pix (* (if (= position 1) -4 2)
                                    (if (numberp stretch) (* unit stretch) 1)))))
    
    (om-with-fg-color (when (find object selection) *score-selection-color*)
      
      (unless (= position 1)
        (draw-measure-bar bar-x-pix y-shift font-size staff)
        (om-draw-string bar-x-pix (* (+ 2 y-shift) unit) (number-to-string position)
                        :font (om-def-font :font1 :size (/ font-size 3))))
      
      (when with-signature
        (draw-time-signature (car (tree object)) 
                             sig-x-pix
                             y-shift font-size staff))
      )

    ;;; contents
    (let ((measure-beat-unit (/ (fdenominator (car (tree object)))
                                (find-beat-symbol (fdenominator (car (tree object)))))))
      (loop for element in (inside object) 
            for i from 0 do
            (draw-rhytmic-element element tempo param-obj view 
                                :level 1
                                :position i
                                :x-shift x-shift
                                :y-shift y-shift
                                :font-size font-size
                                :beat-unit measure-beat-unit
                                :selection selection
                                :time-function time-function
                                ))
      )

    (when (typep view 'score-view)
      (let ((staff-y-minmax (staff-y-range staff y-shift unit)))
        (setf (b-box object) 
              (make-b-box :x1 (min bar-x-pix sig-x-pix)
                          :x2 (if with-signature 
                                  (+ sig-x-pix (* 4 unit))
                                (+ bar-x-pix (* 4 unit)))
                          :y1 (car staff-y-minmax)
                          :y2 (cadr staff-y-minmax)
                          ))
        ))
    ))



;;;========================
;;; BEAMING / GROUPS
;;;========================

(defstruct beam-info (line) (direction) (beams))

;;; select up or down according to the mean-picth in this group vs. center of the current staff
;;; accordingly, the beam-point is <beam-size> above max pitch, or below min-pitch
;;; won't work for sub-groups...
(defmethod find-group-beam-line ((self group) staff beat-unit)
  
  (let* ((medium (staff-medium-pitch staff))
         (chords (get-all-chords self)) ;;; can be nil if only rests !!
         (best-directions (loop for c in chords when (get-notes c) collect (stem-direction c staff)))
         (pitches (loop for c in chords append (mapcar #'midic (get-notes c))))
         (default (staff-medium-pitch (car (last (staff-split staff)))))
         (p-max (or (list-max pitches) default)) ;;; default = middle ?
         (p-min (or (list-min pitches) default)) ;;; default = middle ?
         ;; (mean (* (+ p-max p-min) .5)) ;(/ (apply '+ pitches) (length pitches)) 
         (max-beams (or (list-max (mapcar #'(lambda (c) 
                                          (get-number-of-beams (* (symbolic-dur c) beat-unit)))
                                      chords))
                        0))
         (stem-length (+ *stem-height* (* (max 0 (- max-beams 2)) *beamthickness* 1.5))) ;; if more than 1 beams, add 2xbeam-h by beam
         )
    
    (cond ((and (> (abs (- p-min medium)) (abs (- p-max medium)))
                (< (- p-min medium) -1200)) ;;; p-min is really low
           ;;; up
           (make-beam-info :line (+ (pitch-to-line p-max) stem-length) :direction :up))
          ((> (- p-max medium) 1200)  ;;; p-max is really high
           ;;; down
           (make-beam-info :line (- (pitch-to-line p-min) stem-length) :direction :down))
          (t
           (if (>= (count :up best-directions) (count :down best-directions))
               (make-beam-info :line (+ (pitch-to-line p-max) stem-length) :direction :up)
             (make-beam-info :line (- (pitch-to-line p-min) stem-length) :direction :down)
             ))
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
;;; might return a list if the denominator is not a power of two
;;; => entry in the process of determining the beaming for a given chord.
;;; this code is directly adapted from OM6 score editors
(defun get-beaming (val)
  
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


(defun get-number-of-beams (val)
  (let ((beams (get-beaming val)))
    (if (listp beams) (car beams) beams)))
  

(defmethod beam-num ((self score-element) dur)
  (get-number-of-beams dur))

;;; gets the minimum number of beams in a group
(defmethod beam-num ((self group) dur)
 
  (let ((nd (or (numdenom self) (list 1 1))))  
    
    (loop for element in (inside self)
          minimize (beam-num element (* (/ (car nd) (cadr nd)) 
                                        (/ (symbolic-dur element)
                                           (symbolic-dur self))
                                        dur)))
    ))


;;; Get the depth of num/dem line in a group
(defmethod calcule-chiff-level ((self t)) 0)
(defmethod calcule-chiff-level ((self group))
  (+ (if (numdenom self) 1 0) 
     (loop for item in (inside self)
           maximize (calcule-chiff-level item))))

;;; just for debug
(defmethod draw-group-rect ((object group) view tempo level)

   (om-draw-rect (time-to-pixel view (beat-to-time (symbolic-date object) tempo))
                 0
                 (- (time-to-pixel view (beat-to-time (+ (symbolic-date object) (symbolic-dur object)) tempo))
                    (time-to-pixel view (beat-to-time (symbolic-date object) tempo)))
                 (- (h view) (* level 10))
                 :color (om-random-color .5)
                 :fill t)
   )



(defun get-mean-pitch (group) 
  (let ((pitches (mapcar #'midic (loop for elt in (get-all-chords group) append (get-notes elt)))))
    (and pitches 
         (/ (apply #'+ pitches) (length pitches)))
    ))

;;; beam-info : beam-pos (line) , beam-direction (:up/:down) , beams-already-drawn (list of indices)
(defmethod draw-rhytmic-element ((object group) tempo param-obj view 
                                 &key font-size (x-shift 0) (y-shift 0) (level 1) 
                                 position beam-info beat-unit rest-line
                                 selection time-function)
  
  (declare (ignore position))
  
  ;(print (list "=========="))
  ;(print (list "GROUP" (tree object) (numdenom object) (symbolic-dur object)))
  
  
  (let* ((staff (get-edit-param param-obj :staff))
         (unit (font-size-to-unit font-size))
         (x-shift-pix (* x-shift unit))
         (group-ratio (if (numdenom object) (fullratio (numdenom object)) 1))
         
         (beam-pos-and-dir (if beam-info
                               ;; the rest actual list of beams will be set later
                               (make-beam-info :line (beam-info-line beam-info) :direction (beam-info-direction beam-info))
                             (find-group-beam-line object staff (* beat-unit group-ratio))))

         (beams-from-parent (and beam-info (beam-info-beams beam-info)))
         
         (chords (get-all-chords object))
         (pix-beg (+ x-shift-pix
                     ;; (time-to-pixel view (beat-to-time (symbolic-date (car chords)) tempo))
                     (funcall (or time-function #'(lambda (time) (time-to-pixel view time)))
                              (beat-to-time (symbolic-date (car chords)) tempo))
                     ))
         (pix-end (+ x-shift-pix
                     ;; (time-to-pixel view (beat-to-time (symbolic-date (car (last chords))) tempo))
                     (funcall (or time-function #'(lambda (time) (time-to-pixel view time)))
                              (beat-to-time (symbolic-date (car (last chords))) tempo))
                     ))
         
         (n-beams (beam-num object (* (symbolic-dur object) beat-unit)))
         (group-beams (arithm-ser 1 n-beams 1))
         
         )
    
    (om-with-fg-color (if (find object selection) *score-selection-color* nil)
                               
                                     
    ;(print (list group-beams beams-from-parent))
    ;(draw-group-rect object view tempo level)
      (when (list-subtypep (inside object) 'group) 
        ;;; here we allow subgroups to get grouped individiually
        ;;; ..but is that always correct ?
        (setf group-beams (list (car group-beams))))
      
      ;;; local variables for the loop
      (let ((beams-drawn-in-sub-group nil))
    
        ;; the first group catching a beam information transfers to all descendants  
        (loop with sub-group-beams = 0
              for element in (inside object)
              for i from 0 do
                        
              ;;; here we hanlde "virtual sub-group" displayed as tied beams
              ;;; when a sucession of atomic elements in a same group have the same beaming
              (unless (typep element 'group)

                (let*  ((graphic-dur (* (symbolic-dur element) group-ratio beat-unit))
                        (n-beams-in-current (beam-num element graphic-dur))
                        (prev (previous-in-list (inside object) element nil))
                        (next (next-in-list (inside object) element nil))
                        (n-beams-in-previous (and prev (beam-num prev (* (symbolic-dur prev) group-ratio beat-unit))))
                        (n-beams-in-next (and next (beam-num next (* (symbolic-dur next) group-ratio beat-unit)))))
                
                ;(print (list element n-beams-in-current n-beams-in-previous n-beams-in-next n-beams))
                
                  (when  (> n-beams-in-current sub-group-beams) 
                    ;;; first of a new "sub-group" (same number of sub-beams) 
                    ;;; will draw additional single-beams on the right if any
                  
                    (setq sub-group-beams n-beams-in-current)
                  
                    ;;; if there's more beams on the right than on the left, individual beam will be towards right 
                    ;;; => to do so, position in group is artificially reset to 0
                    (when (and prev next 
                               (not (typep next 'group)) ;; also if this one is the last of its group
                               (> n-beams-in-next n-beams-in-previous))
                      (setq i 0))
                    )
                 
                
                  (when (> n-beams-in-current n-beams) ;;; more beams than the current "real" container group
                  
                    (if (= i 0) ;;; this is the 1st element
                      
                        (setq beams-drawn-in-sub-group (arithm-ser 1 (min n-beams-in-current (or n-beams-in-next 0)) 1))
                    
                      (progn
                        (setq beams-drawn-in-sub-group (arithm-ser 1 (min n-beams-in-current (or n-beams-in-previous 0)) 1))
                        ;;; draw beams between i and (i-1) and update the beaming count for sub-elements
                        (draw-beams (+ x-shift-pix 
                                       ;;(time-to-pixel view (beat-to-time (symbolic-date prev) tempo))
                                       (funcall (or time-function #'(lambda (time) (time-to-pixel view time)))
                                                (beat-to-time (symbolic-date prev) tempo))
                                       )
                                    (+ x-shift-pix 
                                       ;; (time-to-pixel view (beat-to-time (symbolic-date element) tempo))
                                       (funcall (or time-function #'(lambda (time) (time-to-pixel view time)))
                                                (beat-to-time (symbolic-date element) tempo))
                                       )
                                    (beam-info-line beam-pos-and-dir)  ;; the beam init line
                                    (beam-info-direction beam-pos-and-dir) ;; the beam direction
                                    beams-drawn-in-sub-group  ;; the beam numbers 
                                    y-shift staff font-size)
                        ))
                    )
                  ))
            
              (draw-rhytmic-element element tempo param-obj view 
                                    :x-shift x-shift
                                    :y-shift y-shift
                                    :font-size font-size
                                    :level (1+ level) 
                                    :beam-info (when beam-pos-and-dir 
                                                 ;;; send the beams already drawn in 3rd position
                                             ; passing '(0) at min will force the stem height of sub-chords 
                                             ; even if there is no beams
                                                 (setf (beam-info-beams beam-pos-and-dir)
                                                       (append (or group-beams '(0)) beams-drawn-in-sub-group))
                                                 beam-pos-and-dir)
                                              
                                    :position i
                                    :beat-unit (* beat-unit group-ratio)
                                    :rest-line (or rest-line (let ((mean-pitch (get-mean-pitch object)))
                                                               (when mean-pitch (pitch-to-line mean-pitch))))
                                    ;;; the higher-level group will determine the y-position for all rests
                                    :selection selection
                                    :time-function time-function
                                    )
              ))
    
    
      ;;; sub-groups or chords wont have to draw these beams
      (draw-beams pix-beg pix-end
                  (beam-info-line beam-pos-and-dir)  ;; the beam init line
                  (beam-info-direction beam-pos-and-dir) ;; the beam direction
                  (set-difference group-beams beams-from-parent)   ;; the beam numbers 
                  y-shift staff font-size)
   
      ;;; subdivision line and numbers 
      (when (numdenom object)
        (let* ((numdenom-level (calcule-chiff-level object)))
          ;;; chiflevel tells us how much above or below the beam this should be placed
          ;; (print (list object chiflevel (numdenom object)))
          (draw-group-div (numdenom object)
                          numdenom-level
                          pix-beg pix-end
                          (beam-info-line beam-pos-and-dir)  ;; the beam init line
                          (beam-info-direction beam-pos-and-dir) ;; the beam direction
                          y-shift staff font-size)
          ))
    
      (when (typep view 'score-view)
    
        (loop for sub in (inside object) 
              minimize (b-box-x1 (b-box sub)) into x1
              maximize (b-box-x2 (b-box sub)) into x2
              minimize (b-box-y1 (b-box sub)) into y1
              maximize (b-box-y2 (b-box sub)) into y2
              finally 
              (setf (b-box object) 
                    (make-b-box :x1 x1 :x2 x2 :y1 y1 :y2 y2)))

        ; (draw-b-box object)
        )
      )
    ))


#|
;;; passed through groups as "durtot" in OM6:
;;; starting at (* symb-beat-val factor) in measure

 (real-beat-val (/ 1 (fdenominator (first tree))))
 (symb-beat-val (/ 1 (find-beat-symbol (fdenominator (first tree)))))
 (dur-obj-noire (/ (extent item) (qvalue item)))
 (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
 (unite (/ durtot denom))
;;; => 

;;; NO GROUP RATIO:
   (let* ((dur-obj (/ (/ (extent item) (qvalue item)) 
                      (/ (extent self) (qvalue self)))))
     (* dur-obj durtot))

;;; GROUP RATIO:
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

; (note-head-and-points 3/8)


;;;===================
;;; CHORD
;;;===================

;;; beam-info : beam-pos (line) , beam-direction (:up/:down) , beams-already-drawn (list of indices)
(defmethod draw-rhytmic-element ((object chord) 
                               tempo param-obj view 
                               &key font-size (x-shift 0) (y-shift 0) (level 1) 
                               (position 0) beam-info beat-unit rest-line
                               selection time-function)
  
  (declare (ignore rest-line))

  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (s-dur (symbolic-dur object))
         
         (staff (get-edit-param param-obj :staff))
         (scale (get-edit-param param-obj :scale))
         (chan (get-edit-param param-obj :channel-display))
         (vel (get-edit-param param-obj :velocity-display))
         (port (get-edit-param param-obj :port-display))
         (dur (get-edit-param param-obj :duration-display))
         (offsets (get-edit-param param-obj :offsets))
         ;; (parent-nd (nth 3 beam-info))
         ;; (parent-ratio (if parent-nd (/ (car parent-nd) (cadr parent-nd)) 1))
         (graphic-dur (* s-dur beat-unit))
         
         ;;; from OM6.. 
         (beams-num (get-number-of-beams graphic-dur))
         (beams-from-parent (when beam-info (beam-info-beams beam-info)))
         (beams-to-draw (set-difference (arithm-ser 1 beams-num 1) beams-from-parent))
         ;; (propre-group (if (listp beams) (cadr beams)))
         
         (beam-start-line (if beams-from-parent (beam-info-line beam-info)
                            (if (and beam-info (equal :up (beam-info-direction beam-info)))
                                (+ (pitch-to-line (list-max (lmidic object))) *stem-height* (* beams-num .25))
                              (- (pitch-to-line (list-min (lmidic object))) *stem-height* (* beams-num .25)))))
         
         (create-bboxes (typep view 'score-view)))
    
    ;;; (print (list "chord" s-dur "unit:" beat-unit "=>" graphic-dur))
    ;; in fact propre-group (= when a standalone chord has a small group indication) will never happen (in OM)
    
    (let ((bbox? 
           (draw-chord object
                       begin
                       x-shift y-shift 
                       0 0
                       (w view) (h view) 
                       font-size
                       :head (multiple-value-list (note-head-and-points graphic-dur))
                       :stem (or (= level 1) beam-start-line)  ;; (car beam-info) is the beam-line 
                       :beams (list beams-to-draw position)
                       :staff staff :scale scale
                       :draw-chans chan
                       :draw-vels vel
                       :draw-ports port
                       :draw-durs dur
                       :offsets offsets
                       :selection (if (find object selection) T selection)
                       :time-function (or time-function #'(lambda (time) (time-to-pixel view time)))
                       :build-b-boxes create-bboxes
                       )))
      
      (when create-bboxes
        (setf (b-box object) bbox?))
      
      bbox?)))
      



;;;===================
;;; CONTINUATION-CHORD
;;;===================

(defmethod draw-rhytmic-element ((object continuation-chord) 
                               tempo param-obj view 
                               &key font-size (x-shift 0) (y-shift 0) (level 1) 
                               (position 0) beam-info beat-unit rest-line
                               selection time-function) 
  
  (declare (ignore rest-line))

  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (staff (get-edit-param param-obj :staff))
         (scale (get-edit-param param-obj :scale))
         
         (s-dur (symbolic-dur object))
         
         ;(parent-nd (nth 3 beam-info))
         ;(parent-ratio (if parent-nd (/ (car parent-nd) (cadr parent-nd)) 1))
         (graphic-dur (* s-dur beat-unit))
        
         ;;; from OM6.. 
         (beams-num (get-number-of-beams graphic-dur))
         (beams-from-parent (and beam-info (beam-info-beams beam-info)))
         (beams-to-draw (set-difference (arithm-ser 1 beams-num 1) beams-from-parent))
         ;;(propre-group (if (listp beams) (cadr beams)))
         
         (beam-start-line (and beam-info (beam-info-line beam-info)))
         ;(if (equal :up (beam-info-direction beam-info))
         ;    (+ (pitch-to-line (list-max (lmidic (get-real-chord object)))) *stem-height* (* beams-num .25))
         ;  (- (pitch-to-line (list-min (lmidic (get-real-chord object)))) *stem-height* (* beams-num .25)))))
         
         (create-bboxes (typep view 'score-view))
         )
    
    (let ((bbox? 
           (draw-chord (get-real-chord object)
                       begin
                       x-shift y-shift 
                       0 0
                       (w view) (h view) 
                       font-size
                       :head (multiple-value-list (note-head-and-points graphic-dur))
                       :stem (or (= level 1) beam-start-line) 
                       :beams (list beams-to-draw position)
                       :staff staff :scale scale
                       :selection (if (find object selection) T selection)
                       :tied-to-ms (beat-to-time (symbolic-date (previous-chord object)) tempo)
                       :time-function (or time-function #'(lambda (time) (time-to-pixel view time)))
                       ; no b-box for notes inside a continuation chord:
                       ; they actually just refer to the main chord's notes
                       :build-b-boxes nil 
                       )))
      
      (when create-bboxes 
        (setf (b-box object) bbox?))

      bbox?)
    ))


;;;=========
;;; REST
;;;=========

(defmethod draw-rhytmic-element ((object r-rest) tempo param-obj view &key 
                               font-size (x-shift 0) (y-shift 0) (level 1) 
                               position beam-info beat-unit rest-line
                               selection time-function)
  
  (declare (ignore level))
    
  (let* ((begin (beat-to-time (symbolic-date object) tempo))
         (graphic-dur (* (symbolic-dur object) beat-unit))
         (beams-num (get-number-of-beams graphic-dur))
         (beams-from-parent (and beam-info (beam-info-beams beam-info)))
         (beams-to-draw (set-difference (arithm-ser 1 beams-num 1) beams-from-parent))
         (beam-start-line (when beam-info (beam-info-line beam-info))))
    
    ;; (print (list beams-from-parent beams-to-draw beam-start-line))

    (let* ((create-bboxes (typep view 'score-view))
           (bbox? 
            (draw-rest object
                       begin
                       x-shift y-shift 
                       0 0 (w view) (h view) 
                       font-size 
                       :head (multiple-value-list (rest-head-and-points graphic-dur))
                       :line rest-line ;; can be NIL for display at default y-pos 
                       :stem  (when beams-from-parent beam-start-line)
                       :beams (list beams-to-draw position)
                       :staff (get-edit-param param-obj :staff)
                       :selection (if (find object selection) T selection)
                       :time-function (or time-function #'(lambda (time) (time-to-pixel view time)))
                       :build-b-boxes create-bboxes
                       )))
      (when create-bboxes
        (setf (b-box object) bbox?))
      
      )
    ))






