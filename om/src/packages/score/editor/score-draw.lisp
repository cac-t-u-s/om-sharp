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


;;;===============
;;; SMuFL fonts
;;;===============

(defvar *score-font* "Bravura")
(defvar *score-font-metadata-table* nil)
(defvar *score-font-bbox-table* nil)


(defvar *stemThickness* 0.12)
(defvar *beamThickness* 0.5)
(defvar *staffLineThickness* 0.13)
(defvar *legerLineThickness* 0.16)
(defvar *legerLineExtension* 0.4)
(defvar *thinBarlineThickness* 0.16)
(defvar *noteheadBlack_StemUpSE* '(1.18 0.168))
(defvar *noteheadBlack_StemDownNW* '(0.0 -0.168))
(defvar *noteheadBlack_bBoxNE* '(1.18 0.5))
(defvar *noteheadBlack_bBoxSW* '(0.0 -0.5))

(defvar *stem-height* 3)

(defun get-font-default (key &optional table)
  (if (consp key)
      (let ((subtable (gethash (car key) (or table *score-font-metadata-table*))))
        (get-font-default (cdr key) subtable))
    table))

;;; return (x1 y1 x2 y2)
(defun get-font-bbox (name)
  (let ((ne (get-font-default (list name "bBoxNE") *score-font-bbox-table*))
        (sw (get-font-default (list name "bBoxSW") *score-font-bbox-table*)))
    (list (car sw) (- (cadr ne)) (car ne) (- (cadr sw)))))
        
(defun get-font-width (name)
  (let ((bbox (get-font-bbox name)))
    (- (nth 2 bbox) (nth 0 bbox))))

(defun get-font-heigth (name)
  (let ((bbox (get-font-bbox name)))
    (- (nth 3 bbox) (nth 1 bbox))))


;(get-font-bbox "noteheadBlack")

(defun load-font-defaults () 
  (let* ((metadatafile (merge-pathnames "fonts/bravura_metadata.json" (om-resources-folder))))

    (setq *score-font-metadata-table* (yason:parse metadatafile))

    (setq *score-font-bbox-table* (get-font-default '("glyphBBoxes")))
    
    ;;; cache some of the most used values
    (setq *stemThickness* (get-font-default '("engravingDefaults" "stemThickness")))
    (setq *beamThickness* (get-font-default '("engravingDefaults" "beamThickness")))
    (setq *staffLineThickness* (get-font-default '("engravingDefaults" "staffLineThickness")))
    (setq *legerLineThickness* (get-font-default '("engravingDefaults" "legerLineThickness")))
    (setq *legerLineExtension* (get-font-default '("engravingDefaults" "legerLineExtension")))
    (setq *thinBarlineThickness* (get-font-default '("engravingDefaults" "thinBarlineThickness")))
    (setq *noteheadBlack_StemUpSE* (get-font-default '("glyphsWithAnchors" "noteheadBlack" "stemUpSE")))
    (setq *noteheadBlack_StemDownNW* (get-font-default '("glyphsWithAnchors" "noteheadBlack" "stemDownNW")))
    (setq *noteheadBlack_bBoxNE* (get-font-default '("glyphBBoxes" "noteheadBlack" "bBoxNE")))
    (setq *noteheadBlack_bBoxSW* (get-font-default '("glyphBBoxes" "noteheadBlack" "bBoxSW")))
    
    t))

; (load-font-defaults)

(add-om-init-fun 'load-font-defaults)

;;;===============
;;; STAVES
;;;===============

(defvar *score-staff-options* '(:g :f :gf :gg :ff :ggf :gff :ggff))
(defvar *score-fontsize-options* '(8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))

(defun staff-split (staff-symbol)
  (case staff-symbol
    (:g '(:g))
    (:f '(:f))
    (:gg '(:g :g+))
    (:ff '(:f- :f))
    (:gf '(:f :g))
    (:ggf '(:f :g :g+))
    (:gff '(:f- :f :g))
    (:ggff '(:f- :f :g :g+))
    ))

;;; we consider line 0 = E3
(defun staff-lines (staff)
  (case staff
    (:g+ '(8 9 10 11 12))
    (:g '(1 2 3 4 5))
    (:f '(-5 -4 -3 -2 -1))
    (:f- '(-12 -11 -10 -9 -8))
    ))

(defun head-leger-lines (head-line staff-lines)
  (cond ((>= head-line (1+ (car (last staff-lines)))) 
         (loop for i from (1+ (car (last staff-lines))) to head-line collect i))
        ((<= head-line (1- (car staff-lines))) 
         (loop for i = (1- (car staff-lines)) then (- i 1) while (>= i head-line) collect i))
        ((and (<= head-line 7) (>= head-line 6)) '(6 7))
        ((= head-line 0) '(0))
        ((and (<= head-line -6) (>= head-line -7)) '(-6 -7))
        (t nil)))
        
(defun staff-key-char (staff)
  (case staff
    (:g (code-char #xE050))
    (:g+ (code-char #xE050))
    (:f (code-char #xE062))
    (:f- (code-char #xE062))
    ))
     
(defun staff-key-line (staff)
  (case staff
    (:g 2)
    (:g+ 9)
    (:f -2)
    (:f- -9)
    ))

(defun staff-line-range (staff)
  (case staff
    (:g '(6400 7700))
    (:g+ '(8800 10100))
    (:f '(4300 5700))
    (:f- '(1900 3300))
    ))

(defun staff-medium-pitch (staff-symb)
  (let ((lines (staff-split staff-symb)))
    (round (+ (cadr (staff-line-range (car (last lines))))
              (car (staff-line-range (car lines))))
           2)))

;;; global display shift-down of the line-0(in number of lines) 
(defun calculate-staff-line-shift (staff)
  (+ 4 ;;; arbitrary top margin in units (line-h)
     ;;; + shift of the max number of lines above line-0 (C3)
     (last-elem (staff-lines (last-elem (staff-split staff))))))

;;;==================
;;; SCALES
;;;==================            
; (degree line accidental)
(defparameter *default-scale* 
  '((0 0 nil)
    (100 0 :sharp) 
    (200 0.5 nil)
    (300 0.5 :sharp)
    (400 1 nil)
    (500 1.5 nil)
    (600 1.5 :sharp)
    (700 2 nil)
    (800 2 :sharp)
    (900 2.5 nil)
    (1000 2.5 :sharp)
    (1100 3 nil)))

(defun accident-char (acc)
  (case acc
    (:sharp (code-char #xE262))
    (otherwise nil)))


(defun pitch-to-line (pitch &optional scale)
  (multiple-value-bind (octave interval) (floor pitch 1200) 
    ;;; line 0 is at 5th octave
    ;;; 1 octave is 3.5 lines
    (+ (* 3.5 (- octave 5))
       (nth 1 (find interval (or scale *default-scale*) 
                    :key 'car :test '>= :from-end t)))
    ))

(defun pitch-to-acc (pitch &optional scale)
  (nth 2 (find (mod pitch 1200) 
               (or scale *default-scale*) 
               :key 'car :test '>= :from-end t)))


;;; (lines are also the score "unit")
(defun line-to-pitch (line &optional scale)
  (let ((sc (or scale *default-scale*)))

    (multiple-value-bind (oct deg)
        (floor line 3.5) 
      
      (let ((scale-step (car (find deg sc :key 'cadr :test '=)))) ; the pitch corresponding to the scale 

        (unless scale-step ;;; not exacltly on the line (accidents and micro-intervals)
          (let* ((low-pos (position (* (floor deg .5) .5) sc :key 'cadr :test '=))
                 (high-pos (or 
                            (position (* (ceiling deg .5) .5) sc :key 'cadr :test '=)
                            (length sc)))
                 (delta (* 2 (mod deg .5))) ;; factor 0.0-1.0 between low and high
                 (pos (mod (+ low-pos (round (* delta (- high-pos low-pos)))) (length sc))))
            
            (when (and (= pos 0) (= high-pos (length sc)))
              (setq oct (1+ oct)))
            (setq scale-step (car (nth pos sc)))))
        
        (+ 6000 ;; the pitch of line 0, by convention
           (* 1200 oct) ;;; the number-of-3.5 line above or under line 0
           scale-step
           ))
      )))

;; (* .5 (floor 3.37 .5))
;; (find 0.3 *default-scale* :key 'cadr :test '<=)
;; (line-to-pitch 1.874)

;;;==================
;;; SYMBOLS
;;;==================  

(defun dynamics-char (symbol)
  (case symbol
    (:ppp (code-char #xE52A))
    (:pp (code-char #xE52B))
    (:p (code-char #xE520))
    (:mp (code-char #xE52C))
    (:mf (code-char #xE52D))
    (:f (code-char #xE522))
    (:ff (code-char #xE52F))
    (:fff (code-char #xE530))    
    (otherwise nil)))


(defun velocity-char (val)
  (dynamics-char 
   (nth (position val '(20 40 55 60 85 100 115 127) :test '<)
        '(:ppp :pp :p :mp :mf :f :ff :fff))))
  

;;;==================
;;; DRAW
;;;================== 

(defun font-size-to-unit (size) (* size .25))
(defun unit-to-font-size (u) (* u 4))

;;; by convention, unit = staff interline
(defun line-to-ypos (line shift unit)
  (* (- shift line) unit))

(defparameter *score-selection-color* (om-def-color :dark-red))


;;;=======================
;;; STAFF
;;;=======================

;;; x and y are in score-units
;;; w and h are pixel-size of the frame
(defun draw-staff (x y w h fontsize staff &key (margin-l 0) (margin-r 0) (keys t))
  
  (let* ((staff-elems (staff-split staff))
         (unit (font-size-to-unit fontsize)) 
         (shift (+ y (calculate-staff-line-shift staff)))
         (thinBarlineThickness (ceiling (* *thinBarLineThickness* unit)))
         (staffLineThickness (ceiling (* *staffLineThickness* unit))))
         
    (flet ((adjust-line-ypos (pos)
             (if (> unit 5) (round pos) pos))
           (x-pos (a) (* (+ x (or margin-l 0) a) unit)))

      (om-with-font 
       (om-make-font *score-font* fontsize)
  
       (loop for staff-symb in staff-elems do 
             ;;; lines
             (loop for line in (staff-lines staff-symb) do
                   (om-draw-line (x-pos 0) 
                                 (adjust-line-ypos (line-to-ypos line shift unit))
                                 (- w (* (or margin-r 0) unit))
                                 (adjust-line-ypos (line-to-ypos line shift unit))
                                 :line staffLineThickness
                                 ))
             ;;; clef
             (when keys 
               (om-draw-char (x-pos 1) 
                             (adjust-line-ypos (line-to-ypos (staff-key-line staff-symb) shift unit)) 
                             (staff-key-char staff-symb))
               )))
       
   ;;; vertical lines at beginning and the end
   (when margin-l
     (om-draw-line (round (x-pos 0)) 
                   (1- (adjust-line-ypos (line-to-ypos (car (staff-lines (car staff-elems))) shift unit)))
                   (round (x-pos 0)) 
                   (1+ (adjust-line-ypos (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit)))
                   :line thinBarlineThickness))
   (when margin-r
     (om-draw-line (- w (* margin-r unit)) 
                   (1- (adjust-line-ypos (line-to-ypos (car (staff-lines (car staff-elems))) shift unit)))
                   (- w (* margin-r unit)) 
                   (1+ (adjust-line-ypos (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit)))
                   :line thinBarlineThickness)
   ))))

;;;=======================
;;; MESURE BARS
;;;=======================
(defun draw-measure-bar (x fontsize staff)

  (let* ((staff-elems (staff-split staff))
         (unit (font-size-to-unit fontsize)) 
         (shift (calculate-staff-line-shift staff))
         (thinBarlineThickness (ceiling (* *thinBarLineThickness* unit))))
    
    (om-draw-line (- x unit) (1- (line-to-ypos (car (staff-lines (car staff-elems))) shift unit))
                  (- x unit) (1+ (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit))
                  :line thinBarlineThickness)
    ))

;;; more efficient: all bars at once
(defun draw-measure-bars (x-list fontsize staff)

  (let* ((staff-elems (staff-split staff))
         (unit (font-size-to-unit fontsize)) 
         (shift (calculate-staff-line-shift staff))
         (thinBarlineThickness (ceiling (* *thinBarLineThickness* unit))))
    
    (loop for x in x-list do
          (om-draw-line x (1- (line-to-ypos (car (staff-lines (car staff-elems))) shift unit))
                        x (1+ (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit))
                        :line thinBarlineThickness)
    )))


;;;=======================
;;; CHORDS
;;;=======================

;;; just for debug
(defmethod draw-b-box ((self score-object))
  (om-draw-rect (b-box-x1 (b-box self)) (b-box-y1 (b-box self))
                (b-box-w (b-box self)) (b-box-h (b-box self))
                :style '(2 2)))
    
(defun draw-chord (notes x y ; ref-position in score units
                         w h ; frame for drawing
                         fontsize 
                         &key 
                         (scale *default-scale*)
                         (staff :gf)
                         draw-chans draw-ports draw-vels draw-durs
                         selection
                         build-b-boxes)
 
  
  (let* ((unit (font-size-to-unit fontsize))
         (shift (+ y (calculate-staff-line-shift staff)))
         (staff-elems (staff-split staff))
         (staff-lines (apply 'append (mapcar 'staff-lines staff-elems)))
         (head-box (get-font-bbox "noteheadBlack"))
         (head-w (- (nth 2 head-box) (nth 0 head-box)))    ;;; the width in units of a note-head
         (head-h (- (nth 3 head-box) (nth 1 head-box)))    ;;; the width in units of a note-head
         (head-w-pix (* head-w unit))    ;;; the width in pixels of a note-head
         (acc-w (* (get-font-width "accidentalSharp") unit 1.5)) ;;; the width in pixels of an accident symbol
         
         cx1 cx2 cy1 cy2  ;;; chord bounding-box values (in pixels)
         
         (dur-max (apply #'max (mapcar 'dur notes)))
         (dur-factor (/ (- w 80) (* dur-max 2)))  ;;; we will multiply durations by this to display them on half-width of the view
         (unique-channel (and (not (equal draw-chans :hidden)) (all-equal (mapcar 'chan notes)) (chan (car notes))))
         (unique-vel (and draw-vels (all-equal (mapcar 'vel notes)) (vel (car notes))))
         (unique-port (and draw-ports (all-equal (mapcar 'port notes)) (or (port (car notes)) :default))))
   
    ;;; positions (in pixels) 
    (flet ((x-pos (a) (1+ (* (+ x a) unit))))
         
      (om-with-font 
       (om-make-font *score-font* fontsize)
    
       ;;; in case of a unique channel we set the color right here
       (om-with-fg-color (if (equal selection t)
                               
                             *score-selection-color*
                               
                           (and unique-channel
                                (member draw-chans '(:color :color-and-number))
                                (get-midi-channel-color unique-channel)))

         ;;; Global stuff here (not needed for the head loop)
         (let* ((pitches (mapcar 'midic notes))
                (pmin (apply #'min pitches))
                (pmax (apply #'max pitches))
                (l-min (pitch-to-line pmin scale))
                (l-max (pitch-to-line pmax scale))
                (y-min (line-to-ypos l-min shift unit))
                (y-max (line-to-ypos l-max shift unit))
                (stem-size (* unit *stem-height*))
                (stemThickness (* *stemThickness* unit)))
                 
           ;;; STEM
           (if (<= (/ (+ pmax pmin) 2) (staff-medium-pitch staff))
               ;;; stem up
                    
               (let ((stemUpSE-x (* unit (car *noteheadBlack_StemUpSE*)))
                     (stemUpSE-y (* unit (cadr *noteheadBlack_StemUpSE*))))
                 (om-draw-line (+ (x-pos 0) stemUpSE-x) (- y-min stemUpSE-y)
                               (+ (x-pos 0) stemUpSE-x) (- y-max stemUpSE-y stem-size)
                               :line stemThickness)
                 (setf cy1 (- y-max stemUpSE-y stem-size)) ;; initial extesion for the chord bounding-box as well
                 )
             ;;; stem down
             (let ((stemDownNW-x (* unit (car *noteheadBlack_StemDownNW*)))
                   (stemDownNW-y (* unit (cadr *noteheadBlack_StemDownNW*))))
               (om-draw-line (+ (x-pos 0) stemDownNW-x) (- y-max stemDownNW-y)
                             (+ (x-pos 0) stemDownNW-x) (- y-min stemDownNW-y (- stem-size))
                             :line stemThickness)
               (setf cy2 (- y-min stemDownNW-y (- stem-size))) ;; initial extesion for the chord bounding-box as well
               )
             )

           ;;; GLOBAL CHANNEL
           (when (and (member draw-chans '(:number :color-and-number)) 
                      unique-channel) ;;; if there's just one channel in the chord we'll display it here
             (om-draw-string (+ (x-pos 0) (* head-w-pix 2)) (+ y-min (* unit .5)) (format nil "~D" unique-channel) 
                             :font (om-def-font :font1 :size (round fontsize 2))))
                   
           ;;; GLOBAL MIDI PORT
           (when (and draw-ports unique-port) ;;; if there's just one port in the chord we'll display it here
             (if (member draw-chans '(:number :color-and-number))
                 (om-draw-string (+ (x-pos 0) (* head-w-pix 3)) (+ y-min (* unit 1.5)) (format nil "(~D)" unique-port)
                                 :font (om-def-font :font1 :size (round fontsize 2.5)))
               (om-draw-string (+ (x-pos 0) (* head-w-pix 2)) (+ y-min unit) (format nil "(~D)" unique-port)
                               :font (om-def-font :font1 :size (round fontsize 2))))
             )
             
           ;;; GLOBAL VELOCITY VALUE OR SYMBOL
           (when (and draw-vels unique-vel) ;;; if there's just one velocity in the chord we'll display it somewhere else
             (case draw-vels
               (:value 
                (om-draw-string (x-pos 0) (+ y-min (* unit 4)) (format nil "~D" unique-vel) 
                                :font (om-def-font :font1 :size (round fontsize 2.5))))
               (:symbol 
                (om-draw-char (x-pos 0) (+ y-min (* unit 4)) (velocity-char unique-vel)))
               ))
                   
           (when draw-durs
             (om-draw-line (x-pos 0) (- h 50) (+ (x-pos 0) (* dur-max dur-factor)) (- h 50) :style '(2 2))
             (om-draw-line (x-pos 0) (- h 54) (x-pos 0) (- h 46))
             (om-draw-line (+ (x-pos 0) (* dur-max dur-factor)) (- h 54) (+ (x-pos 0) (* dur-max dur-factor)) (- h 46))
             (om-draw-string (+ (x-pos 0) (* dur-max dur-factor .5) -20) (- h 55) 
                             (format nil "~D ms" dur-max) 
                             :font (om-def-font :font1)))
           )
           
           
         ;;; for the note-heads loop
         (let* ((head-char (code-char #xE0A4))
                (accidental-columns nil)
                (head-columns nil)
                (leger-lines nil)
                (legerLineThickness (* *legerLineThickness* unit))
                (legerLineExtension (* *legerLineExtension* unit)))
             
           (loop for n in (sort notes '< :key 'midic) do
                 
                 (let* ((line (pitch-to-line (midic n) scale))
                        (line-y (line-to-ypos line shift unit))
                        (head-col (position line head-columns 
                                            :test #'(lambda (line col)
                                                      ;;; there's no other note in this col at less than 1 line away
                                                      (not (find line col :test #'(lambda (a b) (< (abs (- b a)) 1)))))))
                        (head-x nil))
                 
                   (if head-col 
                       (push line (nth head-col head-columns))
                     (setf head-col (length head-columns) ;; add a new column
                           head-columns (append head-columns (list (list line)))))
                    
                   (setq head-x (x-pos (* head-col head-w)))

                   (when build-b-boxes 
                     (let* (;;; bounding-box values
                            (nx1 head-x)
                            (nx2 (+ head-x head-w-pix))
                            (ny1 (line-to-ypos (+ line (* head-h .5)) shift unit))
                            (ny2 (line-to-ypos (- line (* head-h .5)) shift unit))) ;;; lines are expressed bottom-up !!
                       
                       ;;; bounding-box is in pixels
                       (setf (b-box n) (make-b-box :x1 nx1 :x2 nx2 :y1 ny1 :y2 ny2)
                             ;;; update the chord bbox as well..
                             cx1 (if cx1 (min cx1 nx1) nx1)
                             cx2 (if cx2 (max cx2 nx2) nx2)
                             cy1 (if cy1 (min cy1 ny1) ny1)
                             cy2 (if cy2 (max cy2 ny2) ny2)
                             )))
                           
                   ;;; LEGER-LINES
                   (let ((l-lines (head-leger-lines line staff-lines)))
                     ;;; draw add leger-line(s) to the record if they are not already there 
                     (loop for ll in l-lines 
                           unless (find ll leger-lines)
                           do (let ((ypos (line-to-ypos ll shift unit))) 
                                (om-draw-line (- (x-pos 0) legerLineExtension) ypos 
                                              (+ (x-pos 0) head-w-pix legerLineExtension) ypos
                                              :line legerLineThickness)
                                (push ll leger-lines))))

                   (om-with-fg-color 
                       
                       (if (or (equal selection t)
                               (and (listp selection) (find n selection)))
                           
                           *score-selection-color*
                         
                         (cond ((member draw-chans '(:color :color-and-number))
                                (om-make-color-alpha 
                                 (get-midi-channel-color (chan n))
                                 (if (equal draw-vels :alpha) (/ (vel n) 127) 1)))
                               ((equal draw-vels :alpha) (om-make-color 0 0 0 (/ (vel n) 127)))
                               (t nil)))
                     
                     ;;; HEAD
                     (om-draw-char head-x line-y head-char
                                   :font (when (equal draw-vels :size) (om-make-font *score-font* (* (vel n) fontsize .02))))
                      
                     ;;; ACCIDENT (if any)
                     (let ((acc (pitch-to-acc (midic n) scale)))
                       (when acc
                         (let ((col (position (midic n) accidental-columns 
                                              :test #'(lambda (pitch col)
                                                        ;;; there's no other note in this col at less than an octave
                                                        (not (find pitch col :test #'(lambda (a b) (< (abs (- b a)) 1200))))))))
                           (if col 
                               (push (midic n) (nth col accidental-columns))
                          
                             (setf col (length accidental-columns) ;; add a new column
                                   accidental-columns (append accidental-columns (list (list (midic n))))))
                           
                           (om-draw-char (- (x-pos 0) (* (1+ col) acc-w)) line-y (accident-char acc))
                           )))
                     
                     ;;; DURATION line (maybe)
                     (when draw-durs
                       
                       (om-with-fg-color
                           (if t ;(member draw-chans '(:color :color-and-number))
                               (om-make-color-alpha (get-midi-channel-color (chan n)) .5)
                             (om-make-color 0 0 0 .5))
                         (om-draw-rect (x-pos 0) (- line-y (* unit .25)) (* (dur n) dur-factor) (* unit .5) :fill t))
                       )

                     ;;; MIDI channel (maybe)
                     ;;; if there's just one channel in the chord we'll display it somewhere else
                     (when (and (member draw-chans '(:number :color-and-number)) 
                                (not unique-channel)) 
                       (om-draw-string (+ (x-pos 0) (* head-w-pix 2)) (+ line-y (* unit .5)) (format nil "~D" (chan n)) 
                                       :font (om-def-font :font1 :size (round fontsize 2)))
                       )
                 
                     ;;; MIDI port (maybe)
                     ;;; if there's just one port in the chord we'll display it somewhere else
                     (when (and draw-ports (not unique-port)) 
                       (om-draw-string (+ (x-pos 0) 
                                          (if (member draw-chans '(:number :color-and-number)) (* head-w-pix 3.5) (* head-w-pix 2.5)))
                                       (+ line-y unit) (format nil "(~D)" (port n)) 
                                       :font (om-def-font :font1 :size (round fontsize 2.5)))
                       )

                     ;;; VELOCITY (maybe)
                     ;;; if there's just one velocity in the chord we'll display it somewhere else
                     (when (and draw-vels (not unique-vel)) 
                       (case draw-vels
                         (:value 
                          (om-draw-string (x-pos 0) (+ line-y (* unit 2)) (format nil "~D" (vel n)) 
                                          :font (om-def-font :font1 :size (round fontsize 2.5))))
                         (:symbol 
                          (om-draw-char (x-pos 0) (+ line-y (* unit 2)) (velocity-char (vel n))))
                         ))
                 
                     
                     )) ;;; END DO
                 ) ;;; END NOTES LOOP
           ) ;;; END OF THE NOTE-HEADS LET
             
         )))
    
    ;;; return the bounding box
    (when build-b-boxes 
      (make-b-box :x1 cx1 :x2 cx2 :y1 cy1 :y2 cy2))
    ))


