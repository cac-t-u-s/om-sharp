;============================================================================
; o7: visual programming language for computer-aided music composition
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

; (mc degre line accidental)
(defparameter *default-scale* 
  '((0 -1 nil)
    (100 -1 :sharp) 
    (200 -0.5 nil)
    (300 -0.5 :sharp)
    (400 0 nil)
    (500 .5 nil)
    (600 .5 :sharp)
    (700 1 nil)
    (800 1 :sharp)
    (900 1.5 nil)
    (1000 1.5 :sharp)
    (1100 2 nil)))

(defun accident-char (acc)
  (case acc
    (:sharp (code-char #xE262))
    (otherwise nil)))
  
;;; we consider line 0 = E3
(defun staff-lines (staff)
  (case staff
    (:g+ '(7 8 9 10 11))
    (:g '(0 1 2 3 4))
    (:f '(-6 -5 -4 -3 -2))
    (:f- '(-13 -12 -11 -10 -9))
    ))


(defun head-leger-lines (head-line staff-lines)
  (cond ((>= head-line (1+ (car (last staff-lines)))) 
         (loop for i from (1+ (car (last staff-lines))) to head-line collect i))
        ((<= head-line (1- (car staff-lines))) 
         (loop for i = (1- (car staff-lines)) then (- i 1) while (>= i head-line) collect i))
        ((and (<= head-line 6) (>= head-line 5)) '(5 6))
        ((= head-line -1) '(-1))
        ((and (<= head-line -7) (>= head-line -8)) '(-7 -8))
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
    (:g 1)
    (:g+ 8)
    (:f -3)
    (:f- -10)
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

(defun font-size-to-unit (size) (* size .25))
(defun unit-to-font-size (u) (* u 4))


(defmethod score-draw ((self chord) x y w h fontsize &optional 
                       (scale *default-scale*)
                       (staff :ggff))
 
   (let* ((staff-elems (staff-split staff))
          (staff-lines (apply 'append (mapcar 'staff-lines staff-elems)))

          (unit (font-size-to-unit fontsize))
          (interline unit) 
          
          (line-0-pos (+ y (* interline (+ 5 ;;; top margin in units (line-h)
                                           ;;; + shift of the max number of lines above line-0 (E3)
                                           (last-elem staff-lines)))))
                     
          (x0 (+ x 8 (max (/ w 2) 40)))
          (thinBarlineThickness (* *thinBarLineThickness* unit))
          (staffLineThickness (* *staffLineThickness* unit))
          (stemThickness (* *stemThickness* unit))
          (legerLineThickness (* *legerLineThickness* unit))
          (legerLineExtension (* *legerLineExtension* unit)))
          
       
     (flet ((line-pos (line) 
              (- line-0-pos (* line interline))))
      
       (om-with-font 
       
        (om-make-font *score-font* fontsize)
       
        (loop for staff-symb in staff-elems do 
              (loop for line in (staff-lines staff-symb) do
                    ;;; line
                    (om-draw-line (+ x 10) (line-pos line)
                                  (+ x w -10) (line-pos line)
                                  :line staffLineThickness)
                    ;;; clef
                    (om-draw-char (+ x 20) 
                                  (line-pos (staff-key-line staff-symb)) 
                                  (staff-key-char staff-symb))
                    ))
            
        ;;; vertical lines at beginning and the end
        (om-draw-line (+ x 10) (line-pos (car staff-lines))
                      (+ x 10) (line-pos (last-elem staff-lines))
                      :line thinBarlineThickness)
        (om-draw-line (+ x w -10) (line-pos (car staff-lines))
                      (+ x w -10) (line-pos (last-elem staff-lines))
                      :line thinBarlineThickness)
      
        
        (let ((head-w (* (get-font-width "noteheadBlack") unit))
              (acc-w (* (get-font-width "accidentalSharp") unit 1.5))
              (stem-size (* unit 3))
              (head-char (code-char #xE0A4))
              (accidental-columns nil)
              (head-columns nil)
              (leger-lines nil))

          (loop for n in (sort (inside self) '< :key 'midic) 
                minimize (midic n) into pmin
                maximize (midic n) into pmax
                do
                (let* ((line (pitch-to-line (midic n) scale))
                       (line-y (line-pos line)))

                  ;;; note head
                  (let ((head-col (position line head-columns 
                                       :test #'(lambda (line col)
                                                 ;;; there's no other note in this col at less than 1 line away
                                                 (not (find line col :test #'(lambda (a b) (< (abs (- b a)) 1))))))))
                    (if head-col 
                        (push line (nth head-col head-columns))
                      (setf head-col (length head-columns) ;; add a new column
                            head-columns (append head-columns (list (list line)))))
                    
                    (let ((head-x (+ x0 (* head-col head-w))))
                      
                      (om-draw-char head-x line-y head-char)
                      
                      ;;; add leger-line(s) (if needed)
                      (setf leger-lines (remove-duplicates (append leger-lines (head-leger-lines line staff-lines))))
                      
                      ))
                  
                  ;;; accident (if any)
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
                          
                        (om-draw-char (- x0 (* (1+ col) acc-w)) line-y (accident-char acc))
                        )))
                  
                  )
                
                finally
               
                (if (<= (/ (+ pmax pmin) 2) (staff-medium-pitch staff))
                       ;;; stem up
                    
                    (let ((stemUpSE-x (* unit (car *noteheadBlack_StemUpSE*)))
                          (stemUpSE-y (* unit (cadr *noteheadBlack_StemUpSE*))))
                      (om-draw-line (+ x0 stemUpSE-x) (- (line-pos (pitch-to-line pmin scale)) stemUpSE-y)
                                    (+ x0 stemUpSE-x) (- (line-pos (pitch-to-line pmax scale)) stemUpSE-y stem-size)
                                    :line stemThickness))
                  ;;; stem down
                  (let ((stemDownNW-x (* unit (car *noteheadBlack_StemDownNW*)))
                        (stemDownNW-y (* unit (cadr *noteheadBlack_StemDownNW*))))
                    (om-draw-line (+ x0 stemDownNW-x) (- (line-pos (pitch-to-line pmax scale)) stemDownNW-y)
                                  (+ x0 stemDownNW-x) (- (line-pos (pitch-to-line pmin scale)) stemDownNW-y (- stem-size))
                                  :line stemThickness))
                  )
                )
          
          ;;; draw chord-leger-lines
          (loop for ll in leger-lines do
                (let ((ypos (line-pos ll))) 
                  (om-draw-line (- x0 legerLineExtension) ypos 
                                (+ x0 head-w legerLineExtension) ypos
                                :line legerLineThickness)
                  ))

          ))
       t)))






                