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

(defparameter *stem-height* 3.25)

(defun get-smufl-metadata (key &optional table)
  (if (consp key)
      (let ((subtable (gethash (car key) (or table *score-font-metadata-table*))))
        (get-smufl-metadata (cdr key) subtable))
    table))

;;; return (x1 y1 x2 y2)
(defun get-font-bbox (smufl-name)
  (let ((ne (get-smufl-metadata (list smufl-name "bBoxNE") *score-font-bbox-table*))
        (sw (get-smufl-metadata (list smufl-name "bBoxSW") *score-font-bbox-table*)))
    (list (car sw) (- (cadr ne)) (car ne) (- (cadr sw)))))
        
(defun get-font-width (smufl-name)
  (let ((bbox (get-font-bbox smufl-name)))
    (- (nth 2 bbox) (nth 0 bbox))))

(defun get-font-heigth (smufl-name)
  (let ((bbox (get-font-bbox smufl-name)))
    (- (nth 3 bbox) (nth 1 bbox))))


;(get-font-bbox "noteheadBlack")

(defun load-font-defaults () 

  (let* ((metadatafile (merge-pathnames "fonts/bravura_metadata.json" (om-resources-folder))))

    (setq *score-font-metadata-table* (yason:parse metadatafile))

    (setq *score-font-bbox-table* (get-smufl-metadata '("glyphBBoxes")))
    
    ;;; cache some of the most used values
    (setq *stemThickness* (get-smufl-metadata '("engravingDefaults" "stemThickness")))
    (setq *beamThickness* (get-smufl-metadata '("engravingDefaults" "beamThickness")))
    (setq *staffLineThickness* (get-smufl-metadata '("engravingDefaults" "staffLineThickness")))
    (setq *legerLineThickness* (get-smufl-metadata '("engravingDefaults" "legerLineThickness")))
    (setq *legerLineExtension* (get-smufl-metadata '("engravingDefaults" "legerLineExtension")))
    (setq *thinBarlineThickness* (get-smufl-metadata '("engravingDefaults" "thinBarlineThickness")))
    (setq *noteheadBlack_StemUpSE* (get-smufl-metadata '("glyphsWithAnchors" "noteheadBlack" "stemUpSE")))
    (setq *noteheadBlack_StemDownNW* (get-smufl-metadata '("glyphsWithAnchors" "noteheadBlack" "stemDownNW")))
    (setq *noteheadBlack_bBoxNE* (get-smufl-metadata '("glyphBBoxes" "noteheadBlack" "bBoxNE")))
    (setq *noteheadBlack_bBoxSW* (get-smufl-metadata '("glyphBBoxes" "noteheadBlack" "bBoxSW")))
    
    t))

; (load-font-defaults)

(add-om-init-fun 'load-font-defaults)


;;;===============
;;; SMUFL CHARS
;;;===============
;;; all symbols below have a string identifyier in SMuFL which alows to retrieve some metadata
;;; the functions below return two values: char-code and SMuFL string ID
;;; - actually the char code is also retrievable from this ID in the standard's glyphnames.json file

(defun accident-char (acc)
  (case acc
    (:flat (values (code-char #xE260) "accidentalFlat"))
    (:natural (values (code-char #xE261) "accidentalNatural"))
    (:sharp (values (code-char #xE262) "accidentalSharp"))
    (:db-sharp (values (code-char #xE263) "accidentalDoubleSharp"))
    (:1/4-flat (values (code-char #xE280) "accidentalQuarterToneFlatStein"))
    (:3/4-flat (values (code-char #xE281) "accidentalThreeQuarterTonesFlatZimmermann"))
    (:1/4-sharp (values (code-char #xE282) "accidentalQuarterToneSharpStein"))
    (:3/4-sharp (values (code-char #xE283) "accidentalThreeQuarterTonesSharpStein"))
    (:1/8-sharp (values (code-char #xE27A) "accidentalArrowUp"))
    (:3/8-sharp (values (code-char #xE299) "accidentalHalfSharpArrowUp"))
    (:5/8-sharp (values (code-char #xE274) "accidentalThreeQuarterTonesSharpArrowUp"))
    (:7/8-sharp (values (code-char #xE29B) "accidentalOneAndAHalfSharpsArrowUp"))
    
    (otherwise nil)))

(defun staff-key-char (staff)
  (case staff
    (:g (values (code-char #xE050) "gClef"))
    (:g+ (values (code-char #xE050) "gClef"))
    (:g- (values (code-char #xE052) "gClef8vb"))
    (:f (values (code-char #xE062) "fClef"))
    (:f- (values (code-char #xE062) "fClef"))
    (:line (values (code-char #xE069) "unpitchedPercussionClef1"))
    (otherwise nil)
    ))


(defun dynamics-char (symbol)
  (case symbol
    (:ppp (values (code-char #xE52A) "dynamicPPP"))
    (:pp (values (code-char #xE52B) "dynamicPP"))
    (:p (values (code-char #xE520) "dynamicPiano"))
    (:mp (values (code-char #xE52C) "dynamicMP"))
    (:mf(values  (code-char #xE52D) "dynamicMF"))
    (:f (values (code-char #xE522) "dynamicForte"))
    (:ff (values (code-char #xE52F) "dynamicFF"))
    (:fff (values (code-char #xE530) "dynamicFFF"))    
    (otherwise nil)))

(defparameter *smufl-note-heads* 
  '(("noteheadHalf" #xE0A3)
    ("noteheadBlack" #xE0A4)
    ("noteheadXBlack" #xE0A9)
    ("noteheadCircleX" #xE0B3)
    ("noteheadSquareWhite" #xE0B8)
    ("noteheadSquareWhite" #xE0B9)
    ("noteheadTriangleUpWhite" #xE0BD)
    ("noteheadTriangleUpBlack" #xE0BE)
    ("noteheadDiamondBlackWide" #xE0DC)
    ("noteheadDiamondWhiteWide" #xE0DE)))

(defmethod* notehead-char-codes (char)
  :icon :score
  :initvals '(#xE0A3)
  :doc "Some SMuFL note heads char-codes.

See more in https://www.smufl.org/version/latest/range/noteheads/
"
  :menuins (list (list 0 *smufl-note-heads*))
  char)

(defun char-code-to-head-char (code)
  (values 
   (code-char code) 
   (car (find code *smufl-note-heads* :key #'cadr))
   ))

(defun note-head-char (symbol)
  (case symbol
    (:head-1/4 (values (code-char #xE0A4) "noteheadBlack"))
    (:head-1/2 (values (code-char #xE0A3) "noteheadHalf"))
    (:head-1 (values (code-char #xE0A2) "noteheadWhole"))
    (:head-2 (values  (code-char #xE0A1) "noteheadDoubleWholeSquare"))
    (:breve (values (code-char #xE0A0) "noteheadDoubleWhole"))  ;; this is equivalent to :head-2
    (:head-4 (values (code-char #xE937) "mensuralNoteheadLongaWhite"))
    (:head-8 (values (code-char #xE933) "mensuralNoteheadMaximaWhite"))
    
    (otherwise (cond ((consp symbol) ;;; super-longa
                      (note-head-char :head-8))
                     ((numberp symbol)
                      (char-code-to-head-char symbol))
                     (t (note-head-char :head-1/4))))
    ))


(defun flag-char (orientation n)
  (if (equal orientation :up)
      (case n
        ;(1 (values (code-char #xE240) "flag8thUp"))
        (1 (values (code-char #xE250) "flagInternalUp"))
        (2 (values (code-char #xE242) "flag16thUp"))
        (3 (values (code-char #xE244) "flag32ndUp"))
        (4 (values (code-char #xE246) "flag64thUp"))
        (5 (values (code-char #xE248) "flag128thUp"))
        (6 (values (code-char #xE24A) "flag256thUp")))
    (case n
      ;(1 (values (code-char #xE241) "flag8thUp"))
      (1 (values (code-char #xE251) "flagInternalDown"))
      (2 (values (code-char #xE243) "flag16thUp"))
      (3 (values (code-char #xE245) "flag32ndUp"))
      (4 (values (code-char #xE247) "flag64thUp"))
      (5 (values (code-char #xE249) "flag128thUp"))
      (6 (values (code-char #xE24B) "flag256thUp")))
    ))
        
 
(defun rest-char (symbol)
  (case symbol
    (:rest-4 (values (code-char #xE4E1) "restLonga"))
    (:rest-2 (values (code-char #xE4E2) "restDoubleWhole"))
    (:rest-1 (values (code-char #xE4E3) "restWhole"))
    (:rest-1/2 (values (code-char #xE4E4) "restHalf"))
    (:rest-1/4 (values (code-char #xE4E5) "restQuarter"))
    (:rest-1/8 (values (code-char #xE4E6) "rest8th"))
    (:rest-1/16 (values (code-char #xE4E7) "rest16th"))
    (:rest-1/32 (values (code-char #xE4E8) "rest32nd"))
    (:rest-1/64 (values (code-char #xE4E9) "rest64th"))
    (:rest-1/128 (values (code-char #xE4EA) "rest128th"))
    (otherwise (if (consp symbol) ;;; super-longa
                   (values (code-char #xE4EE) "restHBar")
                 (rest-char :head-1/128)))
    ))


(defun timesig-number-char (char)
  (case char
    (#\0 (values (code-char #xE080) "timeSig0"))
    (#\1 (values (code-char #xE081) "timeSig1"))
    (#\2 (values (code-char #xE082) "timeSig2"))
    (#\3 (values (code-char #xE083) "timeSig3"))
    (#\4 (values (code-char #xE084) "timeSig4"))
    (#\5 (values (code-char #xE085) "timeSig5"))
    (#\6 (values (code-char #xE086) "timeSig6"))
    (#\7 (values (code-char #xE087) "timeSig7"))
    (#\8 (values (code-char #xE088) "timeSig8"))
    (#\9 (values (code-char #xE089) "timeSig9"))
    ))

(defun tempo-note (fig)
  (case fig
    (1/8 (values (code-char #xE1D7) "noteEighthUp")) ;; <= verify this
    (otherwise (values (code-char #xE1D5) "noteQuarterUp"))))

; alternate:
; (values (code-char #xE1FC) "textAugmentationDot")
(defun dot-char () 
  (values (code-char #xE1E7) "augmentationDot"))


;;;===============
;;; STAVES
;;;===============

(defparameter *score-staff-options* '(:g :f :g- :gf :gg :ff :ggf :gff :ggff :line :empty))

(defparameter *score-fontsize-options*
  #+darwin '(8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96)
  #-darwin '(6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 60 63 66 69 72 75 78 81 84 87 90))

(defun staff-split (staff-symbol)
  (case staff-symbol
    (:g '(:g))
    (:f '(:f))
    (:g- '(:g-))
    (:f- '(:f-))
    (:g+ '(:g+))
    (:gg '(:g :g+))
    (:ff '(:f- :f))
    (:gf '(:f :g))
    (:ggf '(:f :g :g+))
    (:gff '(:f- :f :g))
    (:ggff '(:f- :f :g :g+))
    (:line '(:line))
    (:empty '(:empty))
    ))

;;; we consider line 0 = E3
(defun staff-lines (staff)
  (case staff
    (:g+ '(8 9 10 11 12))
    (:g '(1 2 3 4 5))
    (:g- '(-2.5 -1.5 -0.5 0.5 1.5))
    (:f '(-5 -4 -3 -2 -1))
    (:f- '(-12 -11 -10 -9 -8))
    (:line '(0))
    (:empty '(0))
    ))

(defun staff-lower-line (staff)
  (car (staff-lines (car (staff-split staff)))))

(defun staff-higher-line (staff)
  (last-elem (staff-lines (last-elem (staff-split staff)))))

(defun head-leger-lines (head-line staff-lines)
  (when (> (length staff-lines) 1) ;;; don't do it for empty or "line" staffs
    (cond ((>= head-line (1+ (car (last staff-lines)))) 
           (loop for i from (1+ (car (last staff-lines))) to head-line collect i))
          ((<= head-line (1- (car staff-lines))) 
           (loop for i = (1- (car staff-lines)) then (- i 1) while (>= i head-line) collect i))
          ((and (<= head-line 7) (>= head-line 6)) '(6 7))
          ((and (= head-line 0) (<= (- (car (last staff-lines)) (car staff-lines)) 5)) nil)
	  ((= head-line 0) '(0))
          ((and (<= head-line -6) (>= head-line -7)) '(-6 -7))
          (t nil))))
          
(defun staff-key-line (staff)
  (case staff
    (:g 2)
    (:g+ 9)
    (:g- -1.5)
    (:f -2)
    (:f- -9)
    (:line 0)
    (:empty 0)
    ))

(defun staff-line-range (staff)
  (case staff
    (:g '(6400 7700))
    (:g+ '(8800 10100))
    (:g- '(5200 6500))
    (:f '(4300 5700))
    (:f- '(1900 3300))
    (:line '(6000 6000))
    (:empty '(6000 6000))
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

(defun pitch-to-line (pitch &optional scale)

  (multiple-value-bind (octave interval) (floor pitch 1200)

    (let* ((scale 
            (append 
             (or scale *default-scale*)
             `((1200 3.5 nil))) ;; in cas ewe are actually closer to the next octave
            )
           (scale-step (closest-match interval scale :key #'car ))) 

      (values 
       ;;; line 0 is at 5th octave
       ;;; 1 octave is 3.5 lines
       (+ (* 3.5 (- octave 5))
          (nth 1 scale-step)
          )

       ;;; returns the accidental as second value
       (nth 2 scale-step))
    )))


(defun pitch-to-acc (pitch &optional scale)
  (multiple-value-bind (octave interval) (round pitch 1200)   
    (declare (ignore octave))
    (let* ((scale 
            (append 
             (or scale *default-scale*)
             `((1200 3.5 nil))) ;; in cas ewe are actually closer to the next octave
            )
           (scale-step (closest-match interval scale :key #'car )))

      (nth 2 scale-step))))


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


(defun velocity-char (val)
  (dynamics-char 
   (nth (position val '(20 40 55 60 85 100 115 127) :test '<=)
        '(:ppp :pp :p :mp :mf :f :ff :fff))))


;;;==================
;;; DRAW
;;;================== 

(defun font-size-to-unit (size) (* size .25))
(defun unit-to-font-size (u) (* u 4))

;;; by convention, unit = staff interline in PIXEL
(defun line-to-ypos (line shift unit)
  (* (- shift line) unit))

(defparameter *score-selection-color* (om-def-color :dark-red))


;;;=======================
;;; STAFF
;;;=======================

(defun staff-y-range (staff y-shift unit)
  (let ((y-full-shift (+ y-shift (calculate-staff-line-shift staff)))
        (staff-elems (staff-split staff)))
    (list (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) y-full-shift unit)
          (line-to-ypos (car (staff-lines (car staff-elems))) y-full-shift unit))))

;;; x and y are in pixels 
;;; y-u is additional shift in score-units
;;; w and h are pixel-size of the frame
;;; margins in units (= proportional to fontsize)
(defun draw-staff (x y y-u w h fontsize staff &key (margin-l 0) (margin-r 0) (keys t))
  
  (declare (ignore h))
  
  (unless (equal staff :empty)
    (let* ((staff-elems (staff-split staff))
           (unit (font-size-to-unit fontsize)) 
           (shift (+ y-u (calculate-staff-line-shift staff)))
           (thinBarlineThickness (* *thinBarLineThickness* unit))
           (staffLineThickness (* *staffLineThickness* unit))
           (x1 (+ x (* (or margin-l 0) unit)))
           (x2 (+ x (- w (* (or margin-r 0) unit)))))
         
      (flet ((adjust-line-ypos (pos)
               (+ y (if (> unit 5) (round pos) pos))))

        (om-with-font 
         (om-make-font *score-font* #+darwin fontsize #-darwin (* 3/4 fontsize))
  
         (loop for staff-symb in staff-elems do 
               ;;; lines
               (loop for line in (staff-lines staff-symb) do
                     (om-draw-line x1
                                   (adjust-line-ypos (line-to-ypos line shift unit))
                                   x2
                                   (adjust-line-ypos (line-to-ypos line shift unit))
                                   :line staffLineThickness
                                   ))
               ;;; clef
               (when keys 
                 (om-draw-char (+ x1 (* unit 1)) 
                               (adjust-line-ypos (line-to-ypos (staff-key-line staff-symb) shift unit)) 
                               (staff-key-char staff-symb))
                 )))
       
        ;;; vertical lines at beginning and the end
        (when margin-l
          (om-draw-line x1
                        (adjust-line-ypos (line-to-ypos (car (staff-lines (car staff-elems))) shift unit))
                        x1
                        (adjust-line-ypos (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit))
                        :line thinBarlineThickness))
        (when margin-r
          (om-draw-line x2 
                        (adjust-line-ypos (line-to-ypos (car (staff-lines (car staff-elems))) shift unit))
                        x2 
                        (adjust-line-ypos (line-to-ypos (last-elem (staff-lines (last-elem staff-elems))) shift unit))
                        :line thinBarlineThickness)
          )))
    ))

;;;=======================
;;; MESURE BARS
;;;=======================
(defun draw-measure-bar (x-pix y-units fontsize staff)

  (let* ((unit (font-size-to-unit fontsize)) 
         (staff-y-minmax (staff-y-range staff y-units unit))
         (thinBarlineThickness (* *thinBarLineThickness* unit)))
    
    (om-draw-line x-pix (- (car staff-y-minmax) 1) 
                  x-pix (+ (cadr staff-y-minmax) 1)
                  :line thinBarlineThickness)
    ))


;; we must use lwx::bmp-string here because string by default only supports base-char charcaters
;; another possibility is to do (lispworks:set-default-character-element-type 'character)

(defun draw-time-signature (signature x-pix y-units fontsize staff)

  (let* ((staff-elems (staff-split staff))
         (unit (font-size-to-unit fontsize)) 
         (shift (+ y-units (calculate-staff-line-shift staff))))
    
    (om-with-font 
     (om-make-font *score-font* #+darwin fontsize #-darwin (* 3/4 fontsize))

     (loop for staff-elem in staff-elems do

           (let ((mid-line (+ (car (staff-lines staff-elem)) 
                              (/ (- (car (last (staff-lines staff-elem))) (car (staff-lines staff-elem))) 2))))

             (om-draw-string x-pix (line-to-ypos (+ mid-line 1) shift unit)
                           (map 'lw::bmp-string #'timesig-number-char (number-to-string (car signature))))
           
           (om-draw-string x-pix (line-to-ypos (- mid-line 1) shift unit)
                           (map 'lw::bmp-string #'timesig-number-char (number-to-string (cadr signature))))
           ))
     )))

(defmethod draw-tempo ((object t) x-pix y-pix font-size) nil)

(defmethod draw-tempo ((object voice) x-pix y-pix font-size) 
  (let* ((unit (font-size-to-unit font-size)))
    
    (om-draw-char x-pix y-pix (tempo-note :quater)
                  :font (om-make-font *score-font* #+darwin (/ font-size 2) #-darwin (* 3/8 font-size)))
    
    (om-draw-string (+ x-pix unit) y-pix 
                    (format nil "= ~D"  (tempo object))
                    :font (om-def-font :font1 :size (round font-size 2.2)))
    ))
     



;;;=======================
;;; RHYTHM
;;;=======================


;;;========================
;;; HEADS
;;;========================

(defun note-symbol (val)
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


;;; get the notehead symbol and eventually points corresponding to a given duration
(defun note-head-and-points (dur)
   (let* ((num (numerator dur))
          (den (denominator dur))
          (bef (bin-value-below num))
          (points 0) (symbol :head-1/4))
     
     (cond
      ((= bef num)
       (setf symbol (note-symbol (/ num den))))
      ((= (* bef 1.5) num)
       (setf symbol (note-symbol (/ bef den)))
       (setf points 1))
      ((= (* bef 1.75) num)
       (setf symbol (note-symbol (/ bef den)))
       (setf points 2)))
     
     (values symbol points)))


(defun rest-head-and-points (dur)
   (let* ((num (numerator dur))
          (den (denominator dur))
          (bef (bin-value-below num))
          (points 0) (symbol :rest-1/4))
     (cond
      ((= bef num)
       (setf symbol (rest-symbol (/ num den))))
      ((= (* bef 1.5) num)
       (setf symbol (rest-symbol (/ bef den)))
       (setf points 1))
      ((= (* bef 1.75) num)
       (setf symbol (rest-symbol (/ bef den)))
       (setf points 2)))
     
     (values symbol points)))


;;;========================
;;; BEAMING
;;;========================

(defun draw-beams (begin-pix end-pix beam-line direction beams y-shift staff fontsize)
  
  (let* ((unit (font-size-to-unit fontsize))
         (beamThickness (* *beamThickness* unit))
         (yshift (+ y-shift (calculate-staff-line-shift staff))))
    
    (loop for i in beams do
      (if (equal direction :up)

          (om-draw-rect (1- (+ begin-pix (* unit (car *noteheadBlack_StemUpSE*)))) ;;; floor ?
                        (- (line-to-ypos (- beam-line (* i .75) -1) yshift unit) 0)
                        (2+ (- end-pix begin-pix))
                        (1+ beamThickness)
                        ;:color (om-random-color .8)
                        :fill t)

        (om-draw-rect (1- (+ begin-pix (* unit (car *noteheadBlack_StemDownNW*))))
                      (1+ (round (- (line-to-ypos (+ beam-line (* i .75) -1) yshift unit) beamThickness)))
                      (2+ (- end-pix begin-pix))
                      (1+ beamThickness)
                      ;:color (om-random-color 0.7)
                      :fill t)
        )
      )))

;;;========================
;;; GROUP DIV NOTATION
;;;========================

(defun draw-group-div (num-den level begin-pix end-pix line direction y-shift staff fontsize)
  
  (let* ((unit (font-size-to-unit fontsize))
         (linethickness (ceiling (* *stemthickness* unit)))
         (yshift (+ y-shift (calculate-staff-line-shift staff)))
         (width (- end-pix begin-pix))
         (x (if (equal direction :up) 
                (floor (+ begin-pix (* unit (car *noteheadBlack_StemUpSE*))))
              (floor (+ begin-pix (* unit (car *noteheadBlack_StemDownNW*))))))
         (y (if (equal direction :up) 
                (line-to-ypos (+ line (* 2 level)) yshift unit)
              (line-to-ypos (- line (* 2 level)) yshift unit)))
         (dy (round (if (equal direction :up) unit (- unit)) 2.))
         (div-str (if (power-of-two-p (cadr num-den))
                        (format nil "~A" (car num-den))
                    (format nil "~A:~A" (car num-den) (cadr num-den))))
         (font (om-def-font :font1 :size (round fontsize 2.2)))
         (mid-space (* (1+ (length div-str)) unit)))

    ;; (om-draw-string begin-pix 20 (format nil "~A" level))
    
    (om-draw-string (+ x (round (- width mid-space) 2) (* unit .5)) 
                    (if (equal direction :up) (+ y 4) (+ y unit -4)) 
                    div-str
                    :font font)

    (om-draw-line x y (+ x (round (- width mid-space) 2)) y :line linethickness)
    (om-draw-line x y x (+ y dy) :line linethickness)
    
    (om-draw-line (+ x (round (+ width mid-space) 2)) y (+ x width) y :line linethickness)
    (om-draw-line (+ x width) y (+ x width) (+ y dy) :line linethickness)
         
    ))


;;;========================
;;; STEM
;;;========================

;;; also works if chord is a 'note'
(defun stem-direction (chord staff)
  (let* ((pitches (sort (mapcar 'midic (get-notes chord)) '<))
         (pmin (car pitches))
         (pmax (car (last pitches)))
         (test-op (if (or (equal staff :line) (equal staff :empty)) '> '>=)))
    (if (and pmax pmin (funcall test-op (/ (+ pmax pmin) 2) (staff-medium-pitch staff)))
        :down :up)))

;;;=======================
;;; CHORDS
;;;=======================

;;; just for debug
(defmethod draw-b-box ((self score-element))
  (when (b-box self)
    (om-draw-rect (b-box-x1 (b-box self)) (b-box-y1 (b-box self))
                  (b-box-w (b-box self)) (b-box-h (b-box self))
                  :style '(2 2)))
  (b-box self))
    

;;; also works if chord is just a 'note':

(defun draw-chord (chord x-ms ; x in ms
                         x-units ; a global x-shift in units
                         y-units ; y-position in score units
                         x y ; absolute offsets in pixels
                         w h ; frame dimensions for drawing
                         fontsize 
                         &key
                         (head :head-1/4)
                         (scale :scale-1/2)
                         (staff :gf)
                         (stem t) ;; stem can be T or a position (in line or score units) 
                         (beams '(nil 0)) ;; (beams position-in-group)
                         draw-chans draw-ports draw-vels draw-durs draw-dur-ruler
                         selection
                         tied-to-ms
                         offsets
                         (time-function #'identity)
                         build-b-boxes)
  
  (declare (ignore w))
 
  (when (get-notes chord)
    (om-with-translation x y 

      (let* ((head-symb (let ((extra-head (find 'head-extra (extras chord) :key #'type-of))) 
                          (if extra-head (head-char extra-head)
                            (if (consp head) (car head) head))))
             (n-points (if (consp head) (cadr head) 0))
             (head-extra-number (if (listp head-symb) (car head-symb)))
             (scale (get-the-scale scale)))
      
        ;;; (print (list head head-symb)) 
        ;;; TODO
        ;;; if head-symb is a list => long figure and the value in (car head-symb) is the duration
    
        (multiple-value-bind (head-char head-name)
            (note-head-char head-symb)

          (let* ((unit (font-size-to-unit fontsize))
                 (x-pix (+ (* x-units unit) (funcall time-function x-ms)))
                 (notes (get-notes chord))
                 (shift (+ y-units (calculate-staff-line-shift staff)))
                 (staff-elems (staff-split staff))
                 (staff-lines (apply 'append (mapcar 'staff-lines staff-elems)))
                 (head-box (get-font-bbox head-name))
                 (head-w (- (nth 2 head-box) (nth 0 head-box)))    ;;; the width in units of a note-head
                 (head-h (- (nth 3 head-box) (nth 1 head-box)))    ;;; the height in units of a note-head
                 (head-w-pix (* head-w unit))    ;;; the width in pixels of a note-head
                 (acc-w (* (get-font-width "accidentalSharp") unit 1.5)) ;;; the width in pixels of an accident symbol
             
                 (stem-direction :up) 
                 cx1 cx2 cy1 cy2  ;;; chord bounding-box values (in pixels)
             
                 (dur-max (apply #'max (mapcar 'dur notes)))
                 ;; (dur-factor (/ (- w 80) (* dur-max 2)))  ;;; we will multiply durations by this to display them on half-width of the view
                 (unique-channel (and (not (equal draw-chans :hidden)) (all-equal (mapcar 'chan notes)) (chan (car notes))))
                 (unique-vel (and (all-equal (mapcar 'vel notes)) (vel (car notes))))
                 (unique-port (and draw-ports (all-equal (mapcar 'port notes)) (or (port (car notes)) :default))))
           
            (om-with-font 
             (om-make-font *score-font* #+darwin fontsize #-darwin (* 3/4 fontsize))
                 
             ;;; in case of a unique channel we set the color right here
             (om-with-fg-color (if (equal selection t)
                               
                                   *score-selection-color*
                               
                                 (and unique-channel
                                      (member draw-chans '(:color :color-and-number))
                                      (get-midi-channel-color unique-channel)))

               ;;; Global stuff here (not needed for the head loop)
               (let* ((chord-notes (if (and (equal offsets :sep-notes) (find 0 notes :key #'offset))
                                       (remove-if-not #'zerop notes :key #'offset)
                                     notes))
                      (pitches (sort (mapcar 'midic chord-notes) '<))
                      (pmin (car pitches))
                      (pmax (car (last pitches)))
                      (l-min (pitch-to-line pmin scale))
                      (l-max (pitch-to-line pmax scale))
                      (y-min (line-to-ypos l-min shift unit))
                      (y-max (line-to-ypos l-max shift unit))
                      (stem? (and stem ;;; passed as argument 
                                  (not (or (listp head-symb) (member head-symb '(:head-1 :head-2 :head-4 :head-8)))))))
             
                 ;;; STEM
                 (when stem?
               
                   ;;; determine direction (default is :up)
                   (setq stem-direction (if (numberp stem) ;;; in a group: stem is already decided
                                            (if (> stem l-min) :up :down)  ;;; stem is higher than the min note of the chord: :down
                                          (stem-direction chord staff)
                                          ))
                     
                   (let ((stem-size (* unit *stem-height*))
                         (stemThickness (* *stemThickness* unit))
                         (stemUpSE-x (* unit (car *noteheadBlack_StemUpSE*))) ;;; SE = anchor for stem-up
                         (stemUpSE-y (* unit (cadr *noteheadBlack_StemUpSE*)))
                         (stemDownNW-x (* unit (car *noteheadBlack_StemDownNW*))) ;;; NW = anchor for stem-down
                         (stemDownNW-y (* unit (cadr *noteheadBlack_StemDownNW*)))
                         (n-beams (car beams))
                         (pos-in-group (cadr beams)))
                 
                     (if (numberp stem) ;;; we are in a group and the max position is fixed (stem, in line-number)
                     
                         (let ((stem-pos (line-to-ypos stem shift unit)))
                       
                           (if (equal stem-direction :up)
                         
                               ;;; up
                               (progn 
                                 (om-draw-line (+ x-pix stemUpSE-x) (- y-min stemUpSE-y)
                                               (+ x-pix stemUpSE-x) stem-pos
                                               :line stemThickness :end-style :projecting)
                             
                                 (when n-beams
                                   (if (zerop pos-in-group) ;; first elem
                                       (draw-beams x-pix (+ x-pix head-w-pix) stem :up n-beams y-units staff fontsize)
                                     (draw-beams (- x-pix head-w-pix) x-pix stem :up n-beams y-units staff fontsize)
                                     ))
                          
                                 (setf cy1 stem-pos))

                          
                             ;;; down
                             (progn
                               (om-draw-line (+ x-pix stemDownNW-x) (- y-max stemDownNW-y)
                                             (+ x-pix stemDownNW-x) stem-pos
                                             :line stemThickness :end-style :projecting)
                               (when n-beams
                                 (if (zerop pos-in-group) ;; first elem
                                     (draw-beams x-pix (+ x-pix head-w-pix) stem :down n-beams y-units staff fontsize)
                                   (draw-beams (- x-pix head-w-pix) x-pix stem :down n-beams y-units staff fontsize)
                                   ))
                           
                               (setf cy2 stem-pos))
                             )
                           )
                 
                       ;;; otherwise, this is a standalone chord with stem 
                       (if (equal stem-direction :up)
                       
                           ;;; stem up
                           (let ((stem-x (+ x-pix stemUpSE-x)))
                            
                             (om-draw-line stem-x (- y-min stemUpSE-y)
                                           stem-x (- y-max stemUpSE-y stem-size)
                                           :line stemThickness)
                       
                             ;; initial extesion for the chord bounding-box 
                             (setf cy1 (- y-max stemUpSE-y stem-size))

                             (when n-beams
                               (let ((flag-char (flag-char :up (list-max n-beams))))
                                 (om-draw-char stem-x (- y-max stemUpSE-y stem-size) flag-char)
                                 ))
                             )

                         ;;; stem down
                         (let ((stem-x (+ x-pix stemDownNW-x)))
                           (om-draw-line stem-x (- y-max stemDownNW-y)
                                         stem-x (- y-min stemDownNW-y (- stem-size))
                                         :line stemThickness)
                          
                           ;; initial extesion for the chord bounding-box 
                           (setf cy2 (- y-min stemDownNW-y (- stem-size))) 
                          
                           (when n-beams
                             (let ((flag-char (flag-char :down (list-max n-beams))))
                               (om-draw-char stem-x (- y-min stemDownNW-y (- stem-size)) flag-char)
                               ))
                           )
                         )
                       )
                     ))
               
                 ;;; GLOBAL CHANNEL
                 (when (and (member draw-chans '(:number :color-and-number)) 
                            unique-channel) ;;; if there's just one channel in the chord we'll display it here
                   (om-draw-string (+ x-pix (* head-w-pix 2)) (+ y-min (* unit .5)) (format nil "~D" unique-channel) 
                                   :font (om-def-font :font1 :size (round fontsize 2))))
             
                 ;;; GLOBAL MIDI PORT
                 (when (and draw-ports unique-port) ;;; if there's just one port in the chord we'll display it here
                   (if (member draw-chans '(:number :color-and-number))
                       (om-draw-string (+ x-pix (* head-w-pix 3)) (+ y-min (* unit 1.5)) (format nil "(~D)" unique-port)
                                       :font (om-def-font :font1 :size (round fontsize 2.5)))
                     (om-draw-string (+ x-pix (* head-w-pix 2)) (+ y-min unit) (format nil "(~D)" unique-port)
                                     :font (om-def-font :font1 :size (round fontsize 2)))
                     ))
             
                 ;;; GLOBAL VELOCITY VALUE OR SYMBOL
                 (let ((vel-extra (find 'vel-extra (extras chord) :key #'type-of)))
                   (when (or (and draw-vels unique-vel) ;;; if there's just one velocity in the chord we'll display it somewhere else
                             vel-extra)   ;;; ... or if there is a vel-extra
                     (let ((vel-x-pix (+ x-pix (if vel-extra (* (or (dx vel-extra) 0) unit) 0)))
                           (vel-y-pix (+ y-min (* unit 4) (if vel-extra (* (or (dy vel-extra) 0) unit) 0))))
                       
                       (cond ((or vel-extra (equal :symbol draw-vels))
                              (om-draw-char vel-x-pix vel-y-pix (velocity-char unique-vel)))
                             ((equal :value draw-vels) 
                              (om-draw-string vel-x-pix vel-y-pix (format nil "~D" unique-vel) 
                                              :font (om-def-font :font1 :size (round fontsize 2.5))))
                             ))))
                 
                   
                 (when (and draw-durs draw-dur-ruler)
               
                   (let ((end-pix (+ (* x-units unit) (funcall time-function (+ x-ms dur-max)))))
                     (om-draw-line x-pix (- h 50) end-pix (- h 50) :style '(2 2))
                     (om-draw-line x-pix (- h 54) x-pix (- h 46))
                     (om-draw-line end-pix (- h 54) 
                                   end-pix (- h 46))
                     (om-draw-string (+ end-pix -20) (- h 55) 
                                     (format nil "~D ms" dur-max) 
                                     :font (om-def-font :font1))))
             
             ; draw symbolic dur (for debug)
             ;(om-draw-string x-pix (- h 55) (format nil "~D" (symbolic-dur chord)) :font (om-def-font :font1))
             ; draw absolute dur (for debug)
             ;(om-draw-string x-pix (- h 35) (format nil "~D" (ldur chord)) :font (om-def-font :font1))
             
                 ;;; for the note-heads loop
                 (let* ((accidental-columns nil)
                        (head-columns nil)
                        (leger-lines nil)
                        (legerLineThickness (* *legerLineThickness* unit))
                        (legerLineExtension (* *legerLineExtension* unit)))
               
                   ;; number for big durations
                   (when head-extra-number
                     (let ((font (om-def-font :font1 :size (round fontsize 2.2))))
                       (om-draw-string (+ x-pix (* head-w-pix .1)) 
                                       (+ y-min (* unit 2)) (number-to-string head-extra-number)
                                       :font font)
                       ))
               
            
               
                   (loop for n in (sort (copy-list notes) '< :key 'midic) do

                         (multiple-value-bind (line acc) (pitch-to-line (midic n) scale)
                           
                           (let* ((line-y (line-to-ypos line shift unit))
                                  (head-col (position line head-columns 
                                                      :test #'(lambda (line col)
                                                                ;;; there's no other note in this col at less than 1 line away
                                                                (not (find line col :test #'(lambda (a b) (< (abs (- b a)) 1)))))))
                                  (display-offset (and (not (zerop (offset n))) offsets (not (equal offsets :hidden))))
                                  (head-x nil)
                                  (note-font (cond 
                                              ((equal draw-vels :size)
                                               (om-make-font *score-font* (* #+darwin 1 #-darwin 3/4 (vel n) fontsize .02)))
                                              ((and display-offset (equal offsets :sep-notes))
                                               (om-make-font *score-font* (* fontsize .7)))
                                              (t nil))))
                 
                             (if head-col 
                                 (push line (nth head-col head-columns))
                               (setf head-col (length head-columns) ;; add a new column
                                     head-columns (append head-columns (list (list line)))))
                       
                             (setq head-x 
                                   (if display-offset
                                       ;;; specific offset:
                                       (+ (* x-units unit) (funcall time-function (+ x-ms (offset n))))
                                     ;;; x-pix from head-col
                                     (+ x-pix (* head-col head-w unit))))

                             (let (;;; bounding-box values
                                   (nx1 head-x)
                                   (nx2 (+ head-x head-w-pix))
                                   (ny1 (line-to-ypos (+ line (* head-h .5)) shift unit))
                                   (ny2 (line-to-ypos (- line (* head-h .5)) shift unit))) ;;; lines are expressed bottom-up !!
                       
                               ;;; bounding-box is in pixels
                               ;;; update the chord bbox as well..
                               (setf cx1 (if cx1 (min cx1 nx1) nx1)
                                     cx2 (if cx2 (max cx2 nx2) nx2)
                                     cy1 (if cy1 (min cy1 ny1) ny1)
                                     cy2 (if cy2 (max cy2 ny2) ny2))
                         
                               (when build-b-boxes
                                 (setf (b-box n) (make-b-box :x1 nx1 :x2 nx2 :y1 ny1 :y2 ny2)))
                         

                               ;;; LEGER-LINES
                               (let ((l-lines (head-leger-lines line staff-lines)))
                                 ;;; draw add leger-line(s) to the record if they are not already there 
                                 (loop for ll in l-lines 
                                       unless (find ll leger-lines)
                                       do (let ((ypos (line-to-ypos ll shift unit))) 
                                            (om-draw-line (- x-pix legerLineExtension) ypos 
                                                          (+ x-pix head-w-pix legerLineExtension) ypos
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
                                 (let ((note-head (if (and display-offset (equal offsets :sep-notes))
                                                      (tempo-note 1/8)
                                                    head-char)))
                                 
                                   (om-draw-char head-x line-y note-head :font note-font)
                                   )
                       
                                 ;;; DOTS
                                 (when (> n-points 0)
                                   (let ((p-y (+ line-y (* unit (if (zerop (rem line 1)) -0.45 0.05)))))
                                     (om-draw-char (+ head-x (* 1.5 head-w-pix)) p-y (dot-char))
                                     (when (= n-points 2)
                                       (om-draw-char (+ head-x (* 2.5 head-w-pix)) p-y (dot-char))
                                       )))
                       
                                 ;;; ACCIDENT (if any)
                                 (when acc
                                   (let ((col (position (midic n) accidental-columns 
                                                        :test #'(lambda (pitch col)
                                                                  ;;; there's no other note in this col at less than an octave
                                                                  (not (find pitch col :test #'(lambda (a b) (< (abs (- b a)) 1200))))))))
                                     (if col 
                                         (push (midic n) (nth col accidental-columns))
                          
                                       (setf col (length accidental-columns) ;; add a new column
                                             accidental-columns (append accidental-columns (list (list (midic n))))))
                           
                                     (om-draw-char (if display-offset 
                                                       (- head-x (* acc-w (if (equal offsets :sep-notes) .7 1))) 
                                                     (- x-pix (* (1+ col) acc-w)))
                                                   line-y 
                                                   (accident-char acc)
                                                   :font note-font)
                                     ))
                     
                                 ;;; DURATION line (maybe)
                                 (when draw-durs
                                   (let ((end-pix (+ (* x-units unit) (funcall time-function (+ x-ms (dur n))))))
                               
                                     (om-with-fg-color
                                         (if t ;(member draw-chans '(:color :color-and-number))
                                             (om-make-color-alpha (get-midi-channel-color (chan n)) .5)
                                           (om-make-color 0 0 0 .5))
                                 
                                       (om-draw-rect (if display-offset head-x x-pix) 
                                                     (- line-y (* unit .25))
                                                     (- end-pix x-pix) 
                                                     (* unit .5) 
                                                     :fill t))
                                     ))

                                 ;;; MIDI channel (maybe)
                                 ;;; if there's just one channel in the chord we'll display it somewhere else
                                 (when (and (member draw-chans '(:number :color-and-number)) 
                                            (not unique-channel)) 
                                   (om-draw-string (+ x-pix (* head-w-pix 2)) (+ line-y (* unit .5)) (format nil "~D" (chan n)) 
                                                   :font (om-def-font :font1 :size (round fontsize 2)))
                                   )
                 
                                 ;;; MIDI port (maybe)
                                 ;;; if there's just one port in the chord we'll display it somewhere else
                                 (when (and draw-ports (not unique-port)) 
                                   (om-draw-string (+ x-pix 
                                                      (if (member draw-chans '(:number :color-and-number)) (* head-w-pix 3.5) (* head-w-pix 2.5)))
                                                   (+ line-y unit) (format nil "(~D)" (port n)) 
                                                   :font (om-def-font :font1 :size (round fontsize 2.5)))
                                   )

                                 ;;; VELOCITY (maybe)
                                 ;;; if there's just one velocity in the chord we'll display it somewhere else
                                 (when (and draw-vels (not unique-vel)) 
                                   (case draw-vels
                                     (:value 
                                      (om-draw-string x-pix (+ line-y (* unit 2)) (format nil "~D" (vel n)) 
                                                      :font (om-def-font :font1 :size (round fontsize 2.5))))
                                     (:symbol 
                                      (om-draw-char x-pix (+ line-y (* unit 2)) (velocity-char (vel n))))
                                     ))
                       
                                 ;;; TIES
                                 (when tied-to-ms 
                                   (let* ((x0 (+ (* x-units unit) (funcall time-function tied-to-ms) head-w-pix (* .3 unit)))
                                          (tie-h (if (> (abs (/ (- x-pix x0) unit)) 5) unit (* unit .5))))
                                   
                                     (om-with-line-size (* *stemThickness* unit 1.8)
                                 
                                       (let ((tie-direction
                                              (if (= (length pitches) 1) ;;; if only one note
                                                  (if (equal stem-direction :up) :down :up) ;;; opposed to beam
                                                (if (>= (position (midic n) pitches :test '=) 
                                                        (ceiling (length pitches) 2)) ;;; depends on position (rank) in the chord
                                                    :up :down))))
                                         (if (equal tie-direction :up)
                                             (om-draw-arc x0 (- ny1 tie-h) (- x-pix x0 (* .3 unit)) (* 2 tie-h) 0 pi)
                                           (om-draw-arc x0 (- ny2 tie-h) (- x-pix x0 (* .3 unit)) (* 2 tie-h) 0 (- pi)))
                                         )
                                       )
                                     ))
                                 ))
                             ))) ;;; END LOOP
                   ) ;;; END NOTES LET
                 
                 ;;; DRAW EXTRAS
                 (when (extras chord)
                   
                   (loop for e in (get-extras chord 'text-extra)
                         do (om-with-font 
                             (om-def-font :font1 :face (font e) :size (/ fontsize 2))
                             (om-draw-string (+ x-pix (* (or (dx e) 0) unit)) 
                                             (+ y-min (* unit (+ 4 (or (dy e) 0)))) 
                                             (text e)))
                         )
                   
                   (loop for e in (get-extras chord 'symb-extra)
                         do (om-draw-char (+ x-pix (*(or (dx e) 0) unit)) 
                                          (+ y-min (* unit (+ 4 (or (dy e) 0)))) 
                                          (code-char (symb-char e)))
                         )
                   
                   (loop for e in (get-extras chord 'score-marker)
                         do (om-with-fg-color (om-def-color :steelblue)
                              (om-draw-line x-pix 0 x-pix h :line (/ fontsize 20))
                              (when (data e)
                                (om-draw-string (+ x-pix unit) 
                                                (- h 20)
                                                (format nil "~A" (data e))
                                                :font (om-def-font :font1 :size (/ fontsize 2))))
                              ))
                   )
                 
               ) ;;; END OF THE NOTE-HEADS LET
               
               ))
    
            ;;; return the bounding box (always)
            (when t ;build-b-boxes 
              (make-b-box :x1 cx1 :x2 cx2 :y1 cy1 :y2 cy2))
         
            )
          )))))


  
;;;==========================================================
;;; RESTS

(defun draw-rest (rest x-ms   ; x-pos in ms
                       x-units 
                       y-units ; ref-position in score units
                       x y ; absolute offsets in pixels
                       w h ; frame for drawing
                       fontsize 
                       &key 
                       (head :rest-1/4)
                       line
                       stem
                       beams
                       (staff :gf)
                       selection
                       (time-function #'identity)
                       build-b-boxes)
  
  (declare (ignore w h))
  
  (om-with-translation x y 

    (let* ((unit (font-size-to-unit fontsize))
           (x-pix (+ (* x-units unit) (funcall time-function x-ms)))
           (head-symb (if (consp head) (car head) head))
           (n-points (if (consp head) (cadr head) 0)))
      
      (multiple-value-bind (head-char head-name)
          (rest-char head-symb)
        
        (let* ((shift (+ y-units (calculate-staff-line-shift staff)))
               (head-box (get-font-bbox head-name))
               (head-w (- (nth 2 head-box) (nth 0 head-box)))    ;;; the width in units of a note-head
               (head-h (- (nth 3 head-box) (nth 1 head-box)))    ;;; the height in units of a note-head
               (staff-lines (staff-lines (car (last (staff-split staff)))))
               (line-pos (or line (+ (car staff-lines) (/ (- (car (last staff-lines)) (car staff-lines)) 2)))) 
               ;;; the default position is the middle of the top staff
               )
          
          (cond ((and (equal head-symb :rest-1) ;; the whole rest is 1 line upper (except on 1-lien staff)
                      (not (equal staff :line)))
                 (setf line-pos (floor (1+ line-pos))))
                ((or (equal head-symb :rest-1) (equal head-symb :rest-1/2)) ;; the 1/2 rest on a line
                 (setf line-pos (floor line-pos))))
          
          ;(print (list line-pos head-symb staff-lines))

          ;;; positions (in pixels) 
          (flet ((x-pos (a) (+ x-pix 1 (* a unit))))
      
            (let ((head-x (x-pos 0))
                  (line-y (line-to-ypos line-pos shift unit))
                  (head-extra-number (if (listp head-symb) (car head-symb))))
      
              (om-with-font 
               (om-make-font *score-font* fontsize)
         
               (om-with-fg-color 
                   (if (or (equal selection t) (and (listp selection) (find rest selection)))
                       *score-selection-color*
                     nil)
                 
                 (when head-extra-number
                   (setf head-x (+ head-x (* unit 1)))
                   (let ((font (om-def-font :font1 :size (round fontsize 2.2))))
                     (om-draw-string (+ head-x (* head-w unit .3))
                                     (- line-y (* unit 1)) (number-to-string head-extra-number)
                                     :font font)
                     )
                   )
                 ;;; SYMBOL
                 (om-draw-char head-x line-y head-char)
               
                 ;;; DOTS
                 (when (> n-points 0)
                   (let ((p-y (+ line-y 
                                 (* unit (if (zerop (rem line-pos 1)) ;; avoid dots on a line 
                                             -0.45 
                                           0.05))
                                 (if (equal head-symb :rest-1) unit 0) ;; the whole rest is on the upper line (4)
                                 )
                              ))
                     (om-draw-char (+ head-x (* (+ head-w .5) unit)) p-y (dot-char))
                     (when (= n-points 2)
                       (om-draw-char (+ head-x (* (+ head-w .5) 2 unit)) p-y (dot-char))
                       )))

                 ;; special case for whole rest and half-rest: draw a small line when needed
                 (when (or (equal head-symb :rest-1)(equal head-symb :rest-1/2))
                   (om-draw-line (- head-x (* unit .5)) line-y (+ head-x (* unit (+ head-w .5))) line-y
                                 :line (* *thinBarLineThickness* unit)))
                 
                 ))
              
              ;;; draw a small beam (when the rest is in a group)
              (when stem
                
                (let ((stemThickness (* *stemThickness* unit))
                      (beam-y (line-to-ypos stem shift unit))
                      (head-w-pix (* (car *noteheadBlack_StemUpSE*) unit)) ;;; to align with beam position
                      (head-h-pix (* head-h unit))
                      (n-beams (car beams))
                      (pos-in-group (cadr beams)))

                  (if (>= stem line-pos)  ;;; stem up
                    
                      (progn 
                        (om-draw-line (+ head-x head-w-pix -1) (- line-y (/ (+ head-h-pix unit) 2))
                                      (+ head-x head-w-pix -1) beam-y
                                      :line stemThickness :end-style :projecting)
                        (when n-beams
                          (if (zerop pos-in-group) ;; first elem
                              (draw-beams x-pix (+ x-pix head-w-pix) stem :up n-beams y-units staff fontsize)
                            (draw-beams (- x-pix head-w-pix) x-pix stem :up n-beams y-units staff fontsize)
                            ))
                        )

                          
                    ;;; down
                    (progn
                      (om-draw-line x-pix (+ line-y (/ (+ head-h-pix unit) 2))
                                    x-pix beam-y
                                    :line stemThickness  :end-style :projecting)
                            
                      (when n-beams
                        (if (zerop pos-in-group) ;; first elem
                            (draw-beams x-pix (+ x-pix head-w-pix) stem :down n-beams y-units staff fontsize)
                          (draw-beams (- x-pix head-w-pix) x-pix stem :down n-beams y-units staff fontsize)
                          ))
                      )
                    )
                  ))


              ;;; return the bounding box
              (when t ; build-b-boxes 
                (make-b-box :x1 head-x :x2 (+ head-x (* head-w unit)) 
                            :y1 (line-to-ypos (+ line-pos (* head-h .5)) shift unit) ;;; lines are expressed bottom-up !!
                            :y2 (line-to-ypos (- line-pos (* head-h .5)) shift unit))
                )
              )))
        ))
    )
  )






