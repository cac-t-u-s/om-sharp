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

;;;=======================
;;; EXPORT MUSICXML
;;;=======================

;; This code is almost taken "as-is" adapted from OM 6.15 export-mxml.lisp
;; Author : Karim Haddad
;; Revision: 1.1 2008/06/23
;; Revision: 2 2015/10/05 J. Bresson


;; Adaptation from OM6 IN PROGRESS !! 2019/10 J. Bresson


(in-package :om)


(defvar *xml-version* "XML 1.0")

;;; PREDICATES

(defmethod group-p ((self group)) t)
(defmethod group-p ((self t)) nil)

(defmethod chord-p ((self chord)) t)
(defmethod chord-p ((self continuation-chord)) t)
(defmethod chord-p ((self t)) nil)


(defmethod cont-chord-p ((self continuation-chord)) t)
(defmethod cont-chord-p ((self t)) nil)

(defmethod rest-p ((self r-rest)) t)
(defmethod rest-p ((self t)) nil)

(defmethod in-group?  ((self chord)) (group-p (parent self)))
(defmethod in-group?  ((self continuation-chord)) (group-p (parent self)))
(defmethod in-group? ((self r-rest)) (group-p (parent self)))
(defmethod in-group? ((self t)) nil)

(defmethod alone-in-group?  ((self score-element)) 
  (and (in-group? self)
       (= (length (inside (parent self))) 1)))


(defmethod singelton-group? ((self group))
  (let* ((inside (inside self)))
    (and (= (length inside) 1) 
         (or (chord-p (car inside))
             (rest-p (car inside))))))

(defmethod singelton-group? ((self t)) nil)


;;; UTILS

(defun lst->ratio (liste) (/ (car liste) (cadr liste)))

(defmethod getratiogroup ((self measure)) (list 1 1))


;; modified version of om::find-denom
;; Find the rigth denom to ratio of tuplet.
(defun findenom (num durtot)
  (if num
      (cond
       ((or (is-binaire? durtot)
            (power-of-two-p durtot))
        (get-denom-bin num)) ;;;changed here from is-binaire? to powerof2?
       ((is-ternaire? durtot) (get-denom-ter num))
       (t (get-denom-other durtot num))) 
    (list 1 1)
    ))




(defparameter *note-types*
  '((2 breve) (1 whole)
    (1/2 half) (1/4 quarter)
    (1/8 eighth) (1/16 16th)
    (1/32 32nd) (1/64 64th)
    (1/128 128th) (1/256 256th)
    (1/512 512th)(1/1024 1024th)))

(defmethod getnotetype ((self group))
  (let* ((tree (tree self))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (dur (* symb-beat-val factor))
         (durtot (if (listp dur) (car dur) dur))
         (num (or (om::get-group-ratio self)  (om::extent self)))
         (denom (om::find-denom num durtot))
         (unite (/ durtot (if (listp denom) (second denom) denom)))) 
    (format nil "~A" (cadr (find unite *note-types* :key 'car)))
    ))


(defmethod getratiogroup ((self om::group))
  (if (singelton-group? self) (list 1 1) ;;; when a group is in fact a single note 
    (let* ((allparents (reverse (cdr (getdemall self))))
           (dur (get-dur-group self))
           (num (om::get-group-ratio self))) ;;; check definition of get-group-ratio
      (if (not allparents) 
          (let* ((denom (findenom num dur)))
            (if (listp denom) denom
              (list num (findenom num dur)))) ;;; a voir !!!!!!!
        (let* ((fact (get-dur-group self))
               (ratios (loop for i in allparents
                             do (setf fact (* fact  (lst->ratio (getratiogroup i)))))) ;;; donne le facteur accumule des nested tup
               (denom (findenom num fact)))
             
          (if (listp denom) 
              denom
            (list num denom)
            )) 
        ))))

;;;ici il fait la meme chose que getratiogroup averc les ratios...

(defmethod getratiodur ((self group))
  (let* ((allparents (reverse (cdr (getdemall self))))
         (dur (get-dur-group self))
         (num (om::get-group-ratio self)))
    (if (not allparents) 
        dur
      (let* ((fact (get-dur-group self))
             (ratios (loop for i in allparents
                           do (setf fact (* fact  (lst->ratio (getratiogroup i))))));;; donne le facteur accumule des nested tup
             (denom (findenom num fact)))
        fact
        ))
    ))



(defun get-grp-level (self)
  "donne l'index des tuplet imbriques"
  (let* ((buf (om::parent self))
         (num (car (getratiogroup buf)))
         (denom (second (getratiogroup buf)))
         (nums (list num))
         (denoms (list denom))
         (index 0)) 
    (loop 
      while (group-p buf)
      do (progn 
           (incf index)
           (setf buf (parent buf))
           (push (car (getratiogroup buf)) nums)
           (push (second (getratiogroup buf)) denoms)
           (setf num (* num (car (getratiogroup buf))))
           (setf denom (* denom (second (getratiogroup buf))))))
    (list index num denom (butlast (reverse nums)) (butlast (reverse denoms)))))


(defun getallgroups (self)
  (let* ((buf (parent self))
         (rep '()))
    (loop while (group-p buf)
          do (let ((ratiogroup (getratiogroup buf)))
               (push (list (car ratiogroup) (cadr ratiogroup) (getnotetype buf))
                     rep)
               (setf buf (parent buf))))
    rep))

(defmethod getratiounite ((self group))
  (/ (getratiodur self) (second (getratiogroup self))))

(defmethod getdemall ((self om::group))
  "For nested tuplets. Returns all group-parents to a group including the group itself" 
  (let* ((test self)
         (res (list self)))
    (loop while test 
          do (progn 
               (push (parent test) res)
               (if (measure-p (parent test)) 
                   (setf test nil)
                 (setf test (parent test)))))
    (butlast (reverse res))))


(defmethod get-dur-group ((self measure))
"Returns the duration of measure * whole note"
  (let* ((ext (om::extent self))
         (qval (om::qvalue self)))
    (/ ext (* qval 4))))

(defmethod get-dur-group ((self group))
"Returns the duration of measure * whole note.
durtot etant la duree du group parent"
  (let* ((ext (om::extent self))
         (qval (om::qvalue self)))
    (/ ext (* qval 4))))


(defun reduce-num-den-fig (list)
  "Reduces binary ratios and fig dedoubles. C-a-d 
si on a (14 8 1/16) il retourne (7 4 1/8)"
  (if (= (car list) (second list)) list
    (let ((res list))
      (setf res
            (list (/ (car res) 2)
                  (/ (second res) 2)
                  (* (third res) 2)))
      (if (or (om::ratiop (car res))
              (om::ratiop (second res)))
          (list (* 2 (car res))
                (* 2 (second res))
                (/ (third res) 2))
        (reduce-num-den-fig res)
        ))))


(defmethod get-group-info ((self group))
  (let* ((rat (om::first-n (getratiogroup self) 2))
         (unit (getratiounite self))
         (reduce (reduce-num-den-fig (om::flat (list rat unit)))))
    (list (car reduce) (second reduce) 
          (format nil "~A" (cadr (find (third reduce) *note-types* :key 'car))))
    ))



(defmethod get-group-info ((self measure))
  (list 1 1))




;;============================================


(defun first-of-this-group (self grp)
       (let ((frst (car (om::collect-chords grp))))
         (equal self frst)))

(defun last-of-this-group (self grp)
       (let ((lst (car (reverse (om::collect-chords grp)))))
         (equal self lst)))

(defun tied? (self)
  (or (and (not (cont-chord-p self))
           (cont-chord-p (om::next-container self '(chord))))
      (and (cont-chord-p self)
           (not (cont-chord-p (om::next-container self '(chord)))))
      (and (cont-chord-p self)
           (cont-chord-p (om::next-container self '(chord))))))


(defun cons-xml-tied (self)
  (cond ((and (not (cont-chord-p self))
              (cont-chord-p (om::next-container self '(chord))))
         "<tie type=\"start\"/>")
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(chord)))))
        "<tie type=\"stop\"/>")
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(chord))))
        "<tie type=\"stop\"/><tie type=\"start\"/>")
       (t nil))) ;;;;the nil thing is comin from here

(defun cons-xml-tied-notation (self)
  (cond ((and (not (om::cont-chord-p self))
              (om::cont-chord-p (om::next-container self '(om::chord))))
         "<tied type=\"start\"/>")
        ((and (om::cont-chord-p self)
             (not (om::cont-chord-p (om::next-container self '(om::chord)))))
        "<tied type=\"stop\"/>")
        ((and (om::cont-chord-p self)
              (om::cont-chord-p (om::next-container self '(om::chord))))
        "<tied type=\"stop\"/><tied type=\"start\"/>")
       (t NIL))) ;;;;the nil thing is comin from here


(defun cons-xml-tuplet-start (num denom notetype nbr)
  (list 
   (format nil "<tuplet bracket=\"yes\" number=\"~A\" placement=\"above\" type=\"start\">" nbr)
   (list "<tuplet-actual>"
         (list (format nil "<tuplet-number>~A</tuplet-number>" num)
               (format nil "<tuplet-type>~A</tuplet-type>" notetype))
         "</tuplet-actual>"
         "<tuplet-normal>"
         (list (format nil "<tuplet-number>~A</tuplet-number>" denom)
               (format nil "<tuplet-type>~A</tuplet-type>" notetype))
         "</tuplet-normal>")
   "</tuplet>"))

(defun cons-xml-tuplet-end (nbr)
  (list (format nil "<tuplet number=\"~A\" type=\"stop\"/>" nbr)))

;; disons que tous les char sont des accents :)
(defun accent? (self) (om::get-extras self "char"))

(defun cons-xml-articulation (self)
  (list "<articulations>"
        (list "<accent default-x=\"-1\" default-y=\"-61\" placement=\"below\"/>")
        "</articulations>"))

(defun cons-xml-groupnotation (self)
  (list "<notations>"
        (if (in-group? self)
            (let* ((lvl (get-grp-level self))
                   (ratio (getratiogroup (parent self)))
                   (act-note (second lvl))
                   (norm-note (third lvl))
                   (indx (car lvl))
                   (numdeno (getallgroups self))
                   (numdenom (remove nil 
                                     (loop for i in numdeno
                                           collect (if (not (= 1 (/ (car i) (second i)))) i )   ;;; PB if group (n n) !!!
                                           )))
                   (simpli (/ act-note norm-note)))
               
              (if (not (= (/ act-note norm-note) 1))
                  
                  (cond 
                   ((and (om::last-of-group? self) (om::first-of-group? self))
                    (when (accent? self) (list (cons-xml-articulation self))))
             
                   ((and (om::first-of-group? self)  (not (om::last-of-group? self)))
                    (remove nil 
                            (append 
                             (let ((obj self)
                                   (indx (+ (length numdenom) 1)))
                               (remove nil (loop for i in (reverse numdenom)           
                                                 append (progn 
                                                          (setf obj (om::parent obj))
                                                          (setf indx (- indx 1))
                                                          (when (first-of-this-group self obj)
                                                            (cons-xml-tuplet-start (car i) (second i) (third i) indx))))))
                             (list (cons-xml-tied-notation self)
                                   (when (accent? self) (cons-xml-articulation self))))))
                   
                   ((and (om::last-of-group? self) (not (om::first-of-group? self)))
                    (remove nil 
                            (append 
                             (let ((obj self)
                                   (indx (+ (length numdenom) 1)))
                               (remove nil (loop for i in numdenom           
                                                 append (progn 
                                                          (setf obj (om::parent obj))
                                                          (setf indx (- indx 1))
                                                          (when (last-of-this-group self obj)
                                                            (cons-xml-tuplet-end indx))))))
                             (list (cons-xml-tied-notation self)
                                   (when (accent? self) (cons-xml-articulation self))))))
                   
                   (t (when (accent? self) (list (cons-xml-articulation self))))
                   )

                (when (or (tied? self) (accent? self))
                  (remove nil 
                          (list 
                           (when (tied? self) (cons-xml-tied-notation self))
                           (when (accent? self) (cons-xml-articulation self)))))
                ))


          (when (or (tied? self) (accent? self))
            (remove nil 
                    (list 
                     (when (tied? self) (cons-xml-tied-notation self))
                     (when (accent? self) (cons-xml-articulation self)))))
          )
        
        ;;; VEL
        (cons-xml-velocity self)
        
        "</notations>"
        ))


(defun get-parent-measure (self)
  "Donne la mesure liee a l'obj chord par exemple"
  (let ((obj (om::parent self)))
  (loop 
    while (not (om::measure-p obj))
    do (setf obj (om::parent obj)))
  obj))


(defmethod donne-figure ((self om::chord))
  (let* ((mesure (get-parent-measure self))
         (inside (om::inside mesure))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (stem (om::extent self))
         (obj self))
    
     (loop 
      while (not (om::measure-p obj))
      do (progn 
           (setf stem (* stem (om::extent (om::parent obj))))
           (setf obj (om::parent obj)))) 
   (let ((numbeams  (om::get-number-of-beams (* symb-beat-val factor))))
     (if (listp numbeams) (car numbeams) numbeams ))))


(defmethod donne-figure ((self om::r-rest))
  (let* ((mesure (get-parent-measure self))
         (inside (om::inside mesure))
         (tree (om::tree mesure))
         (real-beat-val (/ 1 (om::fdenominator (first tree))))
         (symb-beat-val (/ 1 (om::find-beat-symbol (om::fdenominator (first tree)))))
         (dur-obj-noire (/ (om::extent self) (om::qvalue self)))
         (factor (/ (* 1/4 dur-obj-noire) real-beat-val))
         (stem (om::extent self))
         (obj self))
    
     (loop 
      while (not (om::measure-p obj))
      do (progn 
           (setf stem (* stem (om::extent (om::parent obj))))
           (setf obj (om::parent obj)))) 
   (let ((numbeams  (om::get-number-of-beams (* symb-beat-val factor))))
     (if (listp numbeams) (car numbeams) numbeams ))))

(defmethod donne-figure ((self t)) 0)

(defun cons-xml-beam (self)
  (let* ((beamself (donne-figure self))
         (beamprev (donne-figure (prv-cont self)))
         (beamnext (donne-figure (nxt-cont self))))
    (remove nil (list
                 (cond ((and (in-group? self)
                             (not (om::first-of-group? self))
                             (not (om::last-of-group? self))
                             (> beamself 0))
                        (cond ((and (> beamprev 0) (> beamnext 0))
                               "<beam>continue</beam>")
                              ((and (> beamprev 0) (not (> beamnext 0)))
                               "<beam>end</beam>")
                              ((and (not (> beamprev 0)) (> beamnext 0))
                               "<beam>begin</beam>")))
                       ((and (om::first-of-group? self) (> beamself 0))
                        (if (and (in-group? (prv-cont self))
                                 (> beamprev 0) (> beamnext 0) (prv-is-samegrp? self))
                            "<beam>continue</beam>" "<beam>begin</beam>"))
                       ((and (om::last-of-group? self) (> beamself 0))
                        (if (and (in-group? (nxt-cont self))
                                 (> beamprev 0) (> beamnext 0) (nxt-is-samegrp? self))
                            "<beam>continue</beam>" "<beam>end</beam>"))
          
                       (t NIL)) 
                 ))
    ))


(defun prv-cont (self)
  (om::previous-container self '(chord r-rest)))

(defun nxt-cont (self)
  (om::next-container self '(chord r-rest)))

(defun prv-is-samegrp? (self)
  (let ((prev (prv-cont self)))
    (equal (parent self) (parent prev))))

(defun nxt-is-samegrp? (self)
  (let ((next (nxt-cont self)))
    (equal (parent self) (parent next))))


;;;--------<PITCH>--------

(defparameter *kascii-note-C-scale*
  (mapc #'(lambda (x) (setf (car x) (string-upcase (string (car x)))))
    '((c . :n) (c . :h) (c . :q) (c . :hq) (c . :s) (c . :hs) (c . :qs) (c . :hqs)
      (d . :n) (d . :h) (d . :q) (d . :hq) (d . :s) (d . :hs) (d . :qs) (d . :hqs)
      (e . :n) (e . :h) (e . :q) (e . :hq)
      (f . :n) (f . :h) (f . :q) (f . :hq) (f . :s) (f . :hs) (f . :qs) (f . :hqs)
      (g . :n) (g . :h) (g . :q) (g . :hq) (g . :s) (g . :hs) (g . :qs) (g . :hqs)
      (a . :n) (a . :h) (a . :q) (a . :hq) (a . :s) (a . :hs) (a . :qs) (a . :hqs)
      (b . :n) (b . :h) (b . :q) (b . :hq))))

(defparameter *kascii-note-scales* (list *kascii-note-C-scale*))

(defparameter *kascii-note-alterations*
   '((:s 1 +100) (:f -1 -100)
     (:q 0.5 +50) (:qs 1.5 +150) (:-q -0.5 -50) (:f-q -1.5 -150)
     (:h 0.25 +25) (:hq 0.75 +75) (:hs 1.25 +125) (:hqs 1.75 +175) (:-h -0.25 -25) (:-hq -0.75 -75)(:-hs -1.25 -125)(:-hqs -1.75 -175)
     (:n 0 0)))


(defparameter *note-accidentals*
  '((0.25 natural-up)
    (0.5 quarter-sharp) 
    (0.75 sharp-down)
    (1 sharp)
    (1.25 sharp-up)
    (1.5 three-quarters-sharp)
    (1.75 sharp-three)
    ))

; (mc->xmlvalues 6548 4)

(defun mc->xmlvalues (midic &optional (approx 2))
  "Converts <midic> to a string representing a symbolic ascii note."
  (let* ((kascii-note-scale (car *kascii-note-scales*))
         (dmidic (/ 1200 (length kascii-note-scale))) 
         (vals (multiple-value-list (round (om::approx-m midic approx) dmidic)))
         (midic/50 (car vals))
         (cents (cadr vals))
         (vals2 (multiple-value-list (floor (* midic/50 dmidic) 1200)))
         (oct+2 (- (car vals2) 1))
         (midic<1200 (cadr vals2))
         (note (nth (/ midic<1200 dmidic) kascii-note-scale))
         (alt (cdr note)))
    (list midic
          (coerce (car note) 'character) 
          (cadr (find alt *kascii-note-alterations* :key 'car))
          oct+2)))




;;;--------</PITCH>--------

;;;--------<NOTE HEADS>--------
(defun notetype (val)
  (cond 
   ((>= val 2) 2)
   ((>= val 1/2) 1/2)
   ((>= val 1/4) 1/4)
   ((>= val 1/8) 1/8)
   ((>= val 1/16) 1/16)
   ((>= val 1/32) 1/32)
   ((>= val 1/64) 1/64)
   ((>= val 1/128) 1/128)
   ((>= val 1/256) 1/256)))

(defun note-strict-lp (val)
  (cond
   ((>= val 16) (car (om::before&after-bin val)))
   ((= val 8) 8)
   ((= val 4) 4)
   ((= val 2) 2)
   (t (denominator val))))

(defun get-head-and-points (val)
  (let* ((haut (numerator val))
         (bas (denominator val))
         (bef (car (om::before&after-bin haut)))
         (points 0) (char 1))
    (cond
     ((= bef haut)
      (setf char (note-strict-lp (/ haut bas)))
      (setf points 0))
     ((= (* bef 1.5) haut)
      (setf char (note-strict-lp (/ bef bas)))
      (setf points 1))
     ((= (* bef 1.75) haut)
      (setf char (note-strict-lp (/ bef bas)))
      (setf points 2)))

    (if (> val 1) 
        (list (/ char 1) points)
      (list (/ 1 char) points))
    ))



;;;-------</NOTE HEADS>--------

;;;-------<MISC>--------

(defun cons-xml-text-extras (self) 
  (when (om::get-extras self "text")
    (list "<lyric default-y=\"-80\" justify=\"left\" number=\"1\">" 
          (list  "<syllabic>single</syllabic>"
                 (format nil "<text>~A</text>" (om::thetext (car (om::get-extras self "text"))))
                 "<extend type=\"start\"/>")
          "</lyric>")))


(defmethod cons-xml-velocity ((self om::chord))
  (when (om::get-extras self "vel")
    
    (let* ((ex (car (om::get-extras self "vel")))
           (schar (or (om::dynamics ex)
                      (om::get-dyn-from-vel (om::get-object-vel (om::object ex))))))
      (list (format nil "<dynamics placement=\"below\"><~A/></dynamics>" schar)))))

(defmethod cons-xml-velocity ((self om::r-rest)) nil)

(defun midi-vel-to-mxml-vel (vel)
  (round (* (/ vel 90.0) 100)))

;;;================
;;; CHORD / NOTES
;;;================

;;; new here in order to put the correct <duration> according to <divisions> of each measure.
;;; for compatibility 

;(defmethod get-xml-duration ((self t))
;  (* (/ (om::extent self) 4) (/ 1 (om::qvalue self))))

(defmethod get-xml-duration ((self t))
  (* (mesure-divisions (get-parent-measure self)) 
     (/ (om::extent self) (om::qvalue self))))

  
(defun cons-xml-time-modification (self)
  (if (and (in-group? self) (not (alone-in-group? self)))
      (let ((ratio (butlast (get-group-info (parent self)))))
        (list "<time-modification>"
        (list (format nil "<actual-notes>~A</actual-notes>" (car ratio))
              (format nil "<normal-notes>~A</normal-notes>" (cadr ratio)))
        "</time-modification>"))
    NIL))

(defmethod cons-xml-expr ((self om::chord) &key free key (approx 2) part)
  (let* (;; (dur free) 
         (dur (if (listp free) (car free) free))
         (head-and-pts (get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) *note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
         (durtot (get-xml-duration self))  ;;; !!!!
         (inside (om::inside self)))
    (loop for note in inside 
           for i = 0 then (+ i 1) append 
           (let* ((note-values (mc->xmlvalues (om::midic note) approx))
                  (step (nth 1 note-values))
                  (alteration (nth 2 note-values))
                  (octave (nth 3 note-values)))
                         
             (list (format nil "<note dynamics=\"~D\">" (midi-vel-to-mxml-vel (om::vel note)))
                   (unless (= i 0) "<chord/>") ;;; if this is not the first note in the chord
                   (remove nil 
                           (append 
                            (list "<pitch>"
                                  (remove nil 
                                          (list (format nil "<step>~A</step>" step)
                                                (when alteration (format nil "<alter>~A</alter>" alteration))
                                                (format nil "<octave>~A</octave>" octave)))
                                  "</pitch>"
                                  (format nil "<duration>~A</duration>" durtot)
                                  (cons-xml-tied self) ;;; ties (performance)
                                  (let ((headstr (format nil "<type>~A</type>" note-head)))
                                    (loop for i from 1 to nbpoints do (setf headstr (concatenate 'string headstr "<dot/>")))
                                    headstr)
                                  (when (find alteration *note-accidentals* :key 'car)  ;;; accidental (if any)
                                    (format nil "<accidental>~A</accidental>" (cadr (find alteration *note-accidentals* :key 'car))))
                                  (format nil "<instrument id=\"P~D-I~D\"/>" part (om::chan note))
                                  )
                            (cons-xml-time-modification self)
                            (cons-xml-beam self)
                            (cons-xml-groupnotation self)
                            (when (= i 0) (cons-xml-text-extras self))
                            ))
                   "</note>")
             ))))

   
(defmethod cons-xml-expr ((self om::r-rest) &key free key (approx 2) part)
  (let* ((dur (if (listp free) (car free) free))
         (head-and-pts (get-head-and-points dur))
         (note-head (cadr (find (car head-and-pts) *note-types* :key 'car)))
         (nbpoints (cadr head-and-pts))
         (durtot (get-xml-duration self)))
    (list "<note>" 
          "<rest/>"
          (remove nil
                  (list 
                   (format nil "<duration>~A</duration>" durtot)
                   (let ((headstr (format nil "<type>~A</type>" note-head)))
                     (loop for i from 1 to nbpoints do (setf headstr (concatenate 'string headstr "<dot/>")))
                     headstr)
                   (cons-xml-time-modification self)
                   (cons-xml-beam self)
                   (cons-xml-groupnotation self)))
          "</note>")))


;;;===================================
;;; RECURSIVE CONTAINERS (JB 29/09/15)
;;;===================================


(defmethod cons-xml-expr ((self group) &key free key (approx 2) part)
  
  (let* ((durtot free)
         (cpt (if (listp free) (cadr free) 0))
         (num (or (get-group-ratio self) (om::extent self)))
         (denom (find-denom num durtot))
         (num (if (listp denom) (car denom) num))
         (denom (if (listp denom) (cadr denom) denom))
         (unite (/ durtot denom)))

    (cond
     
     ((not (get-group-ratio self))
      
      (loop for obj in (inside self) append 
            (let* (; (dur-obj (/ (/ (om::extent obj) (om::qvalue obj)) (/ (om::extent self) (om::qvalue self))))
                   (dur-obj (symbolic-dur self)))
              
              (cons-xml-expr obj 
                             :free dur-obj ;; (* dur-obj durtot) 
                             :approx approx :part part))))
     
     ((= (/ num denom) 1)
      
      (loop for obj in (inside self) 
            append
            (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                 (/ (om::extent self) (om::qvalue self))))
                   (dur-obj (* num operation)))                     
              (cons-xml-expr obj :free (* dur-obj unite) :approx approx :part part)))    ;;;; ACHTUNG !!
      )
     
     (t
      (let ((depth 0) (rep nil))
        (loop for obj in (inside self) do
              (setf rep (append rep 
                                (let* ((operation (/ (/ (om::extent obj) (om::qvalue obj)) 
                                                     (/ (om::extent self) (om::qvalue self))))
                                       (dur-obj (* num operation))
                                       (tmp (multiple-value-list 
                                             (cons-xml-expr obj :free (list (* dur-obj unite) cpt)
                                                            :approx approx :part part)))
                                       (exp (car tmp)))
                                  
                                  (when (and (cadr tmp) (> (cadr tmp) depth))
                                    (setf depth (cadr tmp)))
                                  exp))))
        (values rep (+ depth 1))
        ))
      
     )))


;;;; <divisions> problem....
;;;finale's value to be tested on Sibelius.... (768)
;;;sibelius ' value is 256....


(defun list-pgcd (list)
  (let ((res (car list)))
    (loop for deb in (cdr list)
          do (setf res (pgcd res deb)))
    res))

(defmethod mesure-divisions ((self om::measure))
  (let* ((ratios (tree2ratio (list '? (om-round (list (tree self))))))
         (timesig (car (tree self)))
         (num (car timesig))
         (denom (second timesig))
         (comp (x-append 1/4 ratios))    ;; 1/4 for now... (1/4 = time-signature denom)
         (props (om* (om/ 1 (list-pgcd ratios)) comp)))
    (* num (car props))))


(defmethod cons-xml-expr ((self measure) &key free (key '(G 2)) (approx 2) part)
  (let* ((mesnum free) 
         (inside (inside self))
         (tree (tree self))
         (signature (car tree))
         ;; (real-beat-val (/ 1 (fdenominator signature)))
         (symb-beat-val (/ 1 (find-beat-symbol (fdenominator signature)))))
    (list (format nil "<measure number=\"~D\">" mesnum)
          (append (remove nil
                          (list "<attributes>"
                                (list (format nil "<divisions>~A</divisions>" (mesure-divisions self))  ;;; (caar (dursdivisions self)))
                                      "<key>"
                                      (remove nil
                                              (list  "<fifths>0</fifths>"
                                                     (if (and approx (= approx 2)) "<mode>major</mode>")))
                                      "</key>"
                                      "<time>"
                                      (list (format nil "<beats>~D</beats>" (car signature))
                                            (format nil "<beat-type>~D</beat-type>" (cadr signature)))
                                      "</time>")
                                (and key
                                     (list "<clef>"
                                           (list (format nil "<sign>~:@(~a~)</sign>" (car key))
                                                 (format nil "<line>~D</line>" (cadr key)))
                                           "</clef>"
                                           ))
                                "</attributes>"))
                  (loop for obj in inside ;for fig in (cadr (dursdivisions self))  ;;;;;transmetre les note-types
                        append
                        (let* (;; from OM6:
                               ;; (dur-beats (/ (* 1/4 (/ (om::extent obj) (om::qvalue obj))) real-beat-val))  
                               (dur-beats (symbolic-dur obj)) 
                               )
                          (cons-xml-expr obj :free (* symb-beat-val dur-beats) :approx approx :part part) ;;; NOTE: KEY STOPS PROPAGATING HERE
                          )))
          "</measure>"
          "<!--=======================================================-->")))


(defmethod cons-xml-expr ((self voice) &key free (key '(G 2)) (approx 2) part)

  (declare (ignore free))
  
  (let ((voicenum part)
        (measures (inside self)))
    (list (format nil "<part id=\"P~D\">" voicenum)
          (loop for mes in measures
                for i = 1 then (+ i 1)
                collect (cons-xml-expr mes :free i :key key :approx approx :part part))
          "<!--=======================================================-->"
          "</part>")))


(defun mxml-header () 
  (list "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        "<!DOCTYPE score-partwise PUBLIC \"-//Recordare//DTD MusicXML 1.1 Partwise//EN\" \"http://www.musicxml.org/dtds/partwise.dtd\">"))

(defun get-midi-channels (voice)
  (sort (remove-duplicates (loop for c in (chords voice) append (lchan c))) '<))

(defmethod cons-xml-expr ((self poly) &key free (key '((G 2))) (approx 2) part)

  (declare (ignore part free))

  (let ((voices (inside self)))
    (list "<score-partwise>"
          (list "<identification>"
                (list "<encoding>"
                      (list (concatenate 'string "<software>" *app-name* " " *version-string* "</software>"))
                      "</encoding>")
                "</identification>")
          (list "<part-list>"
                (loop for v in voices 
                      for voice-num = 1 then (+ voice-num 1)
                      append 
                      (let ((channels (get-midi-channels v)))
                        `(
                          ,(format nil "<score-part id=\"P~D\">" voice-num)
                          (,(format nil "<part-name>Part ~D</part-name>" voice-num))
                          
                          ,(loop for ch in channels append
                                 `(
                                   ,(format nil "<score-instrument id=\"P~D-I~D\">" voice-num ch)
                          ;("<instrument-name>Grand Piano</instrument-name>")
                                   "</score-instrument>"
                                   ))
                          ,(loop for ch in channels append
                                 `(
                                   ,(format nil "<midi-instrument id=\"P~D-I~D\">" voice-num ch)
                                   ,(format nil "<midi-channel>~D</midi-channel>" ch)
                                  ; "<midi-program>1</midi-program>")
                                   "</midi-instrument>")
                                 )
                          "</score-part>")
                        )
                      )
                "</part-list>")
          "<!--===================================================================-->"
          (if (= 1 (length key))
              ;;; SAME KEY FOR ALL VOICES
              (loop for v in voices
                    for i = 1 then (+ i 1) 
                    append 
                    (cons-xml-expr v :part i :key (car key) :approx approx))
            ;;; EACH VOICE HAS A KEY
            (loop for v in voices 
                  for i = 1 then (+ i 1)
                  for k in key 
                  append
                  (cons-xml-expr v :part i :key k :approx approx)))
          "</score-partwise>")))
  

;;;===================================
;;; OM INTERFACE / API
;;;===================================

(defun recursive-write-xml (stream text level)
  (if (listp text)
      (loop for elt in text do 
            (recursive-write-xml stream elt (1+ level)))
    (format stream "~A~A~%" (string+ (make-sequence 'string level :initial-element #\Tab)) text)))
    
(defun write-xml-file (list path)
  (WITH-OPEN-FILE (out path :direction :output 
                       :if-does-not-exist :create :if-exists :supersede)
    (loop for line in (mxml-header) do (format out "~A~%" line))
    (recursive-write-xml out list -1)))

(defmethod xml-export ((self t) &key keys approx path name) 
  (declare (ignore (keys approx path name)))
  nil)

(defmethod xml-export ((self voice) &key keys approx path name) 
  (xml-export (make-instance 'poly :voices self) :keys keys :approx approx :path path :name name))
 
(defmethod xml-export ((self poly) &key keys approx path name) 
  (let* ((pathname (or path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :name name 
                                                       :prompt "New XML file"
                                                       :types '("XML Files" "*.xml")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (write-xml-file (cons-xml-expr self :free 0 :key keys :approx approx) pathname)
      pathname)))


(defmethod! export-musicxml ((self t) &optional (keys '((G 2))) (approx 2) (path nil))
  :icon 351
  :indoc '("a VOICE or POLY object" "list of voice keys" "tone subdivision approximation" "a target pathname")
  :initvals '(nil ((G 2)) 2 nil)
  :doc "
Exports <self> to MusicXML format.

- <keys> defines the staff
- <approx> is the microtonal pitch approximation
- <path> is a pathname to write the file in
"
  (xml-export self :keys keys :approx approx :path path))



;;;============================================
;;; OTHER UTILS 
;;;============================================

(defmethod make-empty-voice ((signs list))
  (let ((mesures (loop for i in signs
                       collect (list i '(-1)))))
    (make-instance 'voice :tree (list '? mesures))))


(defmethod normalize-poly ((self poly))
  "Comlpletes the poly in a manner that all voices got the same number of mesures for MusicXML export"
  (let* ((voices (inside self))
         (signs (get-signatures self))
         (lgts (loop for i in signs collect (length i)))
         (maxlgt (list-max lgts))
         (newvoices (loop for i in voices
                          for n in lgts
                          for sg in signs
                          collect (let* ((dif (- maxlgt n))
                                         (comp (last-n sg dif))
                                         (new-vx (make-empty-voice comp)))
                                    (concat i new-vx)))))
    (make-instance 'poly :voices newvoices)
    ))