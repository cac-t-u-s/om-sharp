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

(defclass* n-cercle () 
  ((interval :initform '(60 72) :accessor cercle-interval) ;;; not used atm (could be useful for rendering...)
   (n :initform 12 :initarg :n :accessor n)
   (puntos :initform '(0 5 7) :accessor puntos :initarg :puntos))
  (:documentation 
   "A graphical object representing a series of intervals on a circle.

N-CERCLE can be used to represent chords following the pitch-class set theory, or cyclic rhythmic patterns.

<n> is the 'modulo' of the space
<puntos> is a list of points in this space.

Note: N-CERCLE can also contain a set of chords/patterns: in this case, <puntos> is a list of lists.

"))


;;; just reformat / check the values
(defmethod initialize-instance :after ((self n-cercle) &key args)
  (declare (ignore args))

  (unless (and (n self) (plusp (n self)))
    (om-beep-msg "N-CERCLE: N MUST BE A POSITIVE INTEGER (DEFAULT=12)")
    (setf (n self) 12))
  
  (when (not (consp (car (puntos self))))
    (setf (puntos self) (list (puntos self))))
  
  (setf (puntos self)
        (remove nil (loop for cercle in (puntos self) collect
                          (if (and (listp cercle)
                                   (list-subtypep cercle 'integer))
                              cercle
                            (om-beep-msg (format nil "Wrong data in N-CERCLE: ~A" cercle))))))
  )

;;;=============================
;;; BOX
;;;=============================

(defmethod display-modes-for-object ((self n-cercle))
  '(:mini-view :text :hidden))

(defun polar2car (angle radius)
  (list (round (* radius (cos angle))) (round (* radius (sin angle)))))


(defun draw-point-list-in-circle (angle-list cx cy r &key (thickness 1) color (lines nil) (front t))
  (let ((points-xy (loop for theta in angle-list 
                         collect 
                         (multiple-value-bind (x y) 
                             (pol->car r (+ theta (/ pi -2)))
                           (list (+ cx x) (+ cy y)))))) 
    
    (om-with-line-size thickness
      
      (loop for xy in points-xy
            do 
            (om-draw-circle (car xy) (cadr xy) (* 3 thickness) :color color)
            (when color (om-draw-circle (car xy) (cadr xy) (* 3 thickness) :color color :fill t))
            )
    
      (when lines
        (loop for p on (append points-xy (list (car points-xy)))
              when (cdr p)
              do (om-draw-line (car (car p)) (cadr (car p))
                               (car (cadr p)) (cadr (cadr p))
                               :line (if front (+ 1 thickness) thickness)
                               :color color)
              )))
    ))


(defmethod draw-mini-view ((self n-cercle) (box t) x y w h &optional time)
  (declare (ignore time))
  
  (om-draw-rect x y w h :color (om-def-color :white) :fill t)
  
  (let ((step (/ (* pi 2) (n self)))
        (cx (+ x (round w 2)))
        (cy (+ y (round h 2)))
        (r (round (min w h) 2.5)))
    
    (om-draw-circle cx cy r)
    
    (draw-point-list-in-circle 
     (loop for i from 0 to (- (n self) 1) collect (* step i))
     cx cy r)
    
    (loop for point-list in (reverse (puntos self))
          for i = (1- (length (puntos self))) then (- i 1)
          do 
          (draw-point-list-in-circle 
           (om* point-list step)
           cx cy r
           :color (get-midi-channel-color (1+ (mod i 16)))
           :lines 1)
          )
    ))
                  
;=============================================
;FUNCTIONS
;=============================================
(defmethod* nc-rotate ((self n-cercle) n)
  :initvals '(nil 1) :indoc '("n-cercle" "steps")
  :icon :cercle
  :doc "Generates a N-CERCLE by rotation of <self>." 
  
  (make-instance 'n-cercle
                 :n (n self)
                 :puntos (loop for list in (puntos self)
                               collect (sort 
                                        (loop for p in list collect (mod (+ p n) (n self)))
                                        '<))
                 ))


(defmethod* nc-complement ((self n-cercle))
  :initvals '(nil) :indoc '("n-cercle")
  :icon :cercle
  :doc "Generates the complement of <self> (N-CERCLE)." 
    
  (make-instance 'n-cercle
                 :n (n self)
                 :puntos (loop for list in (puntos self)
                               collect (loop for item from 0 to (- (n self) 1)
                                             when (not (find item list
                                                             :test #'(lambda (a b) (= (mod a (n self)) (mod b (n self))))))
                                             collect item))
                 ))


(defmethod* nc-inverse ((self n-cercle))
  :initvals '(nil) :indoc '("n-cercle")
  :icon :cercle
  :doc "Generates the inverse of <self> (N-CERCLE)."
  
  (make-instance 'n-cercle
                 :n (n self)
                 :puntos (loop for list in (puntos self)
                               collect  (sort 
                                         (x->dx (reverse (dx->x 0 list)))
                                         '<))
                 ))


;=============================================
;CONVERSIONS
;=============================================

(defmethod* chord2c ((self chord) approx)
  :initvals '(nil 2) :indoc '("the chord"  "approx")
  :icon :cercle
  :doc "Generates a N-CERCLE representing <self> (a CHORD) on a circle with a given division of the tone (2=semitone)

<approx> can be 2 (default), 4 or 8.
" 
  (let ((n (case approx (2 12) (4 24) (8 48) (otherwise 12)))
        (div (case approx (2 100) (4 50) (8 25) (otherwise 100))))
    
    (make-instance 'n-cercle
      :n n
      :puntos (remove-duplicates 
               (sort 
                (loop for item in (om/ (om- (approx-m (Lmidic self) approx) 6000) div)
                      collect (mod item n)) 
                '<)))))


;;; use this for automatic conversion from CHORD to N-CERCLE
(defmethod objfromobjs ((model chord) (target n-cercle))
  (chord2c model 2))


(defmethod* c2chord ((self n-cercle) index base approx)
  :initvals '(nil 0 6000 100) :indoc '("n-cercle" "index" "initial value" "approx")
  :doc "Generates a CHORD starting from nth (<index>) point-list in <self> (N-CERCLE)" 
  :icon :cercle
  (let ((points (puntos self)))
    (make-instance 'chord
      :lmidic (om+ base (om* approx (nth index points))))))


(defmethod* c2chord-seq ((self n-cercle) base approx)
  :initvals '(nil 6000 100) :indoc '("n-cercle" "initial value" "approx")
  :doc "Generates a sequence of chords (CHORD-SEQ) starting from all the point-lists in <self>  (N-CERCLE)" 
  :icon :cercle
  (let ((points (puntos self)))
    (make-instance 'chord-seq
      :lmidic (loop for item in points
                    collect (om+ base (om* approx item))))))


(defmethod* chord-seq2c ((self chord-seq) approx)
  :initvals '(nil 2 ) :indoc '("the chord-seq" "approx")
  :doc "Returns a possible circular representations of a sequence of chords (CHORD-SEQ) with a given tone division (2=semitone).

<approx> can be 2 (default), 4 or 8." 
  :icon :cercle
  (let ((n (case approx (2 12) (4 24) (8 48) (otherwise 12)))
        (div (case approx (2 100) (4 50) (8 25) (otherwise 100))))
    
    (make-instance 'n-cercle
      :n n
      :puntos (loop for chord in (inside self) collect
                    (remove-duplicates 
                     (sort (loop for item in (om/ (om- (approx-m (Lmidic chord) approx) 6000) div)
                                 collect (mod item n))
                           '<))))))


(defmethod* c2rhythm ((self n-cercle) index signature times)
  :initvals '(nil 0 (12 8) 3) :indoc '("n-cercle" "index" "signature" "times" )
  :doc "Generates a rhythm (VOICE) from the nth  (<index>) point-list in <self> (N-CERCLE) and repeating the rhythmic pattern a given number of times." 
  :icon :cercle
  (let* ((pattern (nth index (puntos self)))
         (intervals (x->dx pattern))
         (all-intervals (append intervals (list (- (n self) (apply '+ intervals))))))
    (make-instance 'voice :tree (repeat-n (list signature all-intervals) times))))


(defmethod! rhythm2c ((self voice) n)
  :initvals '(nil 16)
  :doc "Returns a possible circular representations of a rhythmic pattern (VOICE) with modulo <n>." 
  :icon :cercle
  (let* ((ratio (tree2ratio (tree self))) 
         (step (car ratio)))
    (loop for item in (cdr ratio) do
          (setf step (pgcd step item)))
    (setf ratio (om/ ratio step))
    (make-instance 'n-cercle
                   :n n
                   :puntos (butlast (dx->x 0 ratio)))
    ))
