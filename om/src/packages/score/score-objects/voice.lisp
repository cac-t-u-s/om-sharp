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


;;;===================================================
;;; VOICE IS A CHORD-SEQ WHOSE STRUCTURE AND TIMING IS RULED BY A TREE
;;;===================================================
;;; tree (text) is used to build the r-struct 
;;; .. and set dates and durations to the chords
;;; r-struct is a hierarchical structure of containers (measures/groups) whose leaves are either chords or rests
;;; the chords in r-struct are simple references to the time-sequence items



(defclass* voice (chord-seq)
  (
   (tree :initform '(((4 4) (1 1 1 1))) :accessor tree :initarg :tree :type list :documentation "a rhythm tree (list of measure-rythm-trees)")
   (Lmidic :initform '((6000)) :initarg :Lmidic :type list :documentation "pitches (mc)/chords: list or list of lists")

   (tempo :accessor tempo :initform 60 :initarg :tempo :documentation "a tempo value or tempo-map")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")
   ))

(defmethod additional-class-attributes ((self voice)) '(lvel loffset lchan lport))


(defclass rhythmic-object (score-object) 
  ((tree :initform '(1 (1 1 1 1)) :accessor tree :initarg :tree :type list :documentation "a rhythm tree")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")))

;;; some additional classes to build a rhythmic structure
(defclass measure (rhythmic-object) ())

(defclass group (rhythmic-object) 
  ((numdenom :accessor numdenom :initarg :numdenom :initform nil)))

(defmethod get-all-chords ((self rhythmic-object))
  (loop for obj in (inside self) append 
        (get-all-chords obj)))

(defmethod get-all-chords ((self chord)) (list self))
(defmethod get-all-chords ((self t)) nil)


(defclass r-rest (score-object) ())
(defmethod get-notes ((self r-rest)) nil)

;;; (defclass cont-chord (score-object) ())
(defclass grace-note (score-object) ())




(defmethod initialize-instance ((self voice) &rest initargs)
  (call-next-method)
  
    ;;; probably "old-formatted" RT, with "?" etc.
  (unless (listp (car (tree self)))
    (setf (tree self) (cadr (tree self))))

  (setf (tree self) (normalize-tree (tree self)))
  
  (build-rhythm-structure self (chords self) -1)
  
  (set-timing-from-tempo (chords self) (tempo self))
     
  self)


(defun beat-to-time (beat tempo)
  (let ((whole-dur (* 4 (/ 60000 tempo))))
    (round (* beat whole-dur))))
  
(defun set-timing-from-tempo (chords tempo)
  (loop for c in chords do
        (setf (date c) (beat-to-time (symbolic-date c) tempo))
        (ldur c) (beat-to-time (symbolic-dur c) tempo)
        ))


(defmethod build-rhythm-structure ((self voice) chords n)
  (let ((curr-beat 0)
        (curr-n-chord n))
    
    (setf (inside self)
          (loop for m-tree in (tree self)
                collect (let* ((m-dur (decode-extent (car m-tree)))
                               (mesure (make-instance 'measure :tree m-tree
                                                      :symbolic-date curr-beat
                                                      :symbolic-dur m-dur)))
                          (setq curr-beat (+ curr-beat m-dur))
                          (setq curr-n-chord (build-rhythm-structure mesure chords curr-n-chord))
                          mesure)))

    (time-sequence-set-timed-item-list self (first-n (chords self) (1+ curr-n-chord)))

    curr-beat))


;;; the duration of a tree
(defmethod tree-extent ((tree list)) 
  (decode-extent (car tree)))

;;; the duration of a leaf
(defmethod tree-extent ((tree number)) 
  (decode-extent tree))


(defmethod build-rhythm-structure ((self rhythmic-object) chords n)

  (let ((total-dur (apply '+ (mapcar 'tree-extent (cadr (tree self))))) ;;; sum of subdivisions
        (sub-dur 0)
        (curr-n-chord n)) 
    
    (setf (inside self) 
          
          (loop for subtree in (cadr (tree self))
                for beat = (symbolic-date self) then (+ beat sub-dur)
                collect 

                (if (listp subtree) 
                    ;;; subgroup
                    (let ((group (make-instance 'group :tree (list (car subtree) (simplify-subtrees (cadr subtree)))
                                                :symbolic-date beat
                                                :symbolic-dur (* (symbolic-dur self) (/ (car subtree) total-dur)))))
                      
                      ;;; set the "numdenom" indicator
                      ;;; direct from OM6: probably possible to simplify
                      ;(print "===")
                      ;(print (list subtree (symbolic-dur group)))
                      (let* ((group-ratio (get-group-ratio (tree group)))
                             (num (or group-ratio (symbolic-dur group)))
                             (denom (find-denom num (symbolic-dur group))))
                        
                         
                        ;(print (list group-ratio num denom))
                        
                        (when (listp denom) 
                          (setq num (car denom))
                          (setq denom (second denom)))

                        ;(print (list num denom))

                        (setf (numdenom group) (cond
                                                ((not group-ratio) nil)
                                                ((= (/ num denom) 1) nil)
                                                (t (list num denom))))
                        
                        (setq sub-dur (symbolic-dur group))
                        (setq curr-n-chord (build-rhythm-structure group chords curr-n-chord))
                        
                        group))
                  
                  (progn ;;; atom (leaf)
                    
                    (setq sub-dur (* (symbolic-dur self) (/ (decode-extent subtree) total-dur)))
                    ;; (print (list "CHORD" sub-dur total-dur))
                    (cond 
                     ((minusp subtree)  ;; rest
                      (make-instance 'r-rest
                                     :symbolic-date beat
                                     :symbolic-dur sub-dur))
                          
                     ((floatp subtree) ;;; tied-chord: keep current-chord in chord-list (important: same reference!)
                      (let ((real-chord (nth curr-n-chord chords)))
                        (setf (symbolic-dur real-chord) (+ (symbolic-dur real-chord) sub-dur)) ;;; extends the duration of the main chord
                        real-chord))
                          
                     (t ;;; normal chord: get the next in chord list
                        (setf curr-n-chord (1+ curr-n-chord))
                        
                        (let ((real-chord (nth curr-n-chord chords)))

                          (unless real-chord ;;; chord-list exhausted: repeat the last one as needed to finish the tree
                            (setq real-chord (clone (car (last chords))))
                            (pushr real-chord chords))

                          (setf (symbolic-date real-chord) beat
                                (symbolic-dur real-chord) sub-dur)
                          
                          real-chord))
                     )
                    ))
                ))
    
  curr-n-chord))



;;;===============================================
;;; UTILS FOR "NUMDENOM" COMPUTATION
;;; direct from OM6
;;;===============================================


(defmethod get-group-ratio (tree)
  (let ((extent (car tree))
        (addition (loop for item in (second tree) sum (floor (abs (if (listp item) (car item) item))))))
     
    (cond
      ((= (round (abs addition)) 1) nil)
      ((integerp (/ extent addition)) addition)
      ;; never happens (?)
      ((and (integerp (/ extent addition)) 
            (or (pwr-of-2-p (/ extent addition))
                (and (integerp (/ addition extent)) 
                     (pwr-of-2-p (/ addition extent))))) 
       nil)
      (t addition))))


(defun find-beat-symbol (den) (bin-value-below den))

; Find the right denom to ratio of tuplet.
(defun find-denom (num durtot)
  (cond
   ((is-binaire? durtot) (get-denom-bin num))
   ((is-ternaire? durtot) (get-denom-ter num))
   (t (get-denom-other durtot num))))


;;; is (denominator dur) a power of 2
(defun is-binaire? (dur)
  (and (= (numerator dur) 1) 
       (= (denominator dur) (next-square-of-n (denominator dur) 2))   ;;; next-pwr-of-2, or closest ?
       ))

(defun is-ternaire? (durtot)
  (and (= 3 (numerator durtot)) 
       (is-binaire? (/ 1 (denominator durtot)))
       ))

(defmethod get-denom-bin (num)
  (case num
    (3 2)
    (4 4) (5 4) (6 4) (7 4) (8 8) (9 8) (10 8) (11 8) (12 8) (13 8) (14 8)
    (15 16) (16 16)
    (otherwise (closest-bin-value num))))

(defmethod get-denom-ter (num)
  (case num
    (2 3) (3 3) (4 3)
    (5 6) (6 6) (7 6) (8 6) (9 6) 
    (10 12) (11 12) (12 12) (13 12) (14 12) (15 12) (16 12) (17 12)
    (otherwise (closest-square-of-n num 3))))

; if the answer is a list, the num should be changed in the caller-group
(defun get-denom-other (dur num)
  (let ((durtot (numerator dur)))
    (cond
     ((= (+ num 1) durtot) durtot)
     ((= num durtot) num)
     ((< num durtot)
      (list (* num 2) durtot))
     ;((< num (- (* 2 durtot) 1)) durtot)  ;; OJO OJO ESTOS CASOS HAY QUE VERLOS CON KARIM
     (t (closest-square-of num durtot))
     )))



