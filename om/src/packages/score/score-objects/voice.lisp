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
  ((Lmidic :initform '((6000)) :initarg :Lmidic :type list :documentation "pitches (mc)/chords: list or list of lists")
   (tree :initform '(((4 4) (1 1 1 1))) :accessor tree :initarg :tree :type list :documentation "a rhythm tree (list of measure-rythm-trees)")
   (tempo :accessor tempo :initform 60 :initarg :tempo :documentation "a tempo value or tempo-map")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")
   ))

(defmethod additional-class-attributes ((self voice)) '(lvel loffset lchan lport))


(defclass rhythmic-object (score-object) 
  ((tree :initform '(1 (1 1 1 1)) :accessor tree :initarg :tree :type list :documentation "a rhythm tree")
   (inside :accessor inside :initform nil :documentation "internal hierarchical structure")))

;;; some additional classes to build a rhythmic structure
(defclass measure (rhythmic-object) ())
(defclass group (rhythmic-object) ())

(defclass rrest (score-object) ())
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

;;; the duration fo a leaf
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
                    (let ((group (make-instance 'group :tree subtree
                                                :symbolic-date beat
                                                :symbolic-dur (* (symbolic-dur self) (/ (car subtree) total-dur)))))
                        (setq sub-dur (symbolic-dur group))
                        (setq curr-n-chord (build-rhythm-structure group chords curr-n-chord))
                        group)
                  
                  (progn ;;; atom (leaf)
                    
                    (setq sub-dur (* (symbolic-dur self) (/ (decode-extent subtree) total-dur)))
                    ;; (print (list "CHORD" sub-dur total-dur))
                    (cond 
                     ((minusp subtree)  ;; rest
                      (make-instance 'rrest
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



;;;; MESURE BARS !!
(defmethod score-object-mini-view ((self voice) x-u y-u w h staff fontsize)
  
  (when (chords self)
    
    (let* ((unit (font-size-to-unit fontsize))
           (shift-x-u 7) ;;  
           (w-u (- (/ w unit) shift-x-u 2));; +1 for margin each side 
           (x-ratio (/ w-u (get-obj-dur self))))
    
      (loop for m in (cdr (inside self)) 
            do (let* ((begin (beat-to-time (symbolic-date m) (tempo self))))
                 (draw-measure-bar (* (* begin x-ratio) unit) fontsize staff)))
      
      (loop for chord in (chords self) do
            (draw-chord (notes chord) 
                        (+ shift-x-u (* (date chord) x-ratio)) 
                        y-u 
                        w h fontsize :scale nil :staff staff)
            ))))



     






