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
   (r-struct :accessor r-struct :initform nil :documentation "internal hierarchical structure")))
;;; rename this slot "inside" ?

(defclass rhythmic-object () 
  ((tree :initform '(1 (1 1 1 1)) :accessor tree :initarg :tree :type list :documentation "a rhythm tree")
   (r-struct :accessor r-struct :initform nil :documentation "internal hierarchical structure")))

;;; some additional classes to build a rhythmic structure
(defclass measure (score-object rhythmic-object) ())
(defclass group (score-object rhythmic-object) ())
(defclass rrest (score-object) ())
(defclass cont-chord (score-object) ())
(defclass grace-note (score-object) ())


(defmethod additional-class-attributes ((self voice)) '(lvel loffset lchan lport))

(defmethod initialize-instance ((self voice) &rest initargs)
  (call-next-method)
  
    ;;; probably "old-formatted" RT, with "?" etc.
  (unless (listp (car (tree self)))
    (setf (tree self) (cadr (tree self))))

  (setf (tree self) (normalize-tree (tree self)))
  
  ;;; build the rhythmic structures
  (let ((m-dur 0))
    (setf (r-struct self) 
          (loop for m-tree in (tree self)
                for beat = 0 then (+ beat m-dur)
                do (setq m-dur (tree-extent m-tree))
                collect (make-instance 'measure :tree m-tree
                                     :symbolic-date beat
                                     :symbolic-dur m-dur)))
    )

  ;;; distribute the actual chords and set the onset according to the tempo map.
  
  self)



(defmethod initialize-instance ((self rhythmic-object) &rest initargs)
 
  (call-next-method)
  
  (let ((total-dur (compute-total-extent (tree self)))
        (sub-dur 0)) 
    (setf (r-struct self) 
          (loop for subtree in (cadr (tree self))
                for beat = (symbolic-date self) then (+ beat sub-dur)
                do 
                collect 
                (if (listp subtree) 
                    ;; subgroup
                    (progn
                      (setq sub-dur (* (symbolic-dur self) (/ (car subtree) total-dur)))
                      (make-instance 'group :tree subtree
                                     :symbolic-date beat
                                     :symbolic-dur sub-dur))
                  
                  (progn 
                    (setq sub-dur (* (symbolic-dur self) (/ (decode-extent subtree) total-dur)))
                    (cond ((minusp subtree)  ;; rest
                           (make-instance 'rrest
                                          :symbolic-date beat
                                          :symbolic-dur sub-dur))
                          ((floatp subtree) ;;; tied-chord
                           (make-instance 'cont-chord
                                          :symbolic-date beat
                                          :symbolic-dur sub-dur))
                          (t ;;; normal chord
                             (make-instance 'chord
                                            :symbolic-date beat
                                            :symbolic-dur sub-dur)))
                    ))
                )))
  self)




     






