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
  ((Lmidic :initform '((6000)) :accessor Lmidic :initarg :Lmidic :type list :documentation "pitches (mc)/chords: list or list of lists")
   (tree :initform '(((4 4) (1 1 1 1))) :accessor tree :initarg :tree :type list :documentation "a rhythm tree (list of measure-rythm-trees)")
   (tempo :accessor tempo :initform 60 :initarg :tempo :documentation "a tempo value or tempo-map")
   (r-struct :accessor r-struct :initform nil :documentation "an internal hierarchical structure")))

;;; some additional classes to build a rhythmic structure
(defclass measure (score-object container) ())
(defclass group (score-object container) ())
(defclass rrest (score-object) ())
(defclass grace-note (score-object) ())


(defmethod additional-class-attributes ((self voice)) '(lvel loffset lchan lport))

(defmethod initialize-instance ((self voice) &rest initargs)
  (call-next-method)
  
    ;;; probably "old-formatted" RT, with "?" etc.
  (unless (listp (car (tree self)))
    (setf (tree self) (cadr (tree self))))

  (setf (tree self) (normalize-tree (tree self)))
  (setf (r-struct self) (init-seq-from-tree self (tree self)))
  self)




(defmethod init-seq-from-tree ((self voice) (tree list)) 
  (let ((Extent (compute-total-extent tree))
        (nbsubunits (reduce  
                     #'(lambda (x y) (+ (abs x) (subtree-extent y))) 
                     tree :initial-value 0))
        (curr-obj nil)
        (current-graces nil) (current-note nil))
    
    (setf (extent self) extent)

#|
    (remove 
     NIL
     (loop 
      for subtree in tree
      do (setf curr-obj 
               (cond
                ((numberp subtree)
                 (let ((object 
                        (cond 
                         ((zerop subtree) ;; GRACE NOTE
                          (if current-note (setf (mus-const current-note) (append (list! (mus-const current-note)) '(1)))
                            (setf current-graces (cons -1 current-graces)))
                          NIL)
                         ((> subtree 0)
                          (let ((note (make-instance 'note :empty t :extent (* (fullratio subtree) (/ Extent nbsubunits)))))
                            (when current-graces ;; add grace notes before
                              (setf (mus-const note) current-graces))
                            (setf current-note note)
                            note))
                         ((< subtree 0)
                          (make-instance 'rest :extent (*  (abs (fullratio subtree)) (/ Extent nbsubunits)))))))
                   (when (and (plusp subtree) (floatp subtree))
                     (setf (tie object) 'continue))
                   object))
                ((listp subtree)
                 (make-instance (next-metric-class self)
                                :tree subtree 
                                :PropagateExtent (/ Extent nbsubunits)
                                :InternalCall t))))
      when curr-obj
      collect curr-obj
      into inside
      finally 
      (setf (slot-value self 'inside) inside
            (slot-value self 'extent) Extent)
    |#
      ))






