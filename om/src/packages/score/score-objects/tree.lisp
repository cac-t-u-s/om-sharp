
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

;;; TOOLS TO WOR ON THE RHYTHM-TREE (list/text) REPRESENTATION
;;; mostly from OM6 code (by C. Agon & G. Assayag)

(in-package :om)

(defmethod fullratio ((self list)) (/ (first self) (second self)))
(defmethod fullratio ((self number)) self)
(defmethod fullratio ((self float)) (round self))

(defmethod fdenominator ((self t)) (denominator (fullratio self)))
(defmethod fdenominator ((self list)) (second self))
(defmethod fnumerator ((self t)) (numerator (fullratio self)))
(defmethod fnumerator ((self list)) (first self))

(defun subtree-extent (subtree)
  (cond ((listp subtree) (fullratio (first subtree)))
        ((floatp subtree) (round (abs subtree)))
        ((or (ratiop subtree) (integerp subtree)) (abs subtree))))

(defun  symbol->ratio (symbol)
  (let ((string (copy-seq (symbol-name symbol))))
  (loop for i from 0 to (1- (length string))
        when (char= (elt string i) #\/) do (setf (elt string i) '#\Space))
  (read-from-string (format () "(~A)" string)) ))


;(reduce #'(lambda (x y) (+ (abs x) (subtree-extent y))) 
;        '((2 8) (3 8) (4 8) (3 8) (2 8))
;        :initial-value 0)


;;; total extent in quarter-notes (formerly "resolve-?")
(defun compute-total-extent (tree)
  (cond 
   ((numberp list) list)
   ((or (numberp (first list)) (listp (first list)))
    (if (listp (second list)) 
        (list (first list) (mapcar #'resolve-? (second list)))
      (error (format nil "Invalid Rhythm Tree : ~A" list))))

   ((and (symbolp (first list)) (equal (symbol-name (first list)) "?"))
    (let ((solved (mapcar #'resolve-? (second list))))
      (list (reduce #'(lambda (x y) (+ (abs x) (subtree-extent y))) 
                    solved :initial-value 0)
            solved)))
   ((symbolp (first list))
    (if (listp (second list)) 
        (list (symbol->ratio (first list)) (mapcar #'resolve-? (second list)))
      (error (format nil "Invalid Rhythm Tree : ~A" list))))))



;;; convert to 'better' values
(defun convert-extent (extent)
  (case extent
    (5 (list 4 1))
    (9 (list 8 1))
    (10 (list 8 2))
    (11 (list 8 3))
    (13 (list 12 1))
    (17 (list 16 1))
    (18 (list 16 2))
    (19 (list 16 3))
    (20 (list 16 4))
    (21 (list 15 6))
    (22 (list 16 6))
    (23 (list 16 7))
    (25 (list 24 1))
    (t  extent)))



(defun normalize-tree (tree)
  
  (unless (listp (car tree))
    ;;; probably "old-formatted" RT, with "?" etc.
    (setf tree (second tree)))
  
  (labels 
    ((normalize (tree)
       
       (cond 
        ((numberp tree)
         (let ((convert (convert-extent (abs (round tree)))))
           (if (listp convert)
               (progn 
                 (if (plusp tree)
                     (progn (setf (second convert) (float (second convert)))
                       (when (floatp tree) (setf (first convert) (float (first convert)))))
                   
                   (setf (first convert) (- (first convert)) (second convert) (- (second convert))))
                 convert)
             (list tree))))
             
        ((listp tree)
         (list (list (first tree) (mapcan #'normalize (second tree))))))))
    
    (first (normalize tree))
    ))


