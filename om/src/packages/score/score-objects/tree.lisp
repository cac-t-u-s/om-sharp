
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

;;; TOOLS TO WORK ON THE RHYTHM-TREE (list/text) REPRESENTATION
;;; mostly from OM6 code (by C. Agon & G. Assayag)

(in-package :om)

;;; get the absolute duration (in proportion)
(defun decode-extent (dur)
  (cond ((listp dur) ;;; e.g. '(4 4)
         (/ (first dur) (second dur)))
        ((floatp dur) 
         (round (abs dur)))
        (t ;;; hopefully this is a number 
         (abs dur))
        ))


; (reduce #'(lambda (x y) (+ (abs x) (fullratio y))) '((2 8) (3 8) (4 8) (3 8) (2 8)) :initial-value 0)
; (compute-total-extent '(1 (1 (2 5)) 1 1)) (((8 4) (1 (1 (2 5)) 1 1)) ((4 4) (1 1 5 1))))


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


;;; simplify complex subdivisions into tied/simpler ones
(defun normalize-tree (tree)
   
  (labels 
    ((normalize-recursive (tree)
       
       (cond 
        
        ;;; normalize recursively (the subdiv-part)
        ((listp tree)
         (list (list (car tree) (mapcan #'normalize-recursive (cadr tree)))))
        
        ((numberp tree) ;;; a leaf
         
         (let ((converted-extent (convert-extent (abs (round tree))))) ;;; (1) convert to positive integer just to call convert-extent
           
           (if (listp converted-extent) ;;; the leaf was converted
               (progn 
                 
                 (if (plusp tree) 
                     ;;; abs did not apply in (1)
                     (progn 
                       ;;; tie the second pulse to the first pulse
                       (setf (second converted-extent) (float (second converted-extent)))
                       
                       (when (floatp tree) 
                         ;;; the first pulse was already a tie
                         (setf (first converted-extent) (float (first converted-extent)))))
                   
                   ;;; else: (1) abs did invert the sign: restore it
                   (setf (first converted-extent) (- (first converted-extent)) 
                         (second converted-extent) (- (second converted-extent))))
                 
                 converted-extent)
             
             ;;; nothing changes
             (list tree))))      
        )))
    
    (mapcan #'normalize-recursive tree)
    ))


