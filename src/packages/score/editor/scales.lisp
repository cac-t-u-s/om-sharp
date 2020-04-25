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


; (degree line accidental)
(defparameter *default-scale* 
  '((0 0 nil)
    (100 0 :sharp) 
    (200 0.5 nil)
    (300 0.5 :sharp)
    (400 1 nil)
    (500 1.5 nil)
    (600 1.5 :sharp)
    (700 2 nil)
    (800 2 :sharp)
    (900 2.5 nil)
    (1000 2.5 :sharp)
    (1100 3 nil)))


(defparameter *1/4-scale* 
  '((0 0 nil)
    (50 0 :1/4-sharp)
    (100 0 :sharp) 
    (150 0 :3/4-sharp) 
    (200 0.5 nil)
    (250 0.5 :1/4-sharp)
    (300 0.5 :sharp)
    (350 0.5 :3/4-sharp)
    (400 1 nil)
    (450 1 :1/4-sharp)
    (500 1.5 nil)
    (550 1.5 :1/4-sharp)
    (600 1.5 :sharp)
    (650 1.5 :3/4-sharp)
    (700 2 nil)
    (750 2 :1/4-sharp)
    (800 2 :sharp)
    (850 2 :3/4-sharp)
    (900 2.5 nil)
    (950 2.5 :1/4-sharp)
    (1000 2.5 :sharp)
    (1050 2.5 :3/4-sharp)
    (1100 3 nil)
    (1150 3 :1/4-sharp)
    ))


(defparameter *1/8-scale* 
  '((0 0 nil)
    (25 0 :1/8-sharp)
    (50 0 :1/4-sharp)
    (75 0 :3/8-sharp)
    (100 0 :sharp) 
    (125 0 :5/8-sharp) 
    (150 0 :3/4-sharp) 
    (175 0 :7/8-sharp) 
    (200 0.5 nil)
    (225 0.5 :1/8-sharp)
    (250 0.5 :1/4-sharp)
    (275 0.5 :3/8-sharp)
    (300 0.5 :sharp)
    (325 0.5 :5/8-sharp)
    (350 0.5 :3/4-sharp)
    (375 0.5 :7/8-sharp)
    (400 1 nil)
    (425 1 :1/8-sharp)
    (450 1 :1/4-sharp)
    (475 1 :3/8-sharp)
    (500 1.5 nil)
    (525 1.5 :1/8-sharp)
    (550 1.5 :1/4-sharp)
    (575 1.5 :3/8-sharp)
    (600 1.5 :sharp)
    (625 1.5 :5/8-sharp)
    (650 1.5 :3/4-sharp)
    (675 1.5 :7/8-sharp)
    (700 2 nil)
    (725 2 :1/8-sharp)
    (750 2 :1/4-sharp)
    (775 2 :3/8-sharp)
    (800 2 :sharp)
    (825 2 :5/8-sharp)
    (850 2 :3/4-sharp)
    (875 2 :7/8-sharp)
    (900 2.5 nil)
    (925 2.5 :1/8-sharp)
    (950 2.5 :1/4-sharp)
    (975 2.5 :3/8-sharp)
    (1000 2.5 :sharp)
    (1025 2.5 :5/8-sharp)
    (1050 2.5 :3/4-sharp)
    (1075 2.5 :7/8-sharp)
    (1100 3 nil)
    (1125 3 :1/8-sharp)
    (1150 3 :1/4-sharp)
    (1175 3 :3/8-sharp)
    ))


(defparameter *all-scales*
  `((:scale-1/2 ,*default-scale*)
    (:scale-1/4 ,*1/4-scale*)
    (:scale-1/8 ,*1/8-scale*)))

(defun get-the-scale (symb)
  (or (cadr (find symb *all-scales* :key #'car))
      *default-scale*))
    
(defun step-from-scale (symb) 
  (let ((scale (get-the-scale symb)))
    (- (first (second scale)) (first (first scale)))))


;;; a utility for user-code to add new scales in score editors:
(defun add-scale (name scale)
  (let ((existing-scale (find name *all-scales* :key #'car)))
    (if existing-scale 
        (setf (cadr existing-scale) scale)
      (pushr (list name scale) *all-scales*)
      )))


