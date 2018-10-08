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
; Author: D. Bouche
;============================================================================

;===========================================================================
;  Tempo Automation
;===========================================================================

(in-package :om)

(defvar *curve-N-factor* 512)

(defstruct (tempo-point
            (:include automation-point (x 0) (y 120))
            (:print-object
             (lambda (a stream)
               (print-unreadable-object (a stream :type t :identity t)
                 (princ `(:bpm ,(tp-y a) :at :beat ,(tp-x a)) stream))))
            (:conc-name tp-)))

;;===========================================================================
;;;Tempo Automation = An automation for tempo 
;;===========================================================================
;;;Structure
(defclass tempo-automation (automation)
  ((past-beat :initform 0 :initarg :past-beat :accessor past-beat)
   (past-time :initform 0 :initarg :past-time :accessor past-time)))

; (make-instance 'tempo-automation)

(defmethod beat-date ((self tempo-point) (obj tempo-automation) beat)
  (let* ((dm (start-date self))
         (fun (fun self obj))
         (xk #'(lambda (k) (+ dm (* k (/ (- beat dm) *curve-N-factor*))))))
    (* (/ (- beat dm) (* 3 *curve-N-factor*))
       (+ (/ 1 (funcall fun (funcall xk (start-value self))))
          (loop for k from 1 to (- (round *curve-N-factor* 2) 1)
                sum
                (/ 2 (funcall fun (funcall xk (* 2 k)))))
          (loop for k from 1 to (round *curve-N-factor* 2)
                sum
                (/ 4 (funcall fun (funcall xk (- (* 2 k) 1)))))
          (/ 1 (funcall fun (funcall xk *curve-N-factor*))))
       60000)))

(defmethod initialize-instance :after ((self tempo-automation) &rest args)
  (setf (point-list self) (loop for x in (or (getf args :x-points) '(0 2000))
                                for y in (or (getf args :y-points) '(120 120))
                                for coeff in (or (getf args :c-points) '(0.5 0.5))
                                collect
                                (make-tempo-point :x x
                                                  :y y
                                                  :coeff coeff)))
  (loop for pt in (point-list self)
        do
        (setf (tp-fun pt) (get-function pt self)))
  self)

#|
(defmethod initialize-instance ((self tempo-automation) &rest initargs)
  ;;;TMP : CONSTANT
  (setf (point-list self) (list (make-tempo-point :y 120 :x 0)
                                (make-tempo-point :y 120 :x *positive-infinity*)))
  (loop for pt in (point-list self)
        do
        (setf (tp-fun pt) (get-function pt self)))
  self)
|#

(defmethod tp-start-date ((self tempo-point) (obj tempo-automation))
  (get-beat-date obj (tp-x self)))
(defmethod tp-end-date ((self tempo-point) (obj tempo-automation))
  (get-beat-date obj (tp-x (or (next-point obj self) self))))

(defmethod tp-start-beat ((self tempo-point))
  (tp-x self))
(defmethod (setf tp-start-beat) (beat (self tempo-point))
  (setf (tp-x self) beat))
(defmethod tp-end-beat ((self tempo-point) (obj tempo-automation))
  (tp-x (or (next-point obj self) self)))

(defmethod tp-start-value ((self tempo-point))
  (tp-y self))
(defmethod tp-end-value ((self tempo-point) (obj tempo-automation))
  (tp-y (or (next-point obj self) self)))

(defmethod fun ((self tempo-point) (obj tempo-automation))
  (or (ap-fun self)
      (setf (ap-fun self) (get-function self obj))))
(defmethod (setf fun) (f (self tempo-point))
  (setf (ap-fun self) f))

(defmethod get-function ((self tempo-point) (obj tempo-automation))
  (if (= (tp-start-value self) (tp-end-value self obj))
      (constantly (tp-start-value self))
    #'(lambda (d)
        (+ (* (expt (/ (- d (tp-start-beat self))
                       (- (tp-end-beat self obj) (tp-start-beat self)))
                    (log 0.5 (tp-coeff self))) (- (tp-end-value self obj) (tp-start-value self)))
           (tp-start-value self)))))

(defmethod (setf point-list) ((point-list t) (self bpf))
  (setf (slot-value self 'point-list) point-list))



(defmethod get-function-auto ((self tempo-point) (auto tempo-automation))
  (or (ap-fun self)
      (let* ((next (nth (1+ (position self (point-list auto))) (point-list auto)))
             (end-beat (if next (tp-start-beat next) *positive-infinity*)))
        (if (= (tp-start-value self) (tp-end-value self auto))
            (constantly (tp-start-value self))
          #'(lambda (d)
              (+ (* (expt (/ (- d (tp-start-beat self))
                             (- end-beat (tp-start-beat self)))
                          (log 0.5 (ap-coeff self))) (- (tp-end-value self auto) (tp-start-value self)))
                 (ap-start-value self)))))))

(defmethod max-tempo ((self tempo-automation))
  (reduce 'max (point-list self) :key 'tp-start-value))
  

(defmethod set-nth-point-start-value ((self tempo-automation) n value)
  ;(setf (tp-start-date (nth n (point-list self))) (get-beat-date self value))
  (setf (tp-start-value (nth n (point-list self))) value))

(defmethod get-beat-date ((self tempo-automation) beat)
  (let ((points (point-list self))
        (current-time 0))
    (loop for i from 0 to (1- (length points))
          do
          (if (and (nth (1+ i) points) (< (start-date (nth (1+ i) points)) beat))
              (incf current-time (beat-date (nth i points) self (start-date (nth (1+ i) points))))
            (progn
              (incf current-time (beat-date (nth i points) self beat))
              (return))))
    (round current-time)))

(defmethod get-action-list-for-play ((self tempo-automation) time-interval &optional parent)
  (loop for ti from (car time-interval) to (cadr time-interval)
        by (rate-ms self)
        collect
        (list
         ti
         #'(lambda () (funcall (action self) (tempo-at-date self ti))))))

(defmethod tempo-at-beat ((self tempo-automation) beat)
  (let ((pt (find beat (point-list self) :test #'(lambda (x y)
                                                   (and (>= x (tp-start-beat y))
                                                        (< x (tp-end-beat y self)))))))
    (if pt
        (funcall (get-function pt self) beat)
      0)))

(defmethod get-beat-grid ((self tempo-automation) tmin tmax &optional (beat-rate 1))
  (let* ((pt-index (position tmin (point-list self) :test #'(lambda (x y)
                                                              (and (>= tmin (get-beat-date self (start-date y)))
                                                                   (< tmin (get-beat-date self (end-date y self)))))))
         (grid '())
         (date 0))
    (when pt-index
      (if (= pt-index (1- (length (point-list self))))
          (let* ((pt (car (last (point-list self))))
                 (b1 (tp-start-beat pt))
                 (t1 (tp-start-date pt self))
                 (tp (tp-start-value pt))
                 (beat-dur (/ 60000 tp))
                 (bs (+ b1 (floor (- tmin t1) (/ 60000 tp))))
                 (be (+ b1 (ceiling (- tmax t1) (/ 60000 tp)))))
            (loop for beat from (- bs (mod bs beat-rate)) to be 
                  by beat-rate
                  for n from 0
                  do
                  (setq date (+ t1 (* beat-dur (- beat b1))))
                  (if (= (mod beat 1) 0)
                      (push (list date beat) grid)
                    (push (list date (list (mod n (/ 1 beat-rate)) (/ 1 beat-rate))) grid))))
        (loop for beat from (tp-start-beat (nth pt-index (point-list self)))
              by beat-rate
              for n from 0
              do
              (setq date (get-beat-date self beat))
              (if (and (< date tmax) (>= date tmin))
                  (if (= (mod beat 1) 0)
                      (push (list date beat) grid)
                    (push (list date (list (mod n (/ 1 beat-rate)) (/ 1 beat-rate))) grid))
                (if (>= date tmax)
                    (return)))))
      (reverse grid))))

(defmethod get-display-beat-factor ((self tempo-automation) t1 t2)
  (let ((factor (/ (- t2 t1) 
                          (/ 60000 
                             (/ (reduce '+ (point-list self) :key 'start-value) 
                                (length (point-list self)))))))
  (if (<= factor 10)
      (/ (expt 2 (round (log (/ factor 40)) 2)) 2)
    (expt 2 (round (log (1+ (/ factor 40)) 2))))))