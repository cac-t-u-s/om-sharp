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
;=========================================================================

;===========================================================================
;  Automations
;===========================================================================

(in-package :om)

;;===========================================================================
;;;Automation Point = A point to define a curve between two dates
;;===========================================================================
;;;Structure
(defstruct (automation-point (:include bpfpoint)
            ;(:print-object
            ; (lambda (a stream)
            ;   (print-unreadable-object (a stream :type t :identity t)
            ;     (princ `(:value ,(ap-y a) :at ,(ap-x a) :ms) stream))))
            (:conc-name ap-))
  (coeff 0.5)
  (fun nil)
  (lock nil)) 

(defmethod item-get-time ((self automation-point)) (om-point-x self))
(defmethod item-set-time ((self automation-point) time) (setf (ap-x self) time))
(defmethod item-get-internal-time ((self automation-point)) (om-point-x self))
(defmethod item-set-internal-time ((self automation-point) time) nil)
(defmethod item-get-type ((self automation-point)) (ap-type self))
(defmethod item-set-type ((self automation-point) type) (setf (ap-type self) type))
(defmethod items-merged-p ((i1 automation-point) (i2 automation-point)) (= (ap-x i1) (ap-x i2)))
(defmethod items-distance ((i1 automation-point) (i2 automation-point)) (- (ap-x i2) (ap-x i1)))

(defun om-make-automationpoint (x y &optional type)
  (declare (ignore type))
  (make-automation-point :x x :y y))

(defmethod get-coeff ((self automation-point))
  (ap-coeff self))

(defmethod om-copy ((self automation-point))
  (make-automation-point :x (ap-x self) :y (ap-y self) :lock (ap-lock self) :coeff (ap-coeff self)))


;;===========================================================================
;;;Automation = An object containing automation points
;;===========================================================================
;;;Structure
(defclass automation (BPF)
  ((x-points :initform '(0.0 2000.0) :initarg :x-points)
   (y-points :initform '(0.0 100.0) :initarg :y-points)
   (c-points :initform '(0.5 0.5) :accessor c-points :initarg :c-points))
  (:default-initargs :interpol (make-number-or-nil :number 10 :t-or-nil t))
  )


(defmethod automation-stretch ((self automation) k)
  (let ((res (make-instance 'automation)))
    (setf (point-list res) (mapcar #'(lambda (pt)
                                       (let ((ptn (om-copy pt)))
                                         (setf (ap-x ptn) (* (ap-x ptn) k))
                                         ptn)) (point-list self)))
    res))

;(defmethod x-points ((self automation))
;  (mapcar 'start-date (point-list self)))
;(defmethod y-points ((self automation))
;  (mapcar 'start-value (point-list self)))

(defmethod c-points ((self automation))
  (mapcar 'ap-coeff (point-list self)))

;;;Property list (like bpf but hidden interpolation)
(defmethod get-properties-list ((self automation))
  (hide-properties 
   (call-next-method)
   '(:interpol)))

;;;Object duration
(defmethod get-obj-dur ((self automation))
  (start-date (last-elem (point-list self))))

;;;Getters and Setters;;;
;;;Point start date (x)
(defmethod start-date ((self automation-point))
  (ap-x self))

(defmethod (setf start-date) (date (self automation-point))
  (setf (ap-x self) date))

;;;Point end date (next point x)
(defmethod end-date ((self automation-point) (obj automation))
  (let ((np (next-point obj self)))
    (start-date (or np self))))

;;;Point start value (y)
(defmethod start-value ((self automation-point))
  (ap-y self))

(defmethod (setf start-value) (val (self automation-point))
  (setf (ap-y self) val))

;;;Point end value (next-point y)
(defmethod end-value ((self automation-point) (obj automation))
  (let ((np (next-point obj self)))
    (start-value (or np self))))

;;;Point function
(defmethod fun ((self automation-point) (obj automation))
  (or (ap-fun self)
      (setf (ap-fun self) (get-function self obj))))
(defmethod (setf fun) (f (self automation-point))
  (setf (ap-fun self) f))

;;;Get point function:
;x = start-date -> end-date
;y = start-value -> end-value
;curve shape <-> coeff
(defmethod get-function ((self automation-point) (obj automation))
  (if (= (start-value self) (end-value self obj))
      (constantly (start-value self))
    #'(lambda (d)
        (+ (* (expt (/ (max 0 (- d (start-date self)))
                       (- (end-date self obj) (start-date self)))
                    (log 0.5 (ap-coeff self))) (- (end-value self obj) (start-value self)))
           (start-value self)))))



;;;Initialization
(defmethod initialize-instance :after ((self automation) &rest args)
  ;(loop for coeff in (getf args :c-points)
  ;      for pt in (point-list self)
  ;      do (setf (ap-coeff pt) coeff))
  self)


(defmethod set-bpf-points ((self automation) &key x y z time time-types)
  
  (setf (point-list self)  (make-points-from-lists (or x (x-values-from-points self)) ;  (slot-value self 'x-points))
                                                   (or y (y-values-from-points self)) ;  (slot-value self 'y-points))
                                                   (decimals self)
                                                   'om-make-automationpoint))
  
  ;;; verify this c-cpoints slot management...
  (loop for coeff in (slot-value self 'c-points)
        for pt in (point-list self)
        do (setf (ap-coeff pt) coeff))
  
  (setf (slot-value self 'x-points) NIL)
  (setf (slot-value self 'y-points) NIL))

(defmethod om-point-mv ((point automation-point) &key x y)
  (if (and x (not (ap-lock point))) 
      (setf (ap-x point) (+ (ap-x point) x)))
  (if y (setf (ap-y point) (+ (ap-y point) y)))
  point)

;;;Refresh points functions (useful?)
(defmethod refresh-automation-points ((self automation))
  (loop for pt in (point-list self)
          do
          (setf (ap-fun pt) (get-function pt self))))

;;;Get next point
(defmethod next-point ((self automation) (pt automation-point))
  (nth (1+ (position pt (point-list self))) (point-list self)))

;;;Get previous point
(defmethod prev-point ((self automation) (pt automation-point))
  (if (not (eq pt (car (point-list self))))
      (nth (1- (position pt (point-list self))) (point-list self))))

;;;Draw
(defmethod draw-mini-view ((self automation) (box t) x y w h &optional time)
  (let* ((points (point-list self))
         (color (color box))
         (y (+ y 10))
         (h (- h 20))
         (npts 100)
         ranges
         step)
    (when points
      (setq ranges (list (start-date (car points))
                         (start-date (last-elem points))
                         (reduce #'min points :key 'start-value)
                         (reduce #'max points :key 'start-value)))
      (when (= (car ranges) (cadr ranges) (setf (cadr ranges) (+ (cadr ranges) 1000))))
      (setq step (/ (- (cadr ranges) (car ranges)) npts))
      (multiple-value-bind (fx ox) 
          (conversion-factor-and-offset (car ranges) (cadr ranges) w x)
        (multiple-value-bind (fy oy) 
            ;;; Y ranges are reversed !! 
            (conversion-factor-and-offset (cadddr ranges) (caddr ranges) h y)
          (om-with-fg-color (om-def-color :gray)
            (loop for pt in points
                  do 
                  (progn
                    (om-draw-circle (+ ox (* fx (start-date pt)))
                                    (+ oy (* fy (start-value pt)))
                                    3 :fill t)                    
                    (let (val lastti) 
                      (loop for ti from (+ step (start-date pt)) to (end-date pt self)
                            by step
                            with prevvi = (start-value pt)
                            do
                            (om-draw-line (+ ox (* fx (- ti step))) 
                                          (+ oy (* fy prevvi))  
                                          (+ ox (* fx ti))
                                          (+ oy (* fy (setq val (funcall (fun pt self) ti)))))
                            (setq prevvi val
                                  lastti ti))
                      (if (and lastti val)
                          (om-draw-line (+ ox (* fx lastti)) 
                                        (+ oy (* fy val))  
                                        (+ ox (* fx (end-date pt self)))
                                        (+ oy (* fy (end-value pt self))))))
                    ))))))))

;;;GET THE VALUE OF THE AUTOMATION FUNCTION AT A SPECIFIC DATE
(defmethod get-value-at-time-unit ((self automation) date)
  (funcall (fun
            (nth (max 0 (1- (or (position date (point-list self) :test '< :key 'start-date) (length (point-list self)))))
                 (point-list self))
            self)
           date))



;;;Scheduler Redefinitions
(defmethod get-action-list-for-play ((self automation) time-interval &optional parent)
  (when (action-fun self)
    (loop for ti from (car time-interval) to (1- (cadr time-interval))
          by (number-number (interpol self))
          collect
          (let ((tmp ti))
            (list
             tmp
             #'(lambda () (funcall (action-fun self) (list tmp (get-value-at-time-unit self tmp)))))))))


