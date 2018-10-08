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


;;;========================================
;;; TIMED POINT
;;;========================================

;In timed point we have a defined time slot, an internal time for editor's given time if not defined and type
;TimeTypes can be :
; nil : not defined
; t :defined
; :master --> Master point for interpolations

(defstruct (tpoint (:include bpfpoint)) time internal-time)

(defun om-make-tpoint (x y &optional time type)
  (make-tpoint :x x :y y :time time :internal-time time :type type))

(defmethod om-copy ((self tpoint))
  (make-tpoint :x (tpoint-x self) :y (tpoint-y self) :time (tpoint-time self) :internal-time (tpoint-internal-time self) :type (tpoint-type self)))

(defmethod om-point-set ((point tpoint) &key x y time type)
  (if x (setf (tpoint-x point) x))
  (if y (setf (tpoint-y point) y))
  (if time (progn
             (setf (tpoint-time point) time)
             (setf (tpoint-internal-time point) time)))
  (if type (setf (tpoint-type point) type))
  point)

(defmethod om-point-mv ((point tpoint) &key x y time)
  (if x (setf (tpoint-x point) (+ (tpoint-x point) x)))
  (if y (setf (tpoint-y point) (+ (tpoint-y point) y)))
  (if time (progn
             (setf (tpoint-time point) (+ (tpoint-time point) time))
             (setf (tpoint-internal-time point) (tpoint-time point))))
  point)

(defmethod om-points-equal-p ((p1 tpoint) (p2 tpoint))
  (and (= (tpoint-x p1) (tpoint-x p2))
       (= (tpoint-y p1) (tpoint-y p2))
       (= (tpoint-time p1) (tpoint-time p2))))

;;; not necessarily at the same time !
(defmethod om-points-at-same-position ((p1 ompoint) (p2 ompoint))
  (and (= (om-point-x p1) (om-point-x p2))
       (= (om-point-y p1) (om-point-y p2))))


(defmethod calc-intermediate-point-at-time ((p1 tpoint) (p2 tpoint) time)
  (let* ((dur (- (tpoint-internal-time p2) (tpoint-internal-time p1)))
         (factor (/ (- time (tpoint-internal-time p1)) dur))
         (x (+ (* (- (tpoint-x p2) (tpoint-x p1)) factor) (tpoint-x p1)))
         (y (+ (* (- (tpoint-y p2) (tpoint-y p1)) factor) (tpoint-y p1))))
    (om-make-tpoint x y time)))

;;; Need to define it for for BPF points, too
(defmethod calc-intermediate-point-at-time ((p1 ompoint) (p2 ompoint) time)
  (let* ((dur (- (om-point-x p2) (om-point-x p1)))
         (factor (/ (- time (om-point-x p1)) dur))
         (y (+ (* (- (om-point-y p2) (om-point-y p1)) factor) (om-point-y p1))))
    (om-make-point time y)))


;;; TIMED-ITEM IMPLEMENTATION
(defmethod item-get-time ((self tpoint)) (tpoint-time self))
(defmethod item-set-time ((self tpoint) time) (setf (tpoint-time self) time))
(defmethod item-get-internal-time ((self tpoint)) (tpoint-internal-time self))
(defmethod item-set-internal-time ((self tpoint) time) (setf (tpoint-internal-time self) time))
(defmethod item-get-type ((self tpoint)) (tpoint-type self))
(defmethod item-set-type ((self tpoint) type) (setf (tpoint-type self) type))

;;;========================================
;;; BPC
;;;========================================

(defclass class-with-times-slot () 
  ((times :initform nil :initarg :times :accessor times)))

(defclass* BPC (bpf class-with-times-slot)
           ((x-points :initform nil :initarg :x-points)
            (y-points :initform nil :initarg :y-points))
           (:icon 'bpc)
           (:documentation "
BREAK-POINTS CURVE: a 2D function defined by a list of [x,y] coordinates.

BPC objects are constructed from the list of X coordinates (<x-points>) and the list of Y coordinates (<y-points>)
<x-point> are NOT necesarily increasing (contrary to BPF objects).
If <x-list> and <y-list> are not of the same length, the last step in the shorter one is repeated.

<decimals> allows to specify the precision of the function (0 = integers, n > 0 = number of decimals)."))

(defmethod additional-class-attributes ((self BPC)) 
  (append '(times) (call-next-method)))

;;; BPCs have a special accessor for times
(defmethod times ((self BPC)) (time-sequence-get-times self))
(defmethod (setf times) ((times t) (self BPC)) (time-sequence-set-times self times) times)

(defmethod om-init-instance ((self bpc) &optional initargs)
  ;;; save/load will work with slot-value only
  (when (slot-value self 'times) 
    (time-sequence-set-times self (slot-value self 'times)))
  (call-next-method))


;; need to redefine from BPF
(defmethod time-sequence-get-times ((self BPC)) (time-values-from-points self))
(defmethod time-sequence-set-times ((self BPC) times) 
  (set-bpf-points self :time times)
  (time-sequence-update-internal-times self))


(defmethod bpc-p ((self bpc)) t)
(defmethod bpc-p ((self t)) nil)

(defmethod time-values-from-points ((self BPC)) (mapcar #'tpoint-time (point-list self)))

(defmethod (setf decimals) ((decimals t) (self BPC))
  (let ((x (x-values-from-points self))
        (y (y-values-from-points self))
        (times (time-values-from-points self)))
  (setf (slot-value self 'decimals) decimals)
  (check-decimals self)
  (set-bpf-points self :x x :y y :time times)
  (decimals self)))

(defmethod init-bpf-points ((self BPC))
  (set-bpf-points self 
                  :x (slot-value self 'x-points)
                  :y (slot-value self 'y-points)
                  :time (slot-value self 'times)
                  :time-types (slot-value self 'time-types))
  (time-sequence-update-internal-times self)
  self)


;;; NO X-SORT IN BPCS
(defmethod set-bpf-points ((self bpc) &key x y z time time-types)
  (let ((times (or time (time-values-from-points self))))
    (setf (point-list self)  (make-points-from-lists (or x (x-values-from-points self)) ;  (slot-value self 'x-points))
                                                     (or y (y-values-from-points self)) ;  (slot-value self 'y-points))
                                                     (decimals self)
                                                     'om-make-tpoint))
    (when times
      (loop for p in (point-list self)
            for time in times do (setf (tpoint-time p) time)))
    (when time-types
      (loop for p in (point-list self)
            for type in time-types do (om-point-set p :type type)))
    (setf (slot-value self 'x-points) NIL)
    (setf (slot-value self 'y-points) NIL)
    (setf (slot-value self 'times) NIL)
    (setf (slot-value self 'time-types) NIL)))


;;; In BPC all moves are possible
(defmethod possible-move ((self bpc) points xkey deltax ykey deltay) t)
(defmethod possible-set ((self bpc) point x y) t)

(defmethod adapt-point ((self bpc) point)
  (setf (tpoint-x point) (funcall (truncate-function (decimals self)) (tpoint-x point)) 
        (tpoint-y point) (funcall (truncate-function (decimals self)) (tpoint-y point)))
  point)

;;; In BPC we add the new points at the end
(defmethod insert-point ((self bpc) new-point &optional position)
  (let ((p (adapt-point self new-point))
        (pp (or position (length (point-list self)))))
    (setf (point-list self) (insert-in-list (point-list self) p pp))
   pp ))

(defmethod set-point-in-bpc ((self bpc) point x y time)
  (let ((xx (funcall (truncate-function (decimals self)) (or x (om-point-x point))))
        (yy (funcall (truncate-function (decimals self)) (or y (om-point-y point)))))        
    (setf (tpoint-x point) xx
          (tpoint-y point) yy
          (tpoint-time point) time)
    point))

;;;=========================================
;;; TIME-SEQUENCE METHODS
;;;=========================================


;(defmethod get-all-times ((self BPC)) (time-sequence-get-internal-times self))

(defmethod get-obj-dur ((self BPC)) 
  (if (point-list self) (tpoint-internal-time (car (last (point-list self)))) 0))

(defmethod time-sequence-make-timed-item-at ((self bpc) at)
  (make-default-tpoint-at-time self at))



;;; Create a new point that preserves the motion of the object
(defmethod make-default-tpoint-at-time ((self bpc) time)
  (if (times self)
      (let ((pos (or (position time (point-list self) :key 'tpoint-internal-time :test '<= ) 
                     (length (point-list self))))
            (len (length (point-list self))))
        ;if length is 1 or if the point is before the others or after use the same position than the before or after point
        (if (or (= len 1) (or (= pos 0) (= pos len))) 
            (let ((point (nth (min pos (1- len)) (point-list self))))
              (om-make-tpoint (om-point-x point) (om-point-y point) time))
          ; if several points, preserve the motion
          (let ((p1 (nth (1- pos) (point-list self)))
                (p2 (nth pos (point-list self))))
            (calc-intermediate-point-at-time p1 p2 time))))
    ;if no points, create a points a pos 
    (om-make-tpoint 0 0 time)))

(defmethod* get-interpolated-sequence ((self bpc) &optional (interpol-time 100))
  :numouts 3
  (if (point-list self)
      (let* ((interpol-times (arithm-ser (tpoint-internal-time (car (point-list self)))
                                         (tpoint-internal-time (car (last (point-list self))))
                                         interpol-time))
             (new-points (loop for time in interpol-times
                               collect
                               (make-default-tpoint-at-time self time)))
             (new-obj (make-instance (type-of self) :decimals (decimals self))))
        (setf (point-list new-obj) new-points)
        (values new-obj (x-points new-obj) (y-points new-obj)))
    (values (make-instance (type-of self) :x-points nil :y-points nil :decimals (decimals self)) nil nil)))




;-------------------get the min  max points in x and y axis------------------------------
;;; using reduce 'mix/max is fatser when interpreted but not when compiled
(defmethod nice-bpf-range ((self bpc))
  (multiple-value-bind (x1 x2 y1 y2 t1 t2)
      (loop for x in (x-values-from-points self) 
            for y in (y-values-from-points self)
            for time in (time-sequence-get-internal-times self)
            minimize x into x1 maximize x into x2
            minimize y into y1 maximize y into y2
            minimize time into t1 maximize time into t2
            finally (return (values x1 x2 y1 y2 t1 t2)))
    (append (list x1 x2) (list y1 y2) (list t1 t2))
    ; (if (< (- x2 x1) 10) (list (- x1 5) (+ x2 5)) (list x1 x2))
    ; (if (< (- y2 y1) 10) (list (- y1 5) (+ y2 5)) (list y1 y2)))
    ))

;;; to be redefined by objects if they have a specific miniview for the maquette
(defmethod draw-maquette-mini-view ((self bpc) (box t) x y w h &optional time) 
  (let* ((x-col (om-def-color :red))
         (y-col (om-def-color :green))
         (ranges (nice-bpf-range self))
         (times (time-sequence-get-internal-times self))
         (x-t-list (mat-trans (list times (x-points self))))
         (x-t-ranges (list 0 (nth 5 ranges) (car ranges) (cadr ranges)))
         (y-t-list (mat-trans (list times (y-points self))))
         (y-t-ranges (list 0 (nth 5 ranges) (caddr ranges) (cadddr ranges))))
        ;draw x = f(t)
    (draw-bpf-points-in-rect x-t-list
                             x-col
                             x-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                             x (+ y 10) w (- h 20)
                             )

            ;draw y = f(t)
    (draw-bpf-points-in-rect y-t-list
                             y-col
                             y-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                             x (+ y 10) w (- h 20)
                             )
    t))

;;;============================
;;; PLAYER
;;;============================
(defmethod get-action-list-for-play ((object bpc) interval &optional parent)
  (when (action object)
    (if (number-? (interpol object))
        (let* ((root (get-active-interpol-time object (car interval))))
          (loop for interpolated-time in (arithm-ser root (1- (cadr interval)) (number-number (interpol object)))
                collect (list 
                         interpolated-time 
                         #'(lambda (pt) (funcall (action-fun object) pt)) 
                         (make-default-tpoint-at-time object interpolated-time))))
      (loop for pt in (filter-list (point-list object) (car interval) (cadr interval) :key 'tpoint-internal-time)
            collect
            (list (tpoint-internal-time pt)
                  #'(lambda (ptmp) (funcall (action-fun object) ptmp))
                  (list pt))))))

;;;=========================================
;;; METHODS CALLED FROM OUTSIDE
;;;=========================================
;TODO SOlution Miracle de Dimitri qui marche pas (parce que t'es nul Jérém)
;(defmethod (setf interpol-time) (new-val (self BPC))
;  (setf (slot-value self 'interpol-time) new-val)
;  (setf (time-window self) new-val))

(defmethod arguments-for-action ((fun (eql 'send-xy-as-osc)))
  '((:string address "/point/xy")
    (:string host "localhost")
    (:int port 3000)))

(defun send-xy-as-osc (point &optional (address "/point/xy") (host "localhost") (port 3000))
  (osc-send (list address (om-point-x point) (om-point-y point)) host port))

(defmethod get-def-action-list ((object BPC))
  '(print send-xy-as-osc bpc-midi-controller))