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

(defstruct (3dpoint (:include tpoint)) (z 0))

(defun om-make-3dpoint (x y z &optional time type)
  (make-3Dpoint :x x :y y :z z :time time :internal-time time :type type))

(defmethod om-point-z ((self 3Dpoint)) (3dpoint-z self))
(defmethod om-point-z ((self t)) 0)

(defmethod om-point-p ((self 3dpoint)) t)

(defmethod om-copy ((self 3dpoint))
  (make-3Dpoint :x (3Dpoint-x self) :y (3Dpoint-y self) :z (3Dpoint-z self) :time (3Dpoint-time self) :internal-time (3Dpoint-internal-time self) :type (3Dpoint-type self)))

(defmethod omng-save ((self 3dpoint))  
  `(:3Dpoint ,(om-point-x self) ,(om-point-y self) ,(om-point-z self)))

(defmethod om-load-from-id ((id (eql :3Dpoint)) data)
  (apply 'om-make-3dpoint data))


(defmethod om-point-set ((point 3dpoint) &key x y z time type)
  (if x (setf (3dpoint-x point) x))
  (if y (setf (3dpoint-y point) y))
  (if z (setf (3dpoint-z point) z))
  (if time (progn
             (setf (3dpoint-time point) time)
             (setf (3dpoint-internal-time point) time)))
  (if type (setf (3dpoint-type point) type))
  point)

(defmethod om-point-set-values-from-point ((point 3Dpoint) (target 3Dpoint))
  (setf (3dpoint-x point) (3dpoint-x target)
        (3dpoint-y point) (3dpoint-y target)
        (3dpoint-z point) (3dpoint-z target))
  point)

(defmethod om-points-equal-p ((point1 3dpoint) (point2 3dpoint))
  (and (= (3Dpoint-x point1) (3Dpoint-x point2))
       (= (3Dpoint-y point1) (3Dpoint-y point2))
       (= (3Dpoint-z point1) (3Dpoint-z point2))
       (= (3Dpoint-time point1) (3Dpoint-time point2))))

;;; not necessarily at the same time !
(defmethod om-points-at-same-position ((p1 3dpoint) (p2 3dpoint))
   (and (= (3Dpoint-x p1) (3Dpoint-x p2))
       (= (3Dpoint-y p1) (3Dpoint-y p2))
       (= (3Dpoint-z p1) (3Dpoint-z p2))))

(defmethod om-add-points ((p1 3dpoint) p2)
  (make-3dpoint :x (+ (3Dpoint-x p1) (om-point-x p2) 
                      :y (+ (3Dpoint-y p1) (om-point-y p2)) 
                      :z (+ (3Dpoint-z p1) (om-point-z p2)))))

(defmethod om-add-points (p1 (p2 3dpoint))
  (make-3dpoint :x (+ (om-point-x p1) (om-point-x p2) 
                      :y (+ (om-point-y p1) (om-point-y p2)) 
                      :z (+ (om-point-z p1) (om-point-z p2)))))

(defmethod om-subtract-points ((p1 3dpoint) p2)
  (make-3dpoint :x (- (om-point-x p1) (om-point-x p2) 
                      :y (- (om-point-y p1) (om-point-y p2)) 
                      :z (- (om-point-z p1) (om-point-z p2)))))

(defmethod om-subtract-points (p1 (p2 3dpoint))
  (make-3dpoint :x (- (om-point-x p1) (om-point-x p2) 
                      :y (- (om-point-y p1) (om-point-y p2)) 
                      :z (- (om-point-z p1) (om-point-z p2)))))

(defmethod om-point-* ((point 3dpoint) fact)
  (make-3dpoint :x (* (3Dpoint-x point) fact) 
                :y (* (3Dpoint-y point) fact) 
                :z (* (3Dpoint-z point) fact)))

(defmethod om-points-distance ((p1 3Dpoint) (p2 3Dpoint))
  (sqrt (+ (expt (- (3Dpoint-x p2) (3Dpoint-x p1)) 2)
           (expt (- (3Dpoint-y p2) (3Dpoint-y p1)) 2)
           (expt (- (3Dpoint-z p2) (3Dpoint-z p1)) 2))))

(defmethod om-point-mv ((point 3Dpoint) &key x y z time)
 (if x (setf (3Dpoint-x point) (+ (3Dpoint-x point) x)))
 (if y (setf (3Dpoint-y point) (+ (3Dpoint-y point) y)))
 (if z (setf (3Dpoint-z point) (+ (3Dpoint-z point) z)))
 (if time (progn
             (setf (tpoint-time point) (+ (tpoint-time point) time))
             (setf (tpoint-internal-time point) (tpoint-time point))))
 point)


(defclass* 3DC (BPC)
           ((x-points :initform nil :initarg :x-points :documentation "X coordinates (list)")
            (y-points :initform nil :initarg :y-points :documentation "Y coordinates (list)")
            (z-points :initform nil :initarg :z-points :documentation "Z coordinates (list)"))
           (:icon 500)
           (:documentation 
            "3D CURVE: a 3D function defined by a list of [x,y,z] coordinates.

3DC objects are constructed from the list of X coordinates (<x-points>), the list of Y coordinates (<y-points>) and the list of Z coordinates (<z-points>).
If <x-list>, <y-list> and <z-list> are not of the same length, the last coordinate (for y and z) or the last step (for x) is repeated in the shorter lists.

<decimals> allows to specify the precision of the function (0 = integers, n > 0 = number of decimals)
"
            ))

;;; REDEFS FOR Z

(defmethod z-values-from-points ((self 3DC)) (mapcar #'om-point-z (point-list self)))
(defmethod z-points ((self 3DC)) (z-values-from-points self))

(defmethod (setf z-points) ((z-points t) (self 3DC))
  (set-bpf-points self :z z-points)
  z-points)

(defmethod (setf decimals) ((decimals t) (self 3DC))
  (let ((x (x-values-from-points self))
        (y (y-values-from-points self))
        (z (z-values-from-points self)))
  (setf (slot-value self 'decimals) decimals)
  (check-decimals self)
  (set-bpf-points self :x x :y y :z z)
  (decimals self)))

(defmethod adapt-point ((self 3DC) point)
  (setf (3Dpoint-x point) (funcall (truncate-function (decimals self)) (3Dpoint-x point)) 
        (3Dpoint-y point) (funcall (truncate-function (decimals self)) (3Dpoint-y point))
        (3Dpoint-z point) (funcall (truncate-function (decimals self)) (3Dpoint-z point)))
  point)

(defmethod init-bpf-points ((self 3DC))
  (set-bpf-points self 
                  :x (slot-value self 'x-points)
                  :y (slot-value self 'y-points)
                  :z (slot-value self 'z-points)
                  :time (slot-value self 'times)
                  :time-types (slot-value self 'time-types))
  (time-sequence-update-internal-times self)
  self)


(defmethod set-bpf-points ((self 3DC) &key x y z time time-types)
  (let ((times (or time (time-values-from-points self))))
    (setf (point-list self)  (make-3D-points-from-lists (or x (x-values-from-points self)) 
                                                        (or y (y-values-from-points self)) 
                                                        (or z (z-values-from-points self))
                                                        (decimals self)
                                                        'om-make-3dpoint))
    (when times
      (if (listp times)
        (loop for p in (point-list self)
              for time in times do (setf (tpoint-time p) time))
        (loop for p in (point-list self)
              do (setf (tpoint-time p) times))))
     (when time-types
      (loop for p in (point-list self)
            for type in time-types do (om-point-set p :type type)))
    
    (setf (slot-value self 'x-points) NIL)
    (setf (slot-value self 'y-points) NIL)
    (setf (slot-value self 'z-points) NIL)
    (setf (slot-value self 'times) NIL)
    (setf (slot-value self 'time-types) NIL)))


(defmethod make-3D-points-from-lists ((listx list) (listy list) (listz list) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (when (or listx listy listz)
    (if (and (list-subtypep listx 'number) (list-subtypep listy 'number) (list-subtypep listz 'number))
        (let* ((listx (mapcar (truncate-function decimals) (or listx '(0))))
               (listy (mapcar (truncate-function decimals) (or listy '(0))))
               (listz (mapcar (truncate-function decimals) (or listz '(0))))
               (defx (car (last listx)))
               (defy (car (last listy)))
               (defz (car (last listz))))
          (loop for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) defx)
                for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) defy)
                for zpoin = (if listz (pop listz) 0) then (if listz (pop listz) defz)
                while (or listy listx listz)
                collect (funcall mkpoint xpoin ypoin zpoin) 
                into rep
                finally (return (append rep (list (funcall mkpoint xpoin ypoin zpoin)))))
          )
      (om-beep-msg "BUILD 3DC POINTS: input coordinates are not (all) numbers!")
      )))

;;; 2 LISTS / 1 CONSTANT (3 POSSIBILITIES)
(defmethod make-3D-points-from-lists ((pointx number) (listy list) (listz list) &optional (decimals 0)  (mkpoint 'om-make-3dpoint))
  (if (and (list-subtypep listy 'number) (list-subtypep listz 'number))
      (let* ((pointx (funcall (truncate-function decimals) pointx))
             (listy (mapcar (truncate-function decimals) (or listy '(0))))
             (listz (mapcar (truncate-function decimals) (or listz '(0))))
             (defy (car (last listy)))
             (defz (car (last listz))))
        (loop for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) defy)
              for zpoin = (if listz (pop listz) 0) then (if listz (pop listz) defz)
              while (or listy listz)
              collect (funcall mkpoint pointx ypoin zpoin) 
              into rep
              finally (return (append rep (list (funcall mkpoint pointx ypoin zpoin))))))
    (om-beep-msg "BUILD 3DC POINTS: Y/Z-coordinates are not (all) numbers!")
    ))

(defmethod make-3D-points-from-lists ((listx list) (pointy number) (listz list) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (if (and (list-subtypep listx 'number) (list-subtypep listz 'number))
      (let* ((pointy (funcall (truncate-function decimals) pointy))
             (listx (mapcar (truncate-function decimals) (or listx '(0))))
             (listz (mapcar (truncate-function decimals) (or listz '(0))))
             (defx (car (last listx)))
             (defz (car (last listz))))
        (loop for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) defx)
              for zpoin = (if listz (pop listz) 0) then (if listz (pop listz) defz)
              while (or listx listz)
              collect (funcall mkpoint xpoin pointy zpoin) 
              into rep
              finally (return (append rep (list (funcall mkpoint xpoin pointy zpoin))))))
    (om-beep-msg "BUILD 3DC POINTS: X/Z-coordinates are not (all) numbers!")
    ))

(defmethod make-3D-points-from-lists ((listx list) (listy list) (pointz number) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (if (and (list-subtypep listx 'number) (list-subtypep listy 'number))
      (let* ((pointz (funcall (truncate-function decimals) pointz))
             (listx (mapcar (truncate-function decimals) (or listx '(0))))
             (listy (mapcar (truncate-function decimals) (or listy '(0))))
             (defx (car (last listx)))
             (defy (car (last listy))))
        (loop for xpoin = (if listx (pop listx) 0) then (if listx (pop listx) defx)
              for ypoin = (if listy (pop listy) 0) then (if listy (pop listy) defy)
              while (or listx listy)
              collect (funcall mkpoint xpoin ypoin pointz) 
              into rep
              finally (return (append rep (list (funcall mkpoint xpoin ypoin pointz))))))
    (om-beep-msg "BUILD 3DC POINTS: X/Z-coordinates are not (all) numbers!")
    ))


;;; 2 CONSTANTS / 1 LIST (3 POSSIBILITIES)
(defmethod make-3D-points-from-lists ((listx list) (pointy number) (pointz number) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (if (list-subtypep listx 'number)
      (let* ((tf (truncate-function decimals))
             (pointy (funcall tf pointy))
             (pointz (funcall tf pointz)))
        (loop for xpoin in listx 
              collect (funcall mkpoint (funcall tf xpoin) pointy pointz)))
    (om-beep-msg "BUILD 3DC POINTS: X-coordinates are not (all) numbers!")
    ))

(defmethod make-3D-points-from-lists ((pointx number) (listy list) (pointz number) &optional (decimals 0) (mkpoint 'om-make-3dpoint)) 
  (if (list-subtypep listy 'number)
      (let* ((tf (truncate-function decimals))
             (pointx (funcall tf pointx))
             (pointz (funcall tf pointz)))
        (loop for ypoin in listy 
              collect (funcall mkpoint pointx (funcall tf ypoin) pointz)))
    (om-beep-msg "BUILD 3DC POINTS: Y-coordinates are not (all) numbers!")
    ))

(defmethod make-3D-points-from-lists ((pointx number) (pointy number) (listz list) &optional (decimals 0) (mkpoint 'om-make-3dpoint)) 
  (if (list-subtypep listz 'number)
      (let* ((tf (truncate-function decimals))
             (pointx (funcall tf pointx))
             (pointy (funcall tf pointy)))
        (loop for zpoin in listz 
              collect (funcall mkpoint pointx pointy (funcall tf zpoin))))
    (om-beep-msg "BUILD 3DC POINTS: Z-coordinates are not (all) numbers!")
    ))

;;; 3 CONSTANTS
(defmethod make-3D-points-from-lists ((pointx number) (pointy number) (pointz number) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (list (funcall mkpoint pointx pointy pointz)))

(defmethod make-3D-points-from-lists ((pointx t) (pointy t) (pointz t) &optional (decimals 0) (mkpoint 'om-make-3dpoint))
  (print pointx) (print pointy) (print pointz)
  (om-beep-msg "BUILD 3DC POINTS: Wrong coordinate lists!"))


;;; need to convert the points to 3Dpoints 
(defmethod objfromobjs ((model bpf) (target 3DC))
  (let ((rep (call-next-method)))
    (init-bpf-points rep)
    rep))

;;; need to convert the 3Dpoints to points 
(defmethod objfromobjs ((model 3DC) (target bpf))
  (let ((rep (call-next-method)))
    (init-bpf-points rep)
    rep))

;;;==========================
;;;redefined timed objects methods
;;;==========================

(defmethod make-default-tpoint-at-time ((self 3dc) time)
;this methods create a new point that preserves the motion of the object
  (if (times self)
      (let ((pos (or (position time (point-list self) :key 'tpoint-internal-time :test '<= ) 
                     (length (point-list self))))
            (len (length (time-sequence-get-internal-times self))))
        ;if length is 1 or if the point is before the others or after use the same position than the before or after point
        (if (or (= len 1) (or (= pos 0) (= pos len)))
            (let ((point (nth (min pos (1- len)) (point-list self))))
              (om-make-3dpoint (om-point-x point) (om-point-y point) (3dpoint-z point) time))
          ; if several points, preserve the motion
          (let ((p1 (nth (1- pos) (point-list self)))
                (p2 (nth pos (point-list self))))
            (calc-intermediate-point-at-time p1 p2 time))))
    ;if no points, create a points a pos 
    (om-make-3dpoint 0 0 0 time)))

(defmethod calc-intermediate-point-at-time ((p1 3dpoint) (p2 3dpoint) time)
  (let* ((dur (- (tpoint-internal-time p2) (tpoint-internal-time p1)))
         (factor (/ (- time (tpoint-internal-time p1)) dur))
         (x (+ (* (- (tpoint-x p2) (tpoint-x p1)) factor) (tpoint-x p1)))
         (y (+ (* (- (tpoint-y p2) (tpoint-y p1)) factor) (tpoint-y p1)))
         (z (+ (* (- (3dpoint-z p2) (3dpoint-z p1)) factor) (3dpoint-z p1))))
    (om-make-3Dpoint x y z time)))


;-------------------get the min  max points in x and y axis------------------------------
;;; using reduce 'mix/max is fatser when interpreted but not when compiled
(defmethod nice-bpf-range ((self 3dc))
  (multiple-value-bind (x1 x2 y1 y2 z1 z2 t1 t2)
      (loop for x in (x-values-from-points self) 
            for y in (y-values-from-points self)
            for z in (z-values-from-points self)
            for time in (time-sequence-get-internal-times self)
            minimize x into x1 maximize x into x2
            minimize y into y1 maximize y into y2
            minimize z into z1 maximize z into z2
            minimize time into t1 maximize time into t2
            finally (return (values x1 x2 y1 y2 z1 z2 t1 t2)))
    (append (list x1 x2) (list y1 y2) (list z1 z2) (list t1 t2))
    ; (if (< (- x2 x1) 10) (list (- x1 5) (+ x2 5)) (list x1 x2))
    ; (if (< (- y2 y1) 10) (list (- y1 5) (+ y2 5)) (list y1 y2)))
    ))

;;; to be redefined by objects if they have a specific miniview for the maquette
(defmethod draw-maquette-mini-view ((self 3dc) (box t) x y w h &optional time) 
  (let* ((x-col (om-def-color :red))
         (y-col (om-def-color :green))
         (z-col (om-def-color :blue))
         (ranges (nice-bpf-range self))
         (times (time-sequence-get-internal-times self))
         (x-t-list (mat-trans (list times (x-points self))))
         (x-t-ranges (list 0 (nth 7 ranges) (car ranges) (cadr ranges)))
         (y-t-list (mat-trans (list times (y-points self))))
         (y-t-ranges (list 0 (nth 7 ranges) (caddr ranges) (cadddr ranges)))
         (z-t-list (mat-trans (list times (z-points self))))
         (z-t-ranges (list 0 (nth 7 ranges) (nth 5 ranges) (nth 6 ranges))))
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

            ;draw z = f(t)
    (draw-bpf-points-in-rect z-t-list
                             z-col
                             z-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                             x (+ y 10) w (- h 20)
                             ) 
    t))

;;;============================
;;; SPECIALIZATIONS
;;;============================

(defmethod* point-pairs ((self 3dc)) 
  :initvals '(nil)
  :indoc '("a 3dc")
  :doc "Retruns the list of points in <self> as a list ((x1 y1 z1 t1) (x2 y2 z2 t2) ...)"
  (mat-trans (list (x-points self) (y-points self) (z-points self) (time-sequence-get-internal-times self))))

(defmethod timed-point-pairs ((self 3DC))
  (let ((times (times self)))
    (when (position nil times)
      (setf times (time-sequence-get-internal-times self)))
    (mat-trans (list (x-points self) (y-points self) (z-points self) times))))

;;; called (for instance) in save-as-text
(defmethod write-data ((self 3DC) out)
  (loop for x in (x-points self) 
        for y in (y-points self) 
        for z in (z-points self) 
        do
        (format out "~D ~D ~D~%" x y z)))

(defmethod get-first-point ((self 3DC))
  (car (timed-point-pairs self)))


;;;============================
;;; PLAYER
;;;============================

(defmethod arguments-for-action ((fun (eql 'send-xyz-as-osc)))
  '((:string address "/point/xyz")
    (:string host "localhost")
    (:int port 3000)
    (:int id 1)
    ))

(defun send-xyz-as-osc (point &optional (address "/point/xyz") (host "localhost") (port 3000) (id 1))
  (osc-send (list address id (om-point-x point) (om-point-y point) (om-point-z point)) host port))

(defmethod get-def-action-list ((object 3DC))
  '(print send-xyz-as-osc))