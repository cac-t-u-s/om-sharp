;============================================================================
; om#: visual programming language for computer-assisted music composition
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
; File authors: J. Bresson, M. Schumacher
;============================================================================

(in-package :om)

;;;====================
;;; FUNCTIONS FOR 3DCs
;;;====================

;;; INTERPOLATION
(defmethod 3D-interpolation ((first 3DC) (second 3DC) (steps number)
                             &optional (curve 0.0) (decimals nil) (mode 'points) color)
  (cond ((equal mode 'points)
         (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
               (interp-y (interpolation (y-points first) (y-points second) steps curve))
               (interp-z (interpolation (z-points first) (z-points second) steps curve))
               (interp-times (interpolation (time-sequence-get-internal-times first)
                                            (time-sequence-get-internal-times second)
                                            steps curve)))
           (values
            (loop for x1 in interp-x for y1 in interp-y for z1 in interp-z for t1 in interp-times
                  collect (make-instance (class-of first)
                                         :x-points x1
                                         :y-points y1
                                         :z-points z1
                                         :times t1
                                         :decimals (decimals first)
                                         :color (if (equal color 'random) (om-random-color) color)
                                         ))
            interp-x interp-y interp-z interp-times)
           ))
        ((equal mode 'sample)
         (let ((l1 (length (point-list first)))
               (l2 (length (point-list second))))
           (cond ((> l1 l2) (3D-interpolation first (3D-sample second l1) steps curve decimals 'points color))
                 ((> l2 l1) (3D-interpolation (3d-sample first l2) second steps curve decimals 'points color))
                 (t (3D-interpolation first second steps curve decimals 'points color)))
           ))
        ))

(defmethod* 3D-interpol ((first 3DC) (second 3DC) (steps number) &key (curve 0.0) (decimals nil) (mode 'points) (color nil))
  :icon 'bpf-interpol
  :initvals '(nil nil 1 0.0 nil points nil)
  :indoc '("a 3DC or trajectory" "a 3DC or trajectory" "number of steps" "interpolation curve" "precision" "interpolation mode" "coloring")
  :outdoc '("list of 3DC" "list of x-points" "list of y-points" "list of z-points" "list of times")
  :numouts 5
  :menuins '((5 (("points to point" points) ("resample curves" sample))))
  :doc
  "Interpolates between two 3D curves or trajectories).

<steps> is the number of interpolated curves wanted, including <first> and <second>.
  1 means one value between <first> and <seconds>.
  2 will return only <first> and <second>.
  3 will return <first> and <second> with one more curve in between.
  etc.

<curve> in an exponential factor for the interpolation curve (0 = linear).

<decimals> is the precision (number of decimals) in the returned curves (default NIL = precision of <first>).

<mode> determines how interpolation is done :
 - 'points is point-by-point interpolation: the curves must have the same number of points, or the bigger one will be truncated.
 - 'sample means that the curves are resampled before interpolation. In case of BPfs, x-points will be added if needed so that the interpolated curves all have the same x points. In case of BPCs, the one with fewer points is resampled, then point-by-point interpolation is applied.

<color> sets a color for the resulting curves. Use 'random to assign random colors for each interpolated curve.

Outputs
 1) list of interpolated BPFs/BPCs
 2) list of x-points
 3) list of y-points
 4) list of z-points
 5) list of times
"
  (3D-interpolation first second steps curve decimals mode color))


;;; RESAMPLE
(defmethod* 3D-sample ((self 3Dc) (samples number)  &optional decimals)
  :icon 'bpf-sample
  :initvals '(nil 1000 nil) ;
  :indoc '("object (3Dc)" "number of samples" "decimals")
  :numouts 5
  :doc "samples a 3Dc"
  (let ((x (third (multiple-value-list (om-sample (x-points self) samples))))
        (y (third (multiple-value-list (om-sample (y-points self) samples))))
        (z (third (multiple-value-list (om-sample (z-points self) samples))))
        (times (third (multiple-value-list (om-sample (time-sequence-get-internal-times self) samples)))))
    (values (make-instance (type-of self)
                           :x-points x :y-points y :z-points z
                           :times times :decimals (or decimals (decimals self)))
            x y z times)))

;;;=======================================
;;; REDEFINITION OF BPC FUNCTIONS FOR 3DCs and 3D-trajectories from OMPrisma
;;; by M.Schumacher, http://sourceforge.net/p/omprisma/
;;;=======================================

;;; ROTATION
;;; From OMPrisma traj-rotate
(defmethod* om-rotate ((self 3dc) &key (yaw 0) (pitch 0) (roll 0))
  (let* ((res self))
    (when (and yaw (not (zerop yaw)))
      (setf res
            (multiple-value-bind (a e d) (xyz->aed (x-points res) (y-points res) (z-points res))
              (multiple-value-bind (x y z) (aed->xyz (om+ a yaw) e d)
                (make-instance (type-of self)
                               :x-points x :y-points y :z-points z
                               :times (times self) :decimals (decimals self))
                ))))
    (when (and pitch (not (zerop pitch)))
      (setf res
            (multiple-value-bind (a e d) (xyz->aed (z-points res) (y-points res) (x-points res))
              (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                (make-instance (type-of self)
                               :x-points x :y-points y :z-points z
                               :times (times self) :decimals (decimals self))))))
    (when (and roll (not (zerop roll)))
      (setf res
            (multiple-value-bind (a e d) (xyz->aed (x-points res) (z-points res) (y-points res))
              (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                (make-instance (type-of self)
                               :x-points x :y-points y :z-points z
                               :times (times self) :decimals (decimals self))))))
    (setf (color res) (color self))
    res))

;;; same with a list of (x y z)
(defmethod* om-rotate ((self list) &key (yaw 0) (pitch 0) (roll 0))
  (let* ((xyzlist (mat-trans self)))

    (when (and yaw (not (zerop yaw)))
      (setf xyzlist
            (multiple-value-bind (a e d) (xyz->aed (nth 0 xyzlist) (nth 1 xyzlist) (nth 2 xyzlist))
              (multiple-value-bind (x y z) (aed->xyz (om+ a yaw) e d)
                (list x y z)))))
    (when (and pitch (not (zerop pitch)))
      (setf xyzlist
            (multiple-value-bind (a e d) (xyz->aed (nth 2 xyzlist) (nth 1 xyzlist) (nth 0 xyzlist))
              (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                (list z y x )))))
    (when (and roll (not (zerop roll)))
      (setf xyzlist
            (multiple-value-bind (a e d) (xyz->aed (nth 0 xyzlist) (nth 2 xyzlist) (nth 1 xyzlist))
              (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                (list x z y)))))
    (mat-trans xyzlist)))


;;; TRANSLATION
;;; From OMPrisma traj-translate
(defmethod* om-translate ((self 3dc) &key x y z time)
  (let ((res self)
        (thex x) (they y) (thez z) (thet time))
    (unless (numberp x) (setf thex 0))
    (unless (numberp y) (setf they 0))
    (unless (numberp z) (setf thez 0))
    (unless (numberp time) (setf thet 0))
    (setf res (make-instance
               (type-of self)
               :x-points (om+ (x-points self) thex) :y-points (om+ (y-points self) they) :z-points (om+ (z-points self) thez)
               :times (om+ (time-sequence-get-internal-times self) thet) :decimals (decimals self))
          )
    (setf (color res) (color self))
    res))

(defmethod* om-mirror ((self 3DC) &key x y z times)
  (let ((res (make-instance
              (type-of self)
              :x-points (if x (om* -1 (x-points self)) (x-points self))
              :y-points (if y (om* -1 (y-points self)) (y-points self))
              :z-points (if z (om* -1 (z-points self)) (z-points self))
              :times (if times (om* -1 (time-sequence-get-internal-times self))
                       (time-sequence-get-internal-times self))
              :decimals (decimals self))))

    (setf (color res) (color self))
    res))
