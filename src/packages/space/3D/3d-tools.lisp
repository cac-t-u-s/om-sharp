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
; File author: M. Schumacher
;============================================================================

(in-package :om)


;;;  Euclidean-distances

(defmethod xy-distance ((x number) (y number))
  (sqrt (+ (* x x) (* y y))))
(defmethod xy-distance ((x list) (y list))
  (mapcar 'xy-distance x y))
(defmethod xyz-distance ((x number) (y number) (z number))
  (sqrt (+ (* x x) (* y y) (* z z))))
(defmethod xyz-distance ((x list) (y list) (z list))
  (mapcar 'xyz-distance x y z))


;;; PHASE-UNWRAP
;;; (ported from Matlab)

(defmethod phase-unwrap ((degrees list))
  "Output is the shortest polar distance from the last value (in degrees)."
  (let ((loopres (loop for a in (om- (mapcar #'(lambda (value) (mod value 360))
                                             (om+ (x->dx degrees) 180)) 180)
                       for b in (x->dx degrees)
                       collect
                       (if (and (om= a -180) (om> b 0)) 180 a)
                       )))
    (om+ degrees (dx->x 0 (om- loopres (x->dx degrees))))))


;;;  POL->CAR

(defmethod* pol->car ((r number) (a number))
  :icon :conversion
  :initvals '(0 0)
  :indoc '("radius (magnitude)" "angle (phase)")
  :numouts 2
  :outdoc '("x" "y")
  :doc "Conversion from polar (radius <r>, angle <a>) to cartesian (x, y) coordinates.

<r> and <a> can be numbers or lists."
  (values (* r (cos a)) (* r (sin a))))

(defmethod* pol->car ((r list) (a list))
  (let ((tmplist (mat-trans (mapcar #'(lambda (rr aa) (multiple-value-list (pol->car rr aa))) r a))))
    (values-list tmplist)))

(defmethod* pol->car ((r list) (a number))
  (let ((tmplist (mat-trans (mapcar #'(lambda (rr) (multiple-value-list (pol->car rr a))) r))))
    (values-list tmplist)))

(defmethod* pol->car ((r number) (a list))
  (let ((tmplist (mat-trans (mapcar #'(lambda (aa) (multiple-value-list (pol->car r aa))) a))))
    (values-list tmplist)))


;;;  CAR->POL

(defmethod* car->pol ((x number) (y number))
  :icon :conversion
  :initvals '(0 0)
  :indoc '("x" "y")
  :numouts 2
  :outdoc '("r" "a")
  :doc "Conversion from cartesian (x, y) to polar  (radius, angle) coordinates.

<x> and <y> can be numbers or lists."
  (values (sqrt (+ (* x x) (* y y ))) (atan y x)))

(defmethod* car->pol ((x list) (y list))
  (let ((tmplist (mat-trans (mapcar #'(lambda (xx yy) (multiple-value-list (car->pol xx yy))) x y))))
    (values-list tmplist)))

(defmethod* car->pol ((x list) (y number))
  (let ((tmplist (mat-trans (mapcar #'(lambda (xx) (multiple-value-list (car->pol xx y))) x))))
    (values-list tmplist)))

(defmethod* car->pol ((x number) (y list))
  (let ((tmplist (mat-trans (mapcar #'(lambda (yy) (multiple-value-list (car->pol x yy))) y))))
    (values-list tmplist)))


;;; RAD->DEG

(defmethod* rad->deg ((radians number))
  :icon :conversion
  :indoc '("radians")
  :outdoc '("degrees")
  :doc "Converts radians to degree.

<radians> can be a number or a list."
    ;(/ (* radians 180) pi)
  (* radians 57.29577951308232))

(defmethod* rad->deg ((radians list))
  (mapcar 'rad->deg radians))


;;; DEG->RAD

(defmethod* deg->rad ((degrees number))
  :icon :conversion
  :indoc '("degrees")
  :outdoc '("radians")
  :doc "Donverts degrees to radians.

<degrees> can be a number or a list."
    ;(/ (* degrees pi) 180)
  (* degrees 0.017453292519943295))

(defmethod* deg->rad ((degrees list))
  (mapcar 'deg->rad degrees))


;;; XY->AD

(defmethod* xy->ad ((x number) (y number))
  :icon :conversion
  :initvals '(0 0)
  :indoc '("x" "y")
  :numouts 2
  :outdoc '("azimuth" "distance")
  :doc "Converts 2D cartesian coordinates [x,y] to polar coordinates [azimuth, distance]. Navigational coordinate systems (see www.spatdif.org).

<x> and <y> can be numbers, lists or bpfs.

Returns 2 values (numbers, lists or bpfs) for 'azimuth' and 'distance'."
  (multiple-value-bind (distance azimuth) (car->pol x y)
    (values (- 90 (rad->deg azimuth))
            distance)
    ))

(defmethod* xy->ad ((x list) (y list))
  (let ((result (mat-trans (loop
                            for x1 in x
                            for y1 in y collect
                            (multiple-value-list (xy->ad x1 y1))))))
    (values (phase-unwrap (first result)) (second result))))

(defmethod* xy->ad ((x number) (y list))
  (xy->ad (make-list (length y) :initial-element x) y))

(defmethod* xy->ad ((x list) (y number))
  (xy->ad x (make-list (length x) :initial-element y)))

(defmethod* xy->ad ((x bpf) (y bpf))
  (multiple-value-bind (a d) (xy->ad (y-points x) (y-points y))
    (values (make-instance 'bpf :x-points (x-points x) :y-points a :decimals (decimals x))
            (make-instance 'bpf :x-points (x-points y) :y-points d :decimals (decimals x))
            )))

(defmethod* xy->ad ((x bpf) (y list))
  (multiple-value-bind (a d) (xy->ad (y-points x) y)
    (values (make-instance 'bpf :x-points (x-points x) :y-points a :decimals (decimals x))
            d))
  )

(defmethod* xy->ad ((x bpf) (y number))
  (multiple-value-bind (a d) (xy->ad (y-points x) y)
    (values (make-instance 'bpf :x-points (x-points x) :y-points a :decimals (decimals x))
            d))
  )

(defmethod* xy->ad ((x list) (y bpf))
  (multiple-value-bind (a d) (xy->ad x (y-points y))
    (values a
            (make-instance 'bpf :x-points (x-points y) :y-points d :decimals (decimals y))))
  )

(defmethod* xy->ad ((x number) (y bpf))
  (multiple-value-bind (a d) (xy->ad x (y-points y))
    (values a
            (make-instance 'bpf :x-points (x-points y) :y-points d :decimals (decimals y))
            )))

(defmethod* xy->ad ((self bpc) anything)
  (multiple-value-bind (a d) (xy->ad (x-points self) (y-points self))
    (make-instance 'bpf :x-points a :y-points d :decimals (decimals self))
    ))


;;; AD -> XY

(defmethod* ad->xy ((a number) (d number))
  :icon :conversion
  :initvals '(0 0)
  :indoc '("azimuth" "distance")
  :numouts 2
  :outdoc '("x" "y")
  :doc "Converts 2D polar coordinates [azimuth, distance] to 2D cartesian coordinates [x,y]. Navigational coordinate systems (see www.spatdif.org).

<a> and <d> can be numbers, lists or bpfs.

Returns 2 values (numbers, lists or bpfs) for 'x' and 'y'."

  (multiple-value-bind (x y) (pol->car d (deg->rad (- 90 a)))
    (values x y)))


(defmethod* ad->xy ((a list) (d list))
  (let ((result (mat-trans (loop
                            for a1 in a
                            for d1 in d collect
                            (multiple-value-list (ad->xy a1 d1))))))
    (values-list result)))

(defmethod* ad->xy ((a bpf) (d bpf))
  (multiple-value-bind (x y) (ad->xy (y-points a) (y-points d))
    (values
     (make-instance 'bpf :x-points (x-points a) :y-points x :decimals (decimals a))
     (make-instance 'bpf :x-points (x-points d) :y-points y :decimals (decimals d)))))

(defmethod* ad->xy ((a bpf) (d list))
  (multiple-value-bind (x y) (ad->xy (y-points a) d)
    (values (make-instance 'bpf :x-points (x-points a) :y-points x :decimals (decimals a))
            y))
  )

(defmethod* ad->xy ((a bpf) (d number))
  (multiple-value-bind (x y) (ad->xy (y-points a) d)
    (values (make-instance 'bpf :x-points (x-points a) :y-points x :decimals (decimals a))
            y))
  )

(defmethod* ad->xy ((a list) (d bpf))
  (multiple-value-bind (x y) (ad->xy a (y-points d))
    (values x
            (make-instance 'bpf :x-points (x-points d) :y-points y :decimals (decimals d))))
  )

(defmethod* ad->xy ((a number) (d bpf))
  (multiple-value-bind (x y) (ad->xy a (y-points d))
    (values x
            (make-instance 'bpf :x-points (x-points d) :y-points y :decimals (decimals d))
            ))
  )

(defmethod* ad->xy ((a number) (d list))
  (ad->xy (make-list (length d) :initial-element a) d))

(defmethod* ad->xy ((a list) (d number))
  (ad->xy a (make-list (length a) :initial-element d)))

(defmethod* ad->xy ((self bpc) anything)
  (multiple-value-bind (x y) (ad->xy (x-points self) (y-points self))
    (make-instance 'bpf :x-points x :y-points y :decimals (decimals self)))
  )


;;; XYZ->AED

(defmethod* xyz->aed ((x number) (y number) (z number))
  :icon :conversion
  :initvals '(0 0 0)
  :indoc '("x" "y" "z")
  :numouts 3
  :outdoc '("azimuth" "elevation" "distance")
  :doc "Converts 3D cartesian coordinates [x,y,z] to spherical coordinates [azimuth, elevation, distance]. Navigational coordinate systems (see www.spatdif.org).

<x>, <y>, and <z> can be numbers or lists.

Returns 3 values (or lists) for 'azimuth', 'elevation' and 'distance'."
  (multiple-value-bind (dist ang) (car->pol x y)
    (multiple-value-bind (distz angz) (car->pol dist z)
      (values (- 90 (rad->deg ang))
              (rad->deg angz)
              distz)
      )))


(defmethod* xyz->aed ((x number) (y number) (z list))
  (xyz->aed (make-list (length z) :initial-element x) (make-list (length z) :initial-element y) z))

(defmethod* xyz->aed ((x number) (y list) (z number))
  (xyz->aed (make-list (length y) :initial-element x) y (make-list (length y) :initial-element z)))

(defmethod* xyz->aed ((x number) (y list) (z list))
  (xyz->aed (make-list (length y) :initial-element x) y z))

(defmethod* xyz->aed ((x list) (y list) (z list))
  (let ((result (mat-trans (loop
                            for x1 in x
                            for y1 in y
                            for z1 in z collect
                            (multiple-value-list (xyz->aed x1 y1 z1))))))
    (values (phase-unwrap (first result)) (phase-unwrap (second result)) (third result))))

(defmethod* xyz->aed ((x list) (y list) (z number))
  (xyz->aed x y (make-list (length x) :initial-element z)))

(defmethod* xyz->aed ((x list) (y number) (z list))
  (xyz->aed x (make-list (length x) :initial-element y) z))

(defmethod* xyz->aed ((x list) (y number) (z number))
  (xyz->aed x (make-list (length x) :initial-element y) (make-list (length x) :initial-element z)))


;;; AED -> XYZ

(defmethod* aed->xyz ((a number) (e number) (d number))
  :icon :conversion
  :initvals '(0 0 0)
  :indoc '("azimuth" "elevation" "distance")
  :numouts 3
  :outdoc '("x" "y" "z")
  :doc "Converts 3D spherical coordinates [azimuth, elevation, distance] to cartesian coordinates [x,y,z]. Navigational coordinate systems (see www.spatdif.org).

<a>, <e>, amd <d> can be numbers or lists.

Returns 3 values (or lists) for 'x', 'y' and 'z'."

  (values (* d (sin (deg->rad (- 90 e))) (sin (deg->rad a)))
          (* d (sin (deg->rad (- 90 e))) (cos (deg->rad a)))
          (* d (cos (deg->rad (- 90 e))))))

(defmethod* aed->xyz ((a number) (e number) (d list))
  (aed->xyz (make-list (length d) :initial-element a) (make-list (length d) :initial-element e) d))

(defmethod* aed->xyz ((a number) (e list) (d number))
  (aed->xyz (make-list (length e) :initial-element a) e (make-list (length a) :initial-element d)))

(defmethod* aed->xyz ((a number) (e list) (d list))
  (aed->xyz (make-list (length e) :initial-element a) e d))

(defmethod* aed->xyz ((a list) (e list) (d list))
  (let ((result (mat-trans (loop
                            for a1 in a
                            for e1 in e
                            for d1 in d collect
                            (multiple-value-list (aed->xyz a1 e1 d1))))))
    (values-list result)))

(defmethod* aed->xyz ((a list) (e list) (d number))
  (aed->xyz a e (make-list (length a) :initial-element d)))

(defmethod* aed->xyz ((a list) (e number) (d list))
  (aed->xyz a (make-list (length a) :initial-element e) d))

(defmethod* aed->xyz ((a list) (e number) (d number))
  (aed->xyz a (make-list (length a) :initial-element e) (make-list (length a) :initial-element d)))



;;; UTIL

(defmethod* gen-circles (n r n-points)
  :initvals '(1 10 200)
  :numouts 2
  (ad->xy (arithm-ser 0 (round (* n 360)) (/ (* n 360) n-points) n-points) r))
