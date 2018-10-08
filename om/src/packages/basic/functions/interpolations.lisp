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
;=========================================================================
; Authors: G. Assayag, C. Agon, J. Bresson (adapted from OM6)
;=========================================================================

;;;=============================================================
;;; FUNCTIONS FOR NUMERICAL INTERPOLATIONS, RESAMPLING, ETC...
;;;=============================================================

(in-package :om)


;;;==================================
;;; ditance between points

(defun pts-distance (x1 y1 x2 y2)
  (sqrt (+ (expt (- y2 y1) 2) (expt (- x2 x1) 2))))

(defmethod om-points-distance ((p1 list) (p2 list))
  (sqrt (apply '+ (mapcar #'(lambda (coord) (expt (- (cadr coord) (car coord)) 2)) 
                          (mat-trans (list p1 p2))))))

(defmethod om-points-distance ((p1 ompoint) (p2 ompoint))
  (sqrt (+ (om-square (- (om-point-x p1) (om-point-x p2))) (om-square (- (om-point-y p1) (om-point-y p2))))))

;;;==========================
;;; FUNCTION GENERATOR

(defun linear (x0 y0 x1 y1) 
  (if (= x0 x1) ;;; cas particulier.. a refaire, pour l'instant y = 1 is x = x0 et 0 sinon..
      (let* ((xx x0))
        (eval `(function
                (lambda (x) (if (= x ,xx) 1 0)))))
  (let* ((a (/ (- y1 y0) (- x1 x0)))
         (b (- y1 (* x1 a))))
    (eval `(function
            (lambda (x) (+ ,b (* x ,a))))))))

(defmethod* linear-fun ((x0 number) (y0 number) (x1 number) (y1 number)) 
  :initvals '(0 0 1 1)
  :indoc '("x0" "y0" "x1" "y1")
  :icon 236
  :doc "Constructs a linear function passing through the points (x0 y0) and (x1 y1).
The resulting function can be connected for example to SAMPLEFUN."
  (linear x0 y0 x1 y1))


;;;==========================
;;; INTERPOLATION

(defun x-around (x paires)
  "trouve les paires en dessous et au dessus de x"
  (let ((plus-grand (find x paires :test #'(lambda (x r) (<= x (first r))))))
    (if plus-grand
        (let* ((rang (if (< (1- (first (rang-p paires plus-grand 'equalp))) 0) 
                        0 
                      (1- (first (rang-p paires plus-grand 'equalp)))))
              (plus-petit (nth rang paires)))
          (list 
           (if (< rang 0) plus-grand plus-petit)
           plus-grand))
      (let ((max (car (last paires))))
        (list max max)))))

;(x-around 70 '((0 0) (41 3) (50 6) (69 5) (100 8)))
 
; changé des > / < pour >= / <= ..
(defun y-around (y paires)
  "trouve les paires en dessous et au dessus de y"
  (let ((lst '()))
    (loop 
      for i in paires
      for r in (rest paires)
      do (if (or (and (<= (second i) y) (>= (second r) y)) 
                 (and (>= (second i) y) (<= (second r) y)))
           (push (list i r) lst)))
   (reverse lst)))
;(y-around 5.5 '((0 0) (41 3) (50 6) (69 5) (100 8)))


(defun linear-interpol (x1 x2 y1 y2 x)
  "
          ^
          |
          |
        y2|..................*
          |                  .
        y0|............X     .
          |            .     . 
        y1|......*     .     . 
          |      .     .     . 
          |      .     .     . 
          |      .     .     .    
          |______._____._____.______>
                 x1    x     x2

"
  (if (= x1 x2) y1
    (+ y1
       (* (- y2 y1)
          (/ (- x x1)
             (- x2 x1))))))


(defun interpolate (list-x list-y step)
  (loop for pointer from (first list-x) by step
        with x1
        with y1 
        with x2 = (pop list-x)
        with y2 = (pop list-y)
        if (>= pointer x2)  do (setf x1 x2 y1 y2 x2 (pop list-x) y2 (pop list-y))
        if (null x2) collect y1 and do (loop-finish)
        collect (linear-interpol x1 x2 y1 y2 pointer)))

(defun interpole (list-x list-y x-min x-max nbsamples)
  (if (= 1 nbsamples) 
      (list (x-transfer (mat-trans (list list-x list-y)) (+ x-min (/ (- x-max x-min) 2))))
    (let* ((step (/ (- x-max x-min) (1- (float nbsamples))))
           (last-point (car (last list-y)))
           (series 
            (loop with x = (pop list-x) and xx = (pop list-x) 
                  and y = (pop list-y) and yy = (pop list-y)
                  with x-index = x-min
                  with s-index = 0
                  while (and xx (< s-index nbsamples))
                  if (and (>= x-index x) (<= x-index xx))
                  collect (linear-interpol x xx y yy x-index)
                  and do (setf x-index (+ x-min (* (incf s-index) step)))
                  else do (setf x xx xx (pop list-x) y yy yy (pop list-y)))))
       
      (if (> (+ x-min (* step nbsamples)) x-max) 
          ;; this can happend because of floatig-point errors
          ;; (< (length series) nbsamples)  ;; equivalent
          (append series (list last-point))
        series)
      )))



;;==================================
;; x/y transfer tools

(defmethod* y-transfer ((self list) (y0 number) &optional (dec nil))
  :initvals (list nil 10  0)
  :indoc '("list of points, BPF or BPC"  "Y value"  "number of decimals")
  :icon 233
  :doc "Returns a list of interpolated X values corresponding to a list of points ((x1 y1) (x2 y2) ...), or a BPF/BPC (<self>) and a Y position <y0>. 

Optional <dec> is the number of decimals in the result."

  (let* ((paires self)
         (y-around (y-around y0 paires))
         (xpts (loop for i in y-around
                     collect (om-round (linear-interpol (second (first i))
                                                        (second (second i))
                                                        (first (first i))
                                                        (first (second i))
                                                        y0)))))
    (if dec (om-round xpts dec) xpts)))


(defmethod* x-transfer ((self list) (x-val number) &optional (dec nil))    
  :icon 233
  :indoc '("a list or BPF" "X value" "number of decimals")
  :initvals '(((0 0) (100 100)) 50 nil)
  :doc "Returns the interpolated Y value(s) in a BPF or a list ((x1 y1) (x2 y2) ...) corresponding to an X value or a list of X values (<x-val>).

Optional <dec> is the number of decimals in the result."
  (let* ((paires self) 
         (bornes (x-around x-val paires))
         (ypts (linear-interpol (first (first bornes)) 
                                (first (second bornes))
                                (second (first bornes)) 
                                (second (second bornes)) 
                                x-val)))
    (if dec (om-round ypts dec) ypts)))

(defmethod* x-transfer ((self list) (x-val list) &optional (dec nil))    
  (mapcar #'(lambda (x) (x-transfer self x dec)) x-val))



;;;==========================
;;; GENERAL SAMPLE FUNCTION

(defmethod* om-sample ((self t) (nbs-sr number) &optional xmin xmax dec)
      :initvals '(nil 1 nil nil nil)
      :indoc '("object to resample" "number of samples (int) or sample rate (float)" "" "" "decimals")
      :icon 'bpf-sample
      :numouts 3
      :outdoc '("sampled object" "x-points" "y-points")
      :doc "Resamples a function, a list, a BPF or a BPC object.

Returns :
 - The result as an object (BPF or BPC) (1st output)
 - The list of x points (2nd output)
 - The list of sample values (3rd output)

If <nbs-sr> is an integer (e.g. 100) it is interpreted as the number of samples to be returned
If <nbs-sr> is an float (e.g. 0.5, 1.0...) it is interpreted as the sample rate (or step between two samples) of the function to return

<xmin> and <xmax> allow to specify the x-range to resample.
<dec> (decimals) is the precision of the result
"   
      nil)

(defmethod* om-sample ((self function) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (let* ((x0 (if xmin (float xmin) 0.0))
          (x1 (if xmax (float xmax) 1.0))
          (xlist (if (integerp nbs-sr)
                     (arithm-ser x0 x1 (float (/ (- x1 x0) (max 1 nbs-sr))) nbs-sr)
                   (arithm-ser x0 x1 nbs-sr)))
          (ylist (mapcar self xlist)))
     (values (make-instance 'bpf :x-points xlist :y-points ylist :decimals (or dec 4))
             xlist
             (if dec (om-round ylist dec) ylist)
             )
     ))


(defmethod* om-sample ((self symbol) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (when (fboundp self)
     (om-sample (symbol-function self) nbs-sr xmin xmax dec)))


(defmethod* om-sample ((self list) (nbs-sr number) &optional xmin xmax dec)
   :numouts 3
   (cond ((bpf-p (car self))
          (values-list (mat-trans 
                        (mapcar #'(lambda (bpf) (multiple-value-list (om-sample bpf nbs-sr xmin xmax dec))) self))))
         ((numberp (car self))
          (let* ((x0 (or xmin 0))
                 (x1 (or xmax (1- (length self))))
                 (lst (subseq self x0 (1+ x1)))
                 (xpts (arithm-ser 0 (1- (length lst)) 1))
                 (ylist (if (integerp nbs-sr) 
                            (interpole xpts lst x0 x1 nbs-sr)
                          (interpolate xpts lst nbs-sr)))
                 (xlist (arithm-ser 0 (1- (length ylist)) 1)))
            (values (make-instance 'bpf :x-points xlist :y-points ylist
                                   :decimals (or dec 4))
                    xlist
                    (if dec (om-round ylist dec) ylist) 
                    )))
         (t nil)))
   

;;;====================================
;;; Interpole avec profil
;;; (todo: merge with "interpolation")
(defmethod* interpole-points ((v1 t) (v2 t) (nbsteps integer) &optional profil)
  :icon 233
  :indoc '("value 1" "value 2" "number or intermediate steps" "interpolation profile")
  :doc "Interpolates <nbsteps> values between <v1> and <v2> following a profile.

<v1> and <v2> can be either numbers or list (supposed to be of the same length)
<profile> is a BPF. If nil, a linear profile is used."
  (unless profil (setf profil (make-instance 'bpf)))
  (let ((weightfun (om-scale (nth 2 (multiple-value-list (om-sample profil (+ 2 nbsteps)))) 0.0 1.0)))
    (loop for i from 0 to (+ 1 nbsteps) collect
          (om+ (om* v2 (nth i weightfun)) (om* v1 (om- 1 (nth i weightfun)))))))


;;;====================================
;;; Function reduction tools
;;; from S. Lemouton's code for Chroma

(defmethod* reduce-points ((points list) &optional (approx 0.02))
            :indoc '("a list of (x y) points or a BPF" "a number between 0.0 and 1.0")
  :initvals '(nil 0.02)
  :icon 910
  :doc "Reduces <points> by removing all points closer than [<approx> * the amplitude range of the function] to the corresponding interpolated values.

  <approx> = 1 means the maximum reduction (all intermediate points are removed)
  <approx> = 0 means reduction without loss (only exact matching points are removed)"		
  (let* ((before (list (car points)))
        (after (cdr points))
        (ymin (cadr (car before))) (ymax (cadr (car before)))
        (amplitude 0))
    (loop for p in after do 
          (if (> (cadr p) ymax) (setf ymax (cadr p))
            (if (< (cadr p) ymin) (setf ymin (cadr p)))))
    (setf amplitude (- ymax ymin))
    (if (= 0. amplitude)
        (setf before (append before (last after)))
      (loop for listrest on after
            while (cdr listrest) 
            do (let* ((x_val (caar listrest))
                      (y_val (cadar listrest))
                      (interpolated-y (linear-interpol (car (car (last before))) (car (cadr listrest)) 
                                                       (cadr (car (last before))) (cadr (cadr listrest)) 
                                                       x_val))
                      (error (/ (abs (- interpolated-y y_val)) amplitude)))
                 (if (> error approx)
                     (setf before (append before (list (car listrest))))))
            ;do (setf after (cdr after))
            finally (setf before (append before (last points)))))
    before))


(defmethod* reduce-n-points (points n &optional (precision 10) (verbose nil))
  :indoc '("a list of (x y) points or a BPF" "a number (int)" "a number (int)")
  :initvals '(nil 20 10)
  :icon 910
  :doc "Reduces <points> to less than <n> points using approximations with the function REDUCE-POINTS.

<precision> sets the maximum number of iterations for searching the closest result possible.
"
  (let ((borneMax t)  ; si npoints borne superieure
        (min_factor 0)
        (max_factor 1)
        (curr_factor 0)
        result)
    (loop for i from 0 to precision
          do (setf result (reduce-points points curr_factor))
          while (not (eq (length result) n))
          do  (if  (> (length result) n)
                (setf min_factor curr_factor)
                (setf max_factor curr_factor))
          (setf curr_factor (/ (+ min_factor max_factor) 2)))
    (if (and borneMax (not (eq (length result) n))) 
        (setf result (reduce-points points max_factor)))
    (when verbose
      (om-print (format nil "reduce ~D -> ~D (approx. ~,2F %)"
                        (length points)
                        (length result)
                        (* 100 curr_factor))
                "REDUCE-N-POINTS"))
    result
    ))