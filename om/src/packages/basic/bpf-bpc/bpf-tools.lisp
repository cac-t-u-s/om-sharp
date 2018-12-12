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

;;;===========================
;;; POINTS ACCESS
;;;===========================

(defmethod* point-pairs ((self bpf)) 
  :initvals '(nil)
  :indoc '("a BPC")
  :doc "Retruns the list of points in <self> as a list ((x1 y1) (x2 y2) ...)"
  (mat-trans (list (x-points self) (y-points self))))

(defmethod* point-pairs ((self bpc)) 
  :initvals '(nil)
  :indoc '("a BPC")
  :doc "Retruns the list of points in <self> as a list ((x1 y1 t1) (x2 y2 t2) ...)"
  (mat-trans (list (x-points self) (y-points self))))

;;; compat 
(defmethod* paires ((self bpf)) :icon 128 (point-pairs self))

;;; called (for instance) in save-as-text
(defmethod write-data ((self bpf) out)
  (loop for x in (x-points self) 
        for y in (y-points self) do
        (format out "~D ~D~%" x y)))


;;;===========================
;;; COLOR
;;;===========================
(defmethod* set-color ((self bpf) color &optional (new? t)) 
  :initvals '(nil nil t)
  :indoc '("a BPF or BPC" " color" "create a new object")
  :icon 241 
  :doc "Sets the color of <self> with <color>.

If <new?> the function will retrun a new colored object; otherwise it will change the color of <self>.

If <color> is :random, will choose a random color. It can also be a color symbol designator (:red, :blue, etc..) or a color created from om-make-color.
"

(let ((bpf (if new? (clone self) self)))
  (setf (color bpf) (cond
                     ((equal color :random) (om-random-color))
                     ((symbolp color) (om-def-color color))
                     (t color))) 
  bpf))

;;;=========================================== 
;;; BASIC FUNCTIONS ADAPTED TO BPFS
;;;=========================================== 

;;; TRANSFERs
(defmethod* y-transfer ((self bpf) (y0 number) &optional (dec nil))
  (y-transfer (point-pairs self) y0 dec))

(defmethod* x-transfer ((self bpf) x-val &optional (dec nil))  
   (x-transfer (point-pairs self) x-val dec))


;;; REDUCTION
(defmethod* reduce-points ((self bpf) &optional (approx 0.02))
     (let ((reduced-points (reduce-points (point-pairs self) approx)))
       (make-instance (type-of self) 
                      :x-points (mapcar 'car reduced-points)
                      :y-points (mapcar 'cadr reduced-points)
                      :decimals (decimals self))))

(defmethod* reduce-n-points ((self bpf) n &optional (precision 10) (verbose nil))
     (let ((reduced-points (reduce-n-points (point-pairs self) n precision verbose)))
       (make-instance (type-of self) 
                      :x-points (mapcar 'car reduced-points)
                      :y-points (mapcar 'cadr reduced-points)
                      :decimals (decimals self))))

;;;=========================================== 
;;; OM-SAMPLE FOR BPF
;;;=========================================== 

;;; could be smart to put this directly into arithm-ser
(defun sample-interval (x1 x2 nb-samples)
  (if (= x1 x2) 
      (make-list nb-samples :initial-element x1)
    
    (let* ((inter (/ (- x2 x1) (- nb-samples 1)))
         (series (loop for i from x1 to x2 by inter collect i)))
      (if (> (+ x1 (* inter nb-samples)) x2) 
          ;; this can happend because of floatig-point errors
          ;; (< (length series) nb-samples)  ;; equivalent
          (append series (list x2))
        series))
  ))
      

(defmethod* om-sample ((self bpf) (nbs-sr number) &optional xmin xmax dec)
    :numouts 3
    (if (point-list self)
        (let* ((x0 (or xmin (first (x-points self)))) 
               (x1 (or xmax (car (last (x-points self)))))
               (nn (if (integerp nbs-sr)
                       nbs-sr
                     (+ 1 (floor (/ (- x1 x0) nbs-sr)))
                     ))
               (ylist (interpole (x-points self) (y-points self) x0 x1 nn))
               (xlist (if (integerp nbs-sr)
                          (cond ((> nbs-sr 1)
                                 (sample-interval x0 x1 nbs-sr))
                                ((= nbs-sr 1) 
                                 (list (+ x0 (/ (- x1 x0) 2.0))))
                                (t (om-beep-msg "Number of sample must be > 0 !!!")))
                        (arithm-ser x0 x1 nbs-sr))))
          
          (values (and xlist ylist (make-instance (type-of self) :x-points xlist :y-points ylist 
                                                  :decimals (or dec (decimals self))))
                  xlist
                  (if dec (om-round ylist dec) ylist))
          )
      (values 
       (make-instance (type-of self) :x-points nil :y-points nil :decimals (or dec (decimals self)))
       nil nil)))


(defmethod* om-sample ((self BPC) (nbs-sr number) &optional xmin xmax dec)
 :numouts 3
 (let* ((pts (point-pairs self))
        (seg-len (loop for i from 0 to (- (length pts) 2) collect
                       (pts-distance (car (nth i pts)) (cadr (nth i pts)) (car (nth (1+ i) pts)) (cadr (nth (1+ i) pts)))))
        (total-length (reduce '+ seg-len :initial-value 0))
        (ratios (mapcar #'(lambda (l) (/ l total-length)) seg-len))
        (nsamples (if (integerp nbs-sr) nbs-sr (ceiling total-length nbs-sr)))
        (npts-per-seg (mapcar #'(lambda (r) (round (* r nsamples))) ratios))
        (samples nil) (xylist nil))       
   (if (>= nsamples (length pts))
       (setf samples (cons (car pts)
                           (loop for p1 in pts 
                                 for p2 in (cdr pts) 
                                 for np in npts-per-seg append
                                 (cond ((< np 1) nil)
                                       ((< np 2) (list p2))
                                       (t (let (x1 x2 y1 y2 vals) 
                                            (if (= (car p1) (car p2)) ;; particular case
                                                (setf x1 (cadr p1) x2 (cadr p2) y1 (car p1) y2 (car p2))
                                              (setf x1 (car p1) x2 (car p2) y1 (cadr p1) y2 (cadr p2)))
                                            (setf vals (multiple-value-list 
                                                        (om-sample (linear-fun x1 y1 x2 y2) np x1 x2)))
                                      
                                        
                                            (mat-trans (if (= (car p1) (car p2)) 
                                                           (list (third vals) (second vals))
                                                         (list (second vals) (third vals))))))))))
     (let ((segpos (dx->x 0 seg-len))
           (samplepos (arithm-ser 0 total-length (/ total-length nsamples) nsamples)))
       (setf samples (loop for sp in samplepos collect
                           (let ((po1 (position sp segpos :test '>= :from-end t))
                                 (po2 (position sp segpos :test '<=))
                                 p1 p2 pt)
                             (if po1 (setq p1 (nth po1 pts)))
                             (if po2 (setq p2 (nth po2 pts)))
                             (if (and p1 (not p2)) (setq pt (copy-list p1)))
                             (if (and p2 (not p1)) (setq pt (copy-list p2)))
                             (if (and p1 2)
                                 (setq pt 
                                       (list (linear-interpol (nth po1 segpos) (nth po2 segpos)
                                                              (car p1) (car p2) sp)
                                             (linear-interpol (nth po1 segpos) (nth po2 segpos)
                                                              (cadr p1) (cadr p2) sp))))
                             pt))))
     
       )
   
   (setq xylist (mat-trans samples))
   (values (make-instance (type-of self) :x-points (car xylist) :y-points (cadr xylist) :decimals (or dec (decimals self)))
           (car xylist) 
           (cadr xylist))     ;;;npts-per-seg
   ))
          
;(defmethod* om-sample ((self bpf-lib) (nbs-sr number) &optional xmin xmax dec)
;   :numouts 3
;   (values-list (mat-trans
;                 (mapcar #'(lambda (bpf) (multiple-value-list (om-sample bpf nbs-sr xmin xmax dec))) (bpf-list self)))))  


;;;=========================================== 
;;; SPLINE CURVE FROM BPF
;;;=========================================== 

(defmethod* om-spline ((self bpf) (resolution integer) (degree integer))
  :icon 234
  :initvals '(nil 100 3)
  :indoc '("a BPF or BPC" "number of points" "interpolation degree")
  :numouts 3
  :doc "Computes a B-Spline curve from the points in the BPF or BPC.

B-Splines are smoothed curves where each point is computed by polynomial interpolation from a set of control points.

Returned values :
 - The result as an object (BPF or BPC) (1st output)
 - The list of x points (2nd output)
 - The list of sample values (3rd output)

<resolution> is the number of points in the resulting curve
<degree> is the degree of the polynomial interpolation. higher degrees give smoother curves

Note that splines are supposed to be computed from BPFs with reltively few control points. "


  (let* ((points (point-pairs self))
         (N (- (length points) 1))
         (knots (SplineKnots N degree))
         (splc (SplineCurve2D points N knots degree resolution))
         (xylist (mat-trans splc)))
    (values (make-instance (class-of self) 
                           :x-points (car xylist) :y-points (cadr xylist)
                           :decimals (decimals self))
            (car xylist)
            (cadr xylist)
            )))

;;;=========================================== 
;;; BPF INTERPOLATIONS
;;;=========================================== 

(defmethod* bpf-interpol ((first bpf) (second bpf) (steps number) &optional (curve 0.0) (decimals nil) (mode 'points))
            :icon 'bpf-interpol   
            :initvals '(nil nil 1 0.0 nil points) 
            :indoc '("a bpf or bpc" "a bpf or bpc" "number of steps" "interpolation curve" "precision" "interpolation mode")
            :outdoc '("list of BPFs" "list of x-points" "list of y-points")
            :numouts 3
            :menuins '((5 (("points to point" points) ("resample curves" sample))))
            :doc "Interpolates between two BPFs or two BPCs (<first> and <second>).

<steps> is the number of interpolated curves wanted, including <first> and <second>.
  1 means one value between <first> and <seconds>.
  2 will return only <first> and <second>.
  3 will return <first> and <second> with one more curve in between.
  etc.

<curve> in an exponential factor for the interpolation curve (0 = linear).

<decimals> is the precision (number of decimals) in the returned curves (default NIL = precision of <first>).

<mode> determines how interpolation is done :
 - 'points is point-by-point interpolation: the curves must have the same number of points, or the biggest one will be truncated.
 - 'sample means that the curves are resampled before interpolation. In case of BPfs, x-points will be added if needed so that the interpolated curves all have the same x points. In case of BPCs, the one with fewer points is resampled, then point-by-point interpolation is applied.

Outputs
 1) list of interpolated BPFs/BPCs
 2) list of x-points
 3) list of y-points
"
            (cond ((equal mode 'points)
                   (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
                         (interp-y (interpolation (y-points first) (y-points second) steps curve)))
                     (values 
                      (loop for x1 in interp-x for y1 in interp-y
                            collect (make-instance (type-of first) :x-points x1 :y-points y1 :decimals (or decimals (decimals first))))
                      interp-x interp-y)))
                  ((equal mode 'sample)
                   (let* ((allxpoints (sort (x-union (copy-list (x-points first)) (copy-list (x-points second))) '<))
                          (ypts-a (x-transfer first allxpoints))
                          (ypts-b (x-transfer second allxpoints))
                          (interp-y (interpolation ypts-a ypts-b steps curve)))
                      (values 
                       (loop for ylist in interp-y
                             collect (make-instance (type-of first) :x-points allxpoints :y-points ylist 
                                                    :decimals (or decimals (decimals first))))
                       (make-list steps :initial-element allxpoints)
                       interp-y)
                      )))
            )


(defmethod* bpf-interpol ((first bpc) (second bpc) (steps number) &optional (curve 0.0) (decimals nil) (mode 'points))
  (cond ((equal mode 'points)
         (let ((interp-x (interpolation (x-points first) (x-points second) steps curve))
               (interp-y (interpolation (y-points first) (y-points second) steps curve)))
           (values 
            (loop for x1 in interp-x for y1 in interp-y
                  collect (make-instance (type-of first) :x-points x1 :y-points y1 :decimals (or decimals (decimals first))))
            interp-x interp-y)
           ))
        ((equal mode 'sample)
         (let ((l1 (length (point-list first)))
               (l2 (length (point-list second))))
           (cond ((> l1 l2) (bpf-interpol first (om-sample second l1) steps curve decimals 'points))
                 ((> l2 l1) (bpf-interpol (om-sample first l2) second steps curve decimals 'points))
                 (t (bpf-interpol first second steps curve decimals 'points)))
           ))
        ))


;;;=========================================== 
;;; BPF TRANSFORMATIONS
;;;=========================================== 

(defmethod* bpf-scale ((self bpf) &key x1 x2 y1 y2)
  :icon 233
  :indoc '("a BPF" "xmin" "xmax" "ymin" "ymax")
  :initvals '(nil 0 100 0 100)
  :doc "Rescales <self> betwenn the supplied X (<x1>,<x2>) and/or Y (<y1>,<y2>) values."
 (let* ((xp (x-points self))
        (yp (y-points self))
        (xlist (if (or x1 x2) (om-scale xp (or x1 (car xp)) (or x2 (last-elem xp))) xp))
        (ylist (if (or y1 y2) (om-scale yp (or y1 (car yp)) (or y2 (last-elem yp))) yp)))
   (om-init-instance
    (make-instance (class-of self) :x-points xlist :y-points ylist 
                   :decimals (decimals self)
                   :action (action self) :color (color self)))))


