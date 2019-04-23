;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed; in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;;;=============================
; CONNECTIONS
;;;=============================
(in-package :om)


(defclass OMConnection (OMVPObject)
  ((from :initarg :from :accessor from :initform nil)
   (to :initarg :to :accessor to :initform nil)
   (points :accessor points :initarg :points :initform nil)
   (modif :accessor modif :initarg :modif :initform nil)
   (style :accessor style :initarg :style :initform nil)
   (color :accessor color :initarg :color :initform nil)
   (seleted :accessor selected :initarg :selected :initform nil)
   (graphic-connection :initform nil :accessor graphic-connection)))


(add-preference-section :appearance "Connections" "Default values for connections with unspecified or disabled attributes")
(add-preference :appearance :connection-color "Color" :color (om-def-color :dark-gray))
(add-preference :appearance :connection-style "Style" '(:square :rounded :curved :curved-2 :line) :rounded)

(defmethod get-properties-list ((self OMConnection))
  '(("Appearance" ;;; category
     (:color "Color" :color-or-nil color (:appearance :connection-color)) ;;; id text type dafault
     (:style "Style" (:square :rounded :curved :curved-2 :line :default) style (:appearance :connection-style)))
    ("Execution" ;;; category
     (:reactive "Reactive (r)" :bool reactive))))

(defmethod object-name-in-inspector ((self OMConnection)) "connection")

(defmethod connection-draw-style ((c OMConnection)) 
  (or (style c)
      (get-pref-value :appearance :connection-style)))

(defmethod connection-draw-color ((c OMConnection)) 
  (if (color-? (color c))
      (color-color (color c))
    (get-pref-value :appearance :connection-color)))

;;; called in the properties management / inspector
(defmethod object-accept-transparency ((self OMConnection)) nil)

(defmethod omng-make-new-connection ((from box-output) (to box-input) &optional args)
  (if (recursive-connection-p (box from) (box to))
      (om-beep-msg "cycles not permitted!")
    (apply 'make-instance (append (list 'omconnection :from from :to to) args))))

(defmethod set-graphic-connection ((c OMConnection))
  (when (and (area (from c)) (area (to c))) 
    (update-points c)
    (setf (graphic-connection c)
          (make-instance 'graphic-connection
                         :object c :draw-points (make-graphic-points c nil)))
    ))

(defmethod add-connection-in-view ((self om-view) (c OMConnection)) 
  (set-graphic-connection c)
  (setf (view (graphic-connection c)) self)
  c)

(defmethod omng-connect ((c OMConnection))
  (setf (connections (from c)) (append (connections (from c)) (list c)))
  (setf (connections (to c)) (append (connections (to c)) (list c))))

(defmethod omng-unconnect ((c OMConnection))
  (setf (connections (from c)) (remove c (connections (from c))))
  (setf (connections (to c)) (remove c (connections (to c)))))
  

(defmethod initialize-size ((c OMConnection))
  (setf (modif c) (list 0 0))
  (update-points c)
  (update-graphic-connection c))
                     

;;; called when the boxes are moved etc.
#|
(defmethod update-points ((c OMConnection))
  (when (and (area (from c)) (area (to c))) ;; the two extremities have been created already
    (let* ((p1 (io-position-in-patch (area (from c))))
           (p2 (io-position-in-patch (area (to c))))
           (+x (or (car (modif c)) 0))
           (+y (or (cadr (modif c)) 0)))
      (setf (points c)
            (if (> (om-point-y p2) (om-point-y p1))
                (let ((mid-y (/ (+ (om-point-y p1) (om-point-y p2)) 2)))
                  (list p1
                        (om-make-point (om-point-x p1) (+ mid-y +y))
                        (om-make-point (om-point-x p2) (+ mid-y +y))
                        p2))
              (let ((dy 8)
                    (mid-x (/ (+ (om-point-x p1) (om-point-x p2)) 2)))
                (list p1
                      (om-make-point (om-point-x p1) (+ (om-point-y p1) dy))
                      (om-make-point (+ +x mid-x) (+ (om-point-y p1) dy))
                      (om-make-point (+ +x mid-x) (- (om-point-y p2) dy))
                      (om-make-point (om-point-x p2) (- (om-point-y p2) dy))
                      p2)       
                ))
            ) 
      )))
|#

(defmethod update-points ((c OMConnection))
  ;; the two extremities have been created and ppositioned already
  (when (and (area (from c)) (area (to c))) 
    (let* ((p1 (io-position-in-patch (area (from c))))
           (p2 (io-position-in-patch (area (to c))))
           (y1 (om-point-y p1))
           (y2 (om-point-y p2))
           
           (+x (or (car (modif c)) 0))
           (+y (or (cadr (modif c)) 0))
           
           ;;; decide when to curve the connection => not if the two boxes overlap in x
           (threshold (if (or (>= (box-x (box (from c))) (+ (box-x (box (to c))) (box-w (box (to c)))))
                              (>= (box-x (box (to c))) (+ (box-x (box (from c))) (box-w (box (from c))))))
                          16 4)))
      
      (setf (points c)
            (if (> y2 (+ y1 threshold))
                (list (omp 0 0) (omp 0 (+ 0.5 +y)) (omp 1.0 (+ 0.5 +y)) (omp 1.0 1.0)) 
              (list (omp 0 0) 
                    (omp 0 8)
                    (omp (+ +x 0.5) 8)
                    (omp (+ +x 0.5) (round (- y2 y1 8)))
                    (omp 1.0 (round (- y2 y1 8)))
                    (omp 1.0 1.0))
              ))
      )))


              
(defmethod get-out-connections ((self OMBox))
  (loop for out in (outputs self) append (connections out)))

(defmethod get-in-connections ((self OMBox))
  (loop for inp in (inputs self) append (connections inp)))

(defmethod get-box-connections ((self OMBox))
  (append (get-in-connections self) (get-out-connections self)))

(defmethod get-out-connected-boxes ((self OMBox))
  (remove-duplicates 
   (remove nil 
           (loop for out in (outputs self) 
                 append (loop for c in (connections out) collect (box (to c)))))))
  
(defmethod get-in-connected-boxes ((self OMBox))
  (remove-duplicates 
   (remove nil 
           (loop for in in (inputs self) 
                 append (loop for c in (connections in) collect (box (from c)))))))
  
(defmethod recursive-connection-p ((from OMBox) (to OMBox))
  "Check if there is a cyclic connection"
  (let (rep)
    (loop for cb in (get-out-connected-boxes to)
          while (not rep) do
          (if (equal cb from) 
              (setf rep t)
            (setf rep (recursive-connection-p from cb))))
    rep))


;;; checks if box2 is up-connected to box1
(defmethod is-connected-up-to ((box1 OMBox) (box2 OMBox))
  (let ((up-boxes (get-in-connected-boxes box1)))
    (or (find box2 up-boxes)
        (let ((rep nil))
          (loop for cb in up-boxes 
                while (not rep)
                do (setf rep (is-connected-up-to cb box2)))
          rep))))

(defmethod boxes-connected-p ((box1 OMBox) (box2 OMBox))
  (or (is-connected-up-to box1 box2)
      (is-connected-up-to box2 box1)))
      

(defmethod save-connections-from-boxes (box-list)
  (loop for box in box-list 
        for b = 0 then (+ b 1) append
        (loop for out-c in (get-out-connections box)
              for c = 0 then (+ c 1)
              when (find (box (to out-c)) box-list)
              collect (list :connection
                            (list :from (list :box b 
                                              :out (position (from out-c) (outputs box)))) ;; (box out) 
                            (list :to (list :box (position (box (to out-c)) box-list) 
                                            :in (position (to out-c) (inputs (box (to out-c)))))) ;;; (box in)
                            ;;; nil ;;; points
                            (list :attributes (list :color (omng-save (color out-c))
                                                    :style (style out-c)
                                                    :modif (modif out-c)))
                            ))))
                         
(defmethod restore-connections-to-boxes (connections box-list)
  (remove 
   nil
   (loop for c in connections collect
         (let* ((connection-info (cdr c))
                (b1 (find-value-in-kv-list connection-info :from)) ;;; actually a list (box-num out-num)
                (b2 (find-value-in-kv-list connection-info :to)) ;;; actually a list (box-num in-num)
                (out (when (nth (getf b1 :box) box-list) (nth (getf b1 :out) (outputs (nth (getf b1 :box) box-list)))))
                (in (when (nth (getf b2 :box) box-list) (nth (getf b2 :in) (inputs (nth (getf b2 :box) box-list)))))
                (attributes (find-value-in-kv-list connection-info :attributes)))
           (if (and out in)
               (omng-make-new-connection out
                                         in
                                         (list :color (omng-load (getf attributes :color))
                                               :style (getf attributes :style)
                                               :modif (getf attributes :modif)))
             (progn (om-print "Connection could not be restored !" "WARNING") NIL)
             )
           ))))


(defmethod adopt-connection ((self box-input) (connection omconnection))
  (setf (to connection) self)
  connection)

(defmethod adopt-connection ((self box-output) (connection omconnection))
  (setf (from connection) self)
  connection)


;;; dx and dy are ratios
(defmethod modif-connection ((c OMConnection) dx dy)

  (unless (modif c) (setf (modif c) (list 0 0)))

  (cond ((= 4 (length (points c))) ;; 'vertical' connection : consider only dy
         (setf (cadr (modif c)) (max -0.45 (min 0.45 (+ (cadr (modif c)) dy)))))
        ((= 6 (length (points c))) ;; 'horizontal' connection : consider only dx
         (setf (car (modif c)) (max -0.45 (min 0.45 (+ (car (modif c)) dx)))))
        (t nil))
  
  (update-points c)
  (update-graphic-connection c))

;;; mouse drag: dx and dy are pixels
(defmethod drag-connection ((c OMConnection) dx dy) 
  (let* ((from-p (om-add-points (get-position (area (from c)))
                                (om-view-position (frame (area (from c))))))
         (to-p (om-add-points (get-position (area (to c)))
                              (om-view-position (frame (area (to c))))))
         (x1 (om-point-x from-p))
         (x2 (om-point-x to-p))
         (y1 (om-point-y from-p))
         (y2 (om-point-y to-p))
         (dxx (if (= x1 x2) 0 (/ dx (- x2 x1))))
         (dyy (if (= y1 y2) 0 (/ dy (- y2 y1)))))
 
    (when (or dxx dyy) (modif-connection c dxx dyy))
    ))

;;; called from editor keys (dx / dx = 1 or 10)
(defmethod move-box ((c OMConnection) dx dy)
  (drag-connection c dx dy))



;;;=============================
;;; GRAPHIC CONNECTION
;;;=============================

;;; graphic connections are "fake" frames
(defclass graphic-connection (OMFrame) 
  ((view :accessor view :initarg :view :initform nil) ;;; not used / never accessed ? (cf. inspector)
   (draw-points :accessor draw-points :initarg :draw-points :initform nil)))

(defmethod select-box ((self OMConnection) selected)
  (setf (selected self) selected)
  (when (graphic-connection self) 
    (om-invalidate-view (graphic-connection self))
    (let ((ed (editor (om-view-window (graphic-connection self)))))
      (when ed (update-inspector-for-editor ed))
      )))

(defmethod get-draw-points ((self OMConnection))
  (and (graphic-connection self) 
       (draw-points (graphic-connection self))))

;;; called when graphics are recalculated
(defmethod make-graphic-points ((c omconnection) view)
  (let* ((p1 (io-position-in-patch (area (from c))))
         (p2 (io-position-in-patch (area (to c))))
         (diff-points (om-subtract-points p2 p1))
         (gpts (mapcar #'(lambda (p) 
                           (om-add-points p1
                                          (om-make-point 
                                           (if (integerp (om-point-x p)) (om-point-x p) (* (om-point-x p) (om-point-x diff-points)))
                                           (if (integerp (om-point-y p)) (om-point-y p) (* (om-point-y p) (om-point-y diff-points)))
                                           ))
                           ) (points c))))
    (let ((style (connection-draw-style c)))
      (case style 
        (:rounded (get-rounded-pts gpts 4))
        (:curved (get-spline-pts gpts 40))
        (:curved-2 (get-ramped-sine-pts gpts 40))
        (:line (list (car gpts) (car (last gpts))))
        (otherwise gpts))
      
      )))
  


;;; point generation for rounden connections
(defun get-rounded-pts (pts corner)
  (append 
   
   (list (car pts))
   
   (loop for p on pts 
         while (caddr p)
         append
         (let ((x1 (om-point-x (car p)))
               (y1 (om-point-y (car p)))
               (x2 (om-point-x (cadr p)))
               (y2 (om-point-y (cadr p)))
               (x3 (om-point-x (caddr p)))
               (y3 (om-point-y (caddr p)))
               p1x p1y p2x p2y)
                    
           (if (= x1 x2) 
               
               ;; first segment is vertical
               (let ((direction (- x3 x2))
                     (cc (min corner (/ (abs (- y1 y2)) 2) (/ (abs (- x3 x2)) 2))))
                 
                 (cond
                  ((plusp direction) ;; second segment turns right
                   (setf p1x x2 
                         p1y (+ y2 (if (> y1 y2) cc (- cc))) 
                         p2x (+ x2 cc) 
                         p2y y2))
                  ((minusp direction) ;; second segment turns left
                   (setf p1x x2 
                         p1y (+ y2 (if (> y1 y2) cc (- cc)))
                         p2x (+ x2 (- cc))
                         p2y y2))
                  )
                 (when (and p1x p1y p2x p2y)
                   (list 
                    (omp p1x p1y)
                    (omp (+ p1x (* 0.25 (- p2x p1x))) (+ p1y (* 0.5 (- p2y p1y))))
                    (omp (+ p1x (* 0.5 (- p2x p1x))) (+ p1y (* 0.75 (- p2y p1y))))
                    (omp p2x p2y))
                   ))
             
             ;;; first segment is horizontal
             (let ((direction (- y3 y2))
                   (cc (min corner (/ (abs (- x1 x2)) 2) (/ (abs (- y3 y2)) 2))))
               (cond ((plusp direction) ;; second segment goes down
                      (setf p1x (+ x2 (if (plusp (- x2 x1)) (- cc) cc)) 
                            p1y y2
                            p2x x2 
                            p2y (+ y2 cc)))
                     ((minusp direction) ;; second segment goes down
                      (setf p1x (+ x2 (if (plusp (- x2 x1)) (- cc) cc))
                            p1y y2
                            p2x x2 
                            p2y (+ y2 (- cc))))
                     )
               (when (and p1x p1y p2x p2y)
                 (list 
                  (omp p1x p1y)
                  (omp (+ p1x (* 0.5 (- p2x p1x))) (+ p1y (* 0.25 (- p2y p1y))))
                  (omp (+ p1x (* 0.75 (- p2x p1x))) (+ p1y (* 0.5 (- p2y p1y))))
                  (omp p2x p2y))
                 ))
                     
             ) ;; end if
           ) ;; end let
         ) ;; end loop
   
   ;;; last element to append...
   (last pts))

  )
                


;;; point generation for curved connections
(defun get-spline-pts (pts resolution &optional (order 4))
  (mapcar #'(lambda (p) (om-make-point (car p) (cadr p)))
          (spline (mapcar #'(lambda (omp) (list (om-point-x omp) (om-point-y omp))) pts)
                           4 ; (min 3 (- (length gpts) 1))
                           40)))

;;; another curved connection version by G. Holbrook
(defun get-ramped-sine-pts (pts resolution)
  (let ((x1 (om-point-x (car pts)))
        (y1 (om-point-y (car pts)))
        (x2 (om-point-x (last-elem pts)))
        (y2 (om-point-y (last-elem pts))))
    
    (let* ((width (abs (- x2 x1)))
         ;calculate 'mirrored' y2 (a clipped linear function)
           (anti-y2 (om-max 
                     (+ (* -1/3 y2)
                        (* 4/3 (+ y1 (* 1/2 width))))
                     y2)
                    ))

      (loop for k from 0 to resolution
            for rad = (om-scale k -1.57 1.57 0 resolution)
            for ramp = (* (* (+ (sin (om-scale k -1.57 1.57 0 resolution)) 1) 0.5) ;; 0 to 1 half-sine-curve 
                          (- anti-y2 y2)) ;; positive number
            collect
            (omp (om-scale (sin rad) x1 x2 -1 1)
                 (- (om-scale k y1 anti-y2 0 resolution)
                    ramp
                    ))))))



(defmethod update-graphic-connection ((c omconnection))
  (when (graphic-connection c)
    (setf (draw-points (graphic-connection c)) (make-graphic-points c (view (graphic-connection c))))
    (update-connection-display c (view (graphic-connection c)))
    ))

(defmethod om-draw-contents ((self graphic-connection))
  (let ((line-w (if (selected (object self)) 2.5 1.5))
        (reactive (and (reactive (from (object self)))
                       (reactive (to (object self)))))
        (color (connection-draw-color (object self))))
    (when reactive
      (om-draw (draw-points self) 
               :color (om-make-color-alpha (om-def-color :dark-red) ; (or (color (object self)) (om-def-color :dark-gray))
                                           (if (equal (state self) :disabled) 0.3 1)) 
               :line (1+ line-w)))

    (om-draw (draw-points self) 
             :color (om-make-color-alpha color (if (equal (state self) :disabled) 0.3 1))
             :line line-w)
  ;(om-draw (draw-points self) 
  ;         :color (om-make-color-alpha (om-def-color :light-gray) ; (or (color (object self)) (om-def-color :red))
  ;                                     (if (equal (state self) :disabled) 0.3 1)) 
  ;         :line (if (selected (object self)) 2.5 1.5))
  
  ;(when (and (selected (object self)) (not (equal :spline (style (object self)))))
  ;  (mapcar 
  ;   #'(lambda (p) (om-draw-circle (om-point-x p) (om-point-y p) 3 :fill t))
  ;   (draw-points self)))
  ))

(defmethod x ((self graphic-connection)) 0)
(defmethod y ((self graphic-connection)) 0)


(defmethod graphic-area ((c OMConnection))
  (and (graphic-connection c) (graphic-area (graphic-connection c))))
    
(defmethod graphic-area ((c graphic-connection))
  (list (- (apply 'min (mapcar 'om-point-x (draw-points c))) 10)
        (- (apply 'min (mapcar 'om-point-y (draw-points c))) 10)
        (+ (apply 'max (mapcar 'om-point-x (draw-points c))) 10)
        (+ (apply 'max (mapcar 'om-point-y (draw-points c))) 10)
        ))


(defmethod get-update-frame ((self OMConnection)) (graphic-connection self))

;;; called after an edit from the inspector
(defmethod update-after-prop-edit (view (object omconnection))
  (update-graphic-connection object)
  (om-invalidate-view (graphic-connection object)))

(defmethod om-invalidate-view ((self graphic-connection)) 
  (when (view self)
    (apply 'om-invalidate-area (cons (view self) (graphic-area self)))))

(defmethod om-view-window ((self graphic-connection))
  (and (view self) (om-view-window (view self))))

(defmethod update-connection-display ((c OMConnection) view)
  (when view (apply 'om-invalidate-area (cons view (graphic-area (graphic-connection c))))))

;;;=============================
;;; SELECTION/EDITION
;;;=============================

(defmethod point-in-connection (point (c OMConnection))
  (and (graphic-connection c)
       (point-in-connection point (graphic-connection c))))

(defmethod point-in-connection (point (c graphic-connection))
  (let ((in nil))
    (loop for p on (draw-points c)
          while (and (not in) (cdr p)) do
          (setf in (om-point-in-line-p point (car p) (cadr p) 4)))
    in))
  

;;;=============================
;;;PROPERTIES EDITION (INSPECTOR)
;;;=============================

(defmethod is-reactive ((self OMConnection))
  (and (reactive (from self)) (reactive (to self))))
  
(defmethod set-reactive ((self OMConnection) val) 
  (setf (reactive (from self)) val
        (reactive (to self)) val))

;; REACTIVITY
(defmethod set-reactive-mode ((self OMConnection)) 
    (set-reactive self (not (is-reactive self)))
    (om-invalidate-view (graphic-connection  self)))

;;; from inspector
;;; reactive is not a "real" property
(defmethod valid-property-p ((object OMConnection) (prop-id (eql :reactive))) nil)

(defmethod set-property ((object OMConnection) (prop-id (eql :reactive)) val)
  (set-reactive object val))
      
(defmethod get-property ((object OMConnection) (prop-id (eql :reactive)) &key (warn t))
  (is-reactive object))

;;;=============================
;;; CONNECTION WITH THE INSPECTOR WINDOW
;;;=============================

(defmethod close-internal-element :after ((self omconnection)) 
  (close-inspector-for-box self))

(defmethod set-reactive-mode :after ((self omconnection)) 
  (update-inspector-for-object self))


