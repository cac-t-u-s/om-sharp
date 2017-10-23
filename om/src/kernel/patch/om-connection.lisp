;============================================================================
; o7: visual programming language for computer-aided music composition
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
   (style :accessor style :initarg :style :initform nil) ; :curved)
   (color :accessor color :initarg :color :initform (om-def-color :dark-gray))
   (seleted :accessor selected :initarg :selected :initform nil)
   (graphic-connection :initform nil :accessor graphic-connection)))


(add-preference-section :appearance "Connections" "- Default values for connections with unspecified or disabled attributes")
(add-preference :appearance :connection-color "Color" :color (om-def-color :dark-gray))
(add-preference :appearance :connection-style "Syle" '(:normal :line :curved) :normal)

(defmethod get-properties-list ((self OMConnection))
  '(("Connection properties" ;;; category
               (:color "Color" color-or-nil color) ;;; id text type 
               (:style "Style" (:normal :line :curved nil) style)
               (:reactive "Reactive (r)" :bool reactive))))

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
           (+x (or (car (modif c)) 0))
           (+y (or (cadr (modif c)) 0)))
      (setf (points c)
            (if (> (om-point-y p2) (+ (om-point-y p1) 16))
                (list (omp 0 0) (omp 0 (+ 0.5 +y)) (omp 1.0 (+ 0.5 +y)) (omp 1.0 1.0)) 
              (list (omp 0 0) 
                    (omp 0 8)
                    (omp (+ +x 0.5) 8)
                    (omp (+ +x 0.5) (round (- (om-point-y p2) (om-point-y p1) 8)))
                    (omp 1.0 (round (- (om-point-y p2) (om-point-y p1) 8)))
                    (omp 1.0 1.0))
              )
            ))))


              
(defmethod get-out-connections ((self OMBox))
  (loop for out in (outputs self) append (connections out)))

(defmethod get-in-connections ((self OMBox))
  (loop for inp in (inputs self) append (connections inp)))

(defmethod get-box-connections ((self OMBox))
  (append (get-in-connections self) (get-out-connections self)))

(defmethod get-out-connected-boxes ((self OMBox))
  (remove nil 
          (loop for out in (outputs self) 
                append (loop for c in (connections out) collect (box (to c))))))
         
(defmethod recursive-connection-p ((from OMBox) (to OMBox))
  "Check if there is a cyclic connection"
  (let (rep)
    (loop for cb in (get-out-connected-boxes to)
          while (not rep) do
          (if (equal cb from) 
              (setf rep t)
            (setf rep (recursive-connection-p from cb))))
    rep))

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


;;; called from editor commands
(defmethod move-box ((c OMConnection) dx dy)
  (let* ((x1 (om-point-x (get-position (area (from c)))))
         (x2 (om-point-x (get-position (area (to c)))))
         (y1 (om-point-y (get-position (area (from c)))))
         (y2 (om-point-y (get-position (area (to c)))))
         (dxx (if (= x1 x2) 0 (/ dx (- x1 x2))))
         (dyy (if (= y1 y2) 0 (/ dy (- y1 y2)))))
    (unless (modif c) (setf (modif c) '(0 0)))
    (cond ((= 4 (length (points c)))
           (setf (cadr (modif c)) (+ (cadr (modif c)) dyy)))
          ((= 6 (length (points c)))
           (setf (car (modif c)) (+ (car (modif c)) dxx)))
          (t nil))
    (update-points c)
    (update-graphic-connection c)))


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
      (when ed (update-inspector-for-editor ed)
        ))))

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
    (cond ((equal :curved (style c))
           (mapcar #'(lambda (p) (om-make-point (car p) (cadr p)))
                   (spline (mapcar #'(lambda (omp) (list (om-point-x omp) (om-point-y omp))) gpts)
                           4 ;(max 3 (- (length gpts) 2)) 
                           50)))
          ((equal :line (style c))
           (list (car gpts) (car (last gpts))))
          (t gpts))))

(defmethod update-graphic-connection ((c omconnection))
  (when (graphic-connection c)
    (setf (draw-points (graphic-connection c)) (make-graphic-points c (view (graphic-connection c))))
    (update-connection-display c (view (graphic-connection c)))
    ))

(defmethod om-draw-contents ((self graphic-connection))
  (let ((line-w (if (selected (object self)) 2.5 1.5))
        (reactive (and (reactive (from (object self)))
                       (reactive (to (object self)))))
        (color (if (and (color (object self)) (color-? (color (object self))))
                   (color-color (color (object self)))
                 (get-pref-value :appearance :connection-color))))
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

;;; Graphic-Connections behave differently with the inspector because they need
;;; to stay connected to the editor view
(defmethod get-my-view-for-update ((self graphic-connection)) (view self))

;;; called for instance after an edit from the inspector
(defmethod update-view (view (object omconnection))
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
  (update-inspector-for-box self))


