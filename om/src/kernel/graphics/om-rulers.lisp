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

;;;============================================
;;; GRADUATED VIEWS
;;;============================================

(in-package :om)

(defclass x-graduated-view ()
  ((x1 :accessor x1 :initarg :x1 :initform 0)
   (x2 :accessor x2 :initarg :x2 :initform 100)
   (x-factor :accessor x-factor :initform 1)
   (x-shift :accessor x-shift :initform 0)))

(defclass y-graduated-view ()
  ((y1 :accessor y1 :initarg :y1 :initform 0)
   (y2 :accessor y2 :initarg :y2 :initform 100)
   (y-factor :accessor y-factor :initform 1)
   (y-shift :accessor y-shift :initform 0)))

(defmethod set-shift-and-factor ((self t)) nil)

;(defmethod x1 ((self x-graduated-view))
;  (or (slot-value self 'x1) 0))
;(defmethod x2 ((self x-graduated-view))
;  (or (slot-value self 'x2) 0))

(defmethod set-shift-and-factor ((self x-graduated-view))
  (let ((range (- (x2 self) (x1 self))))
    (unless (zerop range)
      (let ((factor (/ (w self) range)))
        (setf (x-factor self) factor 
              (x-shift self) (- (* (x1 self) factor)))))
    (call-next-method)))

(defmethod set-shift-and-factor ((self y-graduated-view))
  (let* ((range (- (y1 self) (y2 self)))
         (factor (/ (h self) range)))
    (setf (y-factor self) factor 
          (y-shift self) (- (* (y2 self) factor))))
  (call-next-method))

(defmethod om-view-resized :before ((self x-graduated-view) new-size) (set-shift-and-factor self))
(defmethod om-view-resized :before ((self y-graduated-view) new-size) (set-shift-and-factor self))

(defmethod pix-to-x ((self x-graduated-view) x) (+ (x1 self) (float (/ x (x-factor self)))))
(defmethod dpix-to-dx ((self x-graduated-view) dpix) (float (/ dpix (x-factor self))))
(defmethod x-to-pix ((self x-graduated-view) x) (+ (x-shift self) (float (* (x-factor self) x))))
(defmethod dx-to-dpix ((self x-graduated-view) dx) (float (* (x-factor self) dx)))
(defmethod pix-to-y ((self y-graduated-view) y) (+ (y2 self) (float (/ y (y-factor self)))))
(defmethod dpix-to-dy ((self y-graduated-view) dpix) (float (/ (- dpix) (y-factor self))))
(defmethod y-to-pix ((self y-graduated-view) y) (+ (y-shift self) (float (* (y-factor self) y))))
(defmethod dy-to-dpix ((self y-graduated-view) dy) (float (* (y-factor self) dy)))

;;;============================================
;;; INTERACTIVE CONTROLLER FOR GRADUATED VIEWS
;;;============================================
(defclass ruler-view (om-view)
  ((vmin :accessor vmin :initarg :vmin :initform nil)
   (vmax :accessor vmax :initarg :vmax :initform nil)
   (decimals :accessor decimals :initarg :decimals :initform 0) ;; just for display
   (related-views :accessor related-views :initarg :related-views :initform nil)))

(defclass x-ruler-view (ruler-view x-graduated-view) ())
(defclass y-ruler-view (ruler-view y-graduated-view) ())

(defmethod v1 ((self x-ruler-view)) (x1 self))
(defmethod v1 ((self y-ruler-view)) (y1 self))
(defmethod v2 ((self x-ruler-view)) (x2 self))
(defmethod v2 ((self y-ruler-view)) (y2 self))
(defmethod (setf v1) (val (self x-ruler-view)) (setf (x1 self) val))
(defmethod (setf v1) (val (self y-ruler-view)) (setf (y1 self) val))
(defmethod (setf v2) (val (self x-ruler-view)) (setf (x2 self) val))
(defmethod (setf v2) (val (self y-ruler-view)) (setf (y2 self) val))

(defmethod ruler-value-to-pix ((self x-ruler-view) v) (x-to-pix self v) )
(defmethod ruler-value-to-pix ((self y-ruler-view) v) (y-to-pix self v))
(defmethod ruler-value-at-pos ((self x-ruler-view) pix-pos) (pix-to-x self (om-point-x pix-pos)))
(defmethod ruler-value-at-pos ((self y-ruler-view) pix-pos) (pix-to-y self (om-point-y pix-pos)))

;;; DIFFERENT UTILS FOR X vs. Y
(defmethod ruler-size ((self x-ruler-view)) (w self))
(defmethod ruler-size ((self y-ruler-view)) (h self))
(defmethod ruler-width ((self x-ruler-view)) (h self))
(defmethod ruler-width ((self y-ruler-view)) (w self))
(defmethod draw-line-at ((self x-ruler-view) x size) (om-draw-line x 0 x size))
(defmethod draw-line-at ((self y-ruler-view) y size) (om-draw-line (- (w self) size) y (w self) y))
(defmethod draw-string-at ((self x-ruler-view) x str) (om-draw-string (- x 6) 18 str)) ; (round (h self) 1.5)
(defmethod draw-string-at ((self y-ruler-view) y str) (om-draw-string 4 (- y 2) str))
(defmethod shift-value ((self x-ruler-view) point) (om-point-x point))
(defmethod shift-value ((self y-ruler-view) point) (om-point-y point))
(defmethod zoom-value ((self x-ruler-view) point) (om-point-y point))
(defmethod zoom-value ((self y-ruler-view) point) (om-point-x point))
(defmethod pix-diff-to-value ((self x-ruler-view) diff) diff)
(defmethod pix-diff-to-value ((self y-ruler-view) diff) (- diff))

(defmethod update-view-from-ruler ((self ruler-view) (view om-view))
  (om-invalidate-view view))


(defmethod update-view-from-ruler ((self x-ruler-view) (view x-graduated-view))
  (setf (x1 view) (v1 self) (x2 view) (v2 self))
  (set-shift-and-factor view)
  (call-next-method))

(defmethod update-view-from-ruler ((self y-ruler-view) (view y-graduated-view)) 
  (setf (y1 view) (v1 self) (y2 view) (v2 self))
  (set-shift-and-factor view)
  (call-next-method))

(defmethod update-views-from-ruler ((self ruler-view))
  (loop for v in (related-views self) do (update-view-from-ruler self v)))

(defmethod attach-view-to-ruler ((ruler ruler-view) view)
  (setf (related-views ruler) (append (related-views ruler) (list view)))
  (update-view-from-ruler ruler view))

(defmethod scale-ruler ((ruler ruler-view) val)
  (when (vmin ruler) (setf (vmin ruler) (round (* (vmin ruler) val))))
  (when (vmax ruler) (setf (vmax ruler) (round (* (vmax ruler) val))))
  (setf (v1 ruler) (round (* (v1 ruler) val))
        (v2 ruler) (round (* (v2 ruler) val)))
  (if (= (v1 ruler) (v2 ruler)) (setf (v2 ruler) (1+ (v1 ruler))))
  (set-shift-and-factor ruler)
  (om-with-delayed-redraw
      (om-invalidate-view ruler)
    (update-views-from-ruler ruler)))

(defmethod unit-value-str ((self ruler-view) value &optional (unit-dur 0))
  (declare (ignore unit-dur))
  (if (zerop (decimals self))
     (format nil "~D" value)
    (format nil (format nil "~~,~DF" (decimals self)) (/ value (expt 10 (decimals self))))))

(defmethod set-ruler-range ((self ruler-view) v1 v2)
  (let* ((v11 (min v1 v2))
         (v22 (max v1 v2)))
    (when (>= (- v22 v11) 10)
      (setf (v1 self) (max? (vmin self) v11))
      (setf (v2 self) (min? (vmax self) v22))))
  (set-shift-and-factor self)
  (om-with-delayed-redraw
      (om-invalidate-view self)
    (update-views-from-ruler self)))


(defmethod om-view-cursor ((self x-ruler-view)) (om-get-cursor :h-size))
(defmethod om-view-cursor ((self y-ruler-view)) (om-get-cursor :v-size))

;; GET THE 'DUR' AND SIZE (pixels) of a unit
;; so that both maxsize and maxdur are not reached
(defmethod get-units ((self ruler-view) &optional (maxsize 100)) 
  (let* ((rsize (ruler-size self))
         (dur (max 1 (abs (- (v2 self) (v1 self)))))
         (unit-dur (expt 10 (round (log dur 10))))
         (unit-size (/ (* rsize unit-dur) dur)))
    (when (and (equal maxsize 100) (< rsize 100))
      (setf maxsize 20))
    (loop while (and (> unit-size maxsize)) do  
          (setf unit-dur (/ unit-dur 10)
                unit-size (/ rsize (/ dur unit-dur))))
    (values unit-dur unit-size)))

(defmethod om-draw-contents ((self ruler-view))
  (ruler-draw self))

(defmethod ruler-draw ((self ruler-view))
  (let ((min-unit-size 40))
    (multiple-value-bind (unit-dur unit-size)
        (get-units self)
      ;(when (> unit-size 200) (setf unit-dur (/ unit-dur 10)))
      (om-with-focused-view self
        (om-with-fg-color (om-def-color :black)
          (let ((big-unit (* unit-dur 10)))
            (loop for vi from (* (floor (v1 self) big-unit) big-unit)
                  to (v2 self) by big-unit
                  do 
                  (let ((pixi (ruler-value-to-pix self vi)))
                    (draw-line-at self pixi 6)  ; (round rwidth 2.5)
                    (om-with-font (om-def-font :font1 :size 8)
                                  (draw-string-at self pixi (unit-value-str self vi unit-dur)))
                    ;draw detailed graduation
                    (when (> unit-size 4) ; (>= big-unit 10))
                      (loop for vii from (+ vi unit-dur) 
                            to (- (+ vi big-unit) unit-dur) by unit-dur do
                            (let ((pixii (ruler-value-to-pix self vii)))
                              (draw-line-at self pixii 3)  ;(round rwidth 5)
                              (if (> unit-size min-unit-size)
                                  (om-with-font (om-def-font :font1 :size 8)
                                                (draw-string-at self pixii (unit-value-str self vii unit-dur)))
                                (when (eql (mod vii (/ big-unit 4)) 0)
                                  (om-with-font (om-def-font :font1 :size 8)
                                                (draw-string-at self pixii (unit-value-str self vii unit-dur))))))))))))))))

(defmethod draw-grid-line-from-ruler ((self om-view) (ruler x-ruler-view) x) (draw-v-grid-line self x))
(defmethod draw-grid-line-from-ruler ((self om-view) (ruler y-ruler-view) y) (draw-h-grid-line self y))

;;; no draw without ruler
(defmethod draw-grid-from-ruler ((self om-view) (ruler null)) nil)

(defmethod draw-grid-from-ruler ((self om-view) (ruler ruler-view))
  (let ((unit-dur (get-units ruler)))
    (loop for line from (* (ceiling (v1 ruler) unit-dur) unit-dur)
          to (* (floor (v2 ruler) unit-dur) unit-dur)
          by unit-dur do
          (let ((v (ruler-value-to-pix ruler line)))
            (draw-grid-line-from-ruler self ruler v)
            ))
    ))


(defmethod om-view-click-handler ((self ruler-view) pos)
  (let ((curr-pos pos)
        (vmin (or (vmin self) nil)) ; -1000000
        (vmax (or (vmax self) nil)) ; 1000000
        (r-size (ruler-size self)))
    (om-init-temp-graphics-motion 
     self pos NIL
     :motion #'(lambda (view position)
                 (let* ((curr-v (ruler-value-at-pos self position))
                        (dur (- (v2 self) (v1 self)))
                        (zoom (pix-diff-to-value self (- (zoom-value self position) (zoom-value self curr-pos))))
                        (shift (pix-diff-to-value self (- (shift-value self position) (shift-value self curr-pos)))))

                   (if (> (abs zoom) (abs shift))
                       ;;; ZOOM
                       (let* ((newdur (+ dur (* zoom 0.01 dur)))
                              (v1 (max? vmin (- curr-v (/ (* (- curr-v (v1 self)) newdur) dur))))     
                              (v2 (min? vmax (+ v1 newdur))))
                         (set-ruler-range self v1 v2))
                     ;;; TRANSLATE
                     (let ((dt (* (/ shift r-size) dur)))
                       (unless (or (and (plusp shift) vmin (= vmin (v1 self))) 
                                   (and (minusp shift) vmax (= vmax (v2 self))))
                         (set-ruler-range self 
                                          (max? vmin (- (v1 self) dt))
                                          (min? vmax (- (v2 self) dt))))))
                   (setf curr-pos position)))
     :release #'(lambda (view position) (update-views-from-ruler self)))))

;;; !! reinit ranges apply on the editor attached to the first related-view
;;; to define a more specific behaviour, better sub-class the ruler
(defmethod reinit-x-ranges-from-ruler ((editor t) ruler) nil)
(defmethod reinit-y-ranges-from-ruler ((editor t) ruler) nil)

(defmethod om-view-doubleclick-handler ((self x-ruler-view) pos)
  (let ((ed (and (related-views self) (editor (car (related-views self))))))
    (when ed (reinit-x-ranges-from-ruler ed self))))

(defmethod om-view-doubleclick-handler ((self y-ruler-view) pos)
  (let ((ed (and (related-views self) (editor (car (related-views self))))))
    (when ed (reinit-y-ranges-from-ruler ed self))))

