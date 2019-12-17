;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; File author: D. Bouche
;============================================================================
; METRIC RULER
;=========================================================================

(in-package :om)

;;;========================
;;; METRIC RULER DISPLAY
;;;========================
(defclass metric-ruler (time-ruler)
  ((point-list :initform nil :initarg :point-list :accessor point-list)
   (tempo :initform 60 :initarg :tempo :accessor tempo)
   (tempo-automation :initform nil :initarg tempo-automation :accessor tempo-automation)
   (previous-span :initform '(0 0) :accessor previous-span)))


(defun update-point-list (ruler)
  (let ((ta (tempo-automation ruler)))
    (setf (point-list ruler) 
          (tempo-automation-get-beat-grid ta (v1 ruler) (v2 ruler)
                                          (tempo-automation-get-display-beat-factor ta (v1 ruler) (v2 ruler)))
          )
    ))

(defmethod update-from-tempo ((self metric-ruler))
  (setf (tempo-automation self) 
        (make-instance 'tempo-automation
                       :x-points '(0 1000)
                       :y-points (list (tempo self) (tempo self))))
  (update-point-list self))

(defmethod initialize-instance ((self metric-ruler) &rest args)
  (let ((inst (call-next-method)))
    (unless (tempo-automation inst)
      (update-from-tempo inst))
    inst))


(defmethod get-timed-objects-for-graduated-view ((self metric-ruler))
  ;returns a list of timed-object to retrieve their markers
  nil)

;TIME MARKERS method to redefine by subclasses
(defmethod select-elements-at-time ((self metric-ruler) marker-time)
  ;selects the elements with same time than the marker-time
  nil)

;TIME MARKERS
(defmethod clear-editor-selection ((self metric-ruler))
  nil)



        
(defun update-span (ruler)
  (let ((span (list (v1 ruler) (v2 ruler))))
    (when (not (equal span (previous-span ruler)))
      (update-point-list ruler)
      (setf (previous-span ruler) span))))

(defmethod set-ruler-range ((self metric-ruler) v1 v2)
  (update-span self)
  (call-next-method))

(defmethod update-view-from-ruler ((self x-ruler-view) (view metric-ruler))
  (update-span view)
  (call-next-method))

(defmethod snap-time-to-grid  ((ruler metric-ruler) time &optional (snap-delta nil))
  ;returns a time value corresponding to the given time aligned on the grid with a delta (in ms) treshold.
  ;default treshold is a tenth of the grid
  (let* ((i (position time (point-list ruler) :key 'car :test '<=))
         (unit-dur (if i
                       (if (< i 1)
                           (caar (point-list ruler))
                         (if (< i (1- (length (point-list ruler))))
                             (- (car (nth i (point-list ruler)))
                                (car (nth (1- i) (point-list ruler))))
                           (- (car (nth (1- i) (point-list ruler)))
                              (car (nth (- i 2) (point-list ruler))))))
                     0))
         (delta (if snap-delta (min snap-delta (/ unit-dur 2)) (/ unit-dur 8)))
         (offset (if i
                     (if (< i 1)
                         time
                       (- time (car (nth (1- i) (point-list ruler)))))
                   0)))
    (if i
        (if (> offset (- unit-dur delta))
            (- (+ time unit-dur) offset)
          (if (< offset delta)
              (- time offset)
            time))
      time)))

(defmethod om-draw-contents ((ruler metric-ruler))
  (let ((min-pix-step 20)
        cur-pix
        (last-pix 0))
    (loop for beat in (point-list ruler)
          do
          (setq cur-pix (ruler-value-to-pix ruler (car beat)))
          (if (>= (- cur-pix last-pix) min-pix-step)
              (progn
                (if (not (listp (cadr beat)))
                    (progn
                      (draw-line-at ruler cur-pix 6)
                      (om-with-font (om-def-font :font1b :size 8)
                                    (draw-string-at ruler cur-pix
                                                    (format nil "~A" (cadr beat)))))
                  (progn
                    (draw-line-at ruler cur-pix 4)
                    (om-with-font (om-def-font :font1 :size 7)
                                  (om-draw-string (- cur-pix 5) 12 (format nil "~A" (caadr beat)))
                                  (om-draw-string cur-pix 14 "/")
                                  (om-draw-string (+ cur-pix 4) 16 (format nil "~A" (cadadr beat)))
                                  )))
                (setq last-pix cur-pix))
            (draw-line-at ruler cur-pix 6))))
  (when (markers-p ruler)
    (loop for marker in (get-all-time-markers ruler)
          do
          (let ((pos (time-to-pixel ruler marker)))
            (om-with-fg-color (om-make-color 0.9 0.7 0 (if (find marker (selected-time-markers ruler)) 1 0.45))
              (om-draw-line pos 0 pos (h ruler))
              (if (bottom-p ruler)
                  (om-draw-polygon (list (omp (- pos 4) 0) 
                                         (omp (- pos 4) (- (h ruler) 5))
                                         (omp pos (h ruler))
                                         (omp (+ pos 4) (- (h ruler) 5))
                                         (omp (+ pos 4) 0) ) 
                                   :fill t)
                (om-draw-polygon (list (omp (- pos 4) (h ruler)) 
                                       (omp (- pos 4) 5)
                                       (omp pos 0)
                                       (omp (+ pos 4) 5)
                                       (omp (+ pos 4) (h ruler)))
                                 :fill t))))))
  ;(let ((pos (x-to-pix ruler (cursor-pos ruler))))
  ;  (om-with-fg-color (om-make-color 1 1 1 0.5)
  ;    (if (bottom-p ruler)
  ;        (om-draw-polygon (list (omp (- pos 5) (- (h ruler) 5))
  ;                               (omp (+ pos 5) (- (h ruler) 5))
  ;                               (omp pos (h ruler))) 
  ;                         :fill t)
  ;      (om-draw-polygon (list (omp (- pos 5)  5)
  ;                             (omp (+ pos 5) 5)
  ;                             (omp pos  0)) 
  ;                       :fill t))))
  )

(defmethod draw-grid-from-ruler ((self om-view) (ruler metric-ruler)) 
  (when (markers-p ruler)
    (loop for marker in (get-all-time-markers ruler)
          do (let ((pos (time-to-pixel ruler marker)))
               (om-with-fg-color (om-make-color  0.9 0.7 0 (if (find marker (selected-time-markers ruler)) 1 0.45))
                 (om-draw-line pos 0 pos (h self))))))
  (om-with-line '(2 2)
    (loop for beat in (point-list ruler)
          do
          (draw-grid-line-from-ruler self ruler (ruler-value-to-pix ruler (car beat))))))


