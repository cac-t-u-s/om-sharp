;============================================================================
; o7: visual programming language for computer-aided music composition
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
; Author: D. Bouche
;============================================================================


;;===========================================================================
;  Automation Editor
;;===========================================================================

(in-package :om)

;;;Editor class
(defmethod get-editor-class ((self automation)) 'automation-editor)

(defclass automation-editor (bpf-editor)
  ()
  (:default-initargs :make-point-function 'om-make-automationpoint))

(defclass automation-panel (bpf-panel om-tt-view) ())

(defmethod get-curve-panel-class ((self automation-editor)) 'automation-panel)

(defmethod insert-point-at-pix ((editor automation-editor) (object automation) position &optional (time nil))
  (let ((res (call-next-method)))
    (refresh-automation-points object)
    res))

(defmethod delete-editor-selection ((self automation-editor))
  (call-next-method)
  (refresh-automation-points (object-value self)))


(defmethod draw-coeff-ruler ((self automation-panel) position size)
  (let* ((h (h self))
         (w (w self))
         (x (om-point-x position))
         (left-side (> x (/ w 2.0)))
         (xr (if left-side (- x 20) (+ x 20)))
         (xc (if left-side
                 (- xr 24)
               (+ xr 10))))
    (om-with-line-size 1
      (om-with-fg-color (om-def-color :dark-gray)
        (om-draw-line xr 0 xr h)
        (loop for y from 0 to 1
              by (float (/ 1 10))
              for y2 = (abs (1- y))
              do
              (om-draw-line (- xr 4) (* y2 h) (+ xr 4) (* y2 h))
              (if (> y 0)
              (om-with-font (om-def-font :font1 :size 8)
                            (om-draw-string xc (+ (* y2 h) 2) 
                                            (format nil "~1$" y)))))))))


(defmethod draw-grid-from-ruler ((self automation-panel) (ruler y-ruler-view))
  (let ((unit-dur (get-units ruler)))
    (loop for line from (* (ceiling (v1 ruler) unit-dur) unit-dur)
          to (* (floor (v2 ruler) unit-dur) unit-dur)
          by unit-dur do
          (let ((y (ruler-value-to-pix ruler line)))
            (om-draw-string 0 y (number-to-string line))
            (draw-grid-line-from-ruler self ruler y)))))


;;;Draw a vertical ruler when holding shift while moving the mouse
;;;Only when stopped to not kill the cursor
(defmethod om-view-mouse-motion-handler ((self automation-panel) position)
  (if (eq (state (object-value (editor self))) :stop)
      (if (om-shift-key-p)
          (om-start-transient-drawing
             self #'draw-coeff-ruler
             position
             (omp 0 2))
        (om-stop-transient-drawing self)))
  ;(call-next-method)
  )

;;;Same as for bpf-panel with one more option:
;;;click when holding shift in mouse mode changes coeff (curve shape)
(defmethod om-view-click-handler ((self automation-panel) position)
  (let* ((editor (editor self))
         (p0 position)
         (t0 (get-internal-real-time))
         (obj (object-value editor)))
    (close-point-editor editor)
    (case (edit-mode editor)
      (:mouse
       (cond ((om-add-key-down)
              (let ((p (insert-point-at-pix editor obj position)))
                (when p
                  (setf (selection editor) (list p))   ; (position p (point-list obj))
                  (report-modifications editor)
                  (om-invalidate-view self)
                  (update-timeline-editor editor)
                  ;;; move the new point
                  (om-init-temp-graphics-motion self position nil :min-move 10
                                                :motion #'(lambda (view pos)
                                                            (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                                                  (dy (dpix-to-dy self (- (om-point-y p0) (om-point-y pos)))))
                                                              (move-editor-selection editor :dx dx :dy dy)
                                                              (setf p0 pos)
                                                              (editor-invalidate-views editor)
                                                              (position-display editor pos)))
                                                :release #'(lambda (view pos) 
                                                             (round-point-values editor)
                                                             (time-sequence-update-internal-times obj)
                                                             (report-modifications editor)
                                                             (om-invalidate-view view)
                                                             (update-timeline-editor editor))))))
             ;;;Here
             ((om-shift-key-p)
              (om-init-temp-graphics-motion 
               self position nil
               :motion #'(lambda (view position)
                           (let ((pos (position (pix-to-x self (om-point-x p0)) (point-list obj) :test '<= :key 'ap-x))
                                 (val (float (/ (om-point-y position) (h self)))))
                             (when (and pos (> pos 0) (<= pos (length (point-list obj))))
                               (setf (ap-coeff (nth (1- pos) (point-list obj)))
                                     (max 0.00001 (min 0.95 val)))
                               ;(update-automation-data obj)
                               (om-invalidate-view self))))))
              
             (t (let ((selection (find-clicked-point-or-curve editor obj position))) 
                  (set-selection editor selection)
                  (om-invalidate-view self)
                  (update-timeline-editor editor)
                  ;;; move the selection or select rectangle
                  (if selection
                      (om-init-temp-graphics-motion 
                       self position nil
                       :motion #'(lambda (view pos)
                                   (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                         (dy (dpix-to-dy self (- (om-point-y p0) (om-point-y pos)))))
                                     (move-editor-selection editor :dx dx :dy dy)
                                     (setf p0 pos)
                                     (position-display editor pos)
                                     (editor-invalidate-views editor)
                                     ))
                       :release #'(lambda (view pos) 
                                    (round-point-values editor) 
                                    (time-sequence-update-internal-times obj)
                                    (report-modifications editor) 
                                    (om-invalidate-view view)
                                    (update-timeline-editor editor))
                       :min-move 4)
                    (om-init-temp-graphics-motion 
                     self position 
                     (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
                     :min-move 10
                     :release #'(lambda (view position)
                                  (set-selection editor (points-in-area editor p0 position))
                                  (om-invalidate-view view)
                                  (update-timeline-editor editor))))))))
      (:pen
       (cond ((om-command-key-p)
              (om-init-temp-graphics-motion 
               self position 
               (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
               :min-move 10
               :release #'(lambda (view position)
                            (setf (selection editor) (points-in-area editor p0 position))
                            (om-invalidate-view view)
                            (update-timeline-editor editor))))
             (t 
              (set-selection editor nil)
              (let ((time-offset 0))
                (when (and (bpc-p obj) (> (length (point-list obj)) 0))
                  ;find last time and add the default-offset
                  (setf time-offset (+ (get-obj-dur obj) (gesture-interval-time editor))))
                (insert-point-at-pix editor obj position time-offset)
                (report-modifications editor)
                (om-invalidate-view self)
                (update-timeline-editor editor)
                (om-init-temp-graphics-motion self position nil
                                              :motion #'(lambda (view pos) 
                                                          (when (> (om-points-distance p0 pos) *add-point-distance-treshold*)
                                                            (add-point-at-pix editor obj pos 
                                                                              (+ (- (get-internal-real-time) t0) time-offset))
                                                            (setf p0 pos)
                                                            (om-invalidate-view view)
                                                            ))
                                              :release #'(lambda (view pos) 
                                                           (time-sequence-update-internal-times obj)
                                                           (report-modifications editor)
                                                           (om-invalidate-view view)
                                                           (update-timeline-editor editor)))))))
      (:hand (let ((curr-pos position))
               (om-init-temp-graphics-motion self position nil
                                             :motion #'(lambda (view pos)
                                                         (move-rulers editor 
                                                                      :dx (- (om-point-x pos) (om-point-x curr-pos))
                                                                      :dy (- (om-point-y pos) (om-point-y curr-pos)))
                                                         (setf curr-pos pos)
                                                         )))))))


;; makes everypoint equal to the predecessor
(defun egalise-points (editor)
 (let* ((bpf (object-value editor))
        (points (loop for pos in (selection editor) collect (nth pos (point-list bpf)))))
   (loop for point in points 
         when (prev-point bpf point) do
         (om-point-set point :y (om-point-y (prev-point bpf point))))
   ))

(defun lock-selection (editor)
 (let* ((bpf (object-value editor))
       (points (loop for pos in (selection editor) collect (nth pos (point-list bpf)))))
   (loop for point in points do
         (setf (ap-lock point) (not (ap-lock point))))))
   

(defmethod editor-key-action ((editor automation-editor) key)
  (let ((panel (get-g-component editor :main-panel)))
    (case key
      (#\= (egalise-points editor)
           (editor-invalidate-views editor)
           (report-modifications editor))
      (#\b (lock-selection editor)
           (editor-invalidate-views editor)) 
      (otherwise (call-next-method))
      )))



;;;Draw panel
(defmethod om-draw-contents-area ((self automation-panel) x y w h)
  (let* ((obj (object-value (editor self)))
         (points (point-list obj))
         (editor (editor self)))

      ;;;GRID
      (om-with-fg-color (om-def-color :light-gray)
        (let ((center (list (x-to-pix self 0) (y-to-pix self 0))))
          (when (and (> (cadr center) 0) (< (cadr center) (h self)))
            (om-draw-line 0 (cadr center) (w self) (cadr center)))
          (when (and (> (car center) 0) (< (car center) (h self)))
            (om-draw-line (car center) 0 (car center) (h self))))
        (when (editor-get-edit-param editor :grid)
          (om-with-line '(2 2)
            (draw-grid-from-ruler self (x-ruler self))
            (draw-grid-from-ruler self (y-ruler self)))))
      

      ;;;POINTS AND CURVES
      (when points
        (let* ((t1 (/ (x1 self) (expt 10 (decimals obj))))
               (t2 (/ (x2 self) (expt 10 (decimals obj))))
               (p1 (max (1- (or (position t1 points :test '<= :key 'om-point-x) 0)) 0))
               (p2 (or (position t2 points :test '<= :key 'om-point-x)
                       (length points))))
          ;;;loop through points (visibles + previous + next)
          (loop for pt in (subseq points p1 p2)
                for i = p1 then (1+ i)
                do
                  ;;;draw point if visible
                  (when (in-interval (start-date pt) (list t1 t2))
                    (om-with-fg-color (om-def-color (if (find i (selection editor)) :dark-red :black))
                      (om-draw-circle (x-to-pix self (start-date pt))
                                      (y-to-pix self (start-value pt))
                                      (if (find i (selection editor)) 4 3) :fill t)
                    (when (ap-lock pt)
                      (om-draw-circle (x-to-pix self (start-date pt))
                                      (y-to-pix self (start-value pt))
                                      5 :fill nil)
                      )))
                  ;;;draw curves using 1000 lines by curve 
                  ;;(should rely on coeff for each point)
                  (if (= (ap-coeff pt) 0.5)
                      (om-draw-line (x-to-pix self (start-date pt))
                                    (y-to-pix self (start-value pt))
                                    (x-to-pix self (end-date pt obj))
                                    (y-to-pix self (end-value pt obj)))
                    (let* ((step (max 0.01 (/ (- (end-date pt obj) (start-date pt)) 100)))
                           (start-t (min (end-date pt obj) (max t1 (start-date pt))))
                           (end-t (min (end-date pt obj) t2)))
                      (loop for ti from start-t to end-t
                            by step
                            do
                            (om-draw-line (x-to-pix self ti) 
                                          (y-to-pix self (funcall (fun pt obj) ti))  
                                          (x-to-pix self (min (end-date pt obj) (+ step ti)))
                                          (y-to-pix self (funcall (fun pt obj) (min (end-date pt obj) (+ step ti)))))))
                    )
                  )
          ))
      ))



(defmethod om-draw-contents ((self automation-panel))
  (om-draw-contents-area self nil nil nil nil))


