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
; File author: J. Bresson
;============================================================================

;=========================================================================
;PATCH IN PATCH
;=========================================================================

(in-package :om)

(defclass OMBoxPatch (OMBoxAbstraction) ())

(defmethod special-box-p ((name (eql 'patch))) t)
(defmethod special-box-p ((name (eql 'p))) t)

(defmethod get-box-class ((self OMPatch)) 'OMBoxPatch)

(defmethod omNG-make-special-box ((reference (eql 'patch)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMPatchInternal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "new-patch"))
   pos 
   init-args ;; don't need to pass them in principle..
   ))

(defmethod omNG-make-special-box ((reference (eql 'p)) pos &optional init-args)
  (omNG-make-special-box 'patch pos init-args))

(defmethod update-from-reference ((self OMBoxPatch))
  (when *erased-io*
    (let ((io (find *erased-io* (append (inputs self) (outputs self)) :key 'reference)))
      (when (and io (container self)) (mapc #'(lambda (c) (omng-remove-element (container self) c)) (connections io)))))
  (call-next-method))


;;; set the caller as the first reference so that the meta-inputs work
(defmethod omng-box-value :before ((self OMBoxPatch) &optional numout)  
  (setf (references-to (reference self))
        (cons self (remove self (references-to (reference self))))))

;;;-------------------------------------------------------
;;; DISPLAY
;;;========================
;;; when the value is a box:
;;;========================

(defmethod display-modes-for-object ((self OMPatch)) '(:mini-view :hidden :value))
(defmethod object-for-miniview ((self OMBoxPatch)) (reference self))

;;; the value of the box is a Box...
(defmethod draw-mini-view ((object OMBoxEditCall) (box OMBox) x y w h &optional time) 
  (om-draw-rect (+ x 2) (+ y 4) (- w 4) (- h 16) :fill t :color (om-def-color :white))
  ;(om-draw-rect (+ x 2) (+ y 4) (- w 4) (- h 16) :fill nil :color (om-def-color :dark-gray) :line 1.5)
  (draw-mini-view (get-box-value object) box (+ x 8) (+ y 4) (- w 12) (- h 16) nil))

;;; to draw the mini-view...
(defmethod get-edit-param ((self OMBoxPatch) param)
  (get-patch-value-edit-param (get-box-value self) param))

(defmethod get-patch-value-edit-param ((self t) param)
  (find-value-in-kv-list (object-default-edition-params self) param))

(defmethod get-patch-value-edit-param ((self object-with-edit-params) param)
  (get-edit-param self param))


;;; from inspector
(defmethod set-property ((object OMBoxPatch) (prop-id (eql :display)) val)
  (reset-cache-display object)
  (call-next-method))

;;; patch can draw either the patch or the value:
;;; cache display must be reinitialized
(defmethod set-display ((self OMBoxPatch) val)
  (reset-cache-display self)
  (call-next-method))

(defmethod draw-mini-view ((self OMPatch) box x y w h &optional time)
  (flet 
      ((pos-to-x (xpos) (+ x 15 (round (* xpos (- w 30)))))
       (pos-to-y (ypos) (+ y 12 (round (* ypos (- h 40))))))
    (ensure-cache-display-draw box self)
    (om-with-fg-color (om-def-color :gray)
      (loop for b in (car (get-display-draw box)) do
            (if (equal :b (caddr b))
                (om-draw-rect (- (pos-to-x (car b)) 8) (- (pos-to-y (cadr b)) 4) 16 8 :fill t)
              (progn  
                (om-with-fg-color (cond 
                                   ((equal :in (caddr b)) (om-make-color 0.2 0.6 0.2))
                                   ((equal :out (caddr b)) (om-make-color 0.3 0.6 0.8))
                                   (t (om-def-color :gray)))
                  (om-draw-circle (pos-to-x (car b)) (pos-to-y (cadr b)) 3.8 :fill t))
                (om-draw-rect (- (pos-to-x (car b)) 3) (- (pos-to-y (cadr b)) 3) 6 6 :fill t)
                )))
      (loop for c in (cadr (get-display-draw box)) do
            (when (and (car c) (cadr c)) ;; 2 boxes ok
              (let* ((from (nth (car c) (car (get-display-draw box))))
                     (to (nth (cadr c) (car (get-display-draw box))))
                     (from-x (pos-to-x (car from))) (from-y (pos-to-y (cadr from)))
                     (to-x (pos-to-x (car to))) (to-y (pos-to-y (cadr to)))
                     (mid-x (+ from-x (round (- to-x from-x) 2)))
                     (mid-y (+ from-y (round (- to-y from-y) 2))))
                         
                    ;(om-draw-line (pos-to-x (car from)) (pos-to-y (cadr from))
                    ;              (pos-to-x (car to)) (pos-to-y (cadr to)))
                (if (>= to-y from-y)
                    (progn 
                      (om-draw-line from-x from-y from-x mid-y)
                      (om-draw-line from-x mid-y to-x mid-y)
                      (om-draw-line to-x mid-y to-x to-y))
                  (progn 
                    (om-draw-line from-x from-y mid-x from-y)
                    (om-draw-line mid-x from-y mid-x to-y)
                    (om-draw-line mid-x to-y to-x to-y)))
                ))))))

(defmethod get-cache-display-for-draw ((self OMPatch) box)
  (declare (ignore box))
  (let* ((patch self)
         (p-boxes (get-boxes-of-type patch 'OMBoxCall))
         (bboxes (loop for b in p-boxes 
                       collect (list (box-x b) (box-y b)
                                     (cond ((equal (type-of b) 'ominbox) :in)
                                           ((equal (type-of b) 'omoutbox) :out)
                                           ((equal (type-of b) 'omboxeditcall) :b)
                                           ))))
         (cconecs (loop for c in (connections patch) collect 
                        (list (position (box (from c)) p-boxes)
                              (position (box (to c)) p-boxes)))))
    (when bboxes
      (let* ((x0 (apply #'min (mapcar 'car bboxes)))
             (xs (max 10 (- (apply #'max (mapcar 'car bboxes)) x0)))
             (y0 (apply #'min (mapcar 'cadr bboxes)))
             (ys (max 10 (- (apply #'max (mapcar 'cadr bboxes)) y0))))
        (setf bboxes (loop for bb in bboxes 
                           collect (list (float (/ (- (car bb) x0) xs)) (float (/ (- (cadr bb) y0) ys)) 
                                         (caddr bb))))
        (list bboxes cconecs))
      )))


