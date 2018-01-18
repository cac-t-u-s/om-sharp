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
      (when io (mapc #'(lambda (c) (omng-remove-element (container self) c)) (connections io)))))
  (call-next-method))


;;; set the caller as the first reference so that the meta-inputs work
(defmethod omng-box-value :before ((self OMBoxPatch) &optional numout)  
  (setf (references-to (reference self))
        (cons self (remove self (references-to (reference self))))))

;;;-------------------------------------------------------
;;; DISPLAY
     
(defmethod box-draw ((self OMBoxAbstraction) (frame OMBoxFrame))
  
  (when (> (h frame) 36)
    (case (display self)
      (:mini-view 
       (draw-mini-view (reference self) self 10 0 (- (w frame) 20) (h frame) nil))
      (:value 
       ;;; arrow
       (let ((ax 6) (ay 9)
             (b 3) (w 6) (h 4))
         (om-with-fg-color (om-make-color 1 1 1)
           (om-draw-polygon (list 
                             (+ ax b) ay 
                             (+ ax b w) ay 
                             (+ ax b w) (+ ay h)
                             (+ ax b w b) (+ ay h) 
                             (+ ax b (/ w 2)) (+ ay h 5) 
                             ax (+ ay h)
                             (+ ax b) (+ ay h))
                            :fill t))
         (om-with-fg-color (om-make-color .1 .1 .1)
           (om-draw-polygon (list 
                             (+ ax b) ay 
                             (+ ax b w) ay 
                             (+ ax b w) (+ ay h)
                             (+ ax b w b) (+ ay h) 
                             (+ ax b (/ w 2)) (+ ay h 5) 
                             ax (+ ay h)
                             (+ ax b) (+ ay h))
                            :fill nil))
         )
         (om-with-fg-color (om-def-color :gray)
           (om-with-font (om-def-font :font1b)
                         (om-draw-string 24 18 "VALUES")
                         (loop for v in (value self) for y = 32 then (+ y 12) do 
                               (om-draw-string 26 y  (format nil "- ~A" v)))
                         )))
      (otherwise 
       (om-with-font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))
                     (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
                       (om-draw-string (+ (/ (w frame) 2) -30) (max 22 (+ 6 (/ (h frame) 2))) "PATCH"))))


     ))
  (draw-patch-icon self)
  t)

#|
         ;;; icon
         (om-draw-picture (icon (reference self)) :x (+ x 4) :y (+ y 4) :w 18 :h 18)
         
|#

(defmethod display-modes-for-object ((self OMPatch)) '(:hidden :value :mini-view))
(defmethod object-for-miniview ((self OMBoxPatch)) (reference self))

;;; from inspector
(defmethod set-property ((object OMBoxPatch) (prop-id (eql :display)) val)
  (reset-cache-display object)
  (call-next-method))

;;; display reference instead of value
(defmethod change-display ((self OMBoxPatch)) 
  (when (visible-property (get-properties-list self) :display)
    (let ((next-mode (next-in-list (display-modes-for-object (reference self))
                                   (display self))))
      (set-display self next-mode))))

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

(defmethod get-cache-display-for-draw ((self OMPatch))
  (let* ((patch self)
         (bboxes (loop for b in (get-boxes-of-type patch 'OMBoxCall) 
                       collect (list (box-x b) (box-y b)
                                     (cond ((equal (type-of b) 'ominbox) :in)
                                           ((equal (type-of b) 'omoutbox) :out)
                                           ((equal (type-of b) 'omboxeditcall) :b)
                                           ))))
         (cconecs (loop for c in (connections patch) collect 
                        (list (position (box (from c)) (boxes patch))
                              (position (box (to c)) (boxes patch))))))
    (when bboxes
      (let* ((x0 (list-min (mapcar 'car bboxes)))
             (xs (max 10 (- (list-max (mapcar 'car bboxes)) x0)))
             (y0 (list-min (mapcar 'cadr bboxes)))
             (ys (max 10 (- (list-max (mapcar 'cadr bboxes)) y0))))
        (setf bboxes (loop for bb in bboxes 
                           collect (list (float (/ (- (car bb) x0) xs)) (float (/ (- (cadr bb) y0) ys)) 
                                         (caddr bb))))
        (list bboxes cconecs))
      )))



