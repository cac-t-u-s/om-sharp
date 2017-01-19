


(in-package :om)

(defclass cube (background-element) 
  ((pos :accessor pos :initarg :pos :initform nil)
   (size :accessor size :initarg :size :initform 1)))

(defun make-bg-speakers (speakers)
  (mapcar #'(lambda (spk)
              (make-instance 'cube :pos (make-3dpoint :x (car spk) :y (cadr spk) :z (caddr spk))
                             :size .08))
          speakers))

(defmethod draw-background-element ((self cube) (view bpf-bpc-panel) editor &optional x1 y1 x2 y2)
  (om-draw-rect (x-to-pix view (- (editor-point-x editor (pos self)) (* (size self) .5)))
                (y-to-pix view (- (editor-point-y editor (pos self)) (* (size self) .5)))
                (max (dx-to-dpix view (size self)) 10) 
                (min (dy-to-dpix view (size self)) -10)
                :color (om-def-color :light-gray) :fill t)
  (om-draw-rect (x-to-pix view (- (editor-point-x editor (pos self)) (* (size self) .5)))
                (y-to-pix view (- (editor-point-y editor (pos self)) (* (size self) .5)))
                (max (dx-to-dpix view (size self)) 10) 
                (min (dy-to-dpix view (size self)) -10)
                :line 2 :style :dash :color (om-def-color :gray) :fill nil))

(defmethod make-3D-background-element ((self cube)) 
  (make-instance 
   '3d-cube :size (size self) 
   :center (list (om-point-x (pos self)) (om-point-y (pos self)) (om-point-z (pos self)))
   :color (om-def-color :gray)))
  
