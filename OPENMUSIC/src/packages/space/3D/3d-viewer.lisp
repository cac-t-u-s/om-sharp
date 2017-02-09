
(in-package :om)

(defclass* 3D-viewer ()
  ((data :initarg :data :accessor data :initform nil :documentation "a list of 3D-object (3D-lines, etc..)")
   (center :accessor center :initform (make-3dpoint)  :documentation "a 3D point used as reference for data transformation and output")
   (rotation :accessor rotation :initform '(0.0 0.0 0.0) :documentation "a verctor of rotation angles")
   (scaler-x :accessor scaler-x :initform 1 :documentation "a scaler for the viewer's 'x' axis")
   (scaler-y :accessor scaler-y :initform 1 :documentation "a scaler for the viewer's 'y' axis")
   (scaler-z :accessor scaler-z :initform 1 :documentation "a scaler for the viewer's 'z' axis")
   (x-range :accessor x-range :initform '(0 5.0))
   (y-range :accessor y-range :initform '(0 220150))
   ))

;(defmethod get-properties-list ((self 3D-viewer)) 
;  '((""
;     (:scaler-x "x scale factor" :number scaler-x (nil nil 1))
;     (:scaler-y "y scale factor" :number scaler-y (nil nil 1))
;     (:scaler-z "z scale factor" :number scaler-z (nil nil 1))
;     )))

(defmethod object-has-editor ((self 3D-viewer)) t)
(defmethod get-editor-class ((self 3D-viewer)) '3D-viewer-editor)
(defclass 3d-viewer-editor (OMEditor) ())
(defclass 3D-viewer-view (om-opengl-view) 
  ((viewpoint :accessor viewpoint :initform '(0.0d0 0.0d0 0.0d0) :documentation "x-y-z axis rotation angles")))

(defmethod reinit-x-ranges-from-ruler ((self 3D-viewer-editor) ruler) 
  (set-ruler-range ruler 0 5000))
(defmethod reinit-y-ranges-from-ruler ((editor 3D-viewer-editor) ruler) 
  (set-ruler-range ruler 0 22050000))

(defmethod update-view-from-ruler ((ruler x-ruler-view) (view 3D-viewer-view))
  (setf (x-range (object-value (editor view))) 
        (list (float (/ (v1 ruler) (expt 10 (decimals ruler))))
              (float (/ (v2 ruler) (expt 10 (decimals ruler)))))))

(defmethod update-view-from-ruler ((ruler y-ruler-view) (view 3D-viewer-view))
  (setf (y-range (object-value (editor view))) 
        (list (float (/ (v1 ruler) (expt 10 (decimals ruler))))
              (float (/ (v2 ruler) (expt 10 (decimals ruler)))))))
                             
(defmethod make-editor-window-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor)))
    (let* ((3D-view (om-make-view '3D-viewer-view
                                  :editor editor
                                  :bg-color (om-make-color .1 .2 .2)))
           (rx (om-make-view 'x-ruler-view :related-views (list 3D-view)
                             :size (omp nil 20) 
                             :bg-color (om-def-color :white) 
                             :decimals 3 :x2 (* (cadr (x-range obj)) (expt 10 3))))
           (ry (om-make-view 'y-ruler-view :related-views (list 3D-view) 
                             :size (omp 30 nil) 
                             :bg-color (om-def-color :white) 
                             :decimals 3 :y2 (* (cadr (y-range obj)) (expt 10 3)))))
      
      (set-g-component editor :3D-view 3D-view)
     
      (om-make-layout 'om-row-layout :ratios '(9.9 0.1) 
                      :subviews 
                      (list 
                       (om-make-layout 'om-grid-layout :align :right
                                       :dimensions '(2 2)
                                       :delta 2
                                       :ratios '((0.01 0.99) (0.99 0.01))
                                       :subviews ; (list ry 3D-view nil rx)
                                       (list nil 3D-view nil nil)
                                       )
                       (make-default-editor-view editor)))
      
      )))

(defmethod init-editor-window ((editor 3d-viewer-editor))
  (call-next-method)
  (let ((3D-view (get-g-component editor :3D-view)))
    ;;; works better if the objects are set after everything is on-screen
    (om-set-gl-objects 3D-view (print (data (object-value editor))))
    (om-init-3d-view 3D-view)
    (om-invalidate-view 3D-view)
    ))


(defmethod update-to-editor ((editor 3d-viewer-editor) (from OMBox))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (om-set-gl-objects 3D-view (data (object-value editor)))
      (om-adapt-camera-to-object 3D-view)
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))

(defmethod update-to-editor ((editor 3d-viewer-editor) (from t))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))


;;;============================
;;; OPENGL
;;;============================

(defun draw-grid (x1 x2 y1 y2)
  (let ((xmin (* 2.0 (floor x1 2)))
        (xmax (* 2.0 (ceiling x2 2)))
        (ymin (* 2.0 (floor y1 2)))
        (ymax (* 2.0 (ceiling y2 2))))
        
    (opengl:gl-line-width .5) 
    (opengl:gl-color4-f 0.9 0.9 0.9 0.9)
    (opengl:gl-begin opengl:*GL-LINES*)       
    (loop for x from xmin to xmax by 2 do
          (opengl:gl-vertex3-f (float x) ymin 0.0)
          (opengl:gl-vertex3-f (float x) ymax 0.0))
    (loop for y from ymin to ymax by 2 do
          (opengl:gl-vertex3-f xmin (float y) 0.0)
          (opengl:gl-vertex3-f xmax (float y) 0.0))
    (opengl:gl-end)
    ))


(defmethod om-draw-contents ((self 3d-viewer-view))

  (gl-user::initialize-transform (gl-user::position-transform (gl-user::camera self)))
  (gl-user::polar-rotate (gl-user::position-transform (gl-user::camera self))
                         :dz (car (viewpoint self)) 
                         :dx (cadr (viewpoint self)))
  
  (when (om-get-gl-objects self)
    (multiple-value-bind (xmi xma ymi yma zmi zma)
        (get-extents (om-get-gl-objects self))
      (draw-grid xmi xma ymi yma)))
  
  (let ((3DV (object-value (editor self))))
    (opengl:gl-color4-f 1.0 1.0 1.0 0.5)
    (draw-gl-point 
     (float (om-point-x (center 3DV))) (float (om-point-y (center 3DV))) (float (om-point-z (center 3DV)))
     '(1.0 1.0 1.0) 1.0 20.0)
    
    (gl-user::initialize-transform (gl-user::object-transform self))
    (gl-user::translate (gl-user::object-transform self) 
                        :dx (- (float (om-point-x (center 3DV))))
                        :dy (- (float (om-point-y (center 3DV)))))
    (gl-user::polar-rotate (gl-user::object-transform self)
                           :dx (car (rotation 3DV))
                           :dy (cadr (rotation 3DV)) 
                           :dz (caddr (rotation 3DV)))
    (gl-user::translate (gl-user::object-transform self) 
                        :dx (float (om-point-x (center 3DV)))
                        :dy (float (om-point-y (center 3DV))))

    ))

(defmethod om-adapt-camera-to-object ((self 3d-viewer-view))
  (multiple-value-bind (xmi xma ymi yma zmi zma)
      (get-extents (om-get-gl-objects self))
    (let* ((dist-z (* 2.5d0 (max 3.0d0 (abs xmi) (abs xma) (abs ymi) (abs yma) (abs zmi) (abs zma))))
           (far-z (max 20.0d0 (* 5.0d0 dist-z))))
      (setf (gl-user::eye (gl-user::camera self)) (gl-user::make-xyz :x (+ xmi (* (- xma xmi) 0.5d0))
                                                                     :y (+ ymi (* (- yma ymi) 0.5d0))
                                                                     :z dist-z))
      (setf (gl-user::center (gl-user::camera self)) (gl-user::make-xyz :x (+ xmi (* (- xma xmi) 0.5d0)) 
                                                                        :y (+ ymi (* (- yma ymi) 0.5d0)) 
                                                                        :z 0.0d0))
    (setf (gl-user::up (gl-user::camera self)) (gl-user::make-xyz :y 1.0d0))
    (setf (gl-user::far (gl-user::projection (gl-user::camera self))) far-z))))


(defmethod om-view-key-handler ((self 3d-viewer-view) key) 
  (let ((3DV (object-value (editor self))))
    (case key
      (#\+ (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 0.6))
           (om-invalidate-view self))
      (#\- (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 1.2))
           (om-invalidate-view self))

      (:om-key-right (setf (center 3DV) (om-point-mv (center 3DV) :x 1))
       (om-invalidate-view self))
      (:om-key-left (setf (center 3DV) (om-point-mv (center 3DV) :x -1))
       (om-invalidate-view self))
      (:om-key-up (setf (center 3DV) (om-point-mv (center 3DV) :y 1))
       (om-invalidate-view self))
      (:om-key-down (setf (center 3DV) (om-point-mv (center 3DV) :y -1))
       (om-invalidate-view self))
      (:om-key-esc  
       (setf (viewpoint self) (list 0.0d0 0.0d0 0.0d0))
       (setf (gl-user::lastxy self) nil)
       (gl-user::initialize-transform (gl-user::position-transform (gl-user::camera self)))
       (gl-user::polar-rotate (gl-user::position-transform (gl-user::camera self))
                              :dz (car (viewpoint self)) 
                              :dx (cadr (viewpoint self)))
       (om-invalidate-view self))
    
      (otherwise (call-next-method)))))



(defmethod gl-user::opengl-viewer-motion-click ((self 3d-viewer-view) x y) 
  (let ((last (gl-user::lastxy self)))
    (when last
      (setf (car (viewpoint self)) (+ (car (viewpoint self)) (- x (car last))))
      (setf (cadr (viewpoint self)) (+ (cadr (viewpoint self)) (- y (cdr last))))
      
      (opengl:rendering-on (self)
        (gl-user::opengl-redisplay-canvas self))
      
      (setf (gl-user::lastxy self) (cons x y)))))


(defmethod gl-user::opengl-viewer-motion-shift-click ((self 3d-viewer-view) x y) 
  (let ((last (gl-user::lastxy self))
        (3DV (object-value (editor self))))
   
    (when last
      (setf (car (rotation 3DV)) (+ (car (rotation 3DV)) (- y (cdr last))))
      (setf (caddr (rotation 3DV)) (+ (caddr (rotation 3DV)) (- x (car last)))))
      
    (opengl:rendering-on (self)
      (gl-user::opengl-redisplay-canvas self))
    
    (setf (gl-user::lastxy self) (cons x y))))


(defmethod gl-user::opengl-viewer-motion-alt-click ((self 3d-viewer-view) x y) nil)

