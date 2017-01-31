
(in-package :om)

(defclass* 3D-viewer ()
  ((data :initarg :data :accessor data :initform nil :documentation "a list of 3D-object (3D-lines, etc..)")
   (center :accessor center :initform nil :documentation "a 3D point used as reference for data transformation and output")
   (scaler-x :accessor scaler-x :initform 1 :documentation "a scaler for the viewer's 'x' axis")
   (scaler-y :accessor scaler-y :initform 1 :documentation "a scaler for the viewer's 'y' axis")
   (scaler-z :accessor scaler-z :initform 1 :documentation "a scaler for the viewer's 'z' axis")
   (x-range :accessor x-range :initform '(0 5.0) :documentation "a scaler for the viewer's 'z' axis")
   (y-range :accessor y-range :initform '(0 220150) :documentation "a scaler for the viewer's 'z' axis")
   ))

(defmethod get-properties-list ((self 3D-viewer))
  '((""
     (:scaler-x "x scale factor" :number scaler-x (nil nil 1))
     (:scaler-y "y scale factor" :number scaler-y (nil nil 1))
     (:scaler-z "z scale factor" :number scaler-z (nil nil 1))
     )))

(defmethod object-has-editor ((self 3D-viewer)) t)
(defmethod get-editor-class ((self 3D-viewer)) '3D-viewer-editor)
(defclass 3d-viewer-editor (OMEditor) ())
(defclass 3D-viewer-view (om-opengl-view) ())

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
                                       :subviews (list ry 3D-view 
                                                       nil rx))
                       (make-default-editor-view editor)))
      
      )))

(defmethod init-editor-window ((editor 3d-viewer-editor))
  (call-next-method)
  (let ((3D-view (get-g-component editor :3D-view)))
    ;;; works better if the objects are set after everything is on-screen
    (om-set-gl-objects 3D-view 
                       (cons 
                        (make-instance '3d-lines sphere
                        (data (object-value editor))))
    (om-init-3d-view 3D-view)
    (om-invalidate-view 3D-view)
    )))


(defmethod update-to-editor ((editor 3d-viewer-editor) (from OMBox))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (om-set-gl-objects 3D-view (data (object-value editor)))
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))

(defmethod update-to-editor ((editor 3d-viewer-editor) (from t))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))