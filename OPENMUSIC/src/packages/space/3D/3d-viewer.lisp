
(in-package :om)

(defclass* 3D-viewer ()
  ((data :initarg :data :accessor data :initform nil :documentation "a list of 3D-object (3D-lines, etc..)")
   (center :initarg :center :accessor center :initform nil :documentation "a 3D point used as reference for data transformation and output")))

(defmethod object-has-editor ((self 3D-viewer)) t)
(defmethod get-editor-class ((self 3D-viewer)) '3D-viewer-editor)
(defclass 3d-viewer-editor (OMEditor) ())

(defmethod make-editor-window-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor)))
    (om-make-view 
     'om-opengl-view
     :editor editor
     :bg-color (om-make-color .1 .2 .2)
     :g-objects (append 
               (data obj)
               ;(when (center obj)
               ;  (make-ins 3d-cube ))
               ))))


(defmethod update-to-editor ((self 3d-viewer-editor) (from t))
  (when (window self)
    (let ((GLview (main-view self)))
    (om-set-gl-objects GLview (data (object-value self)))
    (clear-gl-display-list GLview)
    (om-invalidate-view GLview)
    )))