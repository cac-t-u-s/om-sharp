;;;======================
;;; MIXED VIEW INHERITING FROM OM-VIEW (OM-API) and OPENGL-VIEW (GL-USER)
;;;======================

(in-package :om)

(defvar *OM-GL-DEFAULT-LINEWIDTH* 2.0)
(defvar *OM-GL-DEFAULT-COLOR* (list 0.0 0.0 0.0 1.0))

(defmethod restore-om-gl-colors-and-attributes ()
  (opengl:gl-color4-f (nth 0 *OM-GL-DEFAULT-COLOR*) (nth 1 *OM-GL-DEFAULT-COLOR*) (nth 2 *OM-GL-DEFAULT-COLOR*) (nth 3 *OM-GL-DEFAULT-COLOR*))
  (opengl:gl-line-width *OM-GL-DEFAULT-LINEWIDTH*)
  (opengl:gl-enable opengl:*gl-point-size*)
  (opengl:gl-enable opengl:*gl-point-smooth*)
  )

(defclass om-opengl-view (gl-user::opengl-view om-view) ())

(defmethod initialize-instance :after ((self om-opengl-view) &key &allow-other-keys)
  (mapcar #'(lambda (o) (setf (gl-user::viewer o) self)) (gl-user::g-objects self))
  (when (om-get-bg-color self)
    (setf (gl-user::bgcolor (gl-user::camera self)) 
          (om-color-to-single-float-list (om-get-bg-color self)))))

(defmethod om-set-bg-color ((self om-opengl-view) color)
  (call-next-method)
  (setf (gl-user::bgcolor (gl-user::camera self)) 
        (om-color-to-single-float-list (om-get-bg-color self))))

(defmethod om-color-to-single-float-list (color)
  (list (coerce (om-color-r color) 'single-float)
        (coerce (om-color-g color) 'single-float)
        (coerce (om-color-b color) 'single-float)
        (coerce (or (om-color-a color) 1.0) 'single-float)))

(defmethod oa::om-invalidate-view ((self om-opengl-view))
  (gl-user::opengl-redisplay-canvas self))

(defmethod clear-gl-display-list ((viewer om-opengl-view))
  (opengl:rendering-on (viewer)
    (mapcar #'gl-user::delete-display-list (gl-user::g-objects viewer))))

(defmethod gl-user::draw-contents ((self om-opengl-view))
  (oa::om-draw-contents self))

; to be redefined by subclasses
(defmethod oa::om-draw-contents ((self om-opengl-view)) nil)

(defmethod gl-user::opengl-viewer-key-pressed ((self om-opengl-view) x y spec) 
  (oa::om-char-spec-callback self x y spec))

(defmethod gl-user::opengl-viewer-button-1-second-press ((self om-opengl-view) x y)
  (om-init-3d-view self))

(defmethod om-init-3D-view ((self om-opengl-view))
  (gl-user::initialize-viewer self)
  (om-adapt-camera-to-object self)
  (opengl:rendering-on (self)
    (gl-user::polar-rotate (gl-user::icotransform self) -30 20))
  (gl-user::opengl-redisplay-canvas self)
  )

(defmethod zoom-view ((self om-opengl-view) factor)
  (let ((eye (gl-user::eye (gl-user::camera self)))
        (fact (if (= factor 0) 1 (/ 1 factor))))
    (setf (gl-user::xyz-y eye) (min (* (gl-user::xyz-y eye) fact) -0.1d0))
    )
  (gl-user::opengl-redisplay-canvas self))

(defmethod om-get-gl-objects ((self om-opengl-view))
  (gl-user::get-gl-object-list self))

(defmethod om-set-gl-objects ((self om-opengl-view) (obj gl-user::gl-object))
  (gl-user::set-gl-object-list self (list obj)))

(defmethod om-set-gl-objects ((self om-opengl-view) (objlist list))
  (gl-user::set-gl-object-list self objlist))

  
(defmethod om-adapt-camera-to-object ((self om-opengl-view))
  (let* ((dist-z (* 2.5d0 (max 3.0d0 (compute-max-extent (om-get-gl-objects self)))))
         (far-z (max 20.0d0 (* 5.0d0 dist-z))))
    (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 0.0D0)
    (setf (gl-user::xyz-y (gl-user::eye (gl-user::camera self))) (- dist-z))
    (setf (gl-user::xyz-x (gl-user::eye (gl-user::camera self))) 0.0d0)
    (setf (gl-user::far (gl-user::projection (gl-user::camera self))) far-z)))


