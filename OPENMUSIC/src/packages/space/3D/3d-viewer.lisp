
(in-package :om)

(defclass* 3D-viewer ()
  ((data :initarg :data :accessor data :initform nil 
         :documentation "a list of 3D-object (3D-lines, etc..)")
   (scaler-x :accessor scaler-x :initform 1 :documentation "a scaler for the viewer's 'x' axis")
   (scaler-y :accessor scaler-y :initform 1 :documentation "a scaler for the viewer's 'y' axis")
   (scaler-z :accessor scaler-z :initform 1 :documentation "a scaler for the viewer's 'z' axis")
    (center :accessor center :initform (list 0.0 0.0 0.0) 
            :documentation "a 3D point used as reference for data transformation and output")
   (rotation-x :accessor rotation-x :initform 0.0 :documentation "rotation angles on x axis")
   (rotation-y :accessor rotation-y :initform 0.0 :documentation "rotation angles on y axis")
   (rotation-z :accessor rotation-z :initform 0.0 :documentation "rotation angles on z axis")
   ))

#|
(defmethod get-properties-list ((self 3D-viewer))
  '((""
     (:scaler-x "x scale factor" :number scaler-x (.001 1000.0 1))
     (:scaler-y "y scale factor" :number scaler-y (.001 1000.0 1))
     (:scaler-z "z scale factor" :number scaler-z (.001 1000.0 1))
     (:rotation-x "x-axis rotation" :number rotation-x (0 360 1))
     (:rotation-y "y-axis rotation" :number rotation-y (0 360 1))
     (:rotation-z "z-axis rotation" :number rotation-z (0 360 1))
     (:center "center of rotation" :list center (0 0 0))
     
     )))
|#

(defmethod object-default-edition-params ((self 3D-viewer))
  '((:x-grid :auto)
    (:y-grid :auto)
    (:shift-x-edit-mode :z)
    (:shift-y-edit-mode :_)
    (:alt-x-edit-mode :y)
    (:alt-y-edit-mode :x)))
  
(defmethod additional-box-attributes ((self 3D-viewer)) 
  '((:x-grid "the x-range of the grid displayed in the viewer" nil)
    (:y-grid "the y-range of the grid displayed in the viewer" nil)))

(defmethod additional-class-attributes ((self 3D-viewer)) 
  '(scaler-x scaler-y scaler-z center rotation-x rotation-y rotation-z))

(defmethod om-init-instance ((self 3D-viewer) &optional args)
  (let ((c (find-value-in-kv-list args :center)))
    (when c (setf (center self) (copy-list c)))
    (when(data self)
      (multiple-value-bind (xmi xma ymi yma zmi zma)
         (get-extents (data self))
       (let ((scaled-ref 100))
         (unless (find-value-in-kv-list args :scaler-x)
           (setf (scaler-x self) (float (/ scaled-ref xma))))
         (unless (find-value-in-kv-list args :scaler-y)
           (setf (scaler-y self) (float (/ scaled-ref yma))))
         (unless (find-value-in-kv-list args :scaler-z)
           (setf (scaler-z self) (float (/ scaled-ref zma))))
         
         ;(unless (find-value-in-kv-list args :center)
         ;  (setf (center self) (list (- xma xmi) (- yma ymi) 0)))
         )))
         self))
      
;;; destructive
(defmethod apply-scaler ((self om-3d-object) &key x y z)
  (om-set-3Dobj-points 
   self
   (loop for p in (om-3Dobj-points self) collect
         (list (if x (* x (car p)) (car p))
               (if y (* y (cadr p)) (cadr p))
               (if z (* z (caddr p)) (caddr p))
               ))))

;;; returns new object
(defmethod scale-object ((self om-3d-object) &key x y z)
  (let ((copy (om-copy self))) 
    (om-set-3dobj-points 
     copy
     (loop for p in (om-3Dobj-points self) collect
         (list (if x (* x (car p)) (car p))
               (if y (* y (cadr p)) (cadr p))
               (if z (* z (caddr p)) (caddr p))
               )))
    copy))



(defmethod get-transformed-data ((self 3D-viewer))  
  (loop for obj in (data self) collect       
        (let* (;(new-obj (make-instance (type-of obj) :color (color obj)))
              (new-obj (scale-object obj :x (scaler-x self) :y (scaler-y self) :z (scaler-z self)))
              
              (points-xyz (mat-trans (om-3Dobj-points new-obj))))
          
          
          (let ((xlist (om- (car points-xyz) (nth 0 (center self))))
                (ylist (om- (cadr points-xyz) (nth 1 (center self))))
                (zlist (om- (caddr points-xyz) (nth 2 (center self))))
                (yaw (rotation-z self))
                (pitch (rotation-x self))
                (roll (rotation-y self)))
            
            ;;; from om-rotate 
            ;;; YAW = Z axis
            (unless (zerop yaw)
              (multiple-value-bind (a e d) (xyz->aed xlist ylist zlist)
                (multiple-value-bind (x y z) (aed->xyz (om- a yaw) e d)
                  (setf xlist x ylist y zlist z))))
            
            ;;; PITCH = X axis
            (unless (zerop pitch)
              (multiple-value-bind (a e d) (xyz->aed zlist ylist xlist)
               (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                 (setf zlist x ylist y xlist z))))
             
            ;;; ROLL = Y axis
            (unless (zerop roll)
              (multiple-value-bind (a e d) (xyz->aed xlist zlist ylist)
               (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                  (setf xlist x ylist z zlist y))))
                  
            (let ((new-points 
                   (mat-trans 
                    (list (om+ xlist (nth 0 (center self)))
                          (om+ ylist (nth 1 (center self)))
                          (om+ zlist (nth 2 (center self)))
                          ))))
              
              (om-set-3Dobj-points new-obj new-points)
              (apply-scaler new-obj :x (/ 1 (scaler-x self)) :y (/ 1 (scaler-y self)) :z (/ 1 (scaler-z self)))
              new-obj))
          )))


(defmethod object-has-editor ((self 3D-viewer)) t)
(defmethod get-editor-class ((self 3D-viewer)) '3D-viewer-editor)

(defclass 3d-viewer-editor (OMEditor) 
  ((x-grid :accessor x-grid :initform nil) ;; x-grid and y-grid can be the same as the editor's edit param, or they can be computed automatically
   (y-grid :accessor y-grid :initform nil)))

(defclass 3D-viewer-view (om-opengl-view) 
  ((viewpoint :accessor viewpoint :initform '(0.0d0 0.0d0 0.0d0) :documentation "x-y-z axis rotation angles")))

(defmethod init-grid ((editor 3d-viewer-editor))
  (when (data (object-value editor))
    (multiple-value-bind (xmi xma ymi yma zmi zma)
        ;; the data is scaled here. the grid must not
        (get-extents (data (object-value editor)))
    
      (setf (x-grid editor) (if (equal (editor-get-edit-param editor :x-grid) :auto)
                                (list xmi xma)
                              (editor-get-edit-param editor :x-grid)))
    
      (setf (y-grid editor) (if (equal (editor-get-edit-param editor :y-grid) :auto)
                                (list ymi yma)
                              (editor-get-edit-param editor :y-grid))))
    ))


                             
(defmethod make-editor-window-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor)))
    (let* ((3D-view (om-make-view '3D-viewer-view
                                  :editor editor
                                  :bg-color (om-make-color .1 .2 .2)))
           
           
           (control-view 
            (om-make-layout 
             'om-row-layout
             :subviews 
             (list (om-make-layout 
                    'om-column-layout
                    :subviews
                    (list 
                     (om-make-di 'om-simple-text :text "Rotation control" 
                                 :size (omp 200 22) 
                                 :font (om-def-font :font1b))
                     (om-make-layout 
                      'om-row-layout
                      :subviews
                      (list 
                       (om-make-di 'om-simple-text :text "shift:" 
                                   :size (omp 40 22) 
                                   :font (om-def-font :font1))
                       (om-make-di 'om-popup-list :items '(:x :y :z :_)
                                   :size (omp 60 24) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :shift-x-edit-mode)
                                   :di-action #'(lambda (list) 
                                                  (editor-set-edit-param editor :shift-x-edit-mode (om-get-selected-item list))
                                                  ))
                       (om-make-di 'om-popup-list :items '(:x :y :z :_)
                                   :size (omp 60 24) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :shift-y-edit-mode)
                                   :di-action #'(lambda (list) 
                                                  (editor-set-edit-param editor :shift-y-edit-mode (om-get-selected-item list))
                                                  ))
                       ))
                     (om-make-layout 
                      'om-row-layout
                      :subviews
                      (list 
                       (om-make-di 'om-simple-text :text "alt:" 
                                   :size (omp 40 20) 
                                   :font (om-def-font :font1))
                       (om-make-di 'om-popup-list :items '(:x :y :z :_)
                                   :size (omp 60 24) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :alt-x-edit-mode)
                                   :di-action #'(lambda (list) 
                                                  (editor-set-edit-param editor :alt-x-edit-mode (om-get-selected-item list))
                                                  ))
                       (om-make-di 'om-popup-list :items '(:x :y :z :_)
                                   :size (omp 60 24) :font (om-def-font :font1)
                                   :value (editor-get-edit-param editor :alt-y-edit-mode)
                                   :di-action #'(lambda (list) 
                                                  (editor-set-edit-param editor :alt-y-edit-mode (om-get-selected-item list))
                                                  ))
                       ))
                     )))
             )))
      
      (set-g-component editor :3D-view 3D-view)
     
      (om-make-layout 'om-row-layout :ratios '(9.9 0.1) 
                      :subviews 
                      (list 
                       (om-make-layout 'om-column-layout :subviews (list 3D-view control-view))
                       (make-default-editor-view editor)))
      
      )))



(defmethod update-3D-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor))
        (3D-view (get-g-component editor :3D-view)))
    ;;; works better if the objects are set after everything is on-screen
    (om-set-gl-objects 
     3D-view
     (loop for ob in (data obj) collect 
           (scale-object ob :x (scaler-x obj) :y (scaler-y obj) :z (scaler-z obj))))
     
    ;; apparently this needs to be done earlier (currently in draw-grid)
     (init-grid editor) 
    ))

(defmethod init-editor-window ((editor 3d-viewer-editor))
  (call-next-method)
  (let ((3D-view (get-g-component editor :3D-view)))
    (update-3D-contents editor)
    (om-init-3d-view 3D-view)
    (om-invalidate-view 3D-view)))


(defmethod update-to-editor ((editor 3d-viewer-editor) (from OMBox))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (update-3D-contents editor)
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))

(defmethod update-to-editor ((editor 3d-viewer-editor) (from t))
  (when (window editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (update-3D-contents editor)
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))


;;;========================
;;; BOX
;;;========================

(defmethod display-modes-for-object ((self 3D-viewer))
  '(:hidden :text :mini-view))


(defmethod get-cache-display-for-draw ((self 3D-viewer)) 
  (when (data self)
    (multiple-value-bind (xmi xma ymi yma zmi zma) 
        (get-extents (data self))
      (list (list xmi xma ymi yma zmi zma)))))

(defmethod draw-mini-view ((self 3D-viewer) (box t) x y w h &optional time)
  (when(data self)
    (let ((ranges (car (get-display-draw box))))
    (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset (car ranges) (cadr ranges) w x)
    (multiple-value-bind (fy oy) 
        ;;; Y ranges are reversed !! 
        (conversion-factor-and-offset (cadddr ranges) (caddr ranges) h y)

      (om-with-font  (om-def-font :font1 :size 8)
            (om-draw-string (+ x 10) (+ y (- h 4)) (number-to-string (nth 0 ranges)))
            (om-draw-string (+ x (- w (om-string-size (number-to-string (nth 1 ranges))
                                                      (om-def-font :font1 :size 8)) 4))
                            (+ y (- h 4)) 
                            (number-to-string (/ (nth 1 ranges) (scaler-x self))))
            (om-draw-string x (+ y (- h 14)) (number-to-string (nth 2 ranges)))
            (om-draw-string x (+ y 10) (number-to-string (nth 3 ranges))))

      (loop for line in (data self) do
            (when (points line) 
              (let ((lines (loop for pts on (points line)
                                 while (cadr pts)
                                 append
                                 (let ((p1 (car pts))
                                       (p2 (cadr pts)))
                                   (om+ 0.5
                                        (list (+ ox (* fx (car p1)))
                                              (+ oy (* fy (cadr p1)))
                                              (+ ox (* fx (car p2)))
                                              (+ oy (* fy (cadr p2)))))
                                   ))))
                (om-with-fg-color (or (color line) (om-def-color :dark-gray))
                  (om-draw-lines lines)))
              ))
      )))))




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

  (let* ((ed (editor self))
         (3DV (object-value ed)))

    (gl-user::initialize-transform (gl-user::position-transform (gl-user::camera self)))
    (gl-user::polar-rotate (gl-user::position-transform (gl-user::camera self))
                           :dz (car (viewpoint self)) 
                           :dx (cadr (viewpoint self)))
    
    (when (and (x-grid ed) (y-grid ed))
      (draw-grid 
       (* (scaler-x 3DV) (car (x-grid ed))) 
       (* (scaler-x 3DV) (cadr (x-grid ed))) 
       (* (scaler-y 3DV) (car (y-grid ed)))
       (* (scaler-y 3DV) (cadr (y-grid ed)))))

    (opengl:gl-color4-f 1.0 1.0 1.0 0.5)
    (draw-gl-point 
     (float (* (scaler-x 3DV) (nth 0 (center 3DV))))
     (float (* (scaler-y 3DV) (nth 1 (center 3DV))))
     (float (* (scaler-z 3DV) (nth 2 (center 3DV))))
     '(1.0 1.0 1.0) 1.0 20.0)
    
    (gl-user::initialize-transform (gl-user::object-transform self))
    (gl-user::translate (gl-user::object-transform self) 
                        :dx (- (float (* (scaler-x 3DV) (nth 0 (center 3DV)))))
                        :dy (- (float (* (scaler-y 3DV) (nth 1 (center 3DV))))))
    (gl-user::polar-rotate (gl-user::object-transform self)
                           :dx (* (rotation-x 3DV) 10) ;;; rotations in gl-user seem to be 0-3600
                           :dy (* (rotation-y 3DV) 10)
                           :dz (* (rotation-z 3DV) 10))
    (gl-user::translate (gl-user::object-transform self) 
                        :dx (float (* (scaler-x 3DV) (nth 0 (center 3DV))))
                        :dy (float (* (scaler-y 3DV) (nth 1 (center 3DV)))))

    ))

(defmethod om-adapt-camera-to-object ((self 3d-viewer-view))
  (when (om-get-gl-objects self)
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
    (setf (gl-user::far (gl-user::projection (gl-user::camera self))) far-z)))))


(defmethod om-view-key-handler ((self 3d-viewer-view) key) 
  (let ((3DV (object-value (editor self))))
    (case key
      (#\+ (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 0.6))
           (om-invalidate-view self))
      (#\- (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 1.2))
           (om-invalidate-view self))

      (:om-key-right 
       (setf (center 3DV) (list (+ (nth 0 (center 3DV)) (/ 1 (scaler-x 3DV))) 
                                (nth 1 (center 3DV))
                                (nth 2 (center 3DV))))
       (om-invalidate-view self))
      (:om-key-left 
       (setf (center 3DV) (list (- (nth 0 (center 3DV)) (/ 1 (scaler-x 3DV)))
                                (nth 1 (center 3DV)) 
                                (nth 2 (center 3DV))))
       (om-invalidate-view self))
      (:om-key-up 
       (setf (center 3DV) (list (nth 0 (center 3DV))
                                (+ (nth 1 (center 3DV)) (/ 1 (scaler-y 3DV)))
                                (nth 2 (center 3DV))))
       (om-invalidate-view self))
      (:om-key-down 
       (setf (center 3DV) (list (nth 0 (center 3DV))
                                (- (nth 1 (center 3DV)) (/ 1 (scaler-y 3DV)))
                                (nth 2 (center 3DV))))
       (om-invalidate-view self))

      (:om-key-esc  
       (setf (viewpoint self) (list 0.0d0 0.0d0 0.0d0))
       (setf (gl-user::lastxy self) nil)
       (gl-user::initialize-transform (gl-user::position-transform (gl-user::camera self)))
       (gl-user::polar-rotate (gl-user::position-transform (gl-user::camera self))
                              :dz (car (viewpoint self)) 
                              :dx (cadr (viewpoint self)))
       (om-invalidate-view self)
       (om-invalidate-view self) ;;; don't know why this is needed twice...
       )

      (#\o (setf (rotation-x 3DV) 0
                 (rotation-y 3DV) 0
                 (rotation-z 3DV) 0)
                 
       (om-invalidate-view self)
       (om-invalidate-view self) ;;; don't know why this is needed twice...
       )

      (#\Space (report-modifications (editor self)))
    
      (otherwise (call-next-method)))))



(defmethod gl-user::opengl-viewer-motion-click ((self 3d-viewer-view) x y) 
  (let ((last (gl-user::lastxy self)))
    (when last
      (setf (car (viewpoint self)) (mod (+ (car (viewpoint self)) (- x (car last))) 3600))
      (setf (cadr (viewpoint self)) (mod (+ (cadr (viewpoint self)) (- y (cdr last))) 3600))
      
      (opengl:rendering-on (self)
        (gl-user::opengl-redisplay-canvas self))
      
      (setf (gl-user::lastxy self) (cons x y)))))


(defmethod gl-user::opengl-viewer-motion-shift-click ((self 3d-viewer-view) x y) 
  (let* ((last (gl-user::lastxy self))
        (ed (editor self))
        (3DV (object-value ed))
        (dx (* (- x (car last)) .3))
        (dy (* (- y (cdr last)) .3)))
   
   (when last
      (case (editor-get-edit-param ed :shift-x-edit-mode)
        (:x (setf (rotation-x 3DV) (mod (+ (rotation-x 3DV) dx) 360)))
        (:y (setf (rotation-y 3DV) (mod (+ (rotation-y 3DV) dx) 360)))
        (:z (setf (rotation-z 3DV) (mod (+ (rotation-z 3DV) dx) 360))))
      (case (editor-get-edit-param ed :shift-y-edit-mode)
        (:x (setf (rotation-x 3DV) (mod (+ (rotation-x 3DV) dy) 360)))
        (:y (setf (rotation-y 3DV) (mod (+ (rotation-y 3DV) dy) 360)))
        (:z (setf (rotation-z 3DV) (mod (+ (rotation-z 3DV) dy) 360))))) 

    (opengl:rendering-on (self)
      (gl-user::opengl-redisplay-canvas self))
    
    (setf (gl-user::lastxy self) (cons x y))))


(defmethod gl-user::opengl-viewer-motion-alt-click ((self 3d-viewer-view) x y) 
  (let* ((last (gl-user::lastxy self))
         (ed (editor self))
         (3DV (object-value ed))
         (dx (* (- x (car last)) .3))
         (dy (* (- y (cdr last)) .3)))
    
    (when last
      (case (editor-get-edit-param ed :alt-x-edit-mode)
        (:x (setf (rotation-x 3DV) (mod (+ (rotation-x 3DV) dx) 360)))
        (:y (setf (rotation-y 3DV) (mod (+ (rotation-y 3DV) dx) 360)))
        (:z (setf (rotation-z 3DV) (mod (+ (rotation-z 3DV) dx) 360))))
      (case (editor-get-edit-param ed :alt-y-edit-mode)
        (:x (setf (rotation-x 3DV) (mod (+ (rotation-x 3DV) dy) 360)))
        (:y (setf (rotation-y 3DV) (mod (+ (rotation-y 3DV) dy) 360)))
        (:z (setf (rotation-z 3DV) (mod (+ (rotation-z 3DV) dy) 360)))))
   
   
      
    (opengl:rendering-on (self)
      (gl-user::opengl-redisplay-canvas self))
    
    (setf (gl-user::lastxy self) (cons x y))))


