;============================================================================
; om7: visual programming language for computer-aided music composition
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

  
(defmethod additional-box-attributes ((self 3D-viewer)) 
  '((:x-grid "the x-range of the grid displayed in the viewer" nil)
    (:y-grid "the y-range of the grid displayed in the viewer" nil)))

(defmethod additional-class-attributes ((self 3D-viewer)) 
  '(scaler-x scaler-y scaler-z center rotation-x rotation-y rotation-z))

(defmethod om-init-instance ((self 3D-viewer) &optional initargs)
  (let ((c (find-value-in-kv-list initargs :center)))
    (when c (setf (center self) (copy-list c)))
    (when(data self)
      (multiple-value-bind (xmi xma ymi yma zmi zma)
         (get-extents (data self))
       (let ((scaled-ref 100))
         (unless (find-value-in-kv-list initargs :scaler-x)
           (setf (scaler-x self) (float (/ scaled-ref (- xma xmi)))))
         (unless (find-value-in-kv-list initargs :scaler-y)
           (setf (scaler-y self) (float (/ scaled-ref (- yma ymi)))))
         (unless (find-value-in-kv-list initargs :scaler-z)
           (setf (scaler-z self) (float (/ scaled-ref (- zma zmi)))))
         
         ;(unless (find-value-in-kv-list args :center)
         ;  (setf (center self) (list (- xma xmi) (- yma ymi) 0)))
         )))
    self))
      
;;; called from the editor only
(defmethod init-state ((3DV 3D-viewer))
  (setf (rotation-x 3DV) 0
        (rotation-y 3DV) 0
        (rotation-z 3DV) 0))

;;; destructive
;;; does not modify interal glpoints
(defmethod scale-object ((self om-3d-object) &key x y z)
  (loop for p in (om-3Dobj-points self) do
        (if x (setf (car p) (* x (car p))))
        (if y (setf (cadr p) (* y (cadr p))))
        (if z (setf (caddr p) (* z (caddr p)))))
  self)
  
(defmethod translate-object ((self om-3d-object) &key x y z)
  (loop for p in (om-3Dobj-points self) do
        (if x (setf (car p) (+ x (car p))))
        (if y (setf (cadr p) (+ y (cadr p))))
        (if z (setf (caddr p) (+ z (caddr p)))))
  self)


(defmethod get-transformed-data ((self 3D-viewer))  

  (loop for obj in (data self) collect    
        
        (let* ((new-obj (scale-object 
                         (translate-object
                          (om-copy obj) 
                          :x (- (nth 0 (center self)))
                          :y (- (nth 1 (center self)))
                          :z (- (nth 2 (center self))))
                         :x (scaler-x self)
                         :y (scaler-y self)
                         :z (scaler-z self))
                         )
               (points-xyz (mat-trans (om-3Dobj-points new-obj)))
               (xlist (car points-xyz))
               (ylist (cadr points-xyz))
               (zlist (caddr points-xyz))
               (yaw (rotation-z self))
               (pitch (rotation-x self))
               (roll (rotation-y self)))
            
          ;;; !!! the order of the 3 rotations matters !!!
          
          ;;; YAW = Z axis
          (unless nil ;(zerop yaw)
            (multiple-value-bind (a e d) (xyz->aed xlist ylist zlist)
              (multiple-value-bind (x y z) (aed->xyz (om- a yaw) e d)
                (setf xlist x ylist y zlist z))))

          ;;; ROLL = Y axis
          (unless nil ;(zerop roll)
            (multiple-value-bind (a e d) (xyz->aed xlist zlist ylist)
              (multiple-value-bind (x y z) (aed->xyz (om+ a roll) e d)
                (setf xlist x ylist z zlist y))))
          
          ;;; PITCH = X axis
          (unless nil ;(zerop pitch)
            (multiple-value-bind (a e d) (xyz->aed zlist ylist xlist)
              (multiple-value-bind (x y z) (aed->xyz (om+ a pitch) e d)
                (setf zlist x ylist y xlist z))))

          (om-set-3Dobj-points new-obj (mat-trans (list xlist ylist zlist)))
            
          (translate-object 
           (scale-object 
            new-obj
            :x (/ 1 (scaler-x self)) 
            :y (/ 1 (scaler-y self)) 
            :z (/ 1 (scaler-z self)))
           :x (nth 0 (center self))
           :y (nth 1 (center self))
           :z (nth 2 (center self))
           )    
          )))


;;; does not copy the data
(defmethod filter-3D-data ((data-list list) &key xmin xmax ymin ymax zmin zmax)
  (remove-if 
   #'(lambda (line)
       (find-if #'(lambda (p)
                    (or (and xmin (< (car p) xmin))
                        (and xmax (> (car p) xmax))
                        (and ymin (< (cadr p) ymin))
                        (and ymax (> (cadr p) ymax))
                        (and zmin (< (caddr p) zmin))
                        (and zmax (> (caddr p) zmax))))
                (om-3Dobj-points line)))
   data-list))

(defmethod filter-3D-points ((data-list list) &key xmin xmax ymin ymax zmin zmax)
  (remove-if 
   #'(lambda (line) (null (om-3Dobj-points line)))
   (loop for line in data-list collect
         (let ((newline (om-copy line)))
           (setf (points newline)
                 (remove-if 
                  #'(lambda (p)
                      (or (and xmin (< (car p) xmin))
                          (and xmax (> (car p) xmax))
                          (and ymin (< (cadr p) ymin))
                          (and ymax (> (cadr p) ymax))
                          (and zmin (< (caddr p) zmin))
                          (and zmax (> (caddr p) zmax))))
                  (om-3Dobj-points newline)))
           newline))
   ))

;;;=======================================
;;; EDITOR
;;;=======================================

(defmethod object-has-editor ((self 3D-viewer)) t)
(defmethod get-editor-class ((self 3D-viewer)) '3D-viewer-editor)

(defmethod object-default-edition-params ((self 3D-viewer))
  `((:x-grid :auto)
    (:y-grid :auto)
    (:shift-x-edit-mode :z)
    (:shift-y-edit-mode :_)
    (:alt-x-edit-mode :y)
    (:alt-y-edit-mode :x)
    (:show-grid t)
    (:filter-off-grid nil)
    (:3D-bg-color ,(om-make-color .1 .2 .2))
    ))

(defclass 3d-viewer-editor (OMEditor) 
  ((x-grid :accessor x-grid :initform nil) ;; x-grid and y-grid can be the same as the editor's edit param, or they can be computed automatically
   (y-grid :accessor y-grid :initform nil)))

(defclass 3D-viewer-view (OMEditorView om-opengl-view) 
  ((viewpoint :accessor viewpoint :initform '(0.0d0 0.0d0 0.0d0) :documentation "x-y-z axis rotation angles")))

(defmethod init-grid ((editor 3d-viewer-editor))
  (when (data (object-value editor))
    (multiple-value-bind (xmi xma ymi yma zmi zma)
        ;; the data is scaled here. the grid must not
        (get-extents (data (object-value editor)))
    
      (set-x-grid editor (if (equal (editor-get-edit-param editor :x-grid) :auto)
                                (list xmi xma)
                              (editor-get-edit-param editor :x-grid)))
    
      (set-y-grid editor (if (equal (editor-get-edit-param editor :y-grid) :auto)
                                (list ymi yma)
                              (editor-get-edit-param editor :y-grid))))
    ))

(defmethod set-x-grid ((editor 3d-viewer-editor) values)

  (setf (x-grid editor) values)
  
  (when (get-g-component editor :x-grid-min-numbox)
    (set-value (get-g-component editor :x-grid-min-numbox) (car values)))
  (when (get-g-component editor :x-grid-max-numbox)
    (set-value (get-g-component editor :x-grid-max-numbox) (cadr values)))
    
  ;(when (get-g-component editor :center-x-numbox)
  ;  (set-min-max (get-g-component editor :center-x-numbox)
  ;               :min (car values)
  ;               :max (cadr values)))

  (update-lines editor)
  
  values)

(defmethod set-y-grid ((editor 3d-viewer-editor) values)
  (setf (y-grid editor) values)
  
  (when (get-g-component editor :y-grid-min-numbox)
    (set-value (get-g-component editor :y-grid-min-numbox) (car values)))
  (when (get-g-component editor :y-grid-max-numbox)
    (set-value (get-g-component editor :y-grid-max-numbox) (cadr values)))
  
  ;(when (get-g-component editor :center-y-numbox)
  ;  (set-min-max (get-g-component editor :center-y-numbox)
  ;               :min (car values)
  ;               :max (cadr values)))

  (update-lines editor)
  
  values)

(defmethod update-lines ((ed 3D-viewer-editor)) 
  (let ((3D-view (get-g-component ed :3d-view)))
    (loop for line in (get-transformed-data (object-value ed)) do
          for view-line in (om-get-gl-objects 3d-view) collect
          (setf (vertices-colors view-line)
                (loop for p in (om-3Dobj-points line)
                      collect  (if (and (editor-get-edit-param ed :filter-off-grid) (x-grid ed) (y-grid ed)
                                        (or (< (car p) (car (x-grid ed)))
                                            (> (car p) (cadr (x-grid ed)))
                                            (< (cadr p) (car (y-grid ed)))
                                            (> (cadr p) (cadr (y-grid ed)))))
                                   (om-make-color-alpha (om-def-color :gray) 0.1)
                                 NIL))))
    (gl-user::clear-gl-display-list 3D-view)
    ))


(defmethod update-g-components ((ed 3D-viewer-editor))
  (let ((3DV (object-value ed)))
    (set-value (get-g-component ed :center-x-numbox) (car (center 3DV)))
    (set-value (get-g-component ed :center-y-numbox) (cadr (center 3DV)))
    (set-value (get-g-component ed :rotation-x-numbox) (rotation-x 3DV))
    (set-value (get-g-component ed :rotation-y-numbox)  (rotation-y 3DV))
    (set-value (get-g-component ed :rotation-z-numbox)  (rotation-z 3DV))
    (om-set-slider-value (get-g-component ed :rotation-x-slider)  (rotation-x 3DV))
    (om-set-slider-value (get-g-component ed :rotation-y-slider)  (rotation-y 3DV))
    (om-set-slider-value (get-g-component ed :rotation-z-slider)  (rotation-z 3DV))
    ))


(defmethod set-rotation-param-from-editor ((editor 3d-viewer-editor) object-slot value)
  (setf (slot-value (object-value editor) object-slot) value)
  (update-from-editor (object editor) :reactive nil)
  (when (editor-get-edit-param editor :filter-off-grid)
    (update-lines editor)))

    
(defmethod set-3D-viewer-controls-from-object ((editor 3d-viewer-editor))
  
  (let ((obj (object-value editor)))
    
    (set-value (get-g-component editor :center-x-numbox) (car (center obj)))
    (set-value (get-g-component editor :center-y-numbox) (cadr (center obj)))
    (set-value (get-g-component editor :rotation-x-numbox) (rotation-x obj))
    (set-value (get-g-component editor :rotation-y-numbox) (rotation-y obj))
    (set-value (get-g-component editor :rotation-z-numbox) (rotation-z obj))

    (om-set-slider-value (get-g-component editor :rotation-x-slider) (rotation-x obj))
    (om-set-slider-value (get-g-component editor :rotation-y-slider) (rotation-y obj))
    (om-set-slider-value (get-g-component editor :rotation-z-slider) (rotation-z obj))

    ))

(defmethod set-bg-color ((editor 3d-viewer-editor))
  (let ((new-color (om-choose-color-dialog :color (editor-get-edit-param editor :3D-bg-color)
                                           :owner (window editor))))
    (when new-color
      (editor-set-edit-param editor :3D-bg-color new-color)
      (om-set-bg-color (get-g-component editor :3d-view) new-color)
      (om-invalidate-view (get-g-component editor :3d-view))
      )))
                                                  

(defmethod make-editor-window-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor)))
    
    (set-g-component editor :center-x-numbox
                     (om-make-graphic-object 'numbox :size (om-make-point 55 18) :font (om-def-font :font1)
                                             :bg-color (om-def-color :white) :border t
                                             :decimals 1
                                             :value (car (center obj))
                                             ;:min-val (car (x-grid editor)) :max-val (cadr (x-grid editor))
                                             :db-click t
                                             :after-fun #'(lambda (item)
                                                            (setf (car (center obj)) (get-value item))
                                                            (set-rotation-param-from-editor editor 'center (center obj))
                                                            (om-invalidate-view (get-g-component editor :3d-view))
                                                            )))

    (set-g-component editor :center-y-numbox
                     (om-make-graphic-object 'numbox :size (om-make-point 55 18) :font (om-def-font :font1)
                                             :bg-color (om-def-color :white) :border t
                                             :decimals 1
                                             :value (cadr (center obj))
                                             ;:min-val (car (y-grid editor)) :max-val (cadr (x-grid editor))
                                             :db-click t
                                             :after-fun #'(lambda (item)
                                                            (setf (cadr (center obj)) (get-value item))
                                                            (set-rotation-param-from-editor editor 'center (center obj))
                                                            (om-invalidate-view (get-g-component editor :3d-view))
                                                            )))

    (set-g-component editor :rotation-x-numbox
                     (om-make-graphic-object 'numbox :size (om-make-point 40 18) :font (om-def-font :font1)
                                             :fg-color (om-def-color :gray)
                                             :border nil :enabled nil
                                             :value (rotation-x obj)
                                             :decimals 0))
    (set-g-component editor :rotation-y-numbox
                     (om-make-graphic-object 'numbox :size (om-make-point 40 18) :font (om-def-font :font1)
                                             :fg-color (om-def-color :gray)
                                             :border nil :enabled nil
                                             :value (rotation-y obj)
                                             :decimals 0))
    (set-g-component editor :rotation-z-numbox
                     (om-make-graphic-object 'numbox :size (om-make-point 40 18) :font (om-def-font :font1)
                                             :fg-color (om-def-color :gray)
                                             :border nil :enabled nil
                                             :value (rotation-z obj)
                                             :decimals 0))
    
    (set-g-component editor :rotation-x-slider
                     (om-make-di 'om-slider :range '(0 360)
                                 :size (omp 100 24)
                                 :value (rotation-x obj)
                                 :di-action #'(lambda (item) 
                                                (set-rotation-param-from-editor editor 'rotation-x (om-slider-value item))
                                                (om-invalidate-view (get-g-component editor :3d-view))
                                                (set-value (get-g-component editor :rotation-x-numbox)
                                                           (om-slider-value item)
                                                           ))))
    (set-g-component editor :rotation-y-slider
                     (om-make-di 'om-slider :range '(0 360)
                                 :size (omp 100 24)
                                 :value (rotation-y obj)
                                 :di-action #'(lambda (item) 
                                                (set-rotation-param-from-editor editor 'rotation-y (om-slider-value item))
                                                (om-invalidate-view (get-g-component editor :3d-view))
                                                (set-value (get-g-component editor :rotation-y-numbox)
                                                           (om-slider-value item)
                                                           ))))
    (set-g-component editor :rotation-z-slider
                     (om-make-di 'om-slider :range '(0 360)
                                 :size (omp 100 24)
                                 :value (rotation-z obj)
                                 :di-action #'(lambda (item)
                                                (set-rotation-param-from-editor editor 'rotation-z (om-slider-value item))
                                                (om-invalidate-view (get-g-component editor :3d-view))
                                                (set-value (get-g-component editor :rotation-z-numbox)
                                                           (om-slider-value item)
                                                           ))))

    (set-g-component editor :x-grid-min-numbox
                     (om-make-graphic-object 'numbox :size (omp 55 18) :font (om-def-font :font1)
                                             :bg-color (om-def-color :white) :border t
                                             :decimals 1
                                             :value (car (x-grid editor))
                                             :db-click t
                                             :after-fun #'(lambda (item) 
                                                            (set-x-grid editor (list (get-value item) (cadr (x-grid editor))))
                                                            (editor-set-edit-param editor :x-grid (x-grid editor))
                                                            (om-invalidate-view (get-g-component editor :3d-view))
                                                            )))
                     
    (set-g-component editor :x-grid-max-numbox
                     (om-make-graphic-object'numbox :size (omp 55 18) :font (om-def-font :font1)
                                            :bg-color (om-def-color :white) :border t
                                            :decimals 1
                                            :value (cadr (x-grid editor))
                                            :db-click t
                                            :after-fun #'(lambda (item) 
                                                           (set-x-grid editor (list (car (x-grid editor)) (get-value item)))
                                                           (editor-set-edit-param editor :x-grid (x-grid editor))
                                                           (om-invalidate-view (get-g-component editor :3d-view)))))

    (set-g-component editor :y-grid-min-numbox
                     (om-make-graphic-object 'numbox :size (omp 55 18) :font (om-def-font :font1)
                                             :bg-color (om-def-color :white) :border t
                                             :decimals 1
                                             :value (car (y-grid editor))
                                             :db-click t
                                             :after-fun #'(lambda (item) 
                                                            (set-y-grid editor (list (get-value item) (cadr (y-grid editor))))
                                                            (editor-set-edit-param editor :y-grid (y-grid editor))
                                                            (om-invalidate-view (get-g-component editor :3d-view)))))

    (set-g-component editor :y-grid-max-numbox
                     (om-make-graphic-object 'numbox :size (omp 55 18) :font (om-def-font :font1)
                                             :bg-color (om-def-color :white) :border t
                                             :decimals 1
                                             :value (cadr (y-grid editor))
                                             :db-click t
                                             :after-fun #'(lambda (item) 
                                                            (set-y-grid editor (list (car (y-grid editor)) (get-value item)))
                                                            (editor-set-edit-param editor :y-grid (y-grid editor))
                                                            (om-invalidate-view (get-g-component editor :3d-view)))))

    (let ((control-view 
           (om-make-layout 
            'om-row-layout
            :subviews 
            (list 
             ;;; CONTROLERS
             (om-make-layout 
              'om-column-layout
              :subviews
              (list 
               (om-make-di 'om-simple-text :text "Rotation control axes" :size (omp 180 22) :font (om-def-font :font1b))
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "shift:" :size (omp 40 22) :font (om-def-font :font1))
                 (om-make-di 'om-popup-list :size (omp 60 24) :font (om-def-font :font1)
                             :items '(:x :y :z :_)
                             :value (editor-get-edit-param editor :shift-x-edit-mode)
                             :di-action #'(lambda (list) 
                                            (editor-set-edit-param editor :shift-x-edit-mode (om-get-selected-item list))))
                 (om-make-di 'om-popup-list :size (omp 60 24) :font (om-def-font :font1)
                             :items '(:x :y :z :_)
                             :value (editor-get-edit-param editor :shift-y-edit-mode)
                             :di-action #'(lambda (list) 
                                            (editor-set-edit-param editor :shift-y-edit-mode (om-get-selected-item list))))      
                 ))
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "alt:" :size (omp 40 20) :font (om-def-font :font1))
                 (om-make-di 'om-popup-list :size (omp 60 24) :font (om-def-font :font1)
                             :items '(:x :y :z :_)
                             :value (editor-get-edit-param editor :alt-x-edit-mode)
                             :di-action #'(lambda (list) 
                                            (editor-set-edit-param editor :alt-x-edit-mode (om-get-selected-item list))))
                 (om-make-di 'om-popup-list :size (omp 60 24) :font (om-def-font :font1)
                             :items '(:x :y :z :_)
                             :value (editor-get-edit-param editor :alt-y-edit-mode)
                             :di-action #'(lambda (list) 
                                            (editor-set-edit-param editor :alt-y-edit-mode (om-get-selected-item list))))
                 ))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list
                 (om-make-di 'om-simple-text :text "" :size (omp 40 20))
                 (om-make-di 'om-button :text "reinit rotations" :size (omp 120 nil) :font (om-def-font :font1)
                             :di-action #'(lambda (item) 
                                            (declare (ignore item))
                                            (init-state obj)
                                            (update-g-components editor)
                                            (when (editor-get-edit-param editor :filter-off-grid) (update-lines editor))
                                            (om-invalidate-view (om-invalidate-view (get-g-component editor :3d-view)))))))
               ))
                   
             ;;; ROTATION
             (om-make-layout 
              'om-column-layout
              :subviews
              (list 
               (om-make-di 'om-simple-text :text "Rotation values" :size (omp 240 21) :font (om-def-font :font1b))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "center-x:" :size (omp 55 22) :font (om-def-font :font1))
                 (get-g-component editor :center-x-numbox)
                 (om-make-di 'om-simple-text :text "center-y:" :size (omp 55 22) :font (om-def-font :font1))
                 (get-g-component editor :center-y-numbox)
                 ))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "rotation-x:" :size (omp 60 20) :font (om-def-font :font1))
                 (get-g-component editor :rotation-x-slider)
                 (get-g-component editor :rotation-x-numbox)))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "rotation-y:" :size (omp 60 20) :font (om-def-font :font1))
                 (get-g-component editor :rotation-y-slider)
                 (get-g-component editor :rotation-y-numbox)))

               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "rotation-z:" :size (omp 60 20) :font (om-def-font :font1))
                 (get-g-component editor :rotation-z-slider)
                 (get-g-component editor :rotation-z-numbox)))
               ))

             ;;; GRID
             (om-make-layout 
              'om-column-layout
              :subviews
              (list 
               (om-make-di 'om-simple-text :text "Grid" :size (omp 200 22) :font (om-def-font :font1b))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "x-min:" :size (omp 40 22) :font (om-def-font :font1))
                 (get-g-component editor :x-grid-min-numbox)
                 (om-make-di 'om-simple-text :text "x-max:"  :size (omp 40 22) :font (om-def-font :font1))
                 (get-g-component editor :x-grid-max-numbox)
                 ))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-simple-text :text "y-min:" :size (omp 40 22) :font (om-def-font :font1))
                 (get-g-component editor :y-grid-min-numbox)   
                 (om-make-di 'om-simple-text :text "y-max:" :size (omp 40 22) :font (om-def-font :font1))
                 (get-g-component editor :y-grid-max-numbox)
                 ))
                
               (om-make-layout 
                'om-row-layout
                :subviews
                (list 
                 (om-make-di 'om-check-box :text "show" :size (omp 100 20) :font (om-def-font :font1)
                             :checked-p (editor-get-edit-param editor :show-grid)
                             :di-action #'(lambda (item) 
                                            (editor-set-edit-param editor :show-grid (print (om-checked-p item)))
                                            (om-invalidate-view (get-g-component editor :3d-view))))
                 
                 (om-make-di 'om-button :text "fit" :size (omp 60 nil) :font (om-def-font :font1)
                             :di-action #'(lambda (item) 
                                            (declare (ignore item))
                                            (editor-set-edit-param editor :x-grid :auto)
                                            (editor-set-edit-param editor :y-grid :auto)
                                            (init-grid editor)
                                            (update-g-components editor)
                                            (om-invalidate-view (om-invalidate-view (get-g-component editor :3d-view)))))
                 ))
                
               (om-make-di 'om-check-box :text "filter off-grid" :size (omp 100 20) :font (om-def-font :font1)
                           :checked-p (editor-get-edit-param editor :filter-off-grid)
                           :di-action #'(lambda (item) 
                                          (editor-set-edit-param editor :filter-off-grid (om-checked-p item))
                                          (update-lines editor)
                                          (om-invalidate-view (get-g-component editor :3d-view))))

               ))
             ))
           ))

      (set-g-component editor :3D-view 
                       (om-make-view '3D-viewer-view
                                     :editor editor
                                     :bg-color (editor-get-edit-param editor :3D-bg-color)))
     
      (om-make-layout 
       'om-row-layout :ratios '(9.9 0.1) 
       :subviews 
       (list 
        (om-make-layout 
         'om-column-layout :ratios '(99 1) 
         :subviews 
         (list 
          (get-g-component editor :3d-view) 
          control-view))
        (make-default-editor-view editor)))
  
      )))



(defmethod update-3D-contents ((editor 3d-viewer-editor))
  (let ((obj (object-value editor))
        (3D-view (get-g-component editor :3D-view)))
    ;;; works better if the objects are set after everything is on-screen
    (om-set-gl-objects 
     3D-view
     (loop for ob in (data obj) collect 
           (let ((view-ob (om-copy ob)))
             (scale-object view-ob :x (scaler-x obj) :y (scaler-y obj) :z (scaler-z obj))
             (om-update-3Dobj view-ob)
             view-ob)
           ))
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
    (set-3D-viewer-controls-from-object editor)
    (let ((3D-view (get-g-component editor :3D-view)))
      (update-3D-contents editor)
      (gl-user::clear-gl-display-list 3D-view)
      (om-invalidate-view 3D-view)
      )))

(defmethod update-to-editor ((editor 3d-viewer-editor) (from t))
  (when (window editor)
    (set-3D-viewer-controls-from-object editor)
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
        o
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
    
    (when (and (editor-get-edit-param ed :show-grid)
               (x-grid ed) (y-grid ed))
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
     '(1.0 1.0 1.0) 1.0 10.0)
    
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
  (let* ((ed (editor self))
         (3DV (object-value ed)))
    (case key
      (#\+ (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 0.6))
           (om-invalidate-view self))
      (#\- (setf (gl-user::xyz-z (gl-user::eye (gl-user::camera self)))
                 (* (gl-user::xyz-z (gl-user::eye (gl-user::camera self))) 1.2))
           (om-invalidate-view self))

      (:om-key-right 
       (if (om-command-key-p) (call-next-method) ;; will go next in a collection editor
         (let ((new-x (* (round (* (+ (nth 0 (center 3DV)) (max 0.1 (/ 1 (scaler-x 3DV)))) 10)) 0.1)))
           (setf (nth 0 (center 3DV)) new-x)
           (set-rotation-param-from-editor ed 'center (center 3DV))
           (set-value (get-g-component ed :center-x-numbox) new-x)
           (om-invalidate-view self))))
      (:om-key-left 
       (if (om-command-key-p) (call-next-method) ;; will go previous in a collection editor
         (let ((new-x (* (round (* (- (nth 0 (center 3DV)) (max 0.1 (/ 1 (scaler-x 3DV)))) 10)) 0.1)))
           (setf (nth 0 (center 3DV)) new-x)
           (set-rotation-param-from-editor ed 'center (center 3DV))
           (set-value (get-g-component ed :center-x-numbox) new-x)
           (om-invalidate-view self))))
      (:om-key-up 
       (let ((new-y (* (round (* (+ (nth 1 (center 3DV)) (max 0.1 (/ 1 (scaler-y 3DV)))) 10)) 0.1)))
         (setf (nth 1 (center 3DV)) new-y)
         (set-rotation-param-from-editor ed 'center (center 3DV))
         (set-value (get-g-component ed :center-y-numbox) new-y)
         (om-invalidate-view self)))
      (:om-key-down 
       (let ((new-y (* (round (* (- (nth 1 (center 3DV)) (max 0.1 (/ 1 (scaler-y 3DV)))) 10)) 0.1)))
         (setf (nth 1 (center 3DV)) new-y)
         (set-rotation-param-from-editor ed 'center (center 3DV))
         (set-value (get-g-component ed :center-y-numbox) new-y)
         (om-invalidate-view self)))
       
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

      (#\o (init-state 3DV)
           (update-from-editor (object ed) :reactive nil)
           (update-g-components ed)
           (when (editor-get-edit-param ed :filter-off-grid) (update-lines ed))
           (om-invalidate-view self))
      (#\O (setf (center 3DV) (list 0 0 0))
           (update-from-editor (object ed) :reactive nil)
           (update-g-components ed)
           (when (editor-get-edit-param ed :filter-off-grid) (update-lines ed))
           (om-invalidate-view self))
      
      (#\c (set-bg-color ed))

      (#\Space (report-modifications ed))
      
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

   
   (update-g-components ed)
   (when (editor-get-edit-param ed :filter-off-grid) (update-lines ed))
   (update-from-editor (object ed) :reactive nil)

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
   
    (update-g-components ed)
    (when (editor-get-edit-param ed :filter-off-grid) (update-lines ed))
    (update-from-editor (object ed) :reactive nil)
      
    (opengl:rendering-on (self)
      (gl-user::opengl-redisplay-canvas self))
    
    (setf (gl-user::lastxy self) (cons x y))))



