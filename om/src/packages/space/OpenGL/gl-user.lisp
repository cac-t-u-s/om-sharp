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

;===========================================================================
; Mid-level interface between LW OpenGL and OM 
; Adapted from the OpenGL example in LispWorks distribution
;;===========================================================================

(defpackage :gl-user
  (:use :common-lisp))

(in-package :gl-user)

;;; ------------------------------------------------------------
;;; Types and creators.

(deftype gl-double () 'double-float)
(deftype gl-double-vector (n) `(opengl:gl-vector :double ,n))
(deftype gl-single () 'single-float)
(deftype gl-single-vector (n) `(opengl:gl-vector :float ,n))

(defun make-gl-double-vector (size)
  (opengl:make-gl-vector :double size))

(defun make-gl-single-vector (size)
  (opengl:make-gl-vector :float size))

(defun gl-float-vector (type contents)
  (opengl:make-gl-vector type (length contents) :contents contents))

(defun gl-double-vector (&rest contents)
  (gl-float-vector :double contents))

(defun gl-single-vector (&rest contents)
  (gl-float-vector :float contents))



;;; ------------------------------
;;; Vertex can be pass through to 'C'
;;; vertexes list of gl-vertexes (not passed to 'C')
;;; ------------------------------

(declaim (inline gl-vertex gl-vertexes))
(defun gl-vertex (x y z w)
  (gl-double-vector x y z w))

(defun gl-vertexes (contents)
  (mapcar #'(lambda (c) (apply 'gl-double-vector c)) contents))


;;; ------------------------------
;;; XYZ coordinate
;;; ------------------------------

(defstruct xyz 
  (x 0.0d0 :type double-float)
  (y 0.0d0 :type double-float)
  (z 0.0d0 :type double-float))


;;; ------------------------------------------------------------
;;; Geometry Utilities

(defun vector-difference (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (- (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun vector-sum (v1 v2 res)
  (loop for i fixnum below 3 do
        (setf (opengl:gl-vector-aref res i) (+ (opengl:gl-vector-aref v1 i)
                                               (opengl:gl-vector-aref v2 i))))
  res)

(defun normalize (vector)
  (let* ((x (opengl:gl-vector-aref vector 0))
         (y (opengl:gl-vector-aref vector 1))
         (z (opengl:gl-vector-aref vector 2))
         (d (sqrt (+ (* x x) (* y y) (* z z)))))
    (if (zerop d)
        (error "Can't normalize a zero-length vector! ~s" vector)
      (setf (opengl:gl-vector-aref vector 0) (/ x d)
            (opengl:gl-vector-aref vector 1) (/ y d)
            (opengl:gl-vector-aref vector 2) (/ z d)))
    vector))

(defun normalized-cross-product (v1 v2 result)
  (let ((res (or result (make-gl-double-vector (length v1)))))
    (declare (type (gl-double-vector (*)) res))
    (setf (opengl:gl-vector-aref res 0) (- (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 2))
                                           (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 1)))
          (opengl:gl-vector-aref res 1) (- (* (opengl:gl-vector-aref v1 2)
                                              (opengl:gl-vector-aref v2 0))
                                           (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 2)))
          (opengl:gl-vector-aref res 2) (- (* (opengl:gl-vector-aref v1 0)
                                              (opengl:gl-vector-aref v2 1))
                                           (* (opengl:gl-vector-aref v1 1)
                                              (opengl:gl-vector-aref v2 0))))
    (normalize res)
    res))


;;;=============================
;;; TEXT

(defun set-up-gl-fonts (pane obj)
  #+Win32
  (when (name obj)
    (unless (assoc :font (extra-display-lists obj))
      (push (list :font
                  (win32::wgl-use-font pane
                                       :start 0
                                       :count 256
                                       :outlinep t)
                  256)
            (extra-display-lists obj)))))

#+Win32
(defmacro with-3d-text-state-saved (&body body)
  `(opengl:with-matrix-pushed
     (opengl:gl-push-attrib opengl:*gl-all-attrib-bits*)
     ,@body
     (opengl:gl-pop-attrib)))

#+Win32
(defun draw-3d-text (obj text)
  (let* ((base (second (assoc :font (extra-display-lists obj)))))
    ;; Set up for a string-drawing display list call.
    (opengl:gl-list-base base)
    ;; Draw a string using font display lists.
    (fli:with-foreign-string (ptr elts bytes
                                  :external-format win32:*multibyte-code-page-ef*
                                  :null-terminated-p nil)
        text
      (declare (ignore bytes))
      (opengl:gl-call-lists elts
                            opengl:*gl-unsigned-byte*
                            ptr))))

#+Win32
(defun draw-positioned-3d-text (obj text
                                    x-pos y-pos z-pos
                                    x-rotation y-rotation z-rotation
                                    scale)
  (with-3d-text-state-saved
    (opengl:gl-translated x-pos y-pos z-pos)
    (opengl:gl-scaled scale scale scale)
    (opengl:gl-rotated x-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated y-rotation 1.0d0 0.0d0 0.0d0)
    (opengl:gl-rotated z-rotation 0.0d0 0.0d0 1.0d0)
    ;; Draw the text.
    (draw-3d-text obj text)))

;;; ------------------------------------------------------------
;;; TEXTURE
;;; 

(defvar *texture-color* nil)

;;; Setup code
(defun get-texture-color ()
  (unless *texture-color*
    (setf *texture-color* (gl-single-vector 1.0 1.0 1.0 1.0)))
  *texture-color*)

;;; Cleanup code
(defun cleanup-texture-color ()
  (setf *texture-color* nil))

(defun make-object-colors (npolys &optional style)
  (case style
    (:uniform (make-array npolys
                          :initial-element (gl-single-vector (float (random 1.0) 1.0)
                                                             (float (random 1.0) 1.0)
                                                             (float (random 1.0) 1.0) 1.0)))
    (otherwise (coerce (loop for i below npolys collect
                             (gl-single-vector (float (random 1.0) 1.0)
                                               (float (random 1.0) 1.0)
                                               (float (random 1.0) 1.0) 1.0))
                       'vector))))

(defun make-object-vertexes (vertex-list)
  (coerce (gl-vertexes vertex-list) 'simple-vector))

(defun make-object-indexes (index-list)
  (let* ((biggest (loop for i in index-list maximize (length i)))
         (indexes (make-array (list (length index-list) biggest)
                              :element-type 'fixnum
                              :initial-element -1)))
    (loop for id in index-list
          for i from 0 do
          (loop for jd in id
                for j from 0 do
                (setf (aref indexes i j) jd)))
    indexes))

;;; ------------------------------------------------------------
;;; Class object
;;; 
;;; This superclass just manages display lists for the subclasses.

(defclass gl-object ()
  ((name :initarg :name :accessor name :initform nil)
   (use-display-list :initform nil :initarg :use-display-list :accessor use-display-list)
   (display-list :initform nil :accessor display-list)
   (extra-display-lists :initform nil :accessor extra-display-lists)
   (viewer :accessor viewer)))


(defmethod draw :around ((object gl-object))
  (if (use-display-list object)
      (if (display-list object)
          (progn
            (opengl:gl-call-list (display-list object))
            (let ((draw (sys:cdr-assoc :draw (extra-display-lists object))))
              (mapc 'opengl:gl-call-list draw)))
        (progn
          (set-up-gl-fonts (viewer object) object)
          (let ((n (opengl:gl-gen-lists 1)))
	    (if (plusp n)
                (progn
	          (opengl:gl-new-list n opengl:*gl-compile-and-execute*)
                  (call-next-method)
	          (opengl:gl-end-list)
	          (setf (display-list object) n))
	      (progn 
	        (format t "~%~s:No more display list indexes!" object)
                (call-next-method))))))
    (call-next-method)))

(defmethod draw ((object gl-object))
  (print (list "gl-user drawing object" object)))

(defmethod (setf use-display-list) :after (value (object gl-object))
  (unless value
    (delete-display-list object)))

(defmethod delete-display-list ((object gl-object))
  (when (display-list object)
    (opengl:gl-delete-lists (display-list object) 1)
    (setf (display-list object) nil))
  (loop for (nil start length) in (extra-display-lists object)
        do (opengl:gl-delete-lists start length))
  (setf (extra-display-lists object) nil))


;;======================
;; INTERFACE  (exported functions and classes)
;;======================

;;; Main OpenGL view superclass
(defclass opengl-view (opengl:opengl-pane)
  ((g-objects :initarg :g-objects :accessor g-objects :initform nil)
   (icotransform :initform nil :initarg :icotransform :accessor icotransform)
   (light-transform :initform nil :initarg :light-transform :accessor light-transform)
   (object-transform :initform nil :initarg :object-transform :accessor object-transform)
   (double-buffered-p :initform t :initarg :double-buffered-p :accessor double-buffered-p)
   (lastxy :initform nil :initarg :lastxy :accessor lastxy)
   (camera :initform (make-camera :color '(0.9 0.9 0.9 1.0)) :initarg :camera :accessor camera))
  (:default-initargs 
   :configuration
   #-linux (list :rgba t :depth t :double-buffered t :depth-buffer 64) ;depth buffer allows to have depth in 3D drawing
   #+linux (list :rgba t :depth nil :double-buffered t)
   :use-display-list t
   :display-callback 'opengl-redisplay-canvas
   :resize-callback 'opengl-resize-canvas
   :input-model '(((:button-1 :press) opengl-viewer-click)
                  ((:button-1 :shift :press) opengl-viewer-shift-click)
                  ((:button-1 :meta :press) opengl-viewer-alt-click)
                  ((:motion :button-1) opengl-viewer-motion-click)
                  ((:motion :button-1 :shift) opengl-viewer-motion-shift-click)
                  ((:motion :button-1 :meta) opengl-viewer-motion-alt-click)
                  ((:button-1 :second-press) opengl-viewer-double-click)
                  (:gesture-spec opengl-viewer-key-pressed))
   ))

(defmethod (setf g-objects) :before (new-object-list (viewer opengl-view))
  (opengl:rendering-on (viewer)
    (mapc #'delete-display-list (g-objects viewer))))

(defmethod (setf g-objects) :after (new-object-list (viewer opengl-view))
  (when new-object-list
    (mapc #'(lambda (g-object) (setf (viewer g-object) viewer)) (g-objects viewer)))
  (opengl-redisplay-canvas viewer))

(defmethod set-gl-object-list ((self opengl-view) globject-list)
  (setf (g-objects self) globject-list))

(defmethod get-gl-object-list ((self opengl-view))
  (g-objects self))



;;; ------------------------------------------------------------
;;; Class projection
;;; 
;;; A class which defines the fovy, aspect, near and far
;;; values for a call to glu-perspective to define the projection
;;; matrix.

(defparameter *fovy* 45.0d0)
(defparameter *aspect* 1.0d0)
(defparameter *near* 0.2d0)
(defparameter *far* 100.0d0)


(defclass projection (gl-object)
   ((fovy :initform *fovy* :initarg :fovy :accessor fovy)
    (aspect :initform *aspect* :initarg :aspect :accessor aspect)
    (near :initform *near* :initarg :near :accessor near)
    (far :initform *far* :initarg :far :accessor far)))

(defmethod draw ((projection projection))
  (opengl:glu-perspective (fovy projection) (aspect projection) (near projection) (far projection))
  ;(opengl:gl-ortho -1000.0D0 1000.0D0 -1000.0D0 1000.0D0 -1000.0D0 1000.0D0)
  )

(defun make-projection (&key fovy aspect near far)
  (make-instance 'projection
                 :fovy (or fovy *fovy*)
                 :aspect (or aspect *aspect*)
                 :near (or near *near*)
                 :far (or far *far*)))
                                       

;;; ------------------------------------------------------------
;;; Class camera
;;; 
;;; Defines an eye point, a center point and an up vector.
;;; The draw method calls GLU-LOOK-AT to install the camera values.
;;; The up vector is set to y

(defparameter *eye* (make-xyz :y 1.0d0))
(defparameter *center* (make-xyz :y 1.0d0))
(defparameter *up* (make-xyz :z 1.0d0))

(defclass camera (gl-object)
  ((eye :initform (copy-structure *eye*)
        :initarg :eye
        :accessor eye
        :type xyz)
   (center :initform (copy-structure *center*)
           :initarg :center
           :accessor center
           :type xyz)
   (up :initform (copy-structure *up*)
       :initarg :up
       :accessor up
       :type xyz)
   (projection :initform (make-projection)
               :initarg :projection
               :accessor projection)
   (bgcolor :initform nil
            :initarg :bgcolor
            :accessor bgcolor)
   (position-transform :initform nil 
                       :initarg :position-transform 
                       :accessor position-transform)
   ))

(defmethod draw ((camera camera))
  (let ((eye (eye camera))
        (center (center camera))
        (up (up camera))
        (projection (projection camera)))
    (declare (type xyz up eye center))

    (opengl:gl-matrix-mode opengl:*gl-projection*)
    (opengl:gl-load-identity)
    (draw projection)
    (opengl:gl-matrix-mode opengl:*gl-modelview*)
    (opengl:gl-load-identity)
    (opengl:glu-look-at (xyz-x eye) (xyz-y eye) (xyz-z eye)
                        (xyz-x center) (xyz-y center) (xyz-z center)
                        (xyz-x up) (xyz-y up) (xyz-z up))
    
    
    ;;; move the viewpoint
    (opengl:gl-translated (xyz-x center) (xyz-y center) (xyz-z center))
    (opengl:gl-mult-matrixd (position-transform camera))
    (opengl:gl-translated (- (xyz-x center)) (- (xyz-y center)) (- (xyz-z center)))
    
    ;(opengl:gl-enable opengl:*gl-lighting*) ;; can't get it to work correctly..

    (when (bgcolor camera)
      (opengl:gl-clear-color (car (bgcolor camera)) (cadr (bgcolor camera)) (caddr (bgcolor camera)) (cadddr (bgcolor camera)) ))
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)
    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    (opengl:gl-depth-func opengl:*gl-less*)
    (opengl:gl-enable opengl:*gl-depth-test*)))


(defun make-camera (&key eye center up projection color)
  
  (make-instance 'camera
                 :eye (copy-structure (or eye *eye*))
                 :center (copy-structure (or center *center*))
                 :up (copy-structure (or up *up*))
                 :projection (or projection (make-projection))
                 :bgcolor (or color '(0.9 0.9 0.9 1.0)))
  )

(defun init-camera (camera)
  (setf (eye camera) (copy-structure *eye*))
  (setf (center camera) (copy-structure *center*))
  (setf (up camera) (copy-structure *up*))
  (setf (projection camera) (make-projection))
  (setf (position-transform camera) (make-gl-double-vector 16))
  (initialize-transform (position-transform camera))
  camera)
  

;;; ------------------------------------------------------------
;;; The CAPI Interface
;;; ------------------------------------------------------------
    
(defun initialize-transform (transform)
  (opengl:gl-matrix-mode opengl:*gl-modelview*)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun initialize-viewer (canvas)
  ;; Initialize the icotransform to unity.
  (opengl:rendering-on (canvas)
    (init-camera (camera canvas)) ;; new projeaction, needs to be adjusted to the view ratio
    (setf (icotransform canvas) (make-gl-double-vector 16))
    (setf (light-transform canvas) (make-gl-double-vector 16))
    (setf (object-transform canvas) (make-gl-double-vector 16))
    (initialize-transform (icotransform canvas))
    (initialize-transform (light-transform canvas))
    (initialize-transform (object-transform canvas))
    (set-lights-and-materials)
    (multiple-value-bind (w h) (capi::pinboard-pane-size canvas)
      (opengl-resize-canvas canvas 0 0 w h))))

(defun initialize-icotransform (canvas)
  (setf (icotransform canvas) (make-gl-double-vector 16))
  (initialize-transform (icotransform canvas)))

(defparameter *pointer-rotation-gain* 0.1d0)

(defun polar-rotate (transform &key dx dy dz)
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (when dx (opengl:gl-rotated (float (* dx *pointer-rotation-gain*) 1.0d0) 1.0d0 0.0d0 0.0d0))
    (when dy (opengl:gl-rotated (float (* dy *pointer-rotation-gain*) 1.0d0) 0.0d0 1.0d0 0.0d0))
    (when dz (opengl:gl-rotated (float (* dz *pointer-rotation-gain*) 1.0d0) 0.0d0 0.0d0 1.0d0))
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))

(defun translate (transform &key (dx 0.0d0) (dy 0.0d0) (dz 0.0d0))
  (opengl:with-matrix-pushed
    (opengl:gl-load-identity)
    (opengl:gl-translated (float dx 1.0d0) (float dy 1.0d0) (float dz 1.0d0))
    (opengl:gl-mult-matrixd transform)
    (opengl:gl-get-doublev opengl:*gl-modelview-matrix* transform)))


(defun polar-rotate-light (viewer &key dx dy dz)
  (polar-rotate (light-transform viewer) :dx dx :dy dy :dz dz))

(defun polar-rotate-icosahedron (viewer &key dx dy dz)
  (polar-rotate (icotransform viewer)  :dx dx :dy dy :dz dz))

(defun translate-icosahedron (viewer dx dy)
  (let ((factor (/ (xyz-y (eye (camera viewer))) 1500)))
    (translate (icotransform viewer) :dx (* dx factor) :dz (* dy factor))))

;;; camera in canvas à la place de interface
(defun opengl-resize-canvas (canvas x y width height)
  (when #+Win32 (win32:is-window-visible (win32:pane-hwnd (capi-internals:representation canvas)))
    #-Win32 T
    (opengl:rendering-on (canvas)
      (opengl:gl-viewport 0 0 width height))
    (setf (aspect (projection (camera canvas)))
          (coerce (/ width height) 'double-float))
    (opengl-redisplay-canvas canvas)))


(defparameter *light-model-ambient* nil)
(defparameter *light-position* nil)
(defparameter *light-ambient* nil)
(defparameter *light-diffuse* nil)
(defparameter *light-specular* nil)
(defparameter *material-specular* nil)
(defparameter *material-shininess* 25.0)
(defparameter *material-emission* nil)

;; must be called at runtime because of gl-vector pointers
(defun set-lights-and-materials ()
  (setf *light-model-ambient* (gl-single-vector 0.4 0.4 0.4 1.0))  ;; (gl-single-vector 0.0 0.0 0.0 1.0)
  ;(setf *light-position* (gl-single-vector 10.0 10.0 10.0 1.0))
  (setf *light-position* (gl-single-vector 10.0 10.0 10.0 0.0))
  (setf *light-ambient* (gl-single-vector 0.1 0.1 0.1 1.0))
  (setf *light-diffuse* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *light-specular* (gl-single-vector 0.8 0.8 0.8 1.0))
  (setf *material-specular* (gl-single-vector 0.1 0.1 0.1 1.0))  ;; (gl-single-vector 0.1 0.0 0.0 1.0)
  (setf *material-shininess* 25.0)  ;; 64.0
  (setf *material-emission* (gl-single-vector 0.0 0.0 0.0 1.0))  ;; (gl-single-vector 0.0 0.0 0.0 1.0)
)

(defun ensure-gl-vector (check)
  (unless check (set-lights-and-materials)))

(defun opengl-redisplay-canvas (canvas &rest ignore)
  ignore
  (unless (icotransform canvas)
    (initialize-viewer canvas))
  (opengl:rendering-on (canvas)
    (if *om-3d-anaglyph*
        (opengl-redisplay-canvas-anaglyph canvas)
      (opengl-redisplay-canvas-standard canvas))
    ;swap buffers if double buffered
    (when (double-buffered-p canvas)
      (opengl:swap-buffers canvas))
    ))


(defun opengl-redisplay-canvas-standard (canvas)
  (opengl:gl-draw-buffer opengl:*gl-back*)
  (opengl:gl-clear opengl:*gl-color-buffer-bit*)
  (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
  (opengl:gl-color-mask 1 1 1 1)
  ;draw the camera (background and view position)
  (unless (position-transform (camera canvas)) (init-camera (camera canvas)))
  (draw (camera canvas))
  ;apply transform and render canvas light and objects
  (opengl-redisplay-all canvas)
  )


(defun opengl-redisplay-all (canvas)
  
  (ensure-gl-vector (and *material-specular* 
                         *material-emission* 
                         *light-model-ambient* 
                         *light-position* 
                         *light-ambient* 
                         *light-diffuse* 
                         *light-specular*))
  
  (opengl:with-matrix-pushed
      
    (opengl:gl-mult-matrixd (light-transform canvas))
      
    (opengl:gl-light-modelfv opengl:*gl-light-model-ambient* *light-model-ambient*)
    (opengl:gl-light-modelf opengl:*gl-light-model-local-viewer* 0.0)
    (opengl:gl-light-modelf opengl:*gl-light-model-two-side* 0.0)
      
    (opengl:gl-enable opengl:*gl-light0*)
    (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-position* *light-position*)
    (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-ambient* *light-ambient*)
    (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-diffuse* *light-diffuse*)
    (opengl:gl-lightfv opengl:*gl-light0* opengl:*gl-specular* *light-specular*)
  
    )

  (opengl:with-matrix-pushed
    
    ;;(opengl:gl-shade-model opengl:*gl-smooth*)
       
    ;material stuff
    (opengl:gl-cull-face opengl:*gl-back*)
    (opengl:gl-enable opengl:*gl-cull-face*)
      
    (opengl:gl-enable opengl:*gl-color-material*)
    (opengl:gl-color-material opengl:*gl-front* opengl:*gl-ambient-and-diffuse*)
      
    (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-specular* *material-specular*)
    (opengl:gl-materialf opengl:*gl-front* opengl:*gl-shininess* *material-shininess*)
    (opengl:gl-materialfv opengl:*gl-front* opengl:*gl-emission* *material-emission*)
    
    (opengl:gl-mult-matrixd (icotransform canvas))
    ;Draw the content of the pane ant the objects
    (draw-contents canvas)
    
    (opengl:with-matrix-pushed 
      (opengl:gl-mult-matrixd (object-transform canvas))
      (mapc #'draw (g-objects canvas)))
    )
  )

;tobe redefined
(defmethod draw-contents (canvas) (print (list "draw canvas" canvas)))


;;;===========================
;;; ANAGLYPH MODE 
;;;===========================

(defparameter *om-3d-anaglyph* nil)
(defparameter *om-3d-anaglyph-eye-dist* 0.2d0)

(defun opengl-anaglyph-p () *om-3d-anaglyph*)
(defun opengl-enable-or-disable-anaglyph (t-or-nil)
  (setf *om-3d-anaglyph* t-or-nil))

(defun opengl-set-anaglyph-eye-dist (dist)
  (setf *om-3d-anaglyph-eye-dist* (coerce dist 'double-float)))

(defun opengl-redisplay-canvas-anaglyph (canvas)
  (let* ((camera (camera canvas))
         (eye (eye camera))
         (center (center camera))
         (up (up camera))
         (projection (projection camera))
         (camera-left (make-camera :eye eye :center center :up up :color '(0.95 0.95 0.95 1.0) :projection projection))
         (camera-right (make-camera :eye eye :center center :up up :color '(0.95 0.95 0.95 1.0) :projection projection)))
    
    (unless (position-transform camera-left)
      (setf (position-transform camera-left) (make-gl-double-vector 16))
      (initialize-transform (position-transform camera-left)))
    (unless (position-transform camera-right)
      (setf (position-transform camera-right) (make-gl-double-vector 16))
      (initialize-transform (position-transform camera-right)))
    
    ;colors
    ;left = red
    ;right = blue
    ;set camera eye offset
    (setf (xyz-x (eye camera-left)) (- *om-3d-anaglyph-eye-dist*))
    (setf (xyz-x (eye camera-right)) *om-3d-anaglyph-eye-dist*)
    ;clear depth and color buffers for back buffer
    (opengl:gl-draw-buffer opengl:*gl-back*)
    (opengl:gl-clear opengl:*gl-color-buffer-bit*)
    (opengl:gl-clear opengl:*gl-depth-buffer-bit*)
    ;;;;;;;;RED;;;;;;;;;;;;;
    ;set the rendering buffer for red
    (opengl:gl-draw-buffer opengl:*gl-back-left*)
    (opengl:gl-matrix-mode opengl:*gl-modelview*)
    (opengl:gl-load-identity)
    (opengl:gl-color-mask 1 0 0 1)
    (draw camera-left)
    ;apply transform and render canvas lights and objects
    (opengl-redisplay-all canvas)
    ;;;;CYAN;;;;;;;;;;;;
    ;set the rendering buffer for cyan
    (opengl:gl-draw-buffer opengl:*gl-back-right*)
    (opengl:gl-matrix-mode opengl:*gl-modelview*)
    (opengl:gl-load-identity)
    (opengl:gl-color-mask 0 1 1 1)
    (draw camera-right)
    ;apply transform and render canvas lights and objects
    (opengl-redisplay-all canvas)
  )
)



;;; USER INTERACTION
(defmethod opengl-viewer-click (canvas x y)
  (setf (lastxy canvas) (cons x y)))
(defmethod opengl-viewer-shift-click (canvas x y)
  (opengl-viewer-click canvas x y))
(defmethod opengl-viewer-alt-click (canvas x y)
  (opengl-viewer-click canvas x y))

(defmethod opengl-viewer-motion-click (canvas x y)
    (let ((last (lastxy canvas)))
      (when last
        (opengl:rendering-on (canvas)
	  (polar-rotate-icosahedron canvas :dz (* 2 (- x (car last))) :dx (* 2 (- y (cdr last)))))
        (opengl-redisplay-canvas canvas))
      (setf (lastxy canvas) (cons x y))))

(defmethod opengl-viewer-motion-shift-click (canvas x y)
    (let ((last (lastxy canvas)))
      (when last
        (let ((eye (eye (camera canvas))))
          (setf (xyz-y eye)
                (min (- (xyz-y eye) (* (/ (- (cdr last) y) 20) (/ (xyz-y eye) 20))) -0.1d0))
          )
        (opengl-redisplay-canvas canvas))
      (setf (lastxy canvas) (cons x y))))

(defmethod opengl-viewer-motion-alt-click (canvas x y)
  (let ((last (lastxy canvas)))
    (when last
      (opengl:rendering-on (canvas)
        (translate-icosahedron canvas (- (car last) x) (- y (cdr last)) ))
      (opengl-redisplay-canvas canvas))
    (setf (lastxy canvas) (cons x y))))

(defmethod opengl-viewer-double-click (canvas x y) nil)

;; handles key press using CAPI 'gesture-spec'
;; to be redefined in opengl-view
(defmethod opengl-viewer-key-pressed (canvas x y spec) nil)


;(defmethod init-3D-view ((self opengl-view))
;  (init-camera (camera self))
;  (setf (aspect (projection (camera self)))
;        (coerce (/ (capi::pane-width self) (capi::pane-height self)) 'double-float))
;  (initialize-viewer self)
;  (opengl-redisplay-canvas self))

