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

;;;======================
;;; om-3D-object
;;;======================

(defclass om-3D-object (gl-user::gl-object)
  ((color :accessor color :initarg :color :initform nil)
   (points :initarg :points :accessor points :initform nil)
   (glvertexes :accessor glvertexes :initarg :glvertexes :initform nil))
  (:default-initargs :use-display-list nil))

;;; for 3D objects we decide to copy only the initargs (as for omobjects)
(defmethod condition-for-copy-slot ((from om-3D-object) (to t) slot)
  (and (call-next-method) (slot-definition-initargs slot)))

(defun points2vertex (points)
  (gl-user::make-object-vertexes 
   (mapcar #'(lambda (p)
               (list
                (coerce (car p) 'double-float) 
                (coerce (cadr p) 'double-float) 
                (coerce (caddr p) 'double-float)
                1.0d0))
           points)))

(defmethod initialize-instance :after ((self om-3D-object) &key points &allow-other-keys)
  (let ((pts (or points (points self))))
    (setf (glvertexes self) (points2vertex pts))))

(defmethod gl-user::draw ((self om-3D-object))
  (activate-anti-aliasing-parameters)
  (om-draw-contents self))

  ;anti aliasing things. Warning: if depth enable it may not work...
(defun activate-anti-aliasing-parameters ()
  (opengl:gl-enable opengl:*gl-blend*)
  (opengl:gl-enable opengl:*gl-line-smooth*)
  (opengl:gl-blend-func opengl:*gl-src-alpha* opengl:*gl-one-minus-src-alpha*)
  (opengl:gl-hint opengl:*gl-line-smooth-hint* opengl:*gl-dont-care*)
  )

(defmethod om-draw-contents ((self om-3D-object)) nil)
           
(defmethod om-get-gl-points ((self om-3D-object))
  (glvertexes self))

(defmethod om-3Dobj-points ((self om-3D-object))
  (points self))

(defmethod om-set-3Dobj-points ((self om-3D-object) points)
  (setf (points self) points)
  (setf (glvertexes self) (points2vertex points))
  self)

(defmethod om-update-3Dobj ((self om-3D-object))
  (setf (glvertexes self) (points2vertex (points self))))


;naive implementation
(defmethod om-append-3Dobj-point ((self om-3D-object) point)
  (setf (points self) (append (points self) point))
  (setf (glvertexes self) (points2vertex (points self))))

(defmethod om-3Dobj-color ((self om-3D-object))
  (color self))

(defmethod get-extents ((self om-3D-object))
  (when (om-3Dobj-points self)
    (let* ((x-y-z (mat-trans (om-3Dobj-points self)))
           (xpts (nth 0 x-y-z)) 
           (ypts (nth 1 x-y-z))
           (zpts (nth 2 x-y-z)))
      (values 
       (reduce 'min xpts)
       (reduce 'max xpts)
       (reduce 'min ypts)
       (reduce 'max ypts)
       (reduce 'min zpts)
       (reduce 'max zpts)))))

(defmethod get-extents ((self list))
  (let (xmins xmaxs ymins ymaxs zmins zmaxs)
    (when self
      (loop for elt in self do
            (multiple-value-bind (xmin xmax ymin ymax zmin zmax)
                (get-extents elt)
              (push xmin xmins)
              (push xmax xmaxs)
              (push ymin ymins)
              (push ymax ymaxs)
              (push zmin zmins)
              (push zmax zmaxs)))
      (values 
       (reduce 'min xmins)
       (reduce 'max xmaxs)
       (reduce 'min ymins)
       (reduce 'max ymaxs)
       (reduce 'min zmins)
       (reduce 'max zmaxs)))))
            
            
;;;;;;;;;;;;;LIST OF 3D OBJ;;;;;;;;;;;;;

#|
(defclass om-3D-object-list (gl-user::gl-object)
  ((objects :accessor objects :initarg :objects :initform nil)))

(defmethod gl-user::draw ((self om-3D-object-list)) 
  (mapcar 'om-draw-contents (objects self)))

(defmethod om-get-3D-objects ((self om-3D-object-list))
  (objects self))

(defmethod om-3Dobj-points ((self om-3D-object-list))
  (apply 'append (mapcar 'om-3Dobj-points (objects self))))

(defmethod set-draw-style ((self om-3D-object-list) val)
  (mapcar #'(lambda (curve) (set-draw-style curve val)) (om-get-3D-objects self)))

|#


;;; COMMENT: /* QuadricDrawStyle */
;;; DEFINE: #define GLU_POINT                          100010
(defconstant GLU_POINT 100010)
;;; DEFINE: #define GLU_LINE                           100011
(defconstant GLU_LINE 100011)
;;; DEFINE: #define GLU_FILL                           100012
(defconstant GLU_FILL 100012)
;;; DEFINE: #define GLU_SILHOUETTE                     100013
(defconstant GLU_SILHOUETTE 100013)

(defun draw-gl-point (x y z rgb alpha size)
  (opengl:gl-color4-f (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) alpha)
  (opengl:gl-point-size size)
  (opengl:gl-begin opengl:*gl-points*)
  (opengl:gl-vertex3-f x y z)
  (opengl:gl-end))


(defun draw-sphere (position size)
  (let* ((hs (coerce (/ size 2) 'double-float))
         (x (float (car position)))
         (y (float (cadr position)))
         (z (float (caddr position)))
         (glu-quad (opengl:glu-new-quadric)))
    (opengl:gl-push-matrix) 
    (opengl:gl-translatef x y z)
    (opengl:glu-quadric-draw-style glu-quad GLU_FILL)
    (opengl:glu-sphere glu-quad hs 20 20)
    (opengl:gl-pop-matrix)
    ))

(defun draw-cone (position size angle rotation_point)
  (let* ((hs (coerce (/ size 2) 'double-float))
         (x (car position))
         (y (cadr position))
         (z (caddr position))
         (rx (car rotation_point))
         (ry (cadr rotation_point))
         (rz (caddr rotation_point))
         (glu-quad (opengl:glu-new-quadric)))
    (opengl:gl-push-matrix) 
    (opengl:gl-translatef x y z)
    (opengl:gl-rotatef angle rx ry rz)
    (opengl:glu-quadric-draw-style glu-quad GLU_FILL)
    (opengl:glu-cylinder glu-quad hs 0.0d0 hs 20 20)
    (opengl:gl-pop-matrix)
    ))

(defun draw-cube (position size faces)
  (let* ((hs (/ size 2))
         (x (car position))
         (y (cadr position))
         (z (caddr position))
         (cube-points 
          (list (list (- x hs) (- y hs) (- z hs))
                (list (+ x hs) (- y hs) (- z hs))
                (list (+ x hs) (- y hs) (+ z hs))
                (list (- x hs) (- y hs) (+ z hs))
                (list (- x hs) (+ y hs) (- z hs))
                (list (+ x hs) (+ y hs) (- z hs))
                (list (+ x hs) (+ y hs) (+ z hs))
                (list (- x hs) (+ y hs) (+ z hs)))))
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    (opengl:gl-normal3-i 0 1 0) 
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points))) 
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))
    (opengl:gl-end)
    
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINE-LOOP*))
    (opengl:gl-normal3-i 0 1 0)
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))
    (opengl:gl-end)
    
    (if faces
        (opengl:gl-begin opengl:*GL-QUADS*)
      (opengl:gl-begin opengl:*GL-LINES*))
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points)))

    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 0 cube-points)) (cadr (nth 0 cube-points)) (caddr (nth 0 cube-points)))
    (opengl:gl-vertex3-f (car (nth 4 cube-points)) (cadr (nth 4 cube-points)) (caddr (nth 4 cube-points)))

    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 1 cube-points)) (cadr (nth 1 cube-points)) (caddr (nth 1 cube-points)))
    (opengl:gl-vertex3-f (car (nth 5 cube-points)) (cadr (nth 5 cube-points)) (caddr (nth 5 cube-points)))

    (opengl:gl-vertex3-f (car (nth 2 cube-points)) (cadr (nth 2 cube-points)) (caddr (nth 2 cube-points)))
    (opengl:gl-vertex3-f (car (nth 6 cube-points)) (cadr (nth 6 cube-points)) (caddr (nth 6 cube-points)))
    (opengl:gl-vertex3-f (car (nth 7 cube-points)) (cadr (nth 7 cube-points)) (caddr (nth 7 cube-points)))
    (opengl:gl-vertex3-f (car (nth 3 cube-points)) (cadr (nth 3 cube-points)) (caddr (nth 3 cube-points)))

    (opengl:gl-end)))

;;;======================
;;; 3D-cube
;;;======================


(defclass 3D-cube (om-3D-object) 
  ((center :accessor center :initarg :center :initform nil)
   (size :accessor size :initarg :size :initform nil)
   (filled :accessor filled :initarg :filled :initform t )
   (faces :accessor faces :initform '((1 2 3 4)
                                      (1 4 8 5)
                                      (5 8 7 6)
                                      (6 7 3 2)
                                      (4 3 7 8)
                                      (2 1 5 6)))
   (normals :accessor normals :initform '((0 1 0)
                                          (1 0 0)
                                          (0 -1 0)
                                          (-1 0 0)
                                          (0 0 -1)
                                          (0 0 1))))
  )


(defmethod initialize-instance :after ((self 3D-cube) &rest initargs)
  (when (and (center self) (size self))
    (let* ((hw (* 0.5 (if (listp (size self)) (car (size self)) (size self))))
           (hd (* 0.5 (if (listp (size self)) (cadr (size self)) (size self))))
           (hh (* 0.5 (if (listp (size self)) (caddr (size self)) (size self))))
           (x (car (center self)))
           (y (cadr (center self)))
           (z (caddr (center self)))
           (p1 (list (- x hw) (- y hd) (- z hh)))
           (p2 (list (+ x hw) (- y hd) (- z hh)))
           (p3 (list (+ x hw) (- y hd) (+ z hh)))
           (p4 (list (- x hw) (- y hd) (+ z hh)))
           (p5 (list (- x hw) (+ y hd) (- z hh)))
           (p6 (list (+ x hw) (+ y hd) (- z hh)))
           (p7 (list (+ x hw) (+ y hd) (+ z hh)))
           (p8 (list (- x hw) (+ y hd) (+ z hh))))
      (om-set-3Dobj-points self (list p1 p2 p3 p4 p5 p6 p7 p8))
      )))
                  

(defmethod make-cube-face ((self 3D-cube) i &optional (fill t))
  )

(defmethod om-draw-contents ((self 3D-cube))
  (let* ((vertices (om-get-gl-points self)))
    (if (om-3Dobj-color self)
        (let ((col (om-color-to-single-float-list (om-3Dobj-color self))))
          (opengl:gl-color4-f (car col) (cadr col) (caddr col) (cadddr col)))) 
    
    (opengl:gl-shade-model opengl:*gl-flat*)

    (when (filled self)     
      (loop for f in (faces self)
            for n in (normals self) do
            ;for i from 1 do
            (progn  ;(find i '(1) :test '=) 
              (opengl:gl-begin opengl:*GL-QUADS*)
              (loop for p in f do
                ;(apply 'opengl:gl-normal3-i n)
                    (opengl:gl-vertex4-dv (aref (om-get-gl-points self) (1- p)))
                    ;(apply 'opengl:gl-normal3-i n)
                    )
              (opengl:gl-end)))
      )

    (opengl:gl-color3-f 0.3 0.3 0.3)
    (loop for f in (faces self) do
          (opengl:gl-begin opengl:*GL-LINE-LOOP*)
          (loop for p in f do
                (opengl:gl-vertex4-dv (aref (om-get-gl-points self) (1- p))))
          (opengl:gl-end))
    ))
    

;;;======================
;;; SPHERE
;;;======================
(defclass 3D-sphere (om-3D-object) 
  ((center :accessor center :initarg :center :initform nil)
   (size :accessor size :initarg :size :initform nil)))


(defmethod initialize-instance :after ((self 3D-sphere) &rest initargs)
  (when (center self) ;; temporary: the poinst are also used for calculating the bounding-box   
    (om-set-3Dobj-points self (list (center self)))
    ))


(defmethod om-draw-contents ((self 3D-sphere))
  (if (om-3Dobj-color self)
      (let ((col (om-color-to-single-float-list (om-3Dobj-color self))))
        (opengl:gl-color4-f (car col) (cadr col) (caddr col) (cadddr col))))
  (opengl:gl-shade-model opengl:*gl-smooth*)
  (draw-sphere (center self) (size self)))

;;;======================
;;; 3D-curve
;;;======================

(defclass 3d-lines (om-3d-object) 
  ((selected-points :accessor selected-points :initform nil)
   (draw-style :accessor draw-style :initarg :draw-style :initform :draw-all)
   (line-width :accessor line-width :initarg :line-width :initform *OM-GL-DEFAULT-LINEWIDTH*)
   (vertices-colors :accessor vertices-colors :initform nil)
   (vertices-colors-interpol :accessor vertices-colors-interpol :initform nil))
   (:default-initargs :use-display-list T))



(defmethod om-draw-contents ((self 3d-lines))
  (let* ((vertices (om-get-gl-points self))
         (size (- (length vertices) 1))
         (selection (selected-points self)))
    
    (opengl:gl-enable opengl:*gl-light0*)
    (opengl:gl-line-width (float (line-width self)))

    ;draw the lines first
    (when (and (not (equal (draw-style self) :points-only)) (> size 0))
      (if (vertices-colors-interpol self) 
          (opengl:gl-shade-model opengl:*gl-smooth*) 
        (opengl:gl-shade-model opengl:*gl-flat*))
      (opengl:gl-begin opengl:*GL-LINE-STRIP*)
      (loop for i from 0 to size do
            (let ((rgb (om-color-to-single-float-list 
                        (or (nth i (vertices-colors self))
                            (om-3Dobj-color self) 
                            (om-def-color :white)))))
              (opengl:gl-color4-f (nth 0 rgb) (nth 1 rgb) (nth 2 rgb) 0.8)
              (opengl:gl-vertex4-dv (aref vertices i))))
      (opengl:gl-end)    
      )

    ;draw the sphere and the selection (as bigger opaque sphere)
    (when (not (equal (draw-style self) :lines-only))
      (loop for i from 0 to size do
            (let* ((rgb (or (nth i (vertices-colors self)) (om-3Dobj-color self) 
                            (om-def-color :white)))
                   (selected (or (equal '(t) selection) (find i selection)))
                   (alpha (if selected 1.0 0.7))
                   (point (nth i (om-3dobj-points self)))
                   (x (float (car point)))
                   (y (float (cadr point)))
                   (z (float (caddr point))))
              (when selected
                (setf rgb (om-def-color :dark-red)))
              (setf rgb (om-color-to-single-float-list rgb))
              (draw-gl-point x y z rgb alpha (* 3.0 (line-width self)))
              ))))
    ;restore gl params
  (restore-om-gl-colors-and-attributes)
  )


;;;======================
;;; FOR .obj files ;to do... harder than expected to match faces and vertexes.
;;;======================
;code reusing globjule functions
(defclass 3D-fromobjfile (om-3D-object)())

(defmethod om-draw-contents ((self 3D-fromobjfile))
  nil)

(defmethod load-3Dobj-from-file (path)
  (let ((obj (make-instance '3d-fromobjfile))
        (points nil))
    (with-open-file (stream path)
      (loop
       while (peek-char nil stream nil nil)
       do
       (case (intern (string-upcase (read-string-token stream)) :keyword)
         (:v
          (setf points (append (list (read-vec stream)) points))))
       ))
    (setf points (reverse points))
    (om-set-3Dobj-points obj points)
    obj))

(defun read-vec (stream &optional (n 3))
  (let ((vec nil))
    (dotimes (i n)
      (setf vec (append (list (read stream)) vec)))
    vec))

(defun read-string-token (stream)
  (read-until stream #\Space))

(defun read-token (stream)
  (read (read-string-token stream)))

(defun read-until (stream character)
  (when (typep stream 'string)
    (setf stream (make-string-input-stream stream)))
  (loop with head
	for c = (read-char stream nil nil)
	if (or (not c) (char= c character) (char= c #\Newline))
	  do (return (coerce (reverse head) 'string))
	else
	  do (push c head)))


