;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Authors: J. Bresson, C. Agon
;=========================================================================

;===========================================================================
; GRAPHIC OBJECT SUPERCLASS AND METHODS 
;===========================================================================

(in-package :om-api)

;;;=====================
;;; export :
;;;=====================
(export '(
          om-graphic-object
          om-make-graphic-object
          om-initialized-p
          om-width
          om-height
          om-view-size
          om-view-position
          om-set-view-size
          om-set-view-position
          om-change-view-position
          om-set-bg-color
          om-get-bg-color
          om-set-fg-color
          om-get-fg-color
          om-get-font
          om-set-font
          om-set-focus
          om-get-name
          om-view-window
          om-view-container
          om-subviews
          om-add-subviews
          om-remove-subviews
          om-with-delayed-update
          om-create-callback
          ) :om-api)
;;;=====================

;;;=======================
;;; GENERAL GRAPHIC OBJECT
;;;=======================
;;; EQUIVALENT SIMPLE-PANE IN LISPWORK/CAPI
;;; manages general callbacks and behaviors

;;; GTK+ Compatibility
;;; used to instanciate om-graphic-object subclasses
;(defmacro omg-defclass  (Name Superclass Slots &rest Class-Options)
;  `(defclass ,name ,superclass ,slots ,.class-options))


(defclass om-graphic-object () 
  ((vcontainer :initform nil :initarg :vcontainer :accessor vcontainer)
   (vsubviews :initform nil :initarg :vsubviews :accessor vsubviews)
   (locked :initform nil :initarg :locked :accessor locked)
   (vx :initform 0 :initarg :vx :accessor vx)
   (vy :initform 0 :initarg :vy :accessor vy)
   (vw :initform 32 :initarg :vw :accessor vw)
   (vh :initform 32 :initarg :vh :accessor vh)
   (help-string :initform nil :initarg :help-string :accessor help-string)
   (initialized-p :initform nil :accessor initialized-p)
   ))
 
(defmethod om-create-callback (self))
(defmethod om-destroy-callback (self))

;;; different pour om-window
(defmethod om-get-view ((self t)) self)
(defmethod om-get-view ((self om-graphic-object)) (or *default-printer-port* self))
(defmethod om-subviews ((self om-graphic-object)) (vsubviews self))

;;; for windows, skip default window layout
(defmethod container-skip-layout ((self t)) self)

(defmethod om-view-container ((self om-graphic-object))
 (container-skip-layout (vcontainer self))
 ;(container-skip-layout (or (vcontainer self) (capi::element-parent self)))
 )


(defmethod rec-top-level ((self t))
  (if (om-view-container self) 
      (rec-top-level (om-view-container self))
    ;;;self
    nil
    ))

(defmethod rec-top-level ((self capi::interface)) self)

(defmethod om-view-window ((self om-graphic-object))
  (let ((rep (capi::top-level-interface self)))
    (or rep (rec-top-level self))
    ;(while (null rep) (setf rep (capi::top-level-interface self)))
    ;rep
    ))

(defmethod om-initialized-p ((self om-graphic-object)) (initialized-p self))

;;;======================
;;; TOOLS
;;;======================
(defmethod om-view-contains-point-p ((view t) point) nil)
(defmethod om-view-contains-point-p ((view om-graphic-object) point)
  (let* ((x (om-point-x point))
         (y (om-point-y point))
         (vsize (om-view-size view))
         (vpos (or (om-view-position view) (om-make-point 0 0)))
         ;(rx  (+ (om-point-x vpos) (om-h-scroll-position view)))
         ;(ry  (+ (om-point-y vpos) (om-v-scroll-position view)))
         )
    (and (om-point-x vpos) (om-point-y vpos)
         (om-point-x vsize) (om-point-y vsize)
         (> x (om-point-x vpos)) 
         (> y (om-point-y vpos))
         (< x (+ (om-point-x vpos) (om-point-x vsize))) 
         (< y (+ (om-point-y vpos) (om-point-y vsize))))))

;;; capi::pinboard-object-at-position
(defun om-find-view-containing-point (view point &optional (recursive t))
  (if view
      (let ((subviews (om-subviews view)))
	(do ((i (- (length subviews) 1) (- i 1)))
            ((< i 0))
          (let ((subview (nth i subviews)))
            (when (om-view-contains-point-p subview point)
	      (return-from om-find-view-containing-point
                (progn
		  (when recursive (om-find-view-containing-point subview (om-convert-coordinates point view subview))))
                ))))
        view)))

;;; capi::convert-relative-position
(defun om-convert-coordinates (point view1 view2)
  (if (and view1 view2)
       (om-add-points point
                      (om-subtract-points (om-view-origin view2)
                                          (om-view-origin view1)))
   (progn
      (print (format nil "Warning: Can not convert position with NULL views: ~A, ~A." view1 view2))
      point)))
  
(defun om-view-origin (view)
   (let ((container (om-view-container view)))
     (if container
       (let ((position (om-view-position view))
             (container-origin (om-view-origin container)))
         (if position
	     (om-subtract-points container-origin position)
           container-origin))
       (om-make-point 0 0)
       
       )))

;  (capi:map-pane-descendant-children  layout
;   #'(lambda (p)  (when (capi:pane-has-focus-p p) (return-from find-pane-with-focus p)))))
(defun om-get-subview-with-focus (view)
  (capi::pane-descendant-child-with-focus view))


(defun maybe-call-update (graphic-obj)
  (when (and (initialized-p graphic-obj) (not (locked graphic-obj))) ;; (interface-visible-p graphic-obj)
    (update-for-subviews-changes graphic-obj t)
    ))
  
(defmethod update-for-subviews-changes ((self om-graphic-object) &optional (recursive nil)) nil)

(defmacro om-with-delayed-update (view &body body)
   `(progn 
      (setf (locked ,view) t)
      ,@body
      (setf (locked ,view) nil)
      (maybe-call-update ,view)
      ))

(defmethod om-add-subviews ((self om-graphic-object) &rest subviews)
  "Adds subviews to a graphicbject"
  (loop for item in subviews do (internal-add-subview self item))
  (maybe-call-update self)
  )

(defmethod internal-add-subview ((self om-graphic-object) (subview om-graphic-object))
  (setf (vcontainer subview) self)
  (setf (vsubviews self) (append (vsubviews self) (list subview))))

(defmethod om-remove-subviews ((self om-graphic-object) &rest subviews)
  (capi::apply-in-pane-process (om-get-view self) #'(lambda ()
                                                      (loop for item in subviews do (internal-remove-subview self item))
                                                      (maybe-call-update self)
                                                      )))

(defmethod om-remove-all-subviews ((self om-graphic-object))
  (capi::apply-in-pane-process (om-get-view self) #'(lambda ()
                                                        (loop for item in (om-subviews self) do (internal-remove-subview self item))
                                                        (maybe-call-update self))))




(defmethod internal-remove-subview ((self om-graphic-object) (subview om-graphic-object))
  (setf (vcontainer subview) nil)
  (setf (vsubviews self) (remove subview (vsubviews self))))


(defmethod om-width ((item om-graphic-object)) (om-point-x (om-view-size item)))
(defmethod om-height ((item om-graphic-object)) (om-point-y (om-view-size item)))

(defmethod om-interior-size ((self om-graphic-object)) (om-view-size self))

(defmethod om-set-bg-color ((self om-graphic-object) color)
  (let ((col (when color (omcolor-c color))))
    #-cocoa 
    (if (and col (equal col :transparent) (om-view-container self))
        ;;; (omcolor-c (om-get-bg-color (om-view-container self)))
        ;;; (setf col (omcolor-c (om-get-bg-color (om-view-container self))))
        ;;; nil ;;; do nothing...?
      (capi::simple-pane-background (om-get-view self) NIL)
      (setf (capi::simple-pane-background (om-get-view self)) col))
    #+cocoa
    (setf (capi::simple-pane-background (om-get-view self)) col)))
      
(defmethod om-get-bg-color ((self om-graphic-object))
  (let ((c (capi::simple-pane-background (om-get-view self))))
    (when (and c (not (equal c :transparent)))
      (make-omcolor :c c))))


(defmethod om-set-fg-color ((self om-graphic-object) color)
  (let ((col (when color (omcolor-c color))))
    (capi::apply-in-pane-process 
     self
     #'(lambda () 
         (setf (capi::simple-pane-foreground (om-get-view self)) col))
     )))

(defmethod om-get-fg-color ((self om-graphic-object))
  (make-omcolor :c (capi::simple-pane-foreground (om-get-view self))))

(defmethod om-get-font ((self om-graphic-object))
  (let ((font (capi::simple-pane-font self)))
    (if (gp::font-description-p font) font
      (gp::font-description font))))

(defmethod om-set-font ((self t) font) 
  (print (format nil "WARNING NO METHOD DEFINED TO SET FONT WITH ~A" self))
  NIL)

(defmethod om-set-font ((self capi::simple-pane) font) 
  (setf (capi::simple-pane-font self) font))

(defmethod om-set-focus ((self om-graphic-object))
  (capi::set-pane-focus self))

;;; TESTER AVEC capi:activate-pane


(defmethod om-get-name ((self om-graphic-object)) (capi::capi-object-name self))