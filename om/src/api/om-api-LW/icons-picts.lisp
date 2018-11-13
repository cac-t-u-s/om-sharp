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

;=========================================================================
; Pictures and Icons for OM graphics
;=========================================================================


(in-package :om-api)

;;;==========
;;; export :
;;;==========

(export '(
          *om-pict-types*
          om-register-picture
          om-pict-width
          om-pict-height
          om-get-picture-size
          om-draw-picture
          om-draw-icon 
          
          om-picture-values
          om-picture-array
          om-create-picture
         
          om-kill-picture
   

          om-draw-picture-part
          om-record-pict
          om-internal-picture-to-pict
          om-record-picture-in-pict

          om-open-score-page
          om-close-score-page
          ) :om-api)

;;;================

(defvar *om-pict-types* nil)

(defun init-pict-types ()
  (setf *om-pict-types* (mapcar 'string-downcase (append (gp::list-known-image-formats nil) '(:tif)))))

(om-api-add-init-fun 'init-pict-types)

;;; allows to call image by its name from then on
(defun om-register-picture (name path)
  (gp:register-image-translation name (gp:read-external-image path)))


;;; pict is a picture identifier
(defmethod om-draw-picture ((pict-id t) &key (x 0) (y 0) w h (src-x 0) (src-y 0) src-w src-h)
  (when pict-id
    (handler-bind ((error #'(lambda (e) 
                              (print (format nil "~A: ~A" (type-of e) e))
                              (abort))))
      (let* ((port *curstream*)
             (image (ignore-errors (gp::load-image port pict-id))))
        (when image 
          (unwind-protect 
              (gp::draw-image port image x y ; (+ x *pox*) (+ y *poy*)
                              :to-width (or w (gp:image-width image)) :to-height (or h (gp:image-height image))
                              :from-x src-x :from-y src-y
                              :from-width (or src-w (gp:image-width image)) :from-height (or src-h (gp:image-height image)))
            (gp::free-image port image) ;;; will be freed when port is destroyed
            ))))))

(defmethod om-draw-picture ((pict-id gp::image) &key (x 0) (y 0) w h (src-x 0) (src-y 0) src-w src-h)
  (handler-bind ((error #'(lambda (e) 
                            (print (format nil "~A: ~A" (type-of e) e))
                            (abort))))
    (let* ((port *curstream*))
        (gp::draw-image port pict-id x y ; (+ x *pox*) (+ y *poy*)
                        :to-width (or w (gp:image-width pict-id)) :to-height (or h (gp:image-height pict-id))
                        :from-width (or src-w (gp:image-width pict-id)) :from-height (or src-h (gp:image-height pict-id))
                        :from-x src-x :from-y src-y
                        ))))

(defmacro om-record-pict (w h &body body)
  (let ((portname (gensym))
        (imagename (gensym))
        (metafilename (gensym)))
    `(let* ((,metafilename (capi:with-internal-metafile (,portname :pane nil :bounds (list 0 0 ,w ,h))
                             (let ((*curstream* ,portname)) ,@body)))
            (,imagename (capi::draw-metafile-to-image oa::*dummy-view* ,metafilename)))
       (capi::free-metafile ,metafilename)
       ,imagename)))

(defun om-pict-width (image) (gp::image-width image))
(defun om-pict-height (image) (gp::image-height image))



#|

(defclass internal-picture ()
  ((themetafile :initform nil :initarg :themetafile :accessor themetafile)
   (size-x :initform 0 :initarg :size-x :accessor size-x)
   (size-y :initform 0 :initarg :size-y :accessor size-y)))

(defmethod om-kill-picture ((self internal-picture))
  (when (themetafile self)
    (capi::free-metafile (themetafile self)))
  (setf (themetafile self) nil))

(defmethod om-get-view ((self t))  (or *default-printer-port* self))

;;;(defmethod om-draw-picture (view pict-id &key (x 0) (y 0) w h (src-x 0) (src-y 0) src-w src-h)

(defmethod om-draw-picture ((pict internal-picture) &key (x 0) (y 0) w h (src-x 0) (src-y 0) src-w src-h)
  (when (themetafile pict)
    (let* ((port *curstream*))
      ;(om-inspect port)
      (print (list port (themetafile pict) 
                            ; (+ x *pox*) (+ y *poy*)
                           x y
                           (or w 200) ;(gp:image-width (themetafile pict)))
                           (or h 200)))
      (capi::draw-metafile port (themetafile pict) 
                            ; (+ x *pox*) (+ y *poy*)
                           x y
                           (or w 200) ;(gp:image-width (themetafile pict)))
                           (or h 200) ;(gp:image-height (themetafile pict))))
                           )
      ;(if (om-item-view-p view) 
      ;   ; #-win32
      ;  (setf dx (item-x view) dy (item-y view))
      ;    ;#+win32
     ;     ;(setf dx (- (item-x view) (om-h-scroll-position port)) dy (- (item-y view) (om-v-scroll-position port)))                             
      ;  (if (scroller-p view) (setf pw (om-point-x (om-field-size view)) ph (om-point-y (om-field-size view)))
      ;    ))
      ;(let* ((x (om-point-x topleft))
      ;       (y (om-point-y topleft))
      ;       (w  (om-point-x size))
      ;       (h (om-point-y size)))
      ;  (gp::with-graphics-state (port :mask (list dx dy pw ph))
      ;    #+cocoa(draw-metafile  port (themetafile pict) (+ x *pox*) (+ y *poy*) w h)  ; (+ x dx) (+ y dy) w h)
      ;    #-cocoa(draw-metafile port (themetafile pict)  (+ x *pox*) (+ y *poy*) w h)
      ;  )
        
      )))


(defmethod om-internal-picture-to-pict  ((self internal-picture) view)
  (when (themetafile self)
    (capi::draw-metafile-to-image view (themetafile self))))

(defmethod om-draw-picture-part (view (pict internal-picture) srctopleft srcsize desttopleft destsize)
  (when (themetafile pict)
    (let ((image (draw-metafile-to-image view (themetafile pict))))
      (om-draw-picture-part view image srctopleft srcsize desttopleft destsize))))

(defmethod om-get-picture-size ((picture internal-picture))
  (om-make-point (size-x picture) (size-y picture)))

(defmethod om-pict-width ((picture internal-picture))
  (size-x picture))

(defmethod om-pict-height ((picture internal-picture))
  (size-y picture))


                 
     
;     (make-instance 'internal-picture
;                    :themetafile metafile
;                    :size-x (om-point-x ,size)
;                    :size-y (om-point-y ,size))
;     
   

|#




(defmethod om-record-picture-in-pict (pict &optional (pos (om-make-point 0 0)) size)
  (when pict
    (let* ((pw (om-pict-width pict))
           (ph (om-pict-height pict))
           (destw (if size (om-point-x size) pw))
           (desth (if size (om-point-y size) ph))
           (port (gp::create-pixmap-port *record-view* (om-point-x size) (om-point-y size) 
                                         :background :white :clear t))
           (image (gp::load-image port pict)))
      (gp::draw-image *curstream* image (om-point-x pos) (om-point-y pos)
                      :from-width pw :from-height ph
                      :to-width destw :to-height desth)
      (gp::free-image port image)
      )))


(defun om-open-score-page (size font)
  (let ((port (gp::create-pixmap-port *record-view* (om-point-x size) (om-point-y size)  :clear t)))
     (gp::set-graphics-state port :font (if (gp::font-description-p font)
                                                   (gp::find-best-font port font)
                                                 font))
       port))

(defun om-close-score-page (port size)
  (let ((image (gp::make-image-from-port port 0 0 (om-point-x size) (om-point-y size))))
         (gp::externalize-image port image)))


;;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;; SAVE / LOAD PICTURES
; (internalize-image *graph-pres*))

(defvar *temp-pictwin* nil)
(defvar *temp-pictlayout* nil)

(defun init-pictwin ()
  (setf *temp-pictlayout* (make-instance 'capi:pinboard-layout)
        *temp-pictwin* (capi:display (make-instance 'capi:interface
                                           :display-state :hidden
                                           :layout *temp-pictlayout*))))


(defun ensure-pict-win ()
  (or (and *temp-pictwin* *temp-pictlayout*) 
      (init-pictwin)))

(defun clean-pict-win ()
  (when *temp-pictwin*
    (capi::destroy *temp-pictwin*)))

(define-action "When quitting image" "Delete temp pict intervface" 'clean-pict-win)

(defmethod om-externalize-image ((img gp::image))
  (ensure-pict-win)
  (gp::externalize-image *temp-pictlayout* img))

(defmethod om-externalize-image ((img gp::external-image)) img)

(defmethod om-internalize-image ((img gp::image)) img)

(defmethod om-internalize-image ((img gp::external-image))
  (ensure-pict-win)
  (gp::load-image *temp-pictlayout* img :force-plain t))

