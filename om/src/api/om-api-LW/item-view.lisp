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

;;===========================================================================
; INTERNAL SUBVIEW CLASSES OPTIMIZED FOR DRAWING
;;===========================================================================

;; pinboard-object-at-position 

(export '(om-item-view 
          om-item-line 
          om-item-text om-set-text) :om-api)

(in-package :oa)

;;; no subview
;;; optimized display
(defclass om-item-view (om-graphic-object om-interactive-object capi::pinboard-object)   ;; capi::drawn-pinboard-object
  ((item-container :initarg :item-container :accessor item-container :initform nil)
   (item-x  :initarg :item-x :accessor item-x :initform 0)
   (item-y  :initarg :item-y :accessor item-y :initform 0))
  ;(:default-initargs :display-callback 'item-draw-callback)
  )

(defmethod initialize-instance :after ((self om-item-view) &rest args)
  (om-create-callback self))

(defun om-make-graphic-object (class &rest attributes
                                     &key position (size (om-make-point 10 10)) font text 
                                     bg-color fg-color
                                     &allow-other-keys)
  (let*  ((x (and position (om-point-x position)))
          (y (and position (om-point-y position)))
          (w (om-point-x size))
          (h (om-point-y size))
          (view (apply 'make-instance (append 
                                       (list class
                                             :width w :height nil
                                             :default-x x :default-y y :default-width w :default-height h
                                             :visible-min-width w :visible-min-height h
                                             :visible-max-width w
                                             :visible-max-height h
                                             :help-string text
                                             :vx x :vy y :vw w :vh h
                                            :allow-other-keys t)
                                       attributes))))
    (when fg-color (om-set-fg-color view fg-color))
    (when bg-color (om-set-bg-color view bg-color))
    (om-set-font view (or font (om-def-font :font1)))
    view))

(defmethod om-item-view-p ((self t)) nil)
(defmethod om-item-view-p ((self om-item-view)) t)

;(defmethod capi::pane-has-focus-p ((self om-item-view)) nil)

(defmethod update-for-subviews-changes ((self om-item-view) &optional (recursive nil)) 
  (when (and (item-container self) (initialized-p (item-container self)))
    (update-for-subviews-changes (item-container self) nil)))

(defmethod internal-add-subview ((self om-view) (subview om-item-view))
  (call-next-method)
  (setf (item-container subview) self)
  (setf (item-subviews self) (append (item-subviews self) (list subview)))
  (update-po-position subview)
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview))
  )


(defmethod internal-add-subview ((self om-abstract-window) (subview om-item-view))
  (call-next-method)
  (setf (item-container subview) (pane-layout self))
  (setf (item-subviews (pane-layout self)) (append (item-subviews (pane-layout self)) (list subview)))
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview)))

(defmethod internal-add-subview ((self om-item-view) (subview om-item-view))
  (call-next-method)
  (po-add-subview self subview))

;;; recursively set the top-level layout for pinboard-objects
(defmethod po-add-subview ((self om-item-view) (subview om-item-view))
  (setf (item-container subview) (item-container self))
  (when (item-container self)
    (setf (item-subviews (item-container self)) (append (item-subviews (item-container self)) (list subview)))
    (update-po-position subview))
  (mapcar #'(lambda (sv) (po-add-subview subview sv)) (vsubviews subview)))

(defmethod internal-remove-subview ((self om-view) (subview om-item-view))  
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-container subview) nil)
  (setf (item-subviews self) (remove subview (item-subviews self)))
  (capi::apply-in-pane-process self
                               (lambda () (capi::manipulate-pinboard self subview :delete)))
  (call-next-method))

(defmethod internal-remove-subview ((self om-abstract-window) (subview om-item-view))  
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-container subview) nil)
  (setf (item-subviews (pane-layout self)) (remove subview (item-subviews (pane-layout self))))
  (capi::apply-in-pane-process (pane-layout self)
                               (lambda () (capi::manipulate-pinboard (pane-layout self) subview :delete)))
  (call-next-method))

(defmethod internal-remove-subview ((self om-item-view) (subview om-item-view))
  (po-remove-subview self subview)
  (call-next-method))

(defmethod po-remove-subview ((self om-item-view) (subview om-item-view))
  (mapcar #'(lambda (sv) (po-remove-subview subview sv)) (vsubviews subview))
  (setf (item-subviews (item-container self)) (remove subview (item-subviews (item-container self))))
  (capi::apply-in-pane-process (item-container self)
                               (lambda () (capi::manipulate-pinboard (item-container self) subview :delete)))
  (setf (item-container subview) nil))

(defmethod om-subviews ((self om-item-view)) 
  (vsubviews self))

;;; (capi::highlight-pinboard-object (item-container self) self t)
;;; (defmethod om-create-callback ((self om-item-view)) nil)


(defun update-po-position (self)
  (when (and (vcontainer self) (item-container self))
    (capi::apply-in-pane-process (item-container self)
                                 (lambda ()
                                   (let ((abs-pos (om-convert-coordinates (om-view-position self) (vcontainer self) (item-container self))))
                                     (setf (item-x self) (om-point-x abs-pos) (item-y self) (om-point-y abs-pos))
                                     (setf (capi::pinboard-pane-position self) (values (item-x self) (item-y self)))
                                     (capi::set-hint-table self (list :x (om-point-x abs-pos) :y (om-point-y abs-pos)
                                                                      :visible-min-width (vw self) :visible-min-height (vh self)
                                                                      :visible-max-width t :visible-max-height t))
                                     (mapc 'update-po-position (vsubviews self))))
                                 )))


(defmethod om-set-view-position ((self om-item-view) pos-point) 
  (setf (vx self) (om-point-x pos-point)
        (vy self) (om-point-y pos-point))  
  (update-po-position self)
  ;(when (item-container self) (om-invalidate-view (item-container self)))
  )

(defmethod om-view-position ((self om-item-view))
  (om-make-point (vx self) (vy self)))
(defmethod om-view-position ((self t))
  (om-make-point 0 0))

(defmethod position-in-view ((self om-item-view)) (om-view-position self))

(defmethod om-mouse-position ((view om-item-view))
  (om-convert-coordinates (internal-mouse-position (item-container view))
                          (item-container view) view))

(defmacro om-with-redisplay-area ((view x y w h) &body body)
  `(let (ret-value) 
     (setf (mask ,view) (list ,x ,y ,w ,h))
     (let ((ret-value (progn ,@body)))
       (setf (mask ,view) nil)
       ret-value)))
    
  
(defmethod om-set-view-size ((self om-item-view) size-point)
  ;(capi::resize-pinboard-object self :width (om-point-x size-point) :height (om-point-y size-point))
  (setf (vw self) (om-point-x size-point))
  (setf (vh self) (om-point-y size-point))
  ;(gp::with-graphics-mask ((item-container self) (list (vx self) (vy self) (vw self) (vh self)))
  ;(om-with-redisplay-area ((om-get-view self) (vx self) (vy self) (vw self) (vh self))
  (setf (capi::pinboard-pane-size self) (values (om-point-x size-point) (om-point-y size-point)))
  (capi::set-hint-table self (list :visible-min-width (vw self) :visible-min-height (vh self)
                                   :visible-max-width t :visible-max-height t))
  ;;    )
  ;(when (item-container self)
  ;  (capi:remove-capi-object-property (item-container self) 'capi::prev-width-height))
  )

  
(defmethod om-view-size ((self om-item-view))
  (om-make-point (vw self) (vh self)))

(defmethod om-get-view ((self om-item-view)) 
  (or (capi::pinboard-object-pinboard self)
   (item-container self)))
 
(defmethod om-set-bg-color ((self om-item-view) color)
  (let ((col (when color (omcolor-c color))))
    (setf (capi::pinboard-object-graphics-arg self :background) (if (equal col :transparent) nil col))
    (capi:redraw-pinboard-object self)))
      
(defmethod om-get-bg-color ((self om-item-view))
  (make-omcolor :c (capi::pinboard-object-graphics-arg self :background)))

(defmethod om-set-fg-color ((self om-item-view) color)
  (let ((col (when color (omcolor-c color))))
    (setf (capi::pinboard-object-graphics-arg self :foreground) col)
    (capi:redraw-pinboard-object self)))
      
(defmethod om-get-fg-color ((self om-item-view))
  (make-omcolor :c (capi::pinboard-object-graphics-arg self :foreground)))

(defmethod om-get-font ((self om-item-view))
  (let ((font (capi::pinboard-object-graphics-arg self :font)))
    (when font 
      (if (gp::font-description-p font) font
        (gp::font-description font))
      )))

(defmethod om-set-font ((self capi::pinboard-object) font) 
  (setf (capi::pinboard-object-graphics-arg self :font) font)
  (capi:redraw-pinboard-object self))

(defmethod om-set-focus ((self om-item-view))
  (when (item-container self)
    (capi::set-pane-focus (item-container self))))

(defmethod capi::draw-pinboard-object (pane (self om-item-view) &key x y w h)
  (call-next-method)
  (capi::apply-in-pane-process pane 'draw-item-view pane self))

;(defmethod item-draw-callback (pane (obj om-item-view) x y w h) 
;  (om-draw-contents obj))
;(defun item-draw-callback (pane item x y w h) 
;  ;(print (list "draw" pane item)) 
;  (draw-item-view pane item))

(defun draw-item-view (pane po)
   (let ((old-stream *curstream*))
     (setf *curstream* pane)
     (multiple-value-bind (pox poy) (capi::pinboard-pane-position po)
       (let ((font (if (gp::font-description-p (om-get-font po)) (gp::find-best-font pane (om-get-font po)) (om-get-font po))))
         (gp::set-graphics-port-coordinates pane :left (- pox) :top (- poy))
         (gp::with-graphics-state (pane :font font 
                                        ;:mask (list (item-x po) (item-y po) (vw po) (vh po))
                                        :foreground (or (capi::pinboard-object-graphics-arg po :foreground) :black)
                                        :background (capi::pinboard-object-graphics-arg po :background))
           (capi::with-geometry po 
             ;(oa::om-with-clip-rect po 
             ;    capi::%x% capi::%y% 
                 ;capi::%width% capi::%height%
                 ;0 0 ; (item-x po) (item-y po)
                 ;(vw po) (vh po)
               (om-draw-contents po)
             ;  )
           )
           (gp::set-graphics-port-coordinates pane :left 0 :top 0)
           (setf *curstream* old-stream)
           )))))



(defmethod om-invalidate-view ((self om-item-view))
  (when (om-get-view self)
    (multiple-value-bind (pox poy) (capi::pinboard-pane-position self) 
      (when pox  ;;; otherwise the view is not yet positioned
        (capi::apply-in-pane-process  (om-get-view self) 
                                      'gp::invalidate-rectangle 
                                      (om-get-view self) 
                                      (- pox 3) (- poy 3) (+ (vw self) 6) (+ (vh self) 6))))))



;;;===========================
;;; SPECIAL CASES
;;;===========================

#+cocoa
(defclass om-internal-view (om-view) () (:default-initargs :background :transparent))
#-cocoa
(defclass om-internal-view (om-item-view) ())

(defclass om-item-line (om-item-view capi::line-pinboard-object) ())

(defclass om-item-text (om-item-view) 
  ((text :accessor text :initarg :text :initform "")
   (bg :accessor bg :initarg :bg :initform nil)
   (fg :accessor fg :initarg :fg :initform nil)
   (border :accessor border :initarg :border :initform nil)
   ))

;(defmethod initialize-instance :after ((self om-item-text) &rest args)
;  (print self) (print args)(print (getf args :border))(print (border self)))

(defmethod om-draw-contents ((self om-item-text))
  (when (border self) 
    (om-draw-rect 0 0 (vw self) (vh self) :color (om-def-color :gray)))
  (gp:draw-x-y-adjusted-string *curstream* (text self) 0 0 :y-adjust :top))

;(defmethod capi:draw-pinboard-object (pinboard (text om-item-text) &key)
;  (capi:with-geometry text
;    (let (;(foreground (foreground text))
;          ;(background (capi:simple-pane-background pinboard))
;          ;(filled (filled text))
;          )
;      (print text)
;      (gp:draw-x-y-adjusted-string pinboard
;                                   (text text)
;		                   0 ;capi:%x%;;
;		                   0 ;capi:%y;%
;                                   :y-adjust ;:top
;                                   ;:foreground (if filled background foreground)
;                                   ;:background (if filled foreground background)
;                                   ;:block (filled text)
;                                   ))))



(defmethod om-set-text ((self om-item-text) text)
  (setf (text self) text)
  (capi:redraw-pinboard-object self))
  

;#-cocoa
;(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-transparent-view)) 
;  (call-next-method)
;  (om-set-bg-color view (om-get-bg-color cont))
;  (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view)))

;#-cocoa
;(defmethod (setf vcontainer) :around ((cont om-graphic-object) (view om-view)) 
;  (call-next-method)
;  (when (or (null (om-get-bg-color view)) (equal :transparent (omcolor-c (om-get-bg-color view))))
;    (om-set-bg-color view (om-get-bg-color cont))
;    (mapc #'(lambda (v) (setf (vcontainer v) view)) (om-subviews view))))
