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
; OM-VIEW CLASSES : container panes for GUI windows
;===========================================================================

(export '(
          om-view
          om-make-view
          om-view-parent
          om-scroll-position
          om-set-scroll-position
          om-h-scroll-position
          om-v-scroll-position
          om-view-scrolled
          om-view-resized
          om-view-contains-point-p
          om-find-view-containing-point
          om-convert-coordinates
          om-view-origin
          om-get-subview-with-focus) 
    :om-api)

(in-package :oa)

;;;======================
;;; VIEW
;;; General graphic pane
;;;======================

(defclass om-view (capi::pinboard-layout om-graphic-object om-interactive-object)
  ((item-subviews :initarg :item-subviews :initform nil :accessor item-subviews)
   ;(main-pinboard-object :accessor main-pinboard-object :initarg :main-pinboard-object :initform nil)
   )
  (:default-initargs
    :draw-with-buffer t
    :highlight-style :standard
    :scroll-if-not-visible-p nil
    ;:fit-size-to-children t
    :resize-callback 'om-resize-callback
    :scroll-callback 'om-scroll-callback
    :create-callback 'om-create-callback
    :destroy-callback 'om-destroy-callback
    #+macosx :background #+macosx :transparent
    ;#+windows :draw-pinboard-objects #+windows  :once ;:local-buffer :buffer :once 
    ))

(defmethod om-view-p ((self om-view)) t)
(defmethod om-view-p ((self t)) nil)


(defmethod om-create-callback ((self om-view))
  (update-for-subviews-changes self nil)
  (setf (initialized-p self) t))

(defmethod om-destroy-callback  ((self om-view)) nil)

;;; background pinboard for drawing on the view
;;; must keep the same size as the view
;(defclass om-view-pinboard-object (capi::pinboard-object) ())
;(defmethod om-get-view ((self om-view-pinboard-object)) (capi::pinboard-object-pinboard self))

;;;=======================================

(defmethod om-scroll-position ((self om-view))
  (om-make-point (om-h-scroll-position self) (om-v-scroll-position self)))

(defmethod om-set-scroll-position ((self om-view) pos)
  (capi::scroll self :pan :move (list (om-point-x pos) (om-point-y pos))))

(defmethod om-h-scroll-position ((self om-view))
  (or (capi::get-horizontal-scroll-parameters self :slug-position) 0))

(defmethod om-v-scroll-position ((self om-view))
  (or (capi::get-vertical-scroll-parameters self :slug-position) 0))


(defmethod om-h-scroll-position ((self om-graphic-object)) 0)
(defmethod om-v-scroll-position ((self om-graphic-object)) 0)
(defmethod om-scroll-position ((self om-graphic-object)) (om-make-point 0 0))

(defmethod om-subviews ((self om-view)) (append (call-next-method) nil)) ; (item-subviews self)))
(defmethod om-view-parent ((self om-view)) (capi::element-parent self))

;;;=======================================

(defmethod set-layout ((view om-view))
  (setf (capi:layout-description view)
        (remove-duplicates 
         (remove nil ;(cons (main-pinboard-object view)
                 (append (vsubviews view) 
                         (item-subviews view)))))
  )


(defmethod set-layout ((view t)) nil)

(defmethod update-for-subviews-changes ((self om-view) &optional (recursive nil))
  (capi::apply-in-pane-process self (lambda () 
                                (set-layout self)
                                ;;(capi:remove-capi-object-property self 'capi::prev-width-height)
                                ))
  (when recursive (mapc #'(lambda (view) (if (om-view-p view) (update-for-subviews-changes view t)))
                        (vsubviews self))))

(defun om-make-view (class &rest attributes
                     &key position size (resizable t)
                     owner subviews name bg-color fg-color scrollbars (enable t)
                     (direct-draw t)  ;;; DIRECT-DRAW NIL ALLOWS TO DRAW PINBOARDS
                     &allow-other-keys)
  (let  ((x (and position (om-point-x position)))
         (y (and position (om-point-y position)))
         (w (and size (om-point-x size)))
         (h (and size (om-point-y size)))
         (initargs-list (list :name name :font (om-def-font :font1)
                              :vcontainer owner
                              ;:automatic-resize '(:width-ratio 0.5 :aspect-ratio 0.6)
                              :accepts-focus-p enable
                              :pane-can-scroll nil
                              :horizontal-scroll (when (or (equal scrollbars t) (equal scrollbars :h)) scrollbars)
                              :vertical-scroll (when (or (equal scrollbars t) (equal scrollbars :v)) scrollbars)
                              :allow-other-keys t)))

    ;:x x :y y ;; for pinboard-objects
    ;;; :focus-callback #'(lambda (a b) (print (list a b))))
    (setf initargs-list (append initargs-list 
                                (and x (list :default-x x :vx x))
                                (and y (list :default-y y :vy y))
                                (and w (list :width NIL 
                                             :default-width w 
                                             :visible-min-width w
                                             :visible-max-width (if (or (equal resizable t) (equal resizable :w)) nil w)
                                             :scroll-width w 
                                             :vw w
                                             ))
                                (and h (list :height NIL 
                                             :default-height h 
                                             :visible-min-height h
                                             :visible-max-height (if (or (equal resizable t) (equal resizable :h)) nil h)
                                             :scroll-height h
                                             :vh h))
                                ))
                                                        
      (let ((view (apply 'make-instance (cons class (append initargs-list attributes)))))
        
        (when bg-color (om-set-bg-color view bg-color))
        (when fg-color (om-set-fg-color view fg-color))
        (when owner (om-add-subviews owner view))
        (when subviews (mapc (lambda (sv) (om-add-subviews view sv)) (remove nil subviews)))
     
     ;#mswindows(unless (or bg-color (simple-pane-background view)) (om-set-bg-color view (om-def-color :white)))

     ;#+win32(setf (main-pinboard-object view) (make-instance 'om-view-pinboard-object))
     ;#+win32(setf (capi::pinboard-pane-size (main-pinboard-object view)) (values w h))
     ; #+cocoa
        
        (when direct-draw (setf (capi::output-pane-display-callback view) 'om-draw-contents-callback))

    ;(when (om-view-p view)
    ;  (setf (capi::simple-pane-scroll-callback view) 'scroll-update)
    ;  (capi:apply-in-pane-process view #'(lambda ()
    ;                              (capi::set-horizontal-scroll-parameters view :min-range 0 :max-range 200)
    ;                              (capi::set-vertical-scroll-parameters view :min-range 0 :max-range 200)
    ;                              )))
     view)))



(defmethod om-resize-callback ((self om-view) x y w h)
  (setf (vx self) x (vy self) y)
  (when (and (vw self) (vh self))
    (unless (and (= w (vw self)) (= h (vh self)))
      (setf (vw self) w (vh self) h)
      (om-view-resized self (om-make-point w h)))))

(defmethod om-view-resized ((self om-view) size) (declare (ignore self size)) nil)

(defmethod om-scroll-callback ((self om-view) dir op val &key interactive)
  ;(print (list self dir op val))
  (when interactive (om-view-scrolled 
                     self 
                     (case dir 
                       (:vertical (list 0 (if (equal op :step)
                                              (om-v-scroll-position self)
                                              val)))
                       (:horizontal (list (if (equal op :step)
                                              (om-h-scroll-position self)
                                              val) 0))
                       (:pan val)))
    ))
      

(defmethod om-view-scrolled ((self om-view) xy) (declare (ignore self xy)) nil)


