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
; Authors: J. Bresson
;=========================================================================

(in-package :om-api)

(export 
 '(om-transient-drawing-view
   om-start-transient-drawing
   om-start-transient-drawing-process
   om-stop-transient-drawing
   om-transient-drawing-active-p
   om-update-transient-drawing
   om-transient-drawing-item-clicked

   ; om-init-motion-click            ;; only 1 of the 2 !
   om-init-temp-graphics-motion      ;; only 1 of the 2 !
   ) 
 :oa)

(defclass om-transient-drawing-view (om-graphic-object)
  ((drawn-item :initform nil :accessor drawn-item)
   (drawing-process :initform nil :accessor drawing-process))
  (:default-initargs :destroy-callback 'transient-drawing-view-destroy-calback))

(defmethod transient-drawing-view-destroy-calback ((self om-transient-drawing-view))
  (om-stop-transient-drawing self)
  (om-destroy-callback self))

(defmethod om-transient-drawing-active-p ((self om-transient-drawing-view))
  (if (or (drawn-item self) (drawing-process self)) T NIL))

(defparameter *click-motion-view* nil)
(defparameter *click-motion-action* nil)

(defmethod om-transient-drawing-item-clicked ((self om-transient-drawing-view) clicked-pos-in-view) nil)
(defmethod om-transient-drawing-item-double-clicked ((self om-transient-drawing-view) clicked-pos-in-view) nil)

(defmethod om-clic-callback :around ((self om-transient-drawing-view) x y modifiers)
  (if (and (drawn-item self)
           (capi::over-pinboard-object-p (drawn-item self) x y))
      (om-transient-drawing-item-clicked self (om-make-point x y))
  ;(om-view-click-handler self (om-make-point x y))
    (call-next-method)
  ))
    
(defmethod om-multiple-clic-callback :around ((self om-transient-drawing-view) x y modifiers n)
  (if (and (drawn-item self)
           (capi::over-pinboard-object-p (drawn-item self) x y))
      (om-view-doubleclick-handler self (om-make-point x y))
    (call-next-method)))


;;;=====================================
;;; CURSORS AND OTHER MOVING DRAWINGS
;;; CHEAP VERSION BUT ACTIONS ARE LOCKED
;;;=====================================

(defun start-transient-drawing-fun (view draw-fun position size &key display-mode)
  (loop 
   ;;; this loop breaks when killed 
   (capi:start-drawing-with-cached-display 
    view     
    #'(lambda (view x y w h)
        (let ((user-info (capi:output-pane-cached-display-user-info view)))
          (when user-info
            (destructuring-bind (mode x y w h)
                user-info 
              (om-with-focused-view view 
                (funcall draw-fun view (om-make-point x y) (om-make-point w h)))))))
    :user-info (list display-mode
                     (om-point-x position) (om-point-y position)
                     (om-point-x size) (om-point-y size))
    :automatic-cancel nil
    :resize-automatic-cancel #'(lambda (pane)
                                 ;; will break the loop and restart
                                 (setf (capi:output-pane-cached-display-user-info pane) nil))
    )
   (loop 
    ;;; this loop breaks when user-info is NIL (caused by a resize)
    (sleep 0.2)
    (when (not (capi:output-pane-cached-display-user-info view))
      (return)))
   ))


(defmethod om-start-transient-drawing-process ((self om-transient-drawing-view) draw-fun position size &key display-mode)
  (om-stop-transient-drawing self)
  (setf (drawing-process self)
        (mp:process-run-function (format nil "Animation for ~A" self) NIL
                                 'start-transient-drawing-fun 
                                 self draw-fun position size :display-mode display-mode)))


(defmethod om-stop-transient-drawing-process ((self om-transient-drawing-view))
  (when (drawing-process self)
    (capi:output-pane-free-cached-display self)
    (mp:process-kill (drawing-process self))
    (setf (drawing-process self) nil)
    (om-invalidate-view self)))

(defun update-transient-drawing-fun (view &key x y w h)
  (let ((user-info (capi:output-pane-cached-display-user-info view)))
    (when user-info
      (when x (setf (nth 1 user-info) x))
      (when y (setf (nth 2 user-info) y))
      (when w (setf (nth 3 user-info) (+ (nth 1 user-info) w)))
      (when h (setf (nth 4 user-info) (+ (nth 2 user-info) h)))
      (if (car user-info)
          (capi:update-drawing-with-cached-display-from-points 
           view      
           (nth 1 user-info) (nth 2 user-info)
           (nth 3 user-info) (nth 4 user-info)
           :extend (if (numberp (car user-info)) (car user-info) 1))
        (capi:update-drawing-with-cached-display view))
      )))

(defmethod om-update-transient-drawing-process ((self om-transient-drawing-view) &key x y w h)
  (when (and (drawing-process self) (not *click-motion-action*))
    (capi::apply-in-pane-process 
     self
     'update-transient-drawing-fun
     self :x x :y y :w w :h h)))


;;;=====================================
;;; SAME USING A SIMPLE PINBOARD OBJECT
;;; MORE FLEXIBLE BUT MORE DRAWING
;;;=====================================

(defmethod om-draw-contents-callback ((self om-transient-drawing-view) x y w h) 
  (let ((item (drawn-item self)))
    (if item 
      ;;; NO EFFECT
     (capi:with-geometry item
       (gp::with-graphics-state (self :mask (list (or x capi:%x%) (or y capi:%y%) 
                                                 (or w capi:%width%) (or h capi:%height%)))
      (call-next-method)
      (capi::draw-pinboard-object self item)))
    (call-next-method))))

  
(defmethod om-start-transient-drawing ((self om-transient-drawing-view) draw-fun position size &key display-mode)
  (om-stop-transient-drawing self)
  (setf (drawn-item self)
        (make-instance 'capi::drawn-pinboard-object
                       :display-callback #'(lambda (pane obj x y w h)
                                             (when (and x y w h)
                                             (om-with-focused-view pane
                                               (funcall draw-fun pane (om-make-point x y) (om-make-point w h)))))
                       :x (om-point-x position) :y (om-point-y position)
                       :visible-min-width (om-point-x size) :visible-min-height (om-point-y size)
                       ))
  (capi:apply-in-pane-process self 'capi:manipulate-pinboard self (drawn-item self) :add-top)
  )

(defmethod om-stop-transient-drawing ((self om-transient-drawing-view))
  ;;;
  (ignore-errors
    (om-stop-transient-drawing-process self)
    ;;;
    (when (drawn-item self)
      
      (capi::apply-in-pane-process self 'capi:manipulate-pinboard self (drawn-item self) :delete)
      (om-invalidate-view self)
      (setf (drawn-item self) nil)
      
      )))

(defmethod om-update-transient-drawing ((self om-transient-drawing-view) &key x y w h)
  
  ;;; => drawing process not used 
  (om-update-transient-drawing-process self :x x :y y :w w :h h)
 
  (capi::apply-in-pane-process 
   self
   #'(lambda ()
       (when (drawn-item self)
         (capi:with-geometry (drawn-item self)
           (when (or x y)
             (setf (capi:pinboard-pane-position (drawn-item self)) 
                   (values (or x capi:%x% 0) (or y capi:%y% 0))))
           (when (or w h)
             (setf (capi:pinboard-pane-size (drawn-item self)) 
                   (values (or w capi:%width% 0) (or h capi:%height% 0))))
           )
          ;; (capi::redraw-pinboard-object (drawn-item self))
         ))
   ))


;;;=====================
;;; CLICK-AND-DRAG GRAPHICS
;;;=====================

;;; views handling movable graphics must be subclasses of handle-temp-graphics
(defclass handle-click-motion () 
  ((container-view :initform nil :accessor container-view)
   (init-pos :initform nil :accessor init-pos)
   (temp-graphics :initform nil :accessor temp-graphics)
   (motion-fun :initform nil :accessor motion-fun)
   (release-fun :initform nil :accessor release-fun)
   (active :initform nil :accessor active)
   (min-move :initform 0 :accessor min-move)))

(defparameter *global-motion-handler* nil)

(defun init-motion-handler ()
  (or *global-motion-handler* 
      (setf *global-motion-handler* (make-instance 'handle-click-motion))))

;;; initites the handling of a movable graphics
(defmethod om-init-temp-graphics-motion ((self om-graphic-object) pos graphics &key motion release (min-move 0))

  (init-motion-handler)

  (when (and (temp-graphics *global-motion-handler*) (om-view-container (temp-graphics *global-motion-handler*)))
    (om-remove-subviews (om-view-container (temp-graphics *global-motion-handler*))
                        (temp-graphics *global-motion-handler*)))

  (setf (container-view *global-motion-handler*) self
      (init-pos *global-motion-handler*) pos
      (min-move *global-motion-handler*) min-move
      (temp-graphics *global-motion-handler*) graphics
      (motion-fun *global-motion-handler*) motion
      (release-fun *global-motion-handler*) release)
  
  (when (null (min-move *global-motion-handler*))
    ;; if min-move is null the motion is considered active straight away
    (setf (active *global-motion-handler*) t) 
    (when (temp-graphics *global-motion-handler*)
      (om-add-subviews (container-view *global-motion-handler*) (temp-graphics *global-motion-handler*))))
  
  t)

(defmethod default-motion-action ((self t) position) nil)

(defun handle-temp-graphics-motion (sender pos)  
  (when (and *global-motion-handler* (container-view *global-motion-handler*)) 
    (let ((position (om-convert-coordinates pos sender (container-view *global-motion-handler*))))
      (unless (om-points-equal-p (init-pos *global-motion-handler*) position)
        
        (unless (active *global-motion-handler*)
          (when (or (null (min-move *global-motion-handler*))
                    (>= (abs (- (om-point-x position) (om-point-x (init-pos *global-motion-handler*)))) 
                        (min-move *global-motion-handler*))
                    (>= (abs (- (om-point-y position) (om-point-y (init-pos *global-motion-handler*)))) 
                        (min-move *global-motion-handler*)))
            
            (when (temp-graphics *global-motion-handler*)
              (om-add-subviews (container-view *global-motion-handler*) (temp-graphics *global-motion-handler*)))
           
            (setf (active *global-motion-handler*) t)
            ))      
        
        (when (active *global-motion-handler*)
          (default-motion-action (temp-graphics *global-motion-handler*) position)
          (when (motion-fun *global-motion-handler*)
            (funcall (motion-fun *global-motion-handler*) (container-view *global-motion-handler*) position))
          )
        ))))
 

(defmethod restore-drawable-view ((self t)) nil)

(defmethod restore-drawable-view ((self om-transient-drawing-view)) 
  (when (drawn-item self)
    (capi:manipulate-pinboard self (drawn-item self) :delete)
    (capi:manipulate-pinboard self (drawn-item self) :add-top)
    ))
  
        
(defun handle-temp-graphics-release (sender pos)
  
  (when (and *global-motion-handler* (container-view *global-motion-handler*))
    (let ((position (om-convert-coordinates pos sender (container-view *global-motion-handler*))))
      (when (and (or (null (min-move *global-motion-handler*))
                     (not (om-points-equal-p (init-pos *global-motion-handler*) position)))
                 (active *global-motion-handler*)
                 (release-fun *global-motion-handler*))
        (funcall (release-fun *global-motion-handler*) (container-view *global-motion-handler*) position))
      
      (when (temp-graphics *global-motion-handler*)
        (om-remove-subviews (container-view *global-motion-handler*) (temp-graphics *global-motion-handler*)))
      
      (restore-drawable-view (container-view *global-motion-handler*))

      (setf (temp-graphics *global-motion-handler*) nil
            (motion-fun *global-motion-handler*) nil
            (release-fun *global-motion-handler*) nil
            (active *global-motion-handler*) nil
            (init-pos *global-motion-handler*) pos
            (min-move *global-motion-handler*) 0
            (container-view *global-motion-handler*) nil)
      
      )))

;;; for click&drag actions
(defmethod om-click-motion-handler :after ((self om-graphic-object) pos)
  (handle-temp-graphics-motion self pos))

(defmethod om-click-release-handler :after ((self om-graphic-object) pos)
  (handle-temp-graphics-release self pos))




#|

;;;=====================================
;;; CLICK-AND-DRAG DRAWING
;;;=====================================

;;; typically called in a click-handler
(defmethod om-init-motion-click ((self om-graphic-object) position 
                                 &key motion-draw draw-pane motion-action release-action display-mode)
  ;(print (list "start" self))
  (setf *click-motion-view* self)
  (setf *click-motion-action* t)
  (when (or motion-action release-action)
    (setf (temp-data self) 
          (list motion-action release-action
                (om-point-x position) (om-point-y position)
                (om-point-x position) (om-point-y position)
                draw-pane)))
  (when motion-draw
    (setf (capi:output-pane-cached-display-user-info self) nil) ;;; will break the animation loop if any
    (start-transient-click-drawing (or draw-pane self) 
                             motion-draw 
                             (if draw-pane (om-convert-coordinates position self draw-pane) position) 
                             display-mode))
  t)

(defmethod start-transient-click-drawing ((self om-view) motion-draw position display-mode) 
  ;(capi:output-pane-free-cached-display self)
  (capi::apply-in-pane-process 
   self 
   'capi:start-drawing-with-cached-display  
   self 
   #'(lambda (view x y w h) 
       (let ((dragging-info (capi:output-pane-cached-display-user-info view)))
         (when dragging-info
           (destructuring-bind (mode x1 y1 x2 y2)
               dragging-info
             (om-with-focused-view view 
               (funcall motion-draw view (om-make-point x1 y1) (om-make-point x2 y2)))))))
   :user-info (list display-mode
                    (om-point-x position) (om-point-y position)
                    (om-point-x position) (om-point-y position))
   ;:automatic-cancel nil
   ))

(defmethod om-click-motion-handler :around ((self om-graphic-object) position)
  ;(print (list self position *click-motion-view*))
  (if *click-motion-action*
    (let* ((view *click-motion-view*)
           (motion-info (temp-data view))
           (x (om-point-x position)) (y (om-point-y position))
           (pane view))
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info 
          (unless (and (= old-x x) (= old-y y))
            (when motion-action
              (capi::apply-in-pane-process (om-get-view view) motion-action view position (om-make-point old-x old-y)))
            (setf (nth 4 (temp-data view)) x (nth 5 (temp-data view)) y))
          (when draw-pane 
            (let ((pp (om-convert-coordinates position self draw-pane)))
              (setf pane draw-pane
                    x (om-point-x pp)
                    y (om-point-y pp))))
          ))
      (let ((dragging-info (capi:output-pane-cached-display-user-info pane)))
        (when dragging-info
          (destructuring-bind (mode x0 y0 old-x old-y)
              dragging-info 
            (unless (and (= old-x x) (= old-y y))
              (if mode
                  (capi::apply-in-pane-process (om-get-view view) 'capi:update-drawing-with-cached-display-from-points 
                                               pane x0 y0 x y 
                                               :extend (if (numberp mode) mode 0))
                (capi::apply-in-pane-process (om-get-view view) 'capi:update-drawing-with-cached-display pane))
              (setf (nth 3 dragging-info) x (nth 4 dragging-info) y))
            ))))
    (call-next-method)))

(defmethod om-click-release-handler :after ((self om-graphic-object) pos)
  (declare (ignore pos))
  ;(print (list self *click-motion-action* *click-motion-view*))
  (when *click-motion-action* ; (equal *click-motion-view* self) 
    (let* ((view *click-motion-view*)
           (motion-info (temp-data view)))
      (setf *click-motion-action* nil)
      (when motion-info
        (destructuring-bind (motion-action release-action x0 y0 old-x old-y draw-pane)
            motion-info  
          (let ((dragging-info (capi:output-pane-free-cached-display (or draw-pane view))))
           ;; nothing more to do...
           (setf (capi:output-pane-cached-display-user-info (or draw-pane view)) nil))
          (when release-action
            (capi::apply-in-pane-process (om-get-view view) release-action view 
                                         (om-make-point x0 y0) (om-convert-coordinates pos self view))
            )))
      ;(setf *click-motion-action* nil)
      (setf (temp-data view) nil))))

|#

