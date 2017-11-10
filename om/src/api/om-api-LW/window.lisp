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
; GUI WINDOWS
;===========================================================================


(export '(om-window
          om-make-window
          om-window-title
          om-set-window-title
          om-interior-size
          om-set-interior-size
          om-def-higher-level-win
          om-front-window
          om-get-all-windows
          om-fullscreen-window
          om-screen-size
          om-maximize-window
   
          om-dialog
          om-textured-dialog
          om-modal-dialog
          om-return-from-modal-dialog
          
          om-windoid
          om-no-border-win
          
          om-window-check-before-close
          om-window-close-event
          om-close-window
          om-window-open-p
          om-window-visible-p
          om-select-window
          om-hide-window
          om-show-window
          om-open-window
          om-window-activate
          om-window-resized
          om-window-moved
          om-window-maximized
          om-minimum-size
          
          om-remove-all-subviews
          om-substitute-subviews

          ) :om-api)


(in-package :oa)



;;;=============================================
;;; OM-ABSTRACT-WINDOW
;;; Superclass of all windows
;;;=============================================
(defclass om-abstract-window (om-graphic-object capi::interface) 
  ((resizable :initarg :resizable :initform t :accessor resizable)
   (fullscreen :initarg :fullscreen :initform nil :accessor fullscreen))
  (:default-initargs
   :auto-menus nil
   :geometry-change-callback 'om-geometry-change-callback
   :activate-callback 'om-activate-callback
   :confirm-destroy-function 'om-confirm-destroy-function
   :help-callback 'om-help-callback
   :create-callback 'om-create-callback
   :destroy-callback 'om-destroy-callback
   ))

;;; appelé à la création de la fenetre
;;; (pas a l'apparition)
(defmethod om-create-callback ((self om-abstract-window))
  ;; (set-hint-table self (list :default-x (vx self) :default-y (vy self) :default-width (vw self) :default-height (vh self)))
  ;; (loop for item in (vsubviews self) do
  ;;      (om-add-subviews thewin item))
  (setf (initialized-p self) t)
  t)


;;; Called by CAPI when destroying the window
(defmethod om-destroy-callback  ((self om-abstract-window))
  (capi::execute-with-interface self 'om-window-close-event self)
  (setf (initialized-p self) nil))

;;; OM event handler
(defmethod om-window-close-event ((self t)) nil)

(defmethod om-activate-callback (self activatep))

(defun om-confirm-destroy-function (self)
  (om-window-check-before-close self))

(defmethod om-window-check-before-close ((self t)) t)

(defmethod om-help-callback ((interface t) pane type key))

(defmethod om-view-key-handler ((self om-abstract-window) key) nil)

(defmethod om-get-view ((self om-abstract-window)) (capi::pane-layout self))

(defmethod om-window-title ((self om-abstract-window))
  (capi::interface-title self))

(defmethod om-set-window-title ((self om-abstract-window) (title string))
  (setf (capi::interface-title self) title))

(defmethod om-view-position ((self om-abstract-window)) 
  (if (capi::interface-visible-p self)
      (let ((point (capi::interface-geometry self)))
        (om-make-point (first point) (second point)))
    (om-make-point (vx self) (vy self))))

(defmethod om-set-view-position ((self om-abstract-window) pos-point) 
  (when (capi::interface-visible-p self)
      (capi::execute-with-interface self 
                              'capi::set-top-level-interface-geometry 
                              self 
                              :x (om-point-x pos-point)
                              :y (om-point-y pos-point)))
  (setf (vx self) (om-point-x pos-point))
  (setf (vy self) (om-point-y pos-point)))

(defmethod om-view-size ((self om-abstract-window))
  (if (capi::interface-visible-p self)
      (let ((point (capi::interface-geometry self)))
        #+win32(om-make-point (- (third point) (car point)) (- (fourth point) (cadr point)))
        #-win32(om-make-point (third point) (fourth point))  
        )
    (om-make-point (vw self) (vh self))))

(defun set-not-resizable (win &key (w nil w-supplied-p) (h nil h-supplied-p))
  (let ((width (if w-supplied-p (or w (om-point-x (om-view-size win)))))
        (height (if h-supplied-p (or h (om-point-y (om-view-size win))))))
    (capi::set-hint-table win  (list :external-min-width width :external-max-width width 
                               :external-min-height height :external-max-height height
                               ))))

(defmethod om-set-view-size ((self om-abstract-window) size-point) 
  (let ((wi (om-point-x size-point))
        (he (om-point-y size-point)))
    (capi::execute-with-interface self 
                            #'(lambda (w h)
                                ;(print (list w h))
                                (unless (resizable self)
                                  (set-not-resizable self :w w :h h))
                                (when (capi::interface-visible-p self)
                                  (capi::set-top-level-interface-geometry 
                                   self :width w :height h
                                   ;:x (vx self) :y (vy self)    ;; ?
                                   ))
                                (setf (vw self) w (vh self) h)
                                (om-window-resized self size-point)
                                )
                            wi he)
    ))
    
(defmethod om-interior-size ((self om-abstract-window))
 ;(print (capi::interface-geometry self))
  #+win32(if (capi::interface-visible-p (capi::pane-layout self))
             (multiple-value-bind (w h) (capi::pinboard-pane-size (capi::pane-layout self))
               (om-make-point w h))
           (om-view-size self))
  ;;;(om-subtract-points (om-view-size self) (om-make-point 30 30))
  #-win32(om-view-size self)
  )

(defmethod om-set-interior-size ((self om-abstract-window) size)
  (om-set-view-size self size))

(defmethod om-geometry-change-callback ((self om-abstract-window) x y w h)
  
  (unless (and (vx self) (vy self) (= x (vx self)) (= y (vy self)))
    (om-window-moved self (om-make-point x y)))
  
  (unless (and (vw self) (vh self) (= w (vw self)) (= h (vh self)))  
    (om-window-resized self (om-make-point w h))
    #+windows(om-invalidate-view self)
    )
  (setf (vx self) x (vy self) y (vw self) w (vh self) h))


(defmethod om-window-resized ((self om-abstract-window) size)
  (declare (ignore self size)) nil)

(defmethod om-window-moved ((self om-abstract-window) pos)
  (declare (ignore self pos)) nil)

(defmethod om-window-maximized ((self om-abstract-window))
  (equal (capi::top-level-interface-display-state self) :maximized))

(defmethod om-maximize-window ((self om-abstract-window))
  (setf (capi::top-level-interface-display-state self) :maximized))

(defmethod om-minimum-size ((self om-abstract-window))
  (declare (ignore self)) nil)

(defmethod capi::calculate-constraints ((self om-abstract-window))
  (call-next-method)
  (let ((msize (om-minimum-size self)))
    (when msize
      (with-geometry self
        (setf capi:%min-width% (om-point-x msize))
        (setf capi:%min-height% (om-point-y msize))
        ))))

(defmethod om-fullscreen-window ((self om-abstract-window))
  ;;; pour l'instant, maximize... à faire en fullscreen
  (setf (fullscreen self) t)
  ;(execute-with-interface self 
  ;                        'set-top-level-interface-geometry 
  ;                        self 
  ;                        :width (capi::screen-width (capi:convert-to-screen self))
  ;                        :height (capi::screen-height (capi:convert-to-screen self))
  ;                        :x 0 :y 0)
  (om-set-view-position self (om-make-point 0 0))

  (om-set-view-size self (om-make-point (capi::screen-width (capi:convert-to-screen self)) 
                                       (- (capi::screen-height (capi:convert-to-screen self)) 20)))
  )

(defun om-screen-size ()
  (om-make-point (capi::screen-width (capi:convert-to-screen nil))
                 (capi::screen-height (capi:convert-to-screen nil))))

;; test, parfois collect-interfaces plante...
(defun om-front-window () 
  #+(or darwin macos macosx)
  (capi:screen-active-interface (capi:convert-to-screen))
  #-(or darwin macos macosx)
  ; --> ça plante (parfois)
  (car (capi::collect-interfaces 'om-abstract-window :screen :any :sort-by :visible))
)

(defun om-get-all-windows (class)
  (capi::collect-interfaces class))

(defmethod interface-match-p ((self om-abstract-window) &rest initargs  &key name)
  (string-equal (capi::capi-object-name self) name))


(defmethod om-select-window ((self capi::interface))
  #+cocoa(capi::raise-interface self)
  #-cocoa(capi::find-interface (type-of self) :name (capi::capi-object-name self))
  self)

(defmethod om-hide-window ((self om-abstract-window))
  (capi::execute-with-interface self 
                                #'(lambda (x) (setf (top-level-interface-display-state x) :hidden))
                                self))

(defmethod om-window-visible-p ((self om-abstract-window))
  (capi::interface-visible-p self))

(defmethod om-show-window ((self t))
  (capi::execute-with-interface self 
                                #'capi::show-interface
                                self)
  self)

(defmethod om-open-window ((self t))
  (capi::execute-with-interface self 
                                #'(lambda (x) (capi::display x))
                                self))

;;; Explicit call to close window
(defmethod om-close-window ((win t))
  (when win (capi:apply-in-pane-process win 'capi:quit-interface win)))


(defmethod initialized-p ((self t)) t)

(defun om-window-open-p (window) 
  (and (initialized-p window) 
       (not (equal (capi::interface-created-state window) :destroyed))))

;;; Called by CAPI when activating the window
(defmethod om-activate-callback ((self om-abstract-window) activatep)
  ;(print (multiple-value-list (get-constraints self)))
  (when t ; activatep
    (om-window-activate self activatep)
    ))

;;; OM event handler
(defmethod om-window-activate ((self om-abstract-window) &optional (activatep t)) t)
 

;;;=============================================
;;; OM-WINDOW
;;; A simple window with a panel able to host subviews
;;;=============================================
(defclass om-window (om-abstract-window) ())

(defmethod correct-win-h ((win om-window))
  ;#+win32(om-set-view-size win (om-add-points (om-view-size win) (om-make-point 0 20)))
 t)


;;;====================
;;; DIALOG
;;; Pour mettre des dialog-items
;;; Des interfaces modales, etc.
;;;====================
(defclass om-dialog (om-abstract-window) ())

(defclass om-textured-dialog (om-dialog) ())

(defmethod window-dialog-p ((self om-dialog)) t)

(defmethod om-select-window ((self om-dialog))
  (if (initialized-p self)
    #+cocoa(capi::raise-interface self)
    #-cocoa(capi::find-interface (type-of self) :name (capi::capi-object-name self))
    (capi::display self))
  self)

;(defmethod make-window-layout ((self om-dialog) &optional color)
;  (make-instance 'window-layout :internal-border nil :visible-border nil :accepts-focus-p nil
;                 #+cocoa :background #+cocoa :transparent
;                 ))

(defun om-modal-dialog (dialog &optional (owner nil))
  ;(update-for-subviews-changes dialog t)
  (capi::display-dialog dialog :owner (or owner (capi:convert-to-screen)) ; (om-front-window)
                        :position-relative-to (and owner :owner) :x (vx dialog) :y (vy dialog)))

(defun om-return-from-modal-dialog (dialog val) 
  (capi::exit-dialog val))

;;;====================
;;; PALETTE
;;; Always on top, petite barre de titre
;;;====================
(defclass om-windoid (om-window) 
  ((accept-key-evt :accessor accept-key-evt :initarg :accept-key-evt :initform nil))
  (:default-initargs :draw-with-buffer t)
  ; :window-styles '(:internal-borderless :always-on-top :borderless :shadowed))
  )

(defmethod internal-display ((self om-windoid))
  ;#+win32(capi::display self :process nil)
  (capi::display self))


;(defmethod make-window-layout ((self om-windoid) &optional color)
;  (make-instance 'window-layout :internal-border nil :visible-border nil 
;                 :background :transparent))

(defmethod om-set-view-size ((self om-windoid) size-point) 
  (let ((w (om-point-x size-point))
        (h (om-point-y size-point)))
    ;(set-not-resizable self :w w :h h)
    (call-next-method)))

(defmethod correct-win-h ((win om-windoid)) nil)

;;;====================
;;; NO BORDER
;;;====================
(defclass om-no-border-win (om-window) ())

(defmethod om-fullscreen-window ((self om-no-border-win)) (call-next-method))
(defmethod correct-win-h ((win om-no-border-win)) nil)



;;;================================
;;; MAKE WINDOW
;;;================================

(defmethod correct-win-h ((win t)) nil)

(defmethod internal-display ((self t))
  (capi::display self))
 
(defmethod window-dialog-p ((self t)) nil)

; :movable-by-window-background
; :borderless
; :shadowed 
(defmethod get-window-styles-from-class ((class t))
  (cond 
   ((subtypep class 'om-windoid) '(:always-on-top 
                                   :toolbox 
                                   ;:textured-background  ;--> removed because it creates movable-by-bg
                                   :no-geometry-animation
                                   :ignores-keyboard-input
                                   )) ;  
   ((subtypep class 'om-no-border-win) '(:borderless :shadowed :always-on-top))
   ((subtypep class 'om-window) '(:motion-events-without-focus))
   ((subtypep class 'om-textured-dialog) '(:textured-background))
   ((subtypep class 'om-dialog) '())
   ))

;(defclass om-default-view-layout (om-view) ()
;  (:default-initargs :background :transparent))

(defun om-make-window (class &rest attributes 
			  &key position size
			  name title owner font bg-color border menu-items
                          win-layout
			  (show t) subviews (resizable t) (close t) (minimize t) (maximize t) (topmost nil) (toolbox nil)
			  &allow-other-keys)  
  (let* ((winparent (or owner (capi:convert-to-screen)))
         (winname (or name title (string (gensym))))
         (wintitle (or title name "OM Window"))
         (w (and size (om-point-x size)))
         (h (and size (om-point-y size)))
         (x (and position (if (equal position :centered) (round (- (* (capi::screen-width winparent) 0.5) (* w 0.5))) (om-point-x position))))
         (y (and position (if (equal position :centered) (round (- (* (capi::screen-height winparent) 0.5) (* h 0.5))) (om-point-y position))))
         (style (append (get-window-styles-from-class class) 
                        (when (not minimize) (list :never-iconic))
                        (when topmost (list :always-on-top))
                        (when toolbox (list :toolbox))
                        (list :no-character-palette
                              ;:internal-borderless
                              )))
         win)
    
    (setf win (apply 'make-instance 
                     (append (list class
                                   :title wintitle :name winname
                                   :x x :y y 
                                   :width w :height h
                                   :auto-menus nil
                                   :parent winparent
                                   :internal-border border :external-border nil
                                   :display-state (if show :normal :hidden)
                                   :menu-bar-items menu-items
                                   :window-styles style
                                   :resizable resizable
                                   ;;; OM-GRAPHIC-OBJECT SLOTS
                                   :vx x :vy y :vw w :vh h
                                   :allow-other-keys t
                                   )
                             attributes
                             )))

    (capi::execute-with-interface 
     win
     #'(lambda ()
         (setf (capi::pane-layout win) 
               (if (or (null win-layout) (symbolp win-layout))
                   (make-instance (or win-layout 'om-simple-layout) :internal-border nil :visible-border nil :accepts-focus-p nil)
                 win-layout))
         (when bg-color (setf (capi::simple-pane-background (capi::pane-layout win)) (omcolor-c bg-color)))
     
     ;(when (setf layout (make-window-layout thewin ,bg-color))
     ;  #+cocoa(if (drawable-layout layout) (setf (capi::output-pane-display-callback layout) 'om-draw-contents-callback))
     ;  (setf (capi::pane-layout thewin) layout)
     ;  )
     ;(when subviews (mapc (lambda (sv) (om-add-subviews thewin sv)) subviews))
    
         (when subviews (setf (capi::layout-description (capi::pane-layout win)) subviews))
     
         (correct-win-h win)

         (unless (window-dialog-p win)
           (internal-display win))

         (unless (equal resizable t) ; (and w h (or (not resizable) (window-dialog-p win)))
           (apply 'set-not-resizable (append (list win) 
                                             (unless (equal resizable :w) (list :w w))
                                             (unless (equal resizable :h) (list :h h))
                                             )))
         ))
     
    win))





