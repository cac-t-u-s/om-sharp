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
; Author: J. Bresson, C. Agon
;=========================================================================

;;===========================================================================
; OM-GRAPHIC OBJECTS ACTIONS CALLBACKS
;;===========================================================================


(export '(
          om-view-key-handler
          om-view-click-handler
          om-view-right-click-handler
          om-view-doubleclick-handler
          om-view-mouse-enter-handler
          om-view-mouse-leave-handler
          om-view-mouse-motion-handler
          om-view-pan-handler
          om-view-zoom-handler

          om-init-motion
          om-click-motion-handler
          om-click-release-handler
           
          om-shift-key-p
          om-control-key-p
          om-command-key-p
          om-option-key-p
          
          om-mouse-position

          ) :om-api)



(in-package :om-api)

(defparameter +control-button+ #+macosx :hyper #-macosx :control)
(defparameter +2nd-press+ #+win32 :second-press #-win32 :nth-press)
(defparameter +2nd-press-callback+ #+win32 'om-double-clic-callback #-win32 'om-multiple-clic-callback)

(defclass om-interactive-object () ()
  (:default-initargs 
   :input-model  `(
                   ;;; CALLBACKS ARGUMENTS = (SHIF CMD OPTION)
                   ((:button-1 :motion :shift ,+control-button+ :meta)  om-clic-motion-callback (t t t))
                   ((:button-1 :motion :shift ,+control-button+)  om-clic-motion-callback (t t nil))
                   ((:button-1 :motion :shift :meta)  om-clic-motion-callback (t nil t))
                   ((:button-1 :motion :meta ,+control-button+)  om-clic-motion-callback (nil t t))
                   ((:button-1 :motion :shift)  om-clic-motion-callback (t nil nil))
                   ((:button-1 :motion ,+control-button+)  om-clic-motion-callback (nil t nil))
                   ((:button-1 :motion :meta)  om-clic-motion-callback (nil nil t))
                   ((:button-1 :motion)  om-clic-motion-callback (nil nil nil))
                   
                   ((:button-1 :press :shift ,+control-button+ :meta) om-clic-callback (t t t))
                   ((:button-1 :press :shift ,+control-button+) om-clic-callback (t t nil))
                   ((:button-1 :press :shift :meta) om-clic-callback (t  nil t))
                   ((:button-1 :press :meta ,+control-button+) om-clic-callback (nil t t))
                   ((:button-1 :press :shift) om-clic-callback (t nil nil))
                   ((:button-1 :press ,+control-button+) om-clic-callback (nil t nil))
                   ((:button-1 :press :meta) om-clic-callback (nil nil t))
                   ((:button-1 :press) om-clic-callback (nil nil nil))
                               
                   ((:motion :shift ,+control-button+) om-motion-callback (t t nil))
                   ((:motion :shift) om-motion-callback (t nil nil))
                   ((:motion ,+control-button+) om-motion-callback (nil t nil))
                   (:motion om-motion-callback (nil nil nil))
                   
                   ((:button-1 :release :shift ,+control-button+ :meta)  om-clic-release-callback (t t t))
                   ((:button-1 :release :shift ,+control-button+)  om-clic-release-callback (t t nil))
                   ((:button-1 :release :shift :meta)  om-clic-release-callback (t nil t))
                   ((:button-1 :release :meta ,+control-button+)  om-clic-release-callback (nil t t))
                   ((:button-1 :release :shift)  om-clic-release-callback (t nil nil))
                   ((:button-1 :release ,+control-button+)  om-clic-release-callback (nil t nil))
                   ((:button-1 :release :meta)  om-clic-release-callback (nil nil t))
                   ((:button-1 :release)  om-clic-release-callback (nil nil nil))
                  
                   ((:button-1 ,+2nd-press+ :shift ,+control-button+ :meta) ,+2nd-press-callback+ (t t t))
                   ((:button-1 ,+2nd-press+ :shift ,+control-button+) ,+2nd-press-callback+ (t t nil))
                   ((:button-1 ,+2nd-press+ :shift :meta) ,+2nd-press-callback+ (t nil t))
                   ((:button-1 ,+2nd-press+ :meta ,+control-button+) ,+2nd-press-callback+ (nil t t))
                   ((:button-1 ,+2nd-press+ :shift ) ,+2nd-press-callback+ (t nil nil))
                   ((:button-1 ,+2nd-press+ ,+control-button+) ,+2nd-press-callback+ (nil t nil))
                   ((:button-1 ,+2nd-press+ :meta) ,+2nd-press-callback+ (nil nil t))
                   ((:button-1 ,+2nd-press+) ,+2nd-press-callback+ (nil nil nil))
                  
                  ;(:post-menu om-context-menu-callback)
                   ((:button-3 :press) om-context-menu-callback)
                   ((:button-3 :release) om-right-clic-callback (nil nil nil))
                   ((:button-3 :second-press) om-right-clic-callback (nil nil nil))
                   
                   #-linux((:touch :pan) om-pan-callback)
                   #-linux((:touch :zoom) om-zoom-callback)

                   
                   (:gesture-spec om-char-spec-callback)
                   ;; (:character om-char-callback (nil nil nil))
                   )

   ))

#|
(defmethod om-motion-callback ((self om-interactive-object) x y modifiers) t)
(defmethod om-clic-callback ((self om-interactive-object) x y modifiers))
(defmethod om-clic-release-callback ((self om-interactive-object) x y modifiers))
(defmethod om-clic-motion-callback ((self om-interactive-object) x y modifiers))
(defmethod om-double-clic-callback ((self om-interactive-object) x y modifiers))
(defmethod om-right-clic-callback ((self om-interactive-object) x y))
(defmethod om-context-menu-callback ((self om-interactive-object) x y))
(defmethod om-char-callback ((self om-interactive-object) x y c modifiers))
(defmethod om-char-spec-callback ((self om-interactive-object) x y modifiers))
|#

;;;=======================
;;; MOUSE
;;;=======================
;;;===============
;;; TOOLS
;;;===============

(defun internal-mouse-position (view)
  (or (ignore-errors 
        (multiple-value-bind (x y)
            (if view (capi::current-pointer-position :relative-to view :pane-relative-p t)
              (capi::current-pointer-position))
          (om-make-point x y)))
      (om-make-point 0 0)))

(defmethod om-mouse-position ((view null))
  (internal-mouse-position nil))

(defmethod om-mouse-position ((view om-graphic-object))
  (internal-mouse-position view))


;;; Event dispatcher on item subviews (om-view catches events but om-item-views do not)
;;; if function callback is non-NIL for one of the subviews, recursive dispatching is stopped.
(defmethod apply-in-item-subview ((self t) function position) nil)

(defmethod apply-in-item-subview ((self capi::pinboard-layout) function position)
  (let ((po (capi::pinboard-object-at-position self (om-point-x position) (om-point-y position))))
    (if (and po (om-item-view-p po)) (funcall function po (om-convert-coordinates position self po))
      (or po (funcall function self position)))))

;;; Event dispatcher on subviews (when the container has the focus)
;;; useful for mouse move
(defmethod apply-in-subview ((self om-graphic-object) function position)
  (let ((clicked self))
    ;(print (list "subviews of" self))
    (loop for item in (om-subviews self)
          while (equal clicked self) do
          (when (and item (om-view-contains-point-p item position)) ;; (om-convert-coordinates position self (vcontainer item))))
            (setf clicked (apply-in-subview item function (om-add-points 
                                                           (om-convert-coordinates position self item)
                                                           (om-scroll-position item))))))
    (when (or (null clicked) (equal clicked self))
      (setf clicked (apply-in-item-subview self function position)))
    clicked))



; (om-convert-coordinates position self item)

;;;=================
;;; MOTION & CURSOR
;;;=================
(defvar *last-visited-view* nil)

(defmethod om-motion-callback ((self om-interactive-object) x y modifiers)
  (set-meta-keys modifiers)
  (handle-tooltips-in-motion self (om-make-point x y))
  ;(print "==============") (print self)
  (apply-in-subview self 'internal-motion-callback (om-make-point x y))
  )

;; CF. CURSOR.LISP
(defmethod update-view-cursor ((self t) pos) nil)

(defmethod internal-motion-callback ((self om-interactive-object) pos)
  (unless (equal *last-visited-view* self)
    (when *last-visited-view*
      (om-view-mouse-leave-handler *last-visited-view*))
    ;(om-set-focus self)
    (update-view-cursor self pos)
    (om-view-mouse-enter-handler self)
    (setf *last-visited-view* self))
  (om-view-mouse-motion-handler self pos))

(defmethod om-view-mouse-motion-handler ((self om-interactive-object) position)
  (declare (ignore self position)))

(defmethod om-view-mouse-enter-handler ((self t))
  (declare (ignore self)))

(defmethod om-view-mouse-leave-handler ((self t))
  (declare (ignore self)))


;;;=============
;;; CLIC 
;;;=============
(defvar *clicked-view* nil)

(defmethod om-clic-callback ((self om-interactive-object) x y modifiers)
  (om-with-error-handle 
    (set-meta-keys modifiers)
    (apply-in-item-subview self 'om-view-click-handler (om-make-point x y))))

(defmethod om-view-click-handler ((self om-interactive-object) position) nil)
(defmethod om-view-click-handler :before ((self om-interactive-object) position) 
  (setf *clicked-view* self))

(defmethod om-right-clic-callback ((self om-interactive-object) x y modifiers) 
  (om-with-error-handle 
    (set-meta-keys modifiers)
    (apply-in-item-subview self 'om-view-right-click-handler (om-make-point x y))))

(defmethod om-view-right-click-handler ((self om-interactive-object) position) nil)
(defmethod om-view-right-click-handler :before ((self om-interactive-object) position) 
  (setf *clicked-view* self))


;;;==============
;;; RELEASE
;;;==============
(defmethod om-clic-release-callback ((self om-interactive-object) x y modifiers)
  (set-meta-keys modifiers)
  (unless (equal *clicked-view* :abort) 
    (if *clicked-view*
        (om-click-release-handler *clicked-view* (om-convert-coordinates (om-make-point x y) self *clicked-view*))
      (apply-in-item-subview self 'om-click-release-handler (om-make-point x y)))))

(defmethod om-click-release-handler ((self om-interactive-object) pos) nil) 

(defmethod om-click-release-handler :before ((self om-interactive-object) position) 
  (setf *clicked-view* nil
        *drag-disabled* nil))

;;;=================
;;; DOUBLE CLIC
;;;=================
(defmethod om-double-clic-callback ((self om-interactive-object) x y modifiers)
  (om-multiple-clic-callback self x y modifiers 2))

(defmethod om-multiple-clic-callback ((self om-interactive-object) x y modifiers n)
  (setf *clicked-view* :abort)
  ;(print (list self x y))
  (or (apply-in-item-subview self 'om-view-doubleclick-handler (om-make-point x y))
      (apply-in-item-subview self 'om-view-click-handler (om-make-point x y))
      ))

(defmethod om-view-doubleclick-handler ((self om-interactive-object) pos) nil)

;;;=================
;;; CLIC + MOVE
;;;=================

(defmethod om-init-motion (self x y))

(defmethod om-clic-motion-callback ((self om-interactive-object) x y modifiers)
  (set-meta-keys modifiers)
  (unless (equal *clicked-view* :abort)
    (if *clicked-view*
        (om-click-motion-handler *clicked-view* (om-convert-coordinates (om-make-point x y) self *clicked-view*))
      ; ?!! verifier si tout va bien...
      ;(apply-in-item-subview *clicked-view* 'om-click-motion-handler (om-convert-coordinates (om-make-point x y) self *clicked-view*))
      (apply-in-item-subview self 'om-click-motion-handler (om-make-point x y))))
  )
   
(defmethod om-click-motion-handler (self pos) t)

;;;=================
;;; TOUCH GESTURES
;;;=================

(defmethod om-zoom-callback ((self om-interactive-object) x y zoom-factor)
  (om-view-zoom-handler self (om-make-point x y) zoom-factor))

(defmethod om-pan-callback ((self om-interactive-object) x y delta-x delta-y) 
  (om-view-pan-handler self (om-make-point x y) delta-x delta-y))

;;; need to set *clicked-view* ??
(defmethod om-view-zoom-handler ((self om-interactive-object) position zoom-factor) nil)
(defmethod om-view-pan-handler ((self om-interactive-object) position delta-x delta-y) nil)


;;;=====================
;;; KEYBOARD
;;;=====================
        
;;; CHAR-CALLBACK: NOT USED
(defun get-om-character (c)
  (case (char-code c)
    (63276  :om-key-pageup)
    (63277 :om-key-pagedown)
    (63275  :om-key-end)
    (63273  :om-key-home)
    (63234  :om-key-left)
    (63232  :om-key-up)
    (63235 :om-key-right)
    (63233  :om-key-down)
    (127  :om-key-delete)
    (8  :om-key-delete)
    (3  :om-key-enter)
    (13  :om-key-return)
    (27  :om-key-esc)
    (9  :om-key-tab)
    (otherwise c)))

;; never used
(defmethod om-char-callback ((self om-interactive-object) x y c modifiers)
  (print (list "char callback" self x y spec))
  (set-meta-keys modifiers)
  (om-with-error-handle 
    (om-view-key-handler self (get-om-character c)))
  (release-meta-keys))


;;;=====================
;;; DECODE CHAR-SPEC 
;;;=====================
(defun get-om-spec-character (c)
  (cond ((integerp c) (get-om-character (code-char c)))
        ((equal :up c) :om-key-up)
        ((equal :down c) :om-key-down)
        ((equal :left c) :om-key-left)
        ((equal :right c) :om-key-right)
        ((equal :next c) :om-key-pagedown)
        ((equal :prior c) :om-key-pageup)
        ((equal :end c) :om-key-end)
        ((equal :home c) :om-key-home)
        ((equal :kp-enter c) :om-key-enter)   
        (t nil)))

(defun get-om-spec-modifiers (mod)
  (case mod
    (0 '(nil nil nil))
    (1 '(t nil nil))  ; SHIFT
    (2 '(nil nil nil))  ; CTRL
    (3 '(t nil nil))  ; CTRL + SHIFT
    (4 '(nil nil t))  ; ALT
    (5 '(t nil t))  ; ALT + SHIFT
    (6 '(nil nil t))  ; ALT + CTRL
    (7 '(t nil t))  ; ALT + SHIFT + CTRL
    (8 '(nil t nil))  ; CMD ?
    (9 '(t t nil))  ; CMD + SHIFT
    (10 '(nil t nil)) ; CMD + CTRL
    (11 '(t t nil)) ; CMD + CTRL + SHIFT
    (12 '(nil t t)) ; CMD + ALT
    (13 '(t t t)) ; CMD + ALT + SHIFT
    (14 '(nil t t)) ; CMD + ALT + CTRL
    (15 '(t t t)) ; CMD + ALT + CTRL + SHIFT
    (otherwise '(nil nil nil))))

(defvar *om-shift-key-p* nil)
(defvar *om-control-key-p*  nil)
(defvar *om-command-key-p*  nil)
(defvar *om-option-key-p*  nil)

;;; LIST = SHIFT - CMD - ALT
(defun set-meta-keys (list)
  (setf *om-shift-key-p* (first list))
  (setf *om-command-key-p*  (second list) )
  (setf *om-option-key-p*   (third list)))

(defun release-meta-keys ()
  (setf *om-shift-key-p* nil)
  (setf *om-command-key-p*  nil)
  (setf *om-option-key-p*  nil))

(defun om-shift-key-p () *om-shift-key-p* )
(defun om-command-key-p () *om-command-key-p*)
(defun om-option-key-p ()  *om-option-key-p*)

(defun om-control-key-p () nil)

(defmethod om-char-spec-callback ((self om-interactive-object) x y spec)
  ;(print (list "char spec" self x y spec))
  (let ((data (sys:gesture-spec-data spec))
        (modifiers (sys:gesture-spec-modifiers spec)))
     (set-meta-keys (get-om-spec-modifiers modifiers))
     (om-view-key-handler self (get-om-spec-character data))
     (release-meta-keys)
     t))

;;; Exported callback
(defmethod om-view-key-handler ((self t) key) 
  (declare (ignore key)) nil)

;;; by default the key actions are transferred to the window
;;; but any active view can redefine the method
(defmethod om-view-key-handler ((self om-graphic-object) key)
  ;(print (list "gc" self key)) 
  (om-view-key-handler (om-view-window self) key))






