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
; DRAG AND DROP
;=========================================================================

(in-package :om-api)

(export '(
          om-drag-view
          om-drop-view
          om-drag-start
          om-drag-receive
          om-drag-enter-view
          om-drag-leave-view
          om-drag-area
          om-draw-contents-for-drag
          om-import-files-in-app
          om-drag-string-in-app
          ) :om-api)


;;;==================
;;; DRAG/DROP VIEW
;;;==================

(defclass om-drag-view () 
  ((cursor-pos :initform (om-make-point 0 0) :accessor om-drag-view-cursor-pos)))

(defmethod om-drag-view-p ((self t)) nil)
(defmethod om-drag-view-p ((self om-drag-view)) t)

;;; called before drag: must return T or drag will not start
(defmethod om-drag-start ((self om-drag-view) pos) t)

(defclass om-drop-view ()
  ((drag-image :initform nil :accessor drag-image))
  (:default-initargs 
   :drop-callback 'om-drop-callback))

(defmethod om-drop-view-p ((self t)) nil)
(defmethod om-drop-view-p ((self om-drop-view)) t)

(defmethod om-draw-contents-for-drag ((self om-drag-view))
  (om-draw-contents self))

(defmethod om-drag-area ((self om-drag-view))
  (let ((p1 (om-view-position self))
        (p2 (om-add-points (om-view-position self) (om-view-size self))))
  (values (om-point-x p1)
          (om-point-y p1)
          (om-point-x p2)
          (om-point-y p2)
          )))

;;;===========================================================
;;; DOES NOT WORK ON WINDOWS
;;;===========================================================

(defmethod build-d&d-image ((dragged om-drag-view) pane)
  (multiple-value-bind (x1 y1 x2 y2) (om-drag-area dragged)
    (let ((pp (gp:create-pixmap-port pane 
                                     (abs (round (- x2 x1)))
                                     (abs (round (- y2 y1))) 
                                     :background :transparent :clear t)))
      (unwind-protect
          (progn 
                (om-with-focused-view pp
                  (om-draw-contents-for-drag dragged))
                (values (gp:make-image-from-port pp) 
                        (+ (- (om-point-x (om-view-position dragged)) (min x1 x2)) 
                           (om-point-x (om-drag-view-cursor-pos dragged)))
                        (+ (- (om-point-y (om-view-position dragged)) (min y1 y2))
                           (om-point-y (om-drag-view-cursor-pos dragged)))
                        ))
        (progn 
          (gp:destroy-pixmap-port pp)
          ;(print "destroy")
          )))))


;;;===========================================================
;;; FOR WINDOWS ONLY
;;; CAPI D&D IMAGE DOES NOT WORK !
;;;===========================================================
#+mswindows
(defclass clone-view (om-item-view)
  ((view :accessor view :initarg :view :initform nil)
   (clic-pos :accessor clic-pos :initarg :clic-pos :initform nil)
   ))

#+mswindows
(defmethod capi::over-pinboard-object-p ((self clone-view) x y) nil)

#+mswindows
(defmethod om-draw-contents ((self clone-view))
  ;(om-draw-rect 0 0 100 100)
  (om-draw-contents-for-drag (view self)))

#+mswindows
(defmethod start-windows-d&d (self  pane  pos) nil)

#+mswindows
(defmethod start-windows-d&d ((self om-drag-view) (pane om-drop-view) pos)
  (when (drag-image pane) 
    (om-remove-subviews pane (drag-image pane)))
  (multiple-value-bind (x1 y1 x2 y2) (om-drag-area self)
    (let ((topleft (om-make-point (min x1 x2) (min y1 y2))))
  (setf (drag-image pane)
        (om-make-graphic-object 'clone-view 
                                :view self :clic-pos (om-add-points pos (om-subtract-points (om-view-position self)  topleft))
                                :position topleft
                                :size (om-make-point (- x2 x1) (- y2 y1))))
  (om-add-subviews pane (drag-image pane)))))

#+mswindows
(defmethod end-windows-d&d ((pane om-drop-view)) nil)

#+mswindows
(defmethod end-windows-d&d ((pane om-drop-view))
  (when (drag-image pane) 
    (om-remove-subviews pane (drag-image pane))))

#+mswindows
(defmethod update-windows-d&d ((pane om-drop-view) pos)
  (when (drag-image pane)
    ;(om-set-view-cursor self (om-get-cursor (if (om-command-key-p) :wait nil)))
    (om-set-view-position (drag-image pane) (om-subtract-points pos (clic-pos (drag-image pane))))
    ))



#|
;;; for click&drag actions
#+mswindows
(defmethod om-click-motion-handler :after ((self om-drop-view) pos)
  (when (drag-image self)
    (om-set-view-position (drag-image self) (om-subtract-points pos (clic-pos (drag-image self))))))

#+mswindows
(defmethod om-click-release-handler :after ((self om-drop-view) pos)
  (unwind-protect 
      (when (drag-image self)
        (or (om-drag-receive self (view (drag-image self)) pos) 
            (om-beep)))
     (when (drag-image self) (om-remove-subviews self (drag-image self)))
    (setf (drag-image self) nil)
    ))

#+mswindows
(defmethod om-click-motion-handler :after ((self om-drag-view) pos)
  (om-click-motion-handler (om-view-container self) 
                           (om-convert-coordinates pos self (om-view-container self))))

#+mswindows
(defmethod om-click-release-handler :after ((self om-drag-view) pos)
  (om-click-release-handler (om-view-container self) 
                            (om-convert-coordinates (om-subtract-points pos (om-drag-view-cursor-pos self)) self (om-view-container self))))

|#

;;;==============================================
;;; END WINDOWS-SPECIFIC PART  
;;;==============================================



(defun internal-drag-start (self pos)
  (and (om-drag-start self pos)
       #+mswindows (oa::start-windows-d&d self (om-view-container self) pos) 
       (capi:drag-pane-object  
        (om-get-view self)  ; (capi::pane-layout (capi::top-level-interface self))
        self 
        :plist (list :om-object self)
        :operations '(:move :copy)
        :image-function #'(lambda (pane) (build-d&d-image self pane))
        )
       ))

(defmethod om-click-motion-handler :after ((self om-drag-view) pos)
  (setf (om-drag-view-cursor-pos self) (om-point-mv pos :y -1))
  (internal-drag-start self pos))

;(defmethod om-view-click-handler :after ((self om-drag-view) pos) nil)
;  (setf (om-drag-view-cursor-pos self) (om-point-mv pos :y -1))
;  (internal-drag-start self pos)
;  )

(defvar *last-pinboard-under-mouse* nil)

(defun om-drop-callback (self drop-object stage)
  (flet ((set-effect-for-operation (drop-object)
           ;; In a real application, this would be clever about which effects to allow.
           (dolist (effect '(:move :copy))
             (when (capi:drop-object-allows-drop-effect-p drop-object effect)
               (setf (capi:drop-object-drop-effect drop-object) effect)
               (return t)))))
    (case stage
      (:formats
       (capi:set-drop-object-supported-formats drop-object '(:string :value :om-object :filename-list)))
      (:enter
       (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
         (let ((dropview (or (capi::pinboard-object-at-position self x y)  
                             self)))
           (set-effect-for-operation drop-object)
           (when (and *last-pinboard-under-mouse*
                      (not (equal dropview *last-pinboard-under-mouse*)))
             (om-drag-leave-view *last-pinboard-under-mouse*))
           (om-drag-enter-view dropview)
           (setf *last-pinboard-under-mouse* dropview))))
      (:leave
       (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
         (let ((dropview (or (capi::pinboard-object-at-position self x y)  
                             self)))
           (set-effect-for-operation drop-object)
           (om-drag-leave-view dropview))))
      (:drag        
       (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
         (let ((dropview (or (capi::pinboard-object-at-position self x y)  
                             self)))
           (when (and dropview (not (equal dropview *last-pinboard-under-mouse*)))
             (when *last-pinboard-under-mouse*
               (om-drag-leave-view *last-pinboard-under-mouse*))
             (om-drag-enter-view dropview)
             (setf *last-pinboard-under-mouse* dropview))
            ;(print (capi:drop-object-provides-format drop-object :om-object))
           (set-effect-for-operation drop-object)
           
           #+mswindows
           (update-windows-d&d self 
                               (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
                                 (om-make-point x y))) 
           
           )))
      (:drop
       #+mswindows (oa::end-windows-d&d self) 
       (multiple-value-bind (x y) (capi::current-pointer-position :relative-to self :pane-relative-p t)
         (let ((dropview (or (capi::pinboard-object-at-position self x y) self))
               )
           ;(print dropview)
           (setf *last-pinboard-under-mouse* nil)
           (if (or 
                (and (capi:drop-object-provides-format drop-object :filename-list)
                     (om-import-files-in-app self (capi:drop-object-get-object drop-object self :filename-list) 
                                             (om-make-point (capi::drop-object-pane-x drop-object) (capi::drop-object-pane-y drop-object)))) 
                (and (capi:drop-object-provides-format drop-object :string)
                     (om-drag-string-in-app self (capi:drop-object-get-object drop-object self :string)
                                            (om-make-point (capi::drop-object-pane-x drop-object) (capi::drop-object-pane-y drop-object)))))
               (set-effect-for-operation drop-object)
             (let* ((dragged-view (capi:drop-object-get-object drop-object self :om-object)))
               (when dragged-view
                 (let ((drop-position (om-make-point (- (capi::drop-object-pane-x drop-object) 
                                              (om-point-x (om-drag-view-cursor-pos dragged-view)))
                                           (- (capi::drop-object-pane-y drop-object)
                                              1 ;;; VERY BIZARRE: OTHERWISE THERE IS ALWAYS A SHIFT OF 1 PIXEL....
                                              (om-point-y (om-drag-view-cursor-pos dragged-view))))))
               (set-effect-for-operation drop-object)
               (unless (or (not dragged-view)
                           (om-drag-receive
                            dropview dragged-view
                            drop-position
                            (capi:drop-object-drop-effect drop-object)))
                 (setf (capi:drop-object-drop-effect drop-object) nil))
               )))))))
          
      )))


(defmethod om-import-files-in-app ((self t) file-list position) nil)
(defmethod om-drag-string-in-app ((self t) str position) nil)

(defmethod om-drag-receive ((view t) (dragged-view t) position &optional (effect nil))
  (declare (ignore view dragged-view effect)))

(defmethod om-drag-enter-view ((self t)) nil)
(defmethod om-drag-leave-view ((self t)) nil)


