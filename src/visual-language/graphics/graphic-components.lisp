;============================================================================
; om#: visual programming language for computer-assisted music composition
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

;===========================
; Simple horizontal bar
;===========================

(defclass bar-item (om-item-view)
  ((fg-color :accessor fg-color :initarg :fg-color :initform (om-def-color :black))
   (thick :accessor thick :initarg :thick :initform 1)))

(defmethod om-draw-contents ((self bar-item))
  (call-next-method)
  (om-with-line-size (thick self)
    (om-with-fg-color (fg-color self)
      (om-draw-line 0 0 (w self) 0))))

;===========================
; Just a picture
;===========================

(defclass picture-view (om-item-view)
  ((pict :initform nil :initarg :pict :accessor pict)))

(defmethod om-draw-contents ((self picture-view))
  (call-next-method)
  (when (pict self)
    (om-draw-picture self
                     (pict self) (om-make-point 0 0)
                     (om-make-point (w self) (h self)))))

;;;=====================
;;; 3D-border view
;;;=====================

(defclass 3Dborder-view (om-view)
  ((c+ :accessor c+ :initform (om-make-color 0.835 0.835 0.843) :initarg :c+)
   (c++ :accessor c++ :initform (om-make-color 0.87 0.87 0.88) :initarg :c++)
   (c- :accessor c- :initform (om-make-color 0.604 0.604 0.604) :initarg :c-)
   (c-- :accessor c-- :initform (om-make-color 0.514 0.514 0.514) :initarg :c--)))

(defmethod om-draw-contents ((self 3Dborder-view))
  (call-next-method)
  (let ((x (om-h-scroll-position self))
        (y (om-v-scroll-position self))
        (w (om-point-x (om-view-size self)))
        (h (om-point-y (om-view-size self))))
    (draw-3D-border self x y (+ x w) (+ y h))))

(defun draw-3D-border (self x y xx yy)
  (om-with-fg-color self (c++ self)
    (om-draw-line (+ x 1) y (- xx 1) y)
    (om-draw-line x (+ y 1) x (- yy 1)))
  (om-with-fg-color self (c+ self)
    (om-draw-line (+ x 2) (+ y 1) (- xx 2) (+ y 1))
    (om-draw-line (+ x 1) (+ y 2) (+ x 1) (- yy 2)))
  (om-with-fg-color self (c-- self)
    (om-draw-line (+ x 1) (- yy 1) (- xx 1) (- yy 1))
    (om-draw-line (- xx 1) (+ y 1) (- xx 1) (- yy 1)))
  (om-with-fg-color self (c- self)
    (om-draw-line (+ x 2) (- yy 2) (- xx 2) (- yy 2))
    (om-draw-line (- xx 2) (+ y 2) (- xx 2) (- yy 2))))



;==========================================================
; custom button with pict in "resources/di/"
;==========================================================

;(let ((win (om-make-window 'om-window))
;      (v (om-make-view 'om-view :size (om-make-point 500 500)))
;      (but (om-make-graphic-object 'om-icon-button :icon :but :icon-pushed :but-pushed :lock-push t)))
;  (om-add-subviews win v)
;  (om-add-subviews v but)
;  win)

(defclass om-icon-button (om-item-view)
  ((icon :initform nil :accessor icon :initarg :icon)
   (icon-pushed :initform nil :accessor icon-pushed :initarg :icon-pushed)
   (icon-disabled :initform nil :accessor icon-disabled :initarg :icon-disabled)
   (id :initform nil :accessor id :initarg :id)
   (action :initform nil :accessor action :initarg :action)
   (lock-push :initform nil :accessor lock-push :initarg :lock-push)
   (pushed :initform nil :accessor pushed :initarg :pushed)
   (enabled :initform t :accessor enabled :initarg :enabled)
   (text :initform nil :accessor text :initarg :text)
   (fg-color :initform nil :accessor fg-color :initarg :fg-color)
   (font :initform nil :accessor font :initarg :font)))

(defmethod om-set-fg-color ((self om-icon-button) color)
  (setf (fg-color self) color)
  (om-invalidate-view self))

(defmethod button-select ((self om-icon-button))
  (setf (pushed self) t)
  (om-invalidate-view self))

(defmethod button-unselect ((self om-icon-button))
  (setf (pushed self) nil)
  (om-invalidate-view self))

(defmethod button-enable ((self om-icon-button))
  (setf (enabled self) t)
  (om-invalidate-view self))

(defmethod button-disable ((self om-icon-button))
  (setf (enabled self) nil)
  (om-invalidate-view self))

(defmethod om-view-doubleclick-handler ((self om-icon-button) where)
  (om-view-click-handler self where))

(defmethod om-view-click-handler ((self om-icon-button) where)
  (declare (ignore where))
  (when (enabled self)
    (if (lock-push self)
        (setf (pushed self) (not (pushed self)))
      (setf (pushed self) t))
    (om-invalidate-view self)))

(defmethod om-click-release-handler ((self om-icon-button) where)
  (when (and (enabled self) (action self))
    (om-with-error-handle
      (apply (action self) (list self))))
  (unless (lock-push self) (setf (pushed self) nil))
  (om-invalidate-view self))

(defmethod om-draw-contents ((self om-icon-button))
  (call-next-method)
  (let* ((icn (or (and (pushed self) (icon-pushed self))
                  (and (not (enabled self)) (icon-disabled self))
                  (icon self))))
    (om-draw-picture icn :w (w self) :h (h self)))
  (when (text self)
    (let* ((ff (or (font self) (om-def-font :font1)))
           (cc (or (fg-color self) (om-def-color :black)))
           (wh (values-list (om-string-size (text self) ff)))
           (yy (round (+ (- (cadr wh) (if (pushed self) 5 6)) (h self)) 2))
           (xx (max 0 (- (round (w self) 2) (ceiling (car wh) 2)))))
      (om-with-fg-color self cc
        (om-with-font ff
                      (om-draw-string xx yy (text self))))))
  (when (and (lock-push self) (pushed self) (not (icon-pushed self)))
    (om-draw-rect 0 0 (w self) (h self) :fill t :color (om-make-color-alpha (om-def-color :black) 0.5))))


;==========================================================
; custom view to pick a color
;==========================================================

(defclass color-view (om-view)
  ((color :accessor color :initarg :color :initform (om-make-color 0 0 0))
   (after-fun :accessor after-fun :initform nil :initarg :after-fun)
   (with-alpha :accessor with-alpha :initform t :initarg :with-alpha)
   (border :accessor border :initform t :initarg :border)))

(defmethod om-draw-contents ((self color-view))
  (let ((c (or (color self) (om-def-color :gray))))

    (unless (or (= (om-color-a c) 1) (= (om-color-a c) 0))
      (om-draw-rect 0 0 (om-width self) (om-height self) :color (om-make-color 1 1 1) :fill t))
    (om-draw-rect 0 0 (om-width self) (om-height self) :color (or (color self) (om-def-color :gray))
                  :fill (color self))

    (when (not (om-view-enabled self))
      (om-draw-rect 0 0 (om-width self) (om-height self)
                    :color (om-make-color-alpha (om-def-color :white) 0.5)
                    :fill t))

    (when (= (om-color-a c) 0)
      (om-draw-line 0 0 (om-width self) (om-height self) :color (om-def-color :gray))
      (om-draw-line 0 (om-height self) (om-width self) 0 :color (om-def-color :gray)))

    (when (border self)
      (om-draw-rect 0 0 (om-width self) (om-height self)
                    :color (om-def-color :gray)
                    :fill nil))
    ))

(defmethod om-view-click-handler ((self color-view) pos)
  (declare (ignore pos))
  (when (om-view-enabled self)
    (let ((color (om-choose-color-dialog :color (color self) :alpha (with-alpha self) :owner self)))
      (when color (setf (color self) color)
        (om-invalidate-view self)
        (when (after-fun self) (funcall (after-fun self) self))))))



;==========================================================
; custom view to pick a font
;==========================================================

(defclass font-chooser-view (om-view)
  ((font :accessor font :initarg :font :initform nil)
   (after-fun :accessor after-fun :initarg :after-fun :initform nil)
   (face-chooser :accessor face-chooser)
   (size-chooser :accessor size-chooser)
   (style-chooser :accessor style-chooser)))


(defmethod initialize-instance :after ((self font-chooser-view) &rest args)
  (om-add-subviews
   self
   (om-make-layout
    'om-column-layout
    :delta 0
    :subviews
    (list
     (setf (face-chooser self)
           (om-make-di 'om-popup-list
                       :enabled (om-view-enabled self)
                       :size (omp 116 22)
                       :font (om-def-font :font1)
                       :items (om-list-all-fonts)
                       :selected-item (and (font self) (om-font-face (font self)))
                       :di-action #'(lambda (list)
                                      (setf (font self)
                                            (om-make-font
                                             (om-get-selected-item list)
                                             (om-font-size (font self))
                                             :style (om-font-style (font self))))

                                      (when (after-fun self)
                                        (funcall (after-fun self) (font self))))
                       ))

     (om-make-layout
      'om-row-layout
      :delta 2
      :subviews
      (list
       (setf (size-chooser self)
             (om-make-di 'om-popup-list
                         :enabled (om-view-enabled self)
                         :size (omp 50 22)
                         :font (om-def-font :font1)
                         :items '(8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 24 28 32 36 40 52 64)
                         :selected-item (and (font self) (om-font-size (font self)))
                         :di-action #'(lambda (list)
                                        (setf (font self)
                                              (om-make-font
                                               (om-font-face (font self))
                                               (om-get-selected-item list)
                                               :style (om-font-style (font self))))

                                        (when (after-fun self)
                                          (funcall (after-fun self) (font self))))
                         ))
       (setf (style-chooser self)
             (om-make-di 'om-popup-list
                         :enabled (om-view-enabled self)
                         :size (omp 64 22)
                         :font (om-def-font :font1)
                         :items '("plain" "bold" "italic" "bold-italic")
                         :selected-item (cond ((and (font self)
                                                    (find :bold (om-font-style (font self)))
                                                    (find :italic (om-font-style (font self))))
                                               "bold-italic")
                                              ((and (font self)
                                                    (find :bold (om-font-style (font self))))
                                               "bold")
                                              ((and (font self)
                                                    (find :italic (om-font-style (font self))))
                                               "italic")
                                              (t "plain"))
                         :di-action #'(lambda (list)
                                        (setf (font self)
                                              (om-make-font
                                               (om-font-face (font self))
                                               (om-font-size (font self))
                                               :style (case (om-get-selected-item-index list)
                                                        (1 '(:bold))
                                                        (2 '(:italic))
                                                        (3 '(:bold :italic))
                                                        (otherwise '(:plain)))))
                                        (when (after-fun self)
                                          (funcall (after-fun self) (font self))))
                         ))
       ))
     ))
   ))


(defmethod set-enabled ((self font-chooser-view) enabled)
  (om-set-view-enabled self enabled)
  (om-enable-dialog-item (face-chooser self) enabled)
  (om-enable-dialog-item (size-chooser self) enabled)
  (om-enable-dialog-item (style-chooser self) enabled))

(defmethod set-font ((self font-chooser-view) font)
  (setf (font self) font)
  (om-set-selected-item (face-chooser self) (om-font-face font))
  (om-set-selected-item (size-chooser self) (om-font-size font))
  (om-set-selected-item (style-chooser self)
                        (cond ((and (font self)
                                    (find :bold (om-font-style (font self)))
                                    (find :italic (om-font-style (font self))))
                               "bold-italic")
                              ((and (font self)
                                    (find :bold (om-font-style (font self))))
                               "bold")
                              ((and (font self)
                                    (find :italic (om-font-style (font self))))
                               "italic")
                              (t "plain"))))

;==========================================================
; custom view to change a text
;==========================================================

(defclass click-and-edit-text (om-view)
  ((text :accessor text :initform "" :initarg :text)
   (after-fun :accessor after-fun :initform nil :initarg :after-fun)
   (border :accessor border :initform t :initarg :border)
   (wrap-lines :accessor wrap-lines :initform nil :initarg :wrap-lines))
  (:default-initargs :resize-callback nil))

(defmethod om-draw-contents ((self click-and-edit-text))
  (when (border self)
    (om-with-fg-color (border self)
      (om-draw-rect 0 0 (om-width self) (om-height self))))
  (om-with-fg-color
      (if (om-view-enabled self) (om-get-fg-color self) (om-def-color :gray))
    (om-draw-string 0 (cadr (multiple-value-list (om-string-size (text self) (om-get-font self))))
                    (text self)
                    :wrap (if (wrap-lines self) (om-width self) nil))))

(defmethod om-view-click-handler ((self click-and-edit-text) pos)
  (declare (ignore pos))
  (when (om-view-enabled self)
    (let ((txt (om-get-user-string "" :initial-string (text self))))
      (when txt (setf (text self) txt)
        (om-invalidate-view self)
        (when (after-fun self) (funcall (after-fun self) self))))))


;==============
; NUMBOX
;==============
;;; !! except set/get-value eveything is hanled as intergers (expt 10 decimals)
(defclass numbox (om-item-text)
  ((value   :initform 0 :initarg :value :accessor value)
   (min-val :initform 0 :initarg :min-val :accessor min-val)
   (max-val :initform nil :initarg :max-val :accessor max-val)
   (decimals :initform 0 :initarg :decimals :accessor decimals)
   (enabled :initform t :initarg :enabled :accessor enabled)
   (db-click :initform nil :initarg :db-click :accessor db-click)
   (allow-nil :initform nil :initarg :allow-nil :accessor allow-nil)
   (change-fun :initform nil :initarg :change-fun :accessor change-fun)
   (after-fun :initform nil :initarg :after-fun :accessor after-fun))
  (:default-initargs :border t))

(defmethod om-view-cursor ((self numbox)) (om-get-cursor :v-size))

(defmethod initialize-instance :after ((self numbox) &rest args)
  (let ((v (getf args :value)))
    (when v (set-value self v)))
  (set-min-max self :min (getf args :min-val) :max (getf args :max-val))
  (om-set-fg-color self (if (enabled self) (om-def-color :black) (om-def-color :gray))))


;; the value internally is always an integer
(defmethod set-value ((self numbox) value)
  (setf (value self) (if (zerop (decimals self)) (round value)
                       (round (* value (expt 10 (decimals self))))))
  (om-set-text self
               (if (zerop (decimals self))
                   (format () " ~D" (round value))
                 (format () " ~v$" (decimals self) value)
                 ))
  (om-invalidate-view self))

(defmethod set-min-max ((self numbox) &key min max)
  (when min
    (setf (min-val self) (round (* min (expt 10 (decimals self))))))
  (when max
    (setf (max-val self) (round (* max (expt 10 (decimals self))))))
  )

(defmethod get-value ((self numbox))
  (if (zerop (decimals self))
      (value self)
    (float (/ (value self) (expt 10 (decimals self))))))


(defmethod enable-numbox ((self numbox) t-or-nil)
  (setf (enabled self) t-or-nil)
  (om-set-fg-color self (if (enabled self) (om-def-color :black) (om-def-color :gray))))

(defmethod map-mouse-increment ((self numbox))
  (cond ((om-shift-key-p) 10)
        ((om-command-key-p) 100)
        (t 1)))

(defmethod om-view-click-handler  ((self numbox) where)
  (when (enabled self)
    (let ((start-y (om-point-y where))
          (start-v (or (value self)
                       (numberp (allow-nil self)))))
      (when start-v
        (om-init-temp-graphics-motion self where NIL
                                      :motion #'(lambda (view position)
                                                  (declare (ignore view))
                                                  (let* ((inc (- start-y (om-point-y position)))
                                                         (new-val (+ start-v (* (map-mouse-increment self) inc))))
                                                  ;(print (list (min-val self) (max-val self)))
                                                    (when (and (min-val self) (< new-val (min-val self)))
                                                      (setf new-val (min-val self)))
                                                    (when (and (max-val self) (> new-val (max-val self)))
                                                      (setf new-val (max-val self)))

                                                    ;;; in principle that's ok now...
                                                    (when (and (or (null (min-val self)) (>= new-val (min-val self)))
                                                               (or (null (max-val self)) (<= new-val (max-val self))))
                                                      (when (and (numberp (allow-nil self))
                                                                 (= new-val (allow-nil self)))
                                                        (setf new-val nil))
                                                      (setf (value self) new-val)
                                                      (om-set-text self (format () " ~D" (get-value self)))
                                                      (om-invalidate-view self)

                                                      (when (and (change-fun self) (not (= (round new-val) start-v)))
                                                        (funcall (change-fun self) self)))))

                                      :release #'(lambda (view position)
                                                   (declare (ignore view position))
                                                   (when (after-fun self) (funcall (after-fun self) self)))
                                      )
        ))))


(defun mouse-screen-coordinates ()
  (om-subtract-points
   (om-mouse-position nil)
   (omp 20 20)))

(defmethod om-view-doubleclick-handler  ((self numbox) where)
  (when (and (enabled self) (db-click self))
    (let ((pos (mouse-screen-coordinates)))
      (open-mini-edit pos (get-value self)
                      #'(lambda (tf)
                          (let ((val (read-from-string (om-dialog-item-text tf) nil)))
                            (if (or
                                 (and (numberp val)
                                      (or (null (max-val self)) (<= val (/ (max-val self) (expt 10 (decimals self)))))
                                      (or (null (min-val self)) (>= val (/ (min-val self) (expt 10 (decimals self))))))
                                 (and (allow-nil self) (null val)))
                                (progn (set-value self val)
                                  (when (after-fun self) (funcall (after-fun self) self)))
                              (om-beep)))))
      )))


;==============
; MINI EDIT
;==============

(defclass mini-edit-window (om-no-border-win)
  ((textfield :accessor textfield :initform nil)
   (action :accessor action :initform nil)))

(defmethod om-window-activate ((self mini-edit-window) &optional activatep)
  (unless activatep ;; = loose focus
    (when (action self) (funcall (action self) (textfield self)))
    (om-close-window self)))

(defun open-mini-edit (position value action)
  (let ((text (format nil "~A" value))
        (font (om-def-font :font1)))
    (multiple-value-bind (w h)
        (om-string-size text font)
      (let* ((tf (om-make-di 'om-editable-text :text text :font font
                             :bg-color (om-def-color :white)
                             :size (omp (+ w 20) (+ h 20))
                             :border t
                             :di-action #'(lambda (item)
                                            (let ((window (om-view-window item)))
                                              (om-window-activate window nil)
                                              ))
                             ))
             (win (om-make-window
                   'mini-edit-window :position position
                   :win-layout (om-make-layout 'om-simple-layout
                                               :subviews (list tf)))))
        (setf (textfield win) tf)
        (setf (action win) action)
        win))))
