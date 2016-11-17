(in-package :om)

(defclass interface (named-object)
 ((shape :initform nil :initarg :shape :accessor shape :documentation "A control shape (checkbox, button, h-slider, v-slider, )")
  (name :initform nil :initarg :name :accessor name :documentation "A name")
  ;(dimensions :initform nil :initarg :dimensions :accessor dimensions :documentation "A list of dimensions, e.g ((5 7) (10 10) ...)")
  (bounds :initform nil :initarg :bounds :accessor bounds :documentation "A list of min and max values, e.g (-100 100)")
  (lambda-fun :initform nil :initarg :lambda-fun :accessor lambda-fun :documentation "A lambda function")
  (title :initform "Interface" :initarg title :accessor title)))

(defmethod get-editor-class ((self interface)) 'interface-editor)

;;;=========================


(defclass interface-editor (OMEditor) 
  ())

(defmethod object-has-editor ((self interface)) t)
;;;========================
;;; EDITOR WINDOW
;;;========================

(defclass interface-editor-window (OMEditorWindow) ())
(defmethod editor-window-class ((self interface-editor)) 'interface-editor-window)

(defmethod update-inspector-for-editor ((self interface-editor)) nil)

(defmethod update-to-editor ((self interface-editor) (from t))
  (om-invalidate-view (window self)))

(defmethod editor-window-init-size ((self interface-editor)) (om-make-point 16 64))

;;; redefined from patch editor
(defmethod init-window ((win interface-editor) editor)
  (call-next-method)
  ;(when (equal (view-mode editor) :maquette) 
  ;  (put-patch-boxes-in-editor-view (object editor) (get-g-component editor :maq-view)))
  (update-window-name editor)
  win)

(defun map-type (type)
  (cond ((string= type "button")
         'om-button)
        ((string= type "checkbox")
         'om-check-box)
        ((string= type "slider")
         'om-slider)
        (t
         'om-slider)))

(defmethod make-editor-window-contents ((editor interface-editor))
  (let ((interface (object-value editor))
        (di-size (omp 100 40))
        (txt-size (omp 100 20)))
    (om-make-view 'om-view
                  :bg-color (om-def-color :black)
                  :subviews (list
                             (om-make-view 'om-view
                                           :bg-color (om-def-color :white)
                                           :position (omp 2 2);(omp x (+ y-off (om-point-y txt-size)))
                                           :subviews (list
                                                      (om-make-di 'om-simple-text
                                                                  :text (name interface)
                                                                  :position (omp 5 2)
                                                                  :size txt-size)
                                                      (om-make-di (map-type (shape interface))
                                                                  :position (omp 4 (om-point-y txt-size))
                                                                  :size di-size
                                                                  :range (bounds interface)
                                                                  :di-action #'(lambda (b)
                                                                                 (funcall (lambda-fun interface))
                                                                                 ))))))))
            

                                