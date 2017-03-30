(in-package :om)


;===========================
;BOXES
;===========================
 
;; name is redefined with :initarg

(defclass OMBox (OMVPObject) 
   ((container :initform nil :accessor container)
    (inputs :initform nil :accessor inputs)
    (outputs :initform nil :accessor outputs)
    (reference :initform nil :initarg :reference :accessor reference)
    (group-id :initform nil :accessor group-id :initarg :group-id) ;; id used for grouping boxes (e.g. tracks in a sequencer)
    (value :initform nil :accessor value)   ;; value = a list of one or more value(s)
    ;;; graphical attributes
    (frame :initform nil :accessor frame :documentation "the OMFrame representing to the box")
    (box-x :initform nil :accessor box-x :initarg :box-x)
    (box-y :initform nil :accessor box-y :initarg :box-y)
    (box-w :initform nil :accessor box-w :initarg :box-w)
    (box-h :initform nil :accessor box-h :initarg :box-h)
    (display :initform :hidden :accessor display :initarg :display)
    (selected :initform nil :accessor selected :initarg :selected)
    (cache-display :initform nil :accessor cache-display)
    (border :initform 1.5 :accessor border :initarg :border)
    (color :initform (om-def-color :light-gray) :accessor color :initarg :color)
    (text-font :initform nil :accessor text-font :initarg :text-font)
    (text-color :initform nil :accessor text-color :initarg :text-color)
    (text-align :initform :left :accessor text-align :initarg :text-align)
    (icon-pos :initform :left :accessor icon-pos :initarg :icon-pos)
    (show-name :initform t :accessor show-name :initarg :show-name)
    ;;; REACTIVE FLAGS
   (state-lock :accessor state-lock :initform nil) ;; this box is the event source and his evaluation is locked
   (gen-flag :accessor gen-flag :initform nil) ;; this box has already been valuated during this generation 
   (push-tag :accessor push-tag :initform nil) ;; this box is tagged as being is in the notification path for the current event
    )
   (:documentation "OMBox is is the more general class for connectable objects (boxes).")
   (:metaclass omstandardclass))


(add-preference-module :appearance "Appearance")
(add-preference-section :appearance "Boxes")
(add-preference :appearance :box-size "Size (%)" '(25 50 75 100 125 150 175 200) 100)
(add-preference :appearance :box-color "Color" :color (om-def-color :gray))
(add-preference :appearance :box-border "Border" :bool t)
(add-preference :appearance :box-font "Text font" :font (om-def-font :font1))
(add-preference :appearance :box-align "Text align" '(:left :center :right) :left)


(defgeneric gen-code (box &optional numout)  
   (:documentation "Used to generate Lisp code from a box call."))

(defgeneric* omng-box-value (box &optional numout)
   (:documentation "Eval the output indexed by <numout> for the box <self>."))

(defmethod current-box-value ((self OMBox) &optional (numout nil))
  (if numout (nth numout (value self)) (value self)))

(defmethod omng-box-value ((self OMBox) &optional numout)
  (current-box-value self))

;;; when the value is set "from the outside" (i.e. not from evaluation)
(defmethod set-value ((self OMBox) value)
  (setf (value self) value))

(defmethod set-box-outputs ((self OMBox) outputs)
  (setf (outputs self) outputs)
  (when (container self)
    (report-modifications (editor (container self)))))

(defmethod set-box-inputs ((self OMBox) inputs)
  (setf (inputs self) inputs)
  (when (container self)
    (report-modifications (editor (container self)))))

(defmethod set-show-name ((box OMBox)) 
  (when (visible-property (get-properties-list box) :showname)
    (setf (show-name box) (not (show-name box)))
    (update-inspector-for-box box)
    (om-invalidate-view (frame box))))


(defmethod initialize-instance :after ((self OMBox) &rest args)
  (setf (inputs self) (create-box-inputs self))
  (setf (outputs self) (create-box-outputs self)))

(defmethod create-box-inputs ((self OMBox)) nil)

(defmethod box-n-outs ((self OMBox)) (length (outputs self)))

(defmethod create-box-outputs ((self OMBox)) nil)

;;; some boxes can not be renames (ex. functions)
(defmethod allow-rename ((self OMBox)) t)

(defmethod get-box-frame-class ((self OMBox)) 'OMBoxFrame)


;;; applies to the NG Box but must be done when the frames are in place..
;;; (because it uses the positions of the frame areas)
(defmethod update-connections ((self OMBox))
  (mapcar #'(lambda (c)
               (update-points c) 
               (update-graphic-connection c))
          (get-box-connections self)))

(defmethod omng-move ((self OMBox) position)
  (setf (box-x self) (om-point-x position)
        (box-y self) (om-point-y position))
  ;(update-connections self)
  (when (container self)
    (report-modifications (editor (container self)))))


;;; will be true for OMBoxFrame
(defmethod move-frame-to-position ((self t) (container-view t) position) nil)

;;; sometimes there is no boxframe at all (e.g. in sequencer tracks)
(defmethod update-frame-to-position ((self t) position) nil)

(defmethod update-frame-to-position ((self OMBox) position)
  (move-frame-to-position (frame self) (om-view-container (frame self)) position))

(defmethod move-box ((self OMBox) dx dy)
  (let ((pos (om-make-point (max 0 (+ (box-x self) dx)) (max 0 (+ (box-y self) dy)))))
    (omng-move self pos)
    (when (frame self) (update-frame-to-position self pos))
    (update-connections self)
    ))

(defmethod scale-in-x-? ((self OMBox)) t)
(defmethod scale-in-y-? ((self OMBox)) t)

(defmethod omng-resize ((self OMBox) size)
  (setf (box-w self) (om-point-x size)
        (box-h self) (om-point-y size))
  (mapcar 'update-points (get-box-connections self))
  (when (container self)
    (report-modifications (editor (container self)))))


;;; e.g. in sequencer-track-view
(defmethod reset-frame-size ((frame t)) nil)
(defmethod redraw-connections ((self t)) nil)

(defmethod initialize-size ((self OMBox))
  (let ((size (default-size self)))
    (omng-resize self size)
    (when (frame self)
      (reset-frame-size (frame self))
      (om-invalidate-view (frame self))
      (redraw-connections (frame self)))))

(defmethod minimum-size ((self OMBox))
  (let ((text-size (om-string-size (name self) (text-font self))))
  (om-make-point (+ 10 
                    (max (+ 8 text-size (if (equal (icon-pos self) :left) 20 0))
                         22
                         (* (length (inputs self)) 10)
                         (* (box-n-outs self) 10)))
                 (+ 28 (if (equal (icon-pos self) :top) 20 0))
                 )))

(defmethod maximum-size ((self OMBox))
  (if (equal (icon-pos self) :left)
      (om-make-point 500 (+ (get-icon-size self) 8))
    (om-make-point 500 200)))
      
(defmethod get-icon-size ((self OMBox)) 20)
(defmethod default-size ((self OMBox)) (minimum-size self))
(defmethod get-icon-id-from-reference ((self t)) nil)

(defmethod om-cleanup ((self OMBox))
  (omng-delete self))

(defmethod omng-delete ((box OMBox))
  (when (and (frame box) (om-view-container (frame box)))
    (om-remove-subviews (om-view-container (frame box)) (frame box))
    (setf (frame box) nil))
  (call-next-method))

(defmethod get-update-frame ((self OMBox)) (frame self))

(defmethod select-box ((self OMBox) selected)
  (setf (selected self) selected)
  (when (frame self)
    (om-invalidate-view (frame self))
    (let ((ed (and (om-view-window (frame self)) (editor (om-view-window (frame self))))))
      (when ed (update-inspector-for-editor ed))
      )))

(defmethod redraw-frame ((self OMBox))
  (when (frame self)
    (om-invalidate-view (frame self))))
    

(defmethod editor-box-selection ((editor OMEditor) (box OMBox))
  (unless (or (om-shift-key-p) (selected box))
    (select-unselect-all editor nil))
  (select-box box (if (om-shift-key-p) (not (selected box)) t)))

(defmethod editor-box-selection ((editor OMEditor) (box null))
  (unless (om-shift-key-p) (select-unselect-all editor nil)))

;;;=============================
; PROPERTIES
;;;=============================

(defmethod object-name-in-inspector ((self OMBox)) (format nil "~A box" (reference self)))

(defmethod get-properties-list ((self OMBox))
  '(("Appearance" ;;; category
               ;(:icon "Icon position" (:left :top :noicon) icon-pos)
               (:color "Color" :color color)
               (:border "Border" :bool border)
               (:text-font "Text font" :font text-font) ;;; id text type slot-name
               (:align "Text align" (:left :center :right) text-align)
               )
    ("Structure" ;;; category
               (:group-id "Group/Track" (:none 1 2 3 4 5 6 7 8) group-id)
               )))

(defmethod update-container-groups ((self t)) self)

(defmethod set-property ((object OMBox) (prop-id (eql :group-id)) val)
  (call-next-method)
  (when (container object) (update-container-groups (container object))))

(defmethod close-internal-element :after ((self OMBox)) 
  (close-inspector-for-box self))

;;;===========================
;;; CHACHE DISPLAY SYSTEM
;;;===========================
(defstruct cache-display (text) (draw))

;;; shorthands
(defmethod get-display-text ((self OMBox)) (cache-display-text (cache-display self)))
(defmethod get-display-draw ((self OMBox)) (cache-display-draw (cache-display self)))

;;; to be redefined by objects if they have a specific draw/cache strategy
(defmethod get-cache-display-for-text ((object t))
  (loop for sl in (append (mapcar 'slot-name (remove-if-not 'slot-initargs (class-direct-instance-slots (class-of object))))
                          (additional-class-attributes object))
        collect (list sl (get-slot-val object sl))))

;;; to be redefined by objects if they have a specific draw/cache strategy
(defmethod get-cache-display-for-draw ((object t)) nil)

(defmethod set-cache-display ((self OMBox) object)
  (setf (cache-display self)
        (make-cache-display :text (get-cache-display-for-text object)
                            :draw (get-cache-display-for-draw object))))

(defmethod reset-cache-display ((self OMBox))
  (setf (cache-display self) nil))

(defun ensure-cache-display-text (box object)
  (if (cache-display box) 
      (or (cache-display-text (cache-display box))
          (setf (cache-display-text (cache-display box)) (get-cache-display-for-text object)))
    (progn
      (setf (cache-display box)
            (make-cache-display :text (get-cache-display-for-text object)))
      (cache-display-text (cache-display box)))))

(defun ensure-cache-display-draw (box object)
  (if (cache-display box) 
      (or (cache-display-draw (cache-display box))
          (setf (cache-display-draw (cache-display box)) (get-cache-display-for-draw object)))
    (progn
      (setf (cache-display box)
            (make-cache-display :draw (get-cache-display-for-draw object)))
      (cache-display-draw (cache-display box)))))






