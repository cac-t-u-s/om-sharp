(in-package :om)

;;;======================================
;;; DIFFERENT KIND OF DATA (FRAMES/BUNDLES)
;;;======================================
(defclass* data-frame (timed-item)
  ((date :accessor date :initarg :date :initform 0 :documentation "date/time of the frame")
   (attributes :accessor attributes :initarg :attributes :initform nil :documentation "some additional attributes for drawing etc.")))

;;; TIME-SEQUENCE API
;(defmethod date ((self dataframe)) (date self))
;(defmethod (setf date) (date (self dataframe)) (setf (item-time self) date))
(defmethod item-get-time ((self data-frame)) (date self))
(defmethod item-set-time ((self data-frame) time) (setf (date self) time))

(defmethod data-size ((self data-frame)) 1)
(defmethod data-frame-text-description ((self data-frame)) '("DATA FRAME"))

(defmethod get-frame-action ((self data-frame)) 
  #'(lambda () (print "EMPTY ACTION")))


;;; SIMPLEST DATA FRAME
(defclass act-bundle (data-frame)
  ((date :accessor date :initarg :dateg :initform 0 :documentation "date/time of the frame")
   (actions :accessor actions :initarg :actions :initform nil)))

(defmethod get-frame-action ((self act-bundle)) 
  #'(lambda () (mapcar 'funcall (actions self))))

(defun make-act-bundle (date actions)
  (make-instance 'act-bundle
                 :date date
                 :actions actions))

;;;======================================
;;; MAIN CLASS
;;;======================================
(defclass* data-stream (named-object time-sequence schedulable-object)
  ((default-frame-type :accessor default-frame-type :initarg :default-frame-type :initform 'act-bundle)
   (frames :accessor frames :initarg :frames :initform nil :documentation "a list of timed data chunks")
   (slice-duration :accessor slice-duration :initform nil)  ;;; what is it for ?
   ))

(defmethod om-init-instance ((self data-stream) &optional args)
  (call-next-method)
  (setf (frames self) (sort (remove nil (frames self)) '< :key 'date)) 
  (let ((frames (find-value-in-kv-list args :frames)))
    (when frames (setf (default-frame-type self) (type-of (car frames))))
    (mapc #'(lambda (f) (setf (attributes f) nil)) frames))
  self)

;; redefine for other slots
(defmethod data-stream-frames-slot ((self data-stream)) 'frames)

(defmethod frames ((self data-stream)) (slot-value self (data-stream-frames-slot self)))
(defmethod (setf frames) (frames (self data-stream)) (setf (slot-value self (data-stream-frames-slot self)) frames))

(defmethod data-stream-get-frames ((self data-stream)) (frames self))
(defmethod data-stream-set-frames ((self data-stream) frames) (setf (frames self) frames))
 
;;; TIME-SEQUENCE API
(defmethod time-sequence-get-timed-item-list ((self data-stream)) (data-stream-get-frames self))
(defmethod time-sequence-set-timed-item-list ((self data-stream) list) (data-stream-set-frames self list))

(defmethod time-sequence-make-timed-item-at ((self data-stream) at)
  (make-instance (default-frame-type self) :date at))

(defmethod display-modes-for-object ((self data-stream))
  '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self data-stream) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (om-with-fg-color (om-def-color :dark-blue)
      (multiple-value-bind (fx ox)
          (conversion-factor-and-offset 0 (get-obj-dur self) w x)
        (multiple-value-bind (fy oy) 
            (conversion-factor-and-offset 100 -100 (- h 20) (+ y 10))
          (loop for frame in (data-stream-get-frames self) do
                (om-draw-circle (+ ox (* fx (or (date frame) 0))) (+ oy (* fy (getf (attributes frame) :posy 0))) 
                                2 :fill t)))))))


;;;======================================
;;; OBJECT PROPERTIES
;;;======================================
(defmethod play-obj? ((self data-stream)) t)

(defmethod get-obj-dur ((self data-stream)) 
  (or (slice-duration self) ;; ???
      (call-next-method)))

(defmethod get-action-list-for-play ((object data-stream) interval &optional parent)
  (mapcar 
   #'(lambda (frame) 
       (list (date frame)
             #'(lambda () (funcall (get-frame-action frame)))))
   (remove-if #'(lambda (date) (or (< date (car interval)) (> date (cadr interval)))) 
              (data-stream-get-frames object) 
              :key 'date)))

(defmethod prune-object ((self data-stream) t1-ms t2-ms)
  (let ((t1 (max 0 (or t1-ms 0)))
        (t2 (min (get-obj-dur self) (or t2-ms *positive-infinity*))))
    (data-stream-set-frames self (filter-list (data-stream-get-frames self)
                                     t1
                                     t2
                                     :key 'date)
          (slice-duration self) (- t2 t1))
    (om-invalidate-view self)))


;;;======================================
;;; OMMETHOD FOR PATCHES
;;;======================================

(defmethod* add-frame-in-data-stream ((self data-stream) frame) 
   (insert-timed-point-in-time-sequence self frame)
   frame)

(defmethod* add-frame-in-data-stream ((self t) frame) 
  (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* add-frame-in-data-stream ((self omboxeditcall) frame) 
   (insert-timed-point-in-time-sequence (get-box-value self) frame)
   (update-after-eval self)
   frame)



(defmethod* clear-data-stream ((self data-stream))
 (time-sequence-set-timed-item-list self nil))

(defmethod* clear-data-stream ((self t))
 (om-beep-msg "ERROR: ~A is not a valid DATA-STREAM" self))

;;; when editing in mode "box" => allows to update editor
(defmethod* clear-data-stream ((self omboxeditcall)) 
   (clear-data-stream (get-box-value self))
   (update-after-eval self))

;;;======================================
;;; EDITOR
;;;======================================

(defclass stream-editor (OMEditor play-editor-mixin) 
  ((timeline-editor :accessor timeline-editor :initform nil)
   (display-mode :accessor display-mode :initform :blocks)))

(defclass stream-panel (x-cursor-graduated-view y-graduated-view OMEditorView om-tt-view) ())

(defmethod editor-view-class ((self stream-editor)) 'stream-panel)
(defmethod object-has-editor ((self data-stream)) t)
(defmethod get-editor-class ((self data-stream)) 'stream-editor)
(defmethod get-obj-to-play ((self stream-editor)) (object-value self))

(defmethod alllow-insert-point-from-timeline ((self stream-editor)) nil)

;;; from play-editor-mixin
(defmethod cursor-panes ((self stream-editor)) 
  (cons (get-g-component self :main-panel)
        (cursor-panes (timeline-editor self))))

(defmethod editor-window-init-size ((self stream-editor)) (om-make-point 800 180))

;; lesser value and greater values in the ruler (bottom to top)
(defmethod y-range-for-object ((self data-stream)) '(-100 100))

(defmethod frame-display-modes-for-object ((self stream-editor) (object t))
  '((:blocks "blocks") (:bubbles "bubbles")))

(defun make-display-modes-menu (editor)
  (let ((object (object-value editor)))
    (when (> (length (frame-display-modes-for-object editor object)) 1)
      (om-make-di 'om-popup-list :size (omp 80 24) :font (om-def-font :font1)
                  :items (mapcar 'cadr (frame-display-modes-for-object editor object))
                  :di-action #'(lambda (item) 
                                 (setf (display-mode editor) 
                                       (car (find (om-get-selected-item item) 
                                                (frame-display-modes-for-object editor object) 
                                                :key 'cadr :test 'string-equal)))
                                 (clear-frame-attributes object)
                                 (set-graphic-attributes editor)
                                 (om-invalidate-view  (get-g-component editor :main-panel)))
                  ))))

(defun make-timeline-check-box (editor)
  (om-make-di 'om-check-box :text "timeline" :size (omp 65 24) :font (om-def-font :font1)
              :checked-p (editor-get-edit-param editor :show-timeline)
              :enable (timeline-editor editor)
              :di-action #'(lambda (item) 
                             ;(print (list (timeline-editor editor) editor))
                             (let ((timeline-ed (timeline-editor editor)))
                               (clear-timeline timeline-ed)
                               (om-invalidate-view (get-g-component timeline-ed :main-panel))
                               (when (om-checked-p item) (make-timeline-view timeline-ed))
                               (om-update-layout (main-view editor))))
              ))

(defun make-control-bar (editor) 
  (set-g-component editor :mousepos-txt (om-make-graphic-object 'om-item-text :size (omp 60 16)))
  (om-make-layout 
   'om-row-layout
   :ratios '(0.05 1 0.005 0.005 0.005 0.005)
   :subviews (list (get-g-component editor :mousepos-txt) 
                   nil 
                   (make-time-monitor editor)
                   (make-play-button editor :enable t) 
                   (make-pause-button editor :enable t) 
                   (make-stop-button editor :enable t))))
  

(defmethod init-editor ((editor stream-editor))
  (call-next-method)
  (setf (timeline-editor editor) 
        (make-instance 'timeline-editor 
                       :object (object editor) 
                       :container-editor editor))
  )

;;; sets the editor slightly longer that the actual object length  
(defmethod editor-view-after-init-space ((self t)) 1000)

(defmethod make-editor-window-contents ((editor stream-editor))
  
  (let* ((data-stream (object-value editor))
         (dur (if (zerop (get-obj-dur data-stream)) 
                  10000 
                (+ (get-obj-dur data-stream) (editor-view-after-init-space data-stream)))))

    (set-g-component editor :main-panel (om-make-view (editor-view-class editor) 
                                                      :editor editor :size (omp 50 60) 
                                                      :direct-draw t :bg-color (om-def-color :white) 
                                                      :scrollbars nil))
    
    (set-g-component editor :x-ruler (om-make-view 'time-ruler 
                                                   :related-views (list (get-g-component editor :main-panel))
                                                   :size (omp nil 20) 
                                                   :bg-color (om-def-color :white)
                                                   :vmin 0 :vmax dur
                                                   :x1 0 :x2 dur))
    
    (set-g-component (timeline-editor editor) :main-panel (om-make-layout 'om-row-layout))

    (om-make-layout 
     'om-column-layout 
     :ratios '(0.96 0.02)
     :subviews (list 
                ;;; first group with the 'main' editor:
                (om-make-layout 
                 'om-row-layout :ratios '(nil 100) :subviews 
                 (list (om-make-view 'om-view :size (omp 28 nil))
                       (om-make-layout 'om-column-layout :align :right
                                       :subviews (list 
                                                  (make-control-bar editor)
                                                  (get-g-component editor :main-panel) 
                                                  (get-g-component editor :x-ruler))
                                       :delta 2
                                       :ratios '(0.01 0.98 0.01))))
                ;;; the timeline editor:
                (get-g-component (timeline-editor editor) :main-panel)
                ;;; the bottom control bar:
                (om-make-layout 'om-row-layout 
                                :size (omp nil 40) 
                                :subviews (list (make-display-modes-menu editor) nil (make-timeline-check-box editor)))
                ))
    ))


(defmethod init-editor-window ((editor stream-editor))
  (call-next-method)
  (set-graphic-attributes editor)
  (update-views-from-ruler (get-g-component editor :x-ruler))
  (setf (y2 (get-g-component editor :main-panel)) (car (y-range-for-object (object-value editor)))
        (y1 (get-g-component editor :main-panel)) (cadr (y-range-for-object (object-value editor))))
  (set-shift-and-factor (get-g-component editor :main-panel)))


(defmethod update-to-editor ((editor stream-editor) (from ombox))
  
  (let* ((data-stream (object-value editor))
         (new-max-dur (if (zerop (get-obj-dur data-stream)) 
                          10000 
                        (+ (get-obj-dur data-stream) (editor-view-after-init-space data-stream)))))
  
    (set-graphic-attributes editor)
    (when (get-g-component editor :x-ruler)
      (setf (vmax (get-g-component editor :x-ruler)) new-max-dur)
      (set-ruler-range 
       (get-g-component editor :x-ruler) 
       (v1 (get-g-component editor :x-ruler))
       new-max-dur))
    (om-invalidate-view (get-g-component editor :main-panel))
    (update-to-editor (timeline-editor editor) editor)
    ))

(defmethod update-to-editor ((editor stream-editor) (from t))
  (call-next-method)
  (om-invalidate-view (get-g-component editor :main-panel)))

;;; todo : factorize a bit this procedure in different editors..
(defmethod editor-delete-contents-from-timeline ((self stream-editor) timeline-id sel)
  (let ((data-stream (object-value self)))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence data-stream point)) sel)
    (time-sequence-update-internal-times data-stream))
  (editor-invalidate-views self)
  (report-modifications self))

;;;===========================================
;;; FRAMES DRAW, SELECTION USING ATTRIBUTES
;;;===========================================
;;; attributes are no intrinsec data of the frame
;;; they should calculated depending on the frame data and according to a given mode of display

(defmethod frame-graphic-duration ((self data-frame))
  (if (zerop (item-duration self)) 200 (item-duration self)))
  
(defmethod compute-frame-color ((self data-frame) editor) 
  (declare (ignore editor))
  (om-random-color 0.4))

;;; posy and sizey must consider the y-range of the editor !

(defmethod compute-frame-posy ((self data-frame) editor) 
  (case (display-mode editor) 
    (:bubbles (om-random -50 50))
    (otherwise 60)))

(defmethod compute-frame-sizey ((self data-frame) editor) 
  (max 10 (* 2 (data-size self))))  ;;; 4 = arbitrary

(defmethod set-frame-attributes ((f data-frame) editor) 
  (setf (getf (attributes f) :color) (compute-frame-color f editor)
        (getf (attributes f) :posy) (compute-frame-posy f editor)
        (getf (attributes f) :sizey) (compute-frame-sizey f editor)))


(defmethod get-frame-attribute ((self data-frame) attribute editor)
  (unless (attributes self) (set-frame-attributes self editor))
  (getf (attributes self) attribute))
  

;; returns (x y w h)
(defmethod get-frame-area ((frame data-frame) editor)
  (let ((panel (get-g-component editor :main-panel)))
  (case (display-mode editor)
    (:bubbles (values 
               (- (x-to-pix panel (or (date frame) 0))  (dy-to-dpix panel (/ (get-frame-attribute frame :sizey editor) 2)))
               (- (h panel)
                  (y-to-pix panel (- (get-frame-attribute frame :posy editor) 
                                     (/ (get-frame-attribute frame :sizey editor) 2))))
                (dy-to-dpix panel (get-frame-attribute frame :sizey editor))
                (dy-to-dpix panel (get-frame-attribute frame :sizey editor))
               ))
    (:blocks (values (x-to-pix panel (date frame))
                      (- (h panel)
                         (y-to-pix panel (get-frame-attribute frame :posy editor)))
                     (max 3 (dx-to-dpix panel (frame-graphic-duration frame)))
                     (max 3 (dy-to-dpix panel (get-frame-attribute frame :sizey editor)))  ;; !! downwards
                     )))))


(defmethod draw-data-frame ((frame data-frame) editor i)
  (let* ((panel (get-g-component editor :main-panel)))
    (multiple-value-bind (x y w h)
        (get-frame-area frame editor)
      (om-with-fg-color (if (find i (selection editor)) (om-make-color-alpha (om-def-color :dark-red) 0.5)
                          (getf (attributes frame) :color (om-def-color :light-gray)))
        (case (display-mode editor) 
          (:bubbles
           (om-draw-circle (+ x (round w 2)) (+ y (round h 2)) (round h 2) :fill t)
           ;(om-draw-rect (+ x 20) (+ y 20) (- w 40) (- h 40) :fill nil)
           )
          (otherwise 
           ;(print (list 'rect w h)) 
           (om-draw-rect x y w h :fill t)))
        (om-with-font 
         (om-def-font :font1 :size 8)
         (om-draw-string (- x 5) 10 (number-to-string i))
         (when (find i (selection editor))
           (om-draw-string (- x 5) 20 (number-to-string (date frame))))
         )))))

(defmethod frame-at-pos ((editor stream-editor) position)
  (let ((frames (data-stream-get-frames (object-value editor))))
    (when frames 
      (position-if #'(lambda (f) 
                       (multiple-value-bind (x y w h)
                           (get-frame-area f editor)
                         (om-point-in-rect-p position x y w h)))
                   frames))))


(defmethod frames-in-area ((editor stream-editor) p1 p2)
  (loop for f in (data-stream-get-frames (object-value editor)) 
        for i = 0 then (1+ i)
        when (multiple-value-bind (x y w h)
                 (get-frame-area f editor)
               (rect-intersection 
                (om-point-x p1) (om-point-y p1) (om-point-x p2) (om-point-y p2)
                x y (+ x w) (+ y h)))
        collect i))

;;;=======================
;;; EDITOR FUNCTIONS
;;;=======================

(defmethod clear-frame-attributes ((self data-stream))
  (loop for f in (frames self) do (setf (attributes f) nil)))

(defmethod set-graphic-attributes ((self stream-editor))
  (when (object-value self)
    (loop for f in (data-stream-get-frames (object-value self)) do
          (unless (attributes f)
            (set-frame-attributes f self)))))
  
(defmethod om-draw-contents ((self stream-panel))
  (let* ((editor (editor self))
         (stream (object-value editor)))
    
    (om-with-fg-color (om-def-color :light-gray)
      (om-with-line '(2 2)
        (draw-grid-from-ruler self (get-g-component editor :x-ruler))))
    
    (when stream 
      (om-with-fg-color (om-def-color :dark-gray)
        (loop for frame in (data-stream-get-frames stream) for i = 0 then (1+ i) do
              (draw-data-frame frame editor i)))
      )
    ))

(defmethod position-display ((editor stream-editor) pos-pix)
  (let* ((time (float (pix-to-x (get-g-component editor :main-panel) (om-point-x pos-pix)))))
    (om-set-text (get-g-component editor :mousepos-txt) (format nil "~D" time))))


(defmethod move-editor-selection ((self stream-editor) &key (dx 0) (dy 0))
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          ;(setf (date frame) (max 0 (+ (date frame) dx)))
          (item-set-time frame (max 0 (+ (item-get-time frame) dx)))
          )))

(defmethod editor-sort-frames ((self stream-editor))
  (let* ((stream (object-value self))
         (tempselection (loop for pos in (selection self) collect (nth pos (data-stream-get-frames stream)))))
    (data-stream-set-frames stream (sort (data-stream-get-frames stream) '< :key 'date))
    (setf (selection self)
          (loop for selected in tempselection 
                collect (position selected (data-stream-get-frames stream))))))

(defmethod om-view-mouse-motion-handler ((self stream-panel) position)
  (let ((editor (editor self)))
    (position-display editor position)
    (unless (equal (editor-play-state editor) :play)  
    (let ((fp (frame-at-pos editor position)))
      (om-hide-tooltip self)
      (when (and fp (om-command-key-p))
        (let ((frame (nth fp (data-stream-get-frames (object-value editor)))))
          (om-show-tooltip self (data-frame-text-description frame) (omp (- (om-point-x position) 60) 20))
          )
        )))))

(defmethod om-view-click-handler ((self stream-panel) position)
  (let ((editor (editor self))
        (p0 position))
    (let ((selection (frame-at-pos editor position)))
      (set-selection editor selection)
      (om-invalidate-view self)
      ;;; move the selection or select rectangle
      (if selection
          (om-init-temp-graphics-motion 
           self position nil
           :motion #'(lambda (view pos)
                       (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0)))))
                         (move-editor-selection editor :dx dx)
                         (setf p0 pos)
                         (position-display editor pos)
                         (update-timeline-editor editor)
                         (om-invalidate-view self)
                         ))
           :release #'(lambda (view pos) 
                        (editor-sort-frames editor)
                        (time-sequence-update-internal-times (object-value editor))
                        (update-timeline-editor editor)
                        (report-modifications editor) 
                        (om-invalidate-view self))
           :min-move 4)
        (om-init-temp-graphics-motion 
         self position 
         (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
         :min-move 10
         :release #'(lambda (view position)
                      (setf (selection editor) (frames-in-area editor p0 position))
                      (update-timeline-editor editor)
                      (om-invalidate-view self))
         )
        )
      )))

(defmethod editor-key-action ((editor stream-editor) key)
  (let* ((panel (get-g-component editor :main-panel))
         (stream (object-value editor)))
    (case key
      (:om-key-delete 
       (when (selection editor)
         (loop for pos in (sort (selection editor) '>) do 
               (data-stream-set-frames stream (remove (nth pos (frames stream)) (frames stream))))
         (setf (selection editor) nil)
         (om-invalidate-view panel)
         (update-timeline-editor editor)
         (report-modifications editor)))   
      (:om-key-esc 
       (reinit-ranges (get-g-component editor :x-ruler)))
      (:om-key-left
       (move-editor-selection editor :dx (- (get-units (get-g-component editor :x-ruler) (if (om-shift-key-p) 100 10))))
       (editor-sort-frames editor)
       (time-sequence-update-internal-times stream)
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor)
       )
      (:om-key-right
       (move-editor-selection editor :dx (get-units (get-g-component editor :x-ruler) (if (om-shift-key-p) 100 10)))
       (editor-sort-frames editor)
       (time-sequence-update-internal-times stream)
       (om-invalidate-view panel)
       (update-timeline-editor editor)
       (report-modifications editor))
      (:om-key-tab
       (setf (selection editor) 
             (if (selection editor) 
                 (list (mod (1+ (car (selection editor))) (length (data-stream-get-frames stream))))
               '(0)))
       (om-invalidate-view panel))
      (otherwise (call-next-method))
      )))




