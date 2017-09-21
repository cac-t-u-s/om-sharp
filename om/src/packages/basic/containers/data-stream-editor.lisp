(in-package :om)

;;;======================================
;;; EDITOR
;;;======================================

(defclass data-stream-editor (OMEditor play-editor-mixin) 
  ((timeline-editor :accessor timeline-editor :initform nil)
   (display-mode :accessor display-mode :initform :blocks)))

(defclass stream-panel (x-cursor-graduated-view y-graduated-view OMEditorView om-tt-view) ())

(defmethod editor-view-class ((self data-stream-editor)) 'stream-panel)
(defmethod object-has-editor ((self data-stream)) t)
(defmethod get-editor-class ((self data-stream)) 'data-stream-editor)
(defmethod get-obj-to-play ((self data-stream-editor)) (object-value self))

(defmethod alllow-insert-point-from-timeline ((self data-stream-editor)) nil)

;;; from play-editor-mixin
(defmethod cursor-panes ((self data-stream-editor)) 
  (cons (get-g-component self :main-panel)
        (cursor-panes (timeline-editor self))))

(defmethod editor-window-init-size ((self data-stream-editor)) (om-make-point 800 180))

;; lesser value and greater values in the ruler (bottom to top)
(defmethod y-range-for-object ((self data-stream)) '(-100 100))

(defmethod frame-display-modes-for-object ((self data-stream-editor) (object t))
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
  

(defmethod init-editor ((editor data-stream-editor))
  (call-next-method)
  (setf (timeline-editor editor) 
        (make-instance 'timeline-editor 
                       :object (object editor) 
                       :container-editor editor))
  )

;;; sets the editor slightly longer that the actual object length  
(defmethod editor-view-after-init-space ((self t)) 1000)

(defmethod left-panel-for-object ((editor data-stream-editor) (object data-stream))
  (om-make-view 'om-view :size (omp 28 nil)))

(defmethod make-editor-window-contents ((editor data-stream-editor))
  
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
                 'om-grid-layout :ratios '((nil 100) (0.01 0.98 0.01)) 
                 :subviews 
                 (list nil (make-control-bar editor)
                       (left-panel-for-object editor data-stream)
                       (get-g-component editor :main-panel) 
                       nil (get-g-component editor :x-ruler))
                 )
                ;;; the timeline editor:
                (get-g-component (timeline-editor editor) :main-panel)
                ;;; the bottom control bar:
                (om-make-layout 'om-row-layout 
                                :size (omp nil 40) 
                                :subviews (list (make-display-modes-menu editor) nil (make-timeline-check-box editor)))
                ))
    ))


(defmethod init-editor-window ((editor data-stream-editor))
  (call-next-method)
  (set-graphic-attributes editor)
  (update-views-from-ruler (get-g-component editor :x-ruler))
  (setf (y2 (get-g-component editor :main-panel)) (car (y-range-for-object (object-value editor)))
        (y1 (get-g-component editor :main-panel)) (cadr (y-range-for-object (object-value editor))))
  (set-shift-and-factor (get-g-component editor :main-panel)))


(defmethod update-to-editor ((editor data-stream-editor) (from ombox))
  
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

(defmethod update-to-editor ((editor data-stream-editor) (from t))
  (call-next-method)
  (om-invalidate-view (get-g-component editor :main-panel)))

;;; todo : factorize a bit this procedure in different editors..
(defmethod editor-delete-contents-from-timeline ((self data-stream-editor) timeline-id sel)
  (let ((data-stream (object-value self)))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence data-stream point)) sel)
    (time-sequence-update-internal-times data-stream))
  (editor-invalidate-views self)
  (report-modifications self))


;;; called when resetting the x-rulers
(defmethod play-editor-get-ruler-views ((self data-stream-editor))
  (get-g-component self :x-ruler))

;;;===========================================
;;; FRAMES DRAW, SELECTION USING ATTRIBUTES
;;;===========================================
;;; attributes are no intrinsec data of the frame
;;; they should calculated depending on the frame data and according to a given mode of display

(defmethod frame-graphic-duration ((self data-frame))
  (if (zerop (item-get-duration self)) 200 (item-get-duration self)))
  
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

(defmethod get-frame-attribute ((self data-frame) attribute &optional editor)
  (unless (attributes self) 
    (if editor 
        (set-frame-attributes-from-editor self editor)
      (om-beep-msg "no attributes found for data-frame!")))
  (getf (attributes self) attribute))

(defmethod set-frame-attribute ((self data-frame) attribute value)  
  (setf (getf (attributes self) attribute) value))

(defmethod set-frame-attributes-from-editor ((f data-frame) editor) 
  (set-frame-attribute f :color (compute-frame-color f editor))
  (set-frame-attribute f :posy (compute-frame-posy f editor))
  (set-frame-attribute f :sizey (compute-frame-sizey f editor)))


(defmethod finalize-data-frame ((f data-frame)) nil)

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
         ;(om-draw-string (- x 5) 10 (number-to-string i))
         ;(when (find i (selection editor))
         ;  (om-draw-string (- x 5) 20 (number-to-string (date frame))))
         )))))

(defmethod frame-at-pos ((editor data-stream-editor) position)
  (let ((frames (data-stream-get-frames (object-value editor))))
    (when frames 
      (position-if #'(lambda (f) 
                       (multiple-value-bind (x y w h)
                           (get-frame-area f editor)
                         (om-point-in-rect-p position x y w h)))
                   frames))))


(defmethod frames-in-area ((editor data-stream-editor) p1 p2)
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

(defmethod set-graphic-attributes ((self data-stream-editor))
  (when (object-value self)
    (loop for f in (data-stream-get-frames (object-value self)) do
          (unless (attributes f)
            (set-frame-attributes-from-editor f self)))))
  
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

(defmethod position-display ((editor data-stream-editor) pos-pix)
  (let* ((time (round (pix-to-x (get-g-component editor :main-panel) (om-point-x pos-pix)))))
    (om-set-text (get-g-component editor :mousepos-txt) (format nil "~Dms" time))))


(defmethod move-editor-selection ((self data-stream-editor) &key (dx 0) (dy 0))
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          ;(setf (date frame) (max 0 (+ (date frame) dx)))
          (item-set-time frame (max 0 (round (+ (item-get-time frame) dx))))
          )))

(defmethod resize-editor-selection ((self data-stream-editor) &key (dx 0) (dy 0))
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-stream-get-frames (object-value self)))))
          (item-set-duration frame (max 0 (round (+ (item-get-duration frame) dx))))
          )))

(defmethod editor-finalize-selection ((self data-stream-editor))
  (loop for fp in (selection self) do 
        (finalize-data-frame (nth fp (data-stream-get-frames (object-value self))))))
  

(defmethod editor-sort-frames ((self data-stream-editor))
  (let* ((stream (object-value self))
         (tempselection (loop for pos in (selection self) collect (nth pos (data-stream-get-frames stream)))))
    (data-stream-set-frames stream (sort (data-stream-get-frames stream) '< :key 'date))
    (setf (selection self)
          (loop for selected in tempselection 
                collect (position selected (data-stream-get-frames stream))))))


(defmethod resizable-frame ((self data-frame)) nil)

(defmethod om-view-mouse-motion-handler ((self stream-panel) position)
  (let ((editor (editor self)))
    ;;; show the mouse position on screen
    (position-display editor position)
    (om-hide-tooltip self)

    (unless (equal (editor-play-state editor) :play)  
    
      (let ((frames (data-stream-get-frames (object-value editor)))
            (fp (frame-at-pos editor position)))
        (when fp
          (if (om-command-key-p)

            ;;; show tooltip for the frame under the mouse cursor
            (om-show-tooltip self (data-frame-text-description (nth fp frames)) 
                             (omp (- (om-point-x position) 60) 20))

          ;;; show reisize cursor if by the end of a resizable-frame
          (let ((mouse-x (om-point-x position))
                (frame-end-time-x (time-to-pixel self (item-end-time (nth fp frames)))))
            (if (and (<= mouse-x frame-end-time-x) (>= mouse-x (- frame-end-time-x 5)))
                (om-set-view-cursor self (om-get-cursor :h-size))
              (om-set-view-cursor self nil)))
          ))
        ))))


(defmethod om-view-click-handler ((self stream-panel) position)
  (let ((editor (editor self))
        (p0 position))
    (let ((selection (frame-at-pos editor position)))
      (set-selection editor selection)
      (om-invalidate-view self)
      
      (if selection
          
          (let* ((selected-frame (nth selection (data-stream-get-frames (object-value editor))))
                 (selected-frame-end-t (time-to-pixel self (item-end-time selected-frame))))
            
            ;;; resize the selected frame ?
            (if (and (resizable-frame selected-frame)
                     (<= (om-point-x position) selected-frame-end-t) (>= (om-point-x position) (- selected-frame-end-t 5)))
            (om-init-temp-graphics-motion 
             self position nil
             :motion #'(lambda (view pos)
                         (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0)))))
                           (when (> (- (om-point-x pos) (x-to-pix self (item-get-time selected-frame))) 10)
                             (resize-editor-selection editor :dx (round dx))
                             (setf p0 pos)
                             (om-invalidate-view self))))
             :release #'(lambda (view pos) 
                          (editor-finalize-selection editor)
                          (report-modifications editor) 
                          (om-invalidate-view self))
             :min-move 4)
            
          ;;; move the selection
          (om-init-temp-graphics-motion 
           self position nil
           :motion #'(lambda (view pos)
                       (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                             (dy (dpix-to-dy self (- (om-point-y pos) (om-point-y p0)))))
                         (move-editor-selection editor :dx dx :dy dy)
                         (setf p0 pos)
                         (position-display editor pos)
                         (update-timeline-editor editor)
                         (om-invalidate-view self)
                         ))
           :release #'(lambda (view pos) 
                        (editor-finalize-selection editor) 
                        (editor-sort-frames editor)
                        (time-sequence-update-internal-times (object-value editor))
                        (update-timeline-editor editor)
                        (report-modifications editor) 
                        (om-invalidate-view self))
           :min-move 4)
          ))
        
        ;; no selection: start selection lasso
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

(defmethod editor-key-action ((editor data-stream-editor) key)
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
       (reinit-x-ranges editor))
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

