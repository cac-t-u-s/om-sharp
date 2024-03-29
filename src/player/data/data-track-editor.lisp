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

;;;======================================
;;; EDITOR
;;;======================================

(defclass data-track-editor (OMEditor play-editor-mixin undoable-editor-mixin multi-display-editor-mixin)
  ((timeline-editor :accessor timeline-editor :initform nil)
   (record-process :accessor record-process :initform nil)))

;;; compatibility (editors in libraries inheriting from data-stream-editor)
(defclass* data-stream-editor (data-track-editor) ())

(defmethod object-default-edition-params ((self data-track))
  '((:display-mode :blocks)
    (:grid t)
    (:x1 0) (:x2 nil)
    (:y1 0) (:y2 100)))

(defclass data-track-panel (x-cursor-graduated-view y-graduated-view OMEditorView om-tt-view)
  ((stream-id :accessor stream-id :initform 0 :initarg :stream-id)
   (left-view :accessor left-view :initform nil :initarg :left-view))
  (:default-initargs
   :input-model (om-input-model :touch-pan t)))

(defmethod editor-view-class ((self data-track-editor)) 'data-track-panel)
(defmethod object-has-editor ((self internal-data-track)) t)
(defmethod get-editor-class ((self internal-data-track)) 'data-track-editor)
(defmethod get-obj-to-play ((self data-track-editor)) (object-value self))

(defmethod get-object-slots-for-undo ((self internal-data-track)) '(frames))

(defmethod alllow-insert-point-from-timeline ((self data-track-editor)) nil)


;;; from play-editor-mixin
(defmethod cursor-panes ((self data-track-editor))
  ;(append (get-g-component self :data-panel-list)
  (cons (active-panel self)
        (and (timeline-editor self)
             (cursor-panes (timeline-editor self)))))

(defmethod active-panel ((editor data-track-editor))
  (let ((n (or (and (multi-display-p editor)
                    (position (object-value editor) (multi-obj-list editor)))
               0)))
    (nth n (get-g-component editor :data-panel-list))))

(defmethod editor-window-init-size ((self data-track-editor)) (om-make-point 650 200))

(defmethod frame-display-modes-for-object ((self data-track-editor) (object t)) '(:blocks :bubbles))

(defmethod make-editor-controls ((editor data-track-editor))
  (let ((object (object-value editor)))
    (when (> (length (frame-display-modes-for-object editor object)) 1)
      (om-make-di 'om-popup-list :size (omp 80 24) :font (om-def-font :gui)
                  :items (frame-display-modes-for-object editor object)
                  :di-action #'(lambda (item)
                                 (editor-set-edit-param editor :display-mode (om-get-selected-item item))
                                 (mapc 'om-invalidate-view (get-g-component editor :data-panel-list)))
                  :value (editor-get-edit-param editor :display-mode)
                  ))))

(defmethod editor-with-timeline ((self data-track-editor)) t)

(defun make-timeline-check-box (editor)
  (om-make-di 'om-check-box :text "timeline" :size (omp 65 24) :font (om-def-font :gui)
              :checked-p (editor-get-edit-param editor :show-timeline)
              :enabled (timeline-editor editor)
              :di-action #'(lambda (item)
                             (let ((timeline-ed (timeline-editor editor)))
                               (editor-set-edit-param editor :show-timeline (om-checked-p item))
                               (clear-timeline timeline-ed)
                               (when (om-checked-p item) (make-timeline-view timeline-ed))
                               (om-update-layout (main-view editor))))
              ))


(defun make-control-bar (editor)

  (let ((mousepostext (om-make-graphic-object 'om-item-text :size (omp 60 16))))

    (set-g-component editor :mousepos-txt mousepostext)

    (om-make-layout
     'om-row-layout
     :ratios '(1 100 1 1 1 1 1)
     :subviews (list mousepostext
                     nil
                     (make-time-monitor editor)
                     (make-play-button editor :enable t)
                     (make-repeat-button editor :enable t)
                     (make-pause-button editor :enable t)
                     (make-stop-button editor :enable t)
                     (when (can-record editor)
                       (make-rec-button editor :enable t
                                        :record-fun #'(lambda (on)
                                                        (if on (editor-record-on editor)
                                                          (editor-record-off editor)))
                                        ))
                     ))))


(defmethod init-editor ((editor data-track-editor))
  (call-next-method)
  (when (editor-with-timeline editor)
    (setf (timeline-editor editor)
          (make-instance 'timeline-editor
                         :object (object editor)
                         :container-editor editor))
    ))


(defmethod editor-close ((editor data-track-editor))
  (when (can-record editor)
    (editor-record-off editor))
  (call-next-method))


;;; stop record after box eval
(defmethod editor-update-play-state ((editor data-track-editor) object)
  (call-next-method)
  (when (can-record editor)
    (editor-record-off editor)))


;;; sets the editor slightly longer that the actual object length
(defmethod editor-view-after-init-space ((self t)) 1000)

;;; the small view at the left of the timeline should be sized according to the editor's layout
(defmethod make-timeline-left-item ((self data-track-editor) id)
  (om-make-view 'om-view :size (omp 28 15)))


(defmethod make-left-panel-for-object ((editor data-track-editor) (object t) view)
  (let ((ruler (om-make-view 'y-ruler-view
                             :size (omp 30 nil)
                             :related-views (list view)
                             :bg-color (om-def-color :white)
                             :y1 (editor-get-edit-param editor :y1)
                             :y2 (editor-get-edit-param editor :y2))))
    ruler))


(defmethod reinit-y-ranges-from-ruler ((editor data-track-editor) ruler)
  (set-ruler-range ruler
                   (get-default-edit-param (object editor) :y1)
                   (get-default-edit-param (object editor) :y2)))


(defmethod data-track-get-x-ruler-vmin ((self data-track-editor)) 0)


;;; voice editor has a different ruler
(defmethod make-time-ruler ((editor data-track-editor) dur)
  (let ((vmin (data-track-get-x-ruler-vmin editor)))
    (om-make-view 'time-ruler
                  :related-views (get-g-component editor :data-panel-list)
                  :size (omp nil 20)
                  :bg-color (om-def-color :white)
                  :vmin vmin
                  :x1 (or (editor-get-edit-param editor :x1) vmin)
                  :x2 (or (editor-get-edit-param editor :x2) dur))))


(defmethod editor-scroll-v ((self data-track-editor)) nil)

(defmethod make-editor-window-contents ((editor data-track-editor))

  (let* ((data-track (object-value editor))
         (object-s (if (multi-display-p editor)
                       (multi-obj-list editor)
                     (list data-track)))
         (n-objs (length object-s))
         (max-dur (loop for obj in object-s maximize (or (get-obj-dur obj) 0)))
         (ed-dur (if (zerop max-dur)
                     10000
                   (+ max-dur (editor-view-after-init-space data-track)))))

    (set-g-component editor :data-panel-list
                     (loop for d-s in object-s
                           for i = 0 then (+ i 1) collect
                           (let* ((view (om-make-view (editor-view-class editor) :stream-id i
                                                      :editor editor
                                                      :size (omp 50 nil)
                                                      :direct-draw t
                                                      :bg-color (om-def-color :white)
                                                      ;; internal vertical scroller
                                                      :scrollbars (editor-scroll-v editor)
                                                      ))
                                  (ruler (make-left-panel-for-object editor d-s view)))
                             (setf (left-view view) ruler)
                             view)))

    (set-g-component editor :x-ruler (make-time-ruler editor ed-dur))
    (set-g-component editor :main-panel (car (get-g-component editor :data-panel-list)))

    (when (timeline-editor editor)
      (set-g-component (timeline-editor editor) :main-panel (om-make-layout 'om-row-layout))

      (when (editor-get-edit-param editor :show-timeline)
        (make-timeline-view (timeline-editor editor))))

    (om-make-layout
     'om-column-layout
     :ratios '(96 2 2)
     :subviews (list
                ;;; first group with the 'main' editor:
                (om-make-layout
                 'om-simple-layout
                 ;;; if this is a multi-object editor with 3+ objects, add vertical scroller
                 :scrollbars (if (> (length (get-g-component editor :data-panel-list)) 2) :v nil)
                 :subviews
                 (list
                  (om-make-layout
                   'om-grid-layout
                   :delta 0
                   :ratios `((nil 100)
                             ,(append '(0.01)
                                      (make-list n-objs :initial-element (/ 0.98 n-objs))
                                      '(0.01)))
                   :subviews
                   (append (list nil (make-control-bar editor))
                           (loop for view in (get-g-component editor :data-panel-list)
                                 append (list (left-view view) view))
                           (list nil (get-g-component editor :x-ruler)))
                   )
                  ))

                ;;; the timeline editor:
                (when (timeline-editor editor)
                  (get-g-component (timeline-editor editor) :main-panel))

                ;;; the bottom control bar:
                (om-make-layout 'om-row-layout
                                :size (omp nil 40)
                                :subviews (list (make-editor-controls editor) nil
                                                (and (editor-with-timeline editor)
                                                     (make-timeline-check-box editor))))
                ))
    ))


(defmethod om-view-scrolled ((self data-track-panel) pos)
  (om-set-scroll-position (left-view self) (omp 0 (cadr pos))))


;===== MultiDisplay API

(defmethod enable-multi-display ((editor data-track-editor) obj-list)
  (call-next-method)
  (when (container-editor editor)
    (om-substitute-subviews
     (main-view (container-editor editor))
     (main-view editor)
     (setf (main-view editor)
           (make-editor-window-contents editor)))
    (init-editor-window editor)
    ))

(defmethod disable-multi-display ((editor data-track-editor))
  (call-next-method)
  (when (container-editor editor)
    (om-substitute-subviews
     (main-view (container-editor editor))
     (main-view editor)
     (setf (main-view editor)
           (make-editor-window-contents editor)))
    (init-editor-window editor)
    ))

;======================

(defmethod init-editor-window ((editor data-track-editor))
  (call-next-method)

  (when (get-g-component editor :x-ruler)
    (update-views-from-ruler (get-g-component editor :x-ruler)))

  (let ((y1 (editor-get-edit-param editor :y1))
        (y2 (editor-get-edit-param editor :y2)))
    (when (and y1 y2) ;; some editors don't, e.g. chord-seq editor
      (loop for view in (get-g-component editor :data-panel-list)
            do (setf (y1 view) y1
                     (y2 view) y2)
            (set-shift-and-factor view)))))


(defmethod update-to-editor ((editor data-track-editor) (from ombox))
  (let* ((data-track (object-value editor)))
    (when data-track
      (let ((new-max-dur (if (zerop (get-obj-dur data-track))
                             10000
                           (+ (get-obj-dur data-track) (editor-view-after-init-space data-track)))))

        (when (get-g-component editor :x-ruler)
          (setf (vmax (get-g-component editor :x-ruler)) new-max-dur)
          (set-ruler-range
           (get-g-component editor :x-ruler)
           (v1 (get-g-component editor :x-ruler))
           new-max-dur))
        (mapc 'om-invalidate-view (get-g-component editor :data-panel-list))

        (when (and (timeline-editor editor)
                   (editor-get-edit-param editor :show-timeline))
          (update-to-editor (timeline-editor editor) editor))

        (call-next-method)
        ))))

(defmethod update-to-editor ((editor data-track-editor) (from t))
  (call-next-method)
  (mapc 'om-invalidate-view (get-g-component editor :data-panel-list)))


(defmethod editor-invalidate-views ((self data-track-editor))
  (call-next-method)
  (mapc 'om-invalidate-view (get-g-component self :data-panel-list))
  (when (timeline-editor self)
    (editor-invalidate-views (timeline-editor self))))


;;; todo : factorize this in different editors..
(defmethod editor-delete-contents-from-timeline ((self data-track-editor) timeline-id sel)
  (let ((data-track (object-value self)))
    (mapcar #'(lambda (point) (time-sequence-remove-timed-item data-track point)) sel)
    (time-sequence-update-internal-times data-track))
  (editor-invalidate-views self)
  (report-modifications self))


;;; called when resetting the x-rulers
(defmethod play-editor-get-ruler-views ((self data-track-editor))
  (get-g-component self :x-ruler))


;;;===========================================
;;; FRAMES DRAW & SELECTION
;;;===========================================
;;; attributes are no intrinsec data of the frame
;;; they should calculated depending on the frame data and according to a given mode of display

(defmethod get-frame-graphic-duration ((self data-frame))
  (if (zerop (item-get-duration self)) 200 (item-get-duration self)))

(defmethod get-frame-color ((self data-frame))
  (or (getf (attributes self) :color)
      (setf (getf (attributes self) :color) (om-random-color 0.4))))

;; random !
;; compare values to y-range
(defmethod get-frame-posy ((self data-frame))
  (or (getf (attributes self) :posy)
      (setf (getf (attributes self) :posy) (om-random 30 90))))

;; arbitrary !
(defmethod get-frame-sizey ((self data-frame))
  (max 10 (* 1.2 (data-size self))))

(defmethod finalize-data-frame ((f data-frame) &rest args) nil)

;; returns (x y w h)
(defmethod get-frame-area ((frame data-frame) editor)
  (let ((panel (active-panel editor))
        (sizey (get-frame-sizey frame))
        (posy (get-frame-posy frame)))
    (case (editor-get-edit-param editor :display-mode)
      (:bubbles (values
                 (x-to-pix panel (item-get-internal-time frame))
                 (y-to-pix panel (+ posy (/ sizey 2)))
                 (- (dy-to-dpix panel sizey))
                 (- (dy-to-dpix panel sizey))
                 ))
      (otherwise (values (x-to-pix panel (item-get-internal-time frame))
                         (y-to-pix panel posy)
                         (max 3 (dx-to-dpix panel (get-frame-graphic-duration frame)))
                         (max 3 (- (dy-to-dpix panel sizey)))  ;; !! downwards
                         )))))

(defmethod draw ((frame data-frame) x y w h selected) nil)

(defmethod draw-data-frame ((frame data-frame) editor i &optional (active t))
  (multiple-value-bind (x y w h)
      (get-frame-area frame editor)
    (om-with-fg-color (get-frame-color frame)
      (or (draw frame x y w h (and active (find i (selection editor))))
          (case (editor-get-edit-param editor :display-mode)
            (:bubbles
             ;(om-draw-rect x y w h :fill nil)
             (let ((r (abs (min w h))))
               (om-draw-circle (+ x (round r 2))
                               (+ y (round r 2))
                               (round r 2) :fill t)
               (when (and active (find i (selection editor)))
                 (om-draw-circle (+ x (round r 2))
                                 (+ y (round r 2))
                                 (round r 2) :fill t
                                 :color (om-make-color .5 .5 .5 .5)))
               ))
            (otherwise
             (om-draw-rect x y w h :fill t)
             (when (and active (find i (selection editor)))
               (om-draw-rect x y w h :fill t
                             :color (om-make-color .5 .5 .5 .5))))
            ))
      )))

(defmethod frame-at-pos ((editor data-track-editor) position)
  (let ((frames (data-track-get-frames (object-value editor))))
    (when frames
      (position-if #'(lambda (f)
                       (multiple-value-bind (x y w h)
                           (get-frame-area f editor)
                         (om-point-in-rect-p position x y w h)))
                   frames))))


(defmethod frames-in-area ((editor data-track-editor) p1 p2)
  (loop for f in (data-track-get-frames (object-value editor))
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

(defmethod draw-background ((editor data-track-editor) (view data-track-panel)) nil)

(defmethod om-draw-contents ((self data-track-panel))
  (let* ((editor (editor self))
         (stream (if (multi-display-p editor)
                     (nth (stream-id self) (multi-obj-list editor))
                   (object-value editor)))
         (active (if (multi-display-p editor)
                     (equal self (active-panel editor))
                   t)))

    (when active (draw-background editor self))

    (when (editor-get-edit-param editor :grid)
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-line '(2 2)
          (draw-grid-from-ruler self (get-g-component editor :x-ruler)))))

    (when stream
      (om-with-fg-color (om-def-color :dark-gray)
        (loop for frame in (data-track-get-frames stream)
              for i = 0 then (1+ i) do
              (draw-data-frame frame editor i active)))
      )))


(defmethod om-draw-contents :after ((self data-track-panel))
  (when (and (multi-display-p (editor self))
             (equal self (active-panel (editor self))))
    (om-draw-rect 0 0 (w self) (h self) :fill nil
                  :line 4 :color (om-make-color .8 .4 .4))
    ))


(defmethod position-display ((editor data-track-editor) pos-pix)
  (when (active-panel editor)
    (let* ((time (round (pix-to-x (active-panel editor) (om-point-x pos-pix)))))
      (om-set-text (get-g-component editor :mousepos-txt) (format nil "~Dms" time)))))


(defmethod move-editor-selection ((self data-track-editor) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; in a basic data-frame -- subclasses can do it !
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-track-get-frames (object-value self)))))
          (item-set-time frame (max 0 (round (+ (item-get-time frame) dx))))
          )))

(defmethod resize-editor-selection ((self data-track-editor) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; in a basic data-frame -- subclasses can do it !
  (loop for fp in (selection self) do
        (let ((frame (nth fp (data-track-get-frames (object-value self)))))
          (item-set-duration frame (max 0 (round (+ (item-get-duration frame) dx))))
          ))
  (time-sequence-update-obj-dur (object-value self)))

(defmethod delete-editor-selection ((self data-track-editor))
  (loop for pos in (sort (selection self) '>) do
        (time-sequence-remove-nth-timed-item (object-value self) pos)))


;;; sort the frames and reset the selection indices correctly
(defmethod editor-sort-frames ((self data-track-editor))
  (let* ((stream (object-value self))
         (selected-objects (loop for pos in (selection self) collect
                                 (nth pos (data-track-get-frames stream)))))

    (time-sequence-reorder-timed-item-list stream)

    (setf (selection self)
          (loop for selected in selected-objects
                collect (position selected (data-track-get-frames stream))))))


(defmethod resizable-frame ((self data-frame)) nil)


(defmethod update-view-from-ruler ((self x-ruler-view) (view data-track-panel))
  (call-next-method)
  (editor-set-edit-param (editor view) :x1 (x1 self))
  (editor-set-edit-param (editor view) :x2 (x2 self)))

(defmethod update-view-from-ruler ((self y-ruler-view) (view data-track-panel))
  (call-next-method)
  (editor-set-edit-param (editor view) :y1 (y1 self))
  (editor-set-edit-param (editor view) :y2 (y2 self)))


(defmethod om-view-mouse-motion-handler ((self data-track-panel) position)
  (let ((editor (editor self)))

    (when (object-value editor)

      ;;; show the mouse position on screen
      (position-display editor position)

      (unless (or (equal (editor-play-state editor) :play)
                  (and (multi-display-p editor) (not (equal self (active-panel editor)))))

        (om-hide-tooltip self)

        (let ((frames (data-track-get-frames (object-value editor)))
              (fp (frame-at-pos editor position)))
          (when fp
            (let ((frame (nth fp frames)))
              (if (om-command-key-p)
                  ;;; show tooltip for the frame under the  mouse cursor
                  (om-show-tooltip self (data-frame-text-description frame)
                                   (omp (- (om-point-x position) 60) 20)
                                   0)

                ;;; show reisize cursor if by the end of a resizable-frame

                (when (resizable-frame frame)
                  (let ((mouse-x (om-point-x position))
                        (frame-end-time-x (time-to-pixel self (item-end-time frame))))
                    (if (and (<= mouse-x frame-end-time-x) (>= mouse-x (- frame-end-time-x 5)))
                        (om-set-view-cursor self (om-get-cursor :h-size))
                      (om-set-view-cursor self nil)))
                  ))))
          )))))


(defmethod om-view-click-handler :around ((self data-track-panel) position)
  (let ((editor (editor self)))
    (when (and (container-editor editor)
               (not (equal self (active-panel editor))))
      (set-current-nth (container-editor editor) (stream-id self)))
    (call-next-method)
    ))


(defmethod om-view-click-handler ((self data-track-panel) position)

  (let* ((editor (editor self))
         (object (object-value editor)))

    (set-paste-position position self)

    (unless (handle-selection-extent self position) ;; => play-editor-mixin handles cursor etc.

      (when object

        (let ((p0 position)
              (selection (frame-at-pos editor position)))

          (set-selection editor selection)
          (update-timeline-editor editor)
          (om-invalidate-view self)

          (cond

           ((and (null selection) (om-add-key-down)
                 (not (locked object)))

            (store-current-state-for-undo editor)

            (let ((frame (time-sequence-make-timed-item-at object (pixel-to-time self (om-point-x p0)))))
              (finalize-data-frame frame :posy (pix-to-y self (om-point-y p0)))
              (with-schedulable-object object
                                       (time-sequence-insert-timed-item-and-update object frame))
              (report-modifications editor)
              (update-timeline-editor editor)
              (om-invalidate-view self)))

           ((and selection (not (locked object)))

            (let* ((selected-frame (nth selection (data-track-get-frames object)))
                   (selected-frame-end-t (time-to-pixel self (item-end-time selected-frame))))

              (store-current-state-for-undo editor)

              ;;; resize the selected frame ?
              (if (and (resizable-frame selected-frame)
                       (<= (om-point-x position) selected-frame-end-t) (>= (om-point-x position) (- selected-frame-end-t 5)))

                  (om-init-temp-graphics-motion
                   self position nil
                   :motion #'(lambda (view pos)
                               (declare (ignore view))
                               (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0)))))
                                 (when (> (- (om-point-x pos) (x-to-pix self (item-get-time selected-frame))) 10)
                                   (resize-editor-selection editor :dx (round dx))
                                   (setf p0 pos)
                                   (om-invalidate-view self))))
                   :release #'(lambda (view pos)
                                (declare (ignore view pos))
                                (notify-scheduler object)
                                (report-modifications editor)
                                (om-invalidate-view self))
                   :min-move 4)

                ;;; move the selection
                (om-init-temp-graphics-motion
                 self position nil
                 :motion #'(lambda (view pos)
                             (declare (ignore view))
                             (let ((dx (dpix-to-dx self (- (om-point-x pos) (om-point-x p0))))
                                   (dy (- (dpix-to-dy self (- (om-point-y pos) (om-point-y p0))))))
                               (move-editor-selection editor :dx dx :dy dy)
                               (setf p0 pos)
                               (position-display editor pos)
                               (update-timeline-editor editor)
                               (om-invalidate-view self)
                               ))
                 :release #'(lambda (view pos)
                              (declare (ignore view pos))
                              (let ((selected-frames (posn-match (data-track-get-frames object) (selection editor))))
                                (with-schedulable-object (object-value editor)
                                                         (editor-sort-frames editor)
                                                         (move-editor-selection editor :dy :round)
                                                         (time-sequence-reorder-timed-item-list object))
                                (update-timeline-editor editor)
                                ;;; reset the selection:
                                (set-selection editor
                                               (loop for f in selected-frames collect
                                                     (position f (data-track-get-frames object))))
                                )
                              (report-modifications editor)
                              (om-invalidate-view self))
                 :min-move 4)
                ))
            )
           (t
            ;; no selection: start selection lasso
            (om-init-temp-graphics-motion
             self position
             (om-make-graphic-object 'selection-rectangle :position position :size (om-make-point 4 4))
             :min-move 10
             :release #'(lambda (view position)
                          (declare (ignore view))
                          (setf (selection editor) (frames-in-area editor p0 position))
                          (update-timeline-editor editor)
                          (om-invalidate-view self))
             )
            ))
          )))))


(defmethod editor-key-action ((editor data-track-editor) key)
  (let* ((panel (active-panel editor))
         (stream (object-value editor)))

    (case key

      (:om-key-delete
       (when (and (selection editor) (not (locked stream)))
         (store-current-state-for-undo editor)
         (with-schedulable-object (object-value editor)
                                  (delete-editor-selection editor))
         (setf (selection editor) nil)
         (om-invalidate-view panel)
         (update-timeline-editor editor)
         ;; (time-sequence-update-obj-dur stream) ; why not ?
         (report-modifications editor)))

      (:om-key-esc
       ;; maybe not needed ? we already have the db-click on ruler for that...
       ;; (reinit-x-ranges editor)
       )

      (:om-key-left
       (when (and (selection editor) (not (locked stream)))
         (store-current-state-for-undo editor)
         (with-schedulable-object
          (object-value editor)
          (move-editor-selection
           editor
           :dx (- (get-units (get-g-component editor :x-ruler)
                             (if (om-shift-key-p) 100 10))))
          (editor-sort-frames editor)
          (time-sequence-update-internal-times stream))
         (om-invalidate-view panel)
         (update-timeline-editor editor)
         (report-modifications editor)
         ))

      (:om-key-right
       (when (and (selection editor) (not (locked stream)))
         (store-current-state-for-undo editor)
         (with-schedulable-object
          (object-value editor)
          (move-editor-selection
           editor
           :dx (get-units (get-g-component editor :x-ruler)
                          (if (om-shift-key-p) 100 10)))
          (editor-sort-frames editor)
          (time-sequence-update-internal-times stream))
         (om-invalidate-view panel)
         (update-timeline-editor editor)
         (report-modifications editor)))

      (:om-key-tab
       (setf (selection editor)
             (if (selection editor)
                 (list (next-element-in-editor editor (car (selection editor))))
               (list (first-element-in-editor editor))))
       (editor-invalidate-views editor))

      (otherwise (call-next-method))
      )))


;;; in data-strea-editor the selection is an index to elements in the frame sequence
;;; this is not necessarilythe case of editor subclasses
(defmethod first-element-in-editor ((editor data-track-editor))
  (and (data-track-get-frames (object-value editor)) 0))

(defmethod next-element-in-editor ((editor data-track-editor) (element number))
  (let ((seq (object-value editor)))
    (and (data-track-get-frames seq)
         (mod (1+ element) (length (data-track-get-frames seq))))))

(defmethod next-element-in-editor ((editor data-track-editor) (element t)) nil)


(defmethod select-all-command ((self data-track-editor))
  #'(lambda ()
      (set-selection
       self
       (loop for i from 0 to (1- (length (data-track-get-frames (object-value self))))
             collect i))
      (update-timeline-editor self)
      (editor-invalidate-views self)
      ))


(defmethod copy-command ((self data-track-editor))
  (when (selection self)
    #'(lambda ()
        (set-om-clipboard
         (mapcar #'om-copy
                 (posn-match (data-track-get-frames (object-value self)) (selection self)))))))


(defmethod cut-command ((self data-track-editor))
  (when (and (selection self) (not (locked (object-value self))))
    #'(lambda ()
        (set-om-clipboard
         (mapcar #'om-copy
                 (posn-match (data-track-get-frames (object-value self)) (selection self))))

        (store-current-state-for-undo self)
        (with-schedulable-object (object-value self)
                                 (delete-editor-selection self))
        (setf (selection self) nil)
        (om-invalidate-view (active-panel self))
        (update-timeline-editor self)
        (report-modifications self))
    ))


(defmethod paste-command ((self data-track-editor))

  (when (get-om-clipboard)

    #'(lambda ()

        (let ((data-track (object-value self))
              (frames (mapcar
                       #'om-copy
                       (sort
                        (remove-if-not #'(lambda (element)
                                           (subtypep (type-of element) 'data-frame))
                                       (get-om-clipboard))
                        #'< :key #'item-get-time)))

              (view (active-panel self)))

          (if frames

              (let* ((t0 (item-get-time (car frames)))
                     (paste-pos (get-paste-position view))
                     (p0 (if paste-pos
                             (pixel-to-time view (om-point-x paste-pos))
                           (+ t0 200))))

                (loop for f in frames do
                      (item-set-time f (+ p0 (- (item-get-time f) t0))))

                (set-paste-position (omp (time-to-pixel view (+ p0 200))
                                         (if paste-pos (om-point-y paste-pos) 0))
                                    view)

                (store-current-state-for-undo self)

                (with-schedulable-object
                 data-track
                 (loop for f in frames do
                       (time-sequence-insert-timed-item-and-update data-track f)))

                (report-modifications self)
                (editor-invalidate-views self)
                t)
            )))))


;;;==================================
;;; TURN PAGES / FOLLOW PLAY POSITION
;;;==================================

(defmethod play-editor-callback ((editor data-track-editor) time)
  (call-next-method)
  (let ((panel (get-g-component editor :main-panel))
        (x-ruler (get-g-component editor :x-ruler)))

    (when (and panel x-ruler)
      (let ((x-range (round (- (x2 panel) (x1 panel)))))

        (cond ((> time (x2 panel))
               (set-ruler-range x-ruler (+ (v1 x-ruler) x-range) (+ (v2 x-ruler) x-range)))
              ((< time (x1 panel))
               (set-ruler-range x-ruler time (+ time x-range)))
              (t nil))))
    ))


;;;=========================
;;; TOUCH GESTURES
;;;=========================

(defmethod om-view-pan-handler ((self data-track-panel) position dx dy)
  (let ((fact 10))
    (move-rulers self :dx (* fact dx) :dy (* fact dy))))


(defmethod om-view-zoom-handler ((self data-track-panel) position zoom)
  (zoom-rulers self :dx (- 1 zoom) :dy 0 :center position))


(defmethod move-rulers ((self data-track-panel) &key (dx 0) (dy 0))
  (declare (ignore dy)) ;; no y-ruler
  (shift-time-ruler (get-g-component (editor self) :x-ruler) dx))

;;; no y-ruler : zoom just in x
(defmethod zoom-rulers ((panel data-track-panel) &key (dx 0.1) (dy 0.1) center)
  (declare (ignore dy)) ;; no y-ruler
  (let ((x-ruler (get-g-component (editor panel) :x-ruler)))
    (when (and x-ruler (ruler-zoom-? x-ruler))
      (zoom-time-ruler x-ruler dx center panel))))


;;;======================================
;;; RECORD
;;;======================================

(defmethod can-record ((self data-track-editor)) t)

(defmethod editor-record-on ((self data-track-editor))

  (let ((object (get-obj-to-play self))
        (port (get-pref-value :osc :in-port))
        (host nil))

    (setf (record-process self)
          (om-start-udp-server port host
                               #'(lambda (msg)
                                   (when (equal :play (editor-play-state self))
                                     (let ((time-ms (player-get-object-time (player self) object)))
                                       (time-sequence-insert-timed-item-and-update
                                        object
                                        (make-instance 'osc-bundle :onset time-ms
                                                       :messages (process-osc-bundle (osc-decode msg) nil)))
                                       (report-modifications self)
                                       (update-timeline-editor self)
                                       (editor-invalidate-views self))))
                               nil self))

    (when (record-process self)
      (om-print (format nil "Start OSC receive server on ~A ~D" host port) "DATA-TRACK")
      (record-process self))))


(defmethod editor-record-off ((self data-track-editor))
  (when (record-process self)
    (om-print (format nil "Stop ~A" (om-process-name (record-process self))) "DATA-TRACK")

    (om-stop-udp-server (record-process self))))


(defmethod notify-udp-server-stopped ((self data-track-editor) server)
  (editor-record-off self))


(defmethod editor-record-on :around ((self data-track-editor))
  (setf (pushed (rec-button self)) t)
  (editor-invalidate-views self)
  (call-next-method)
  t)

(defmethod editor-record-off :around ((self data-track-editor))
  (setf (pushed (rec-button self)) nil)
  (editor-invalidate-views self)
  (call-next-method)
  t)
