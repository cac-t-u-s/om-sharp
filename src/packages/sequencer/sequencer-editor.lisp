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
; File authors: J. Bresson, D. Bouche
;============================================================================


;=========================================================================
; SEQUENCER EDITOR WINDOW
;=========================================================================


(in-package :om)

(defparameter *track-control-w* 36)
(defparameter *track-h* 40)
(defparameter *ruler-view-h* 20)
(defparameter *control-view-h* 20)

(defparameter +maq-bg-color+ (om-gray-color 0.65))
(defparameter +track-color-1+ (om-gray-color 0.5))
(defparameter +track-color-2+ (om-gray-color 0.6))
(defparameter +font-color+ (om-gray-color 1))


(defclass sequencer-editor (multi-view-editor patch-editor play-editor-mixin)
  ((view-mode :accessor view-mode :initarg :view-mode :initform :tracks)     ;;; :tracks or :maquette
   (show-control-patch :accessor show-control-patch :initarg :show-control-patch :initform nil)
   (snap-to-grid :accessor snap-to-grid :initarg :snap-to-grid :initform t)
   (beat-info :accessor beat-info :initarg :beat-info :initform (list :beat-count 0 :prevtime 0 :nexttime 1))))


(defmethod om-menu-items ((self sequencer-editor))
  (remove nil (list
               (main-app-menu-item)
               (om-make-menu "File" (default-file-menu-items self))
               (om-make-menu "Edit" (append
                                     (default-edit-menu-items self)
                                     (list (om-make-menu-comp
                                            (list
                                             (om-make-menu-item
                                              "Show Inspector"
                                              #'(lambda () (patch-editor-set-window-config
                                                            self
                                                            (if (equal (editor-window-config self) :inspector) nil :inspector)))
                                              :key "i" :selected #'(lambda () (equal (editor-window-config self) :inspector))
                                              )

                                             (om-make-menu-item
                                              "Edit lock"
                                              #'(lambda () (setf (lock (object self)) (not (lock (object self))))
                                                  (om-invalidate-view (main-view self)))
                                              :key "e" :selected #'(lambda () (lock (object self)))
                                              ))
                                            :selection t
                                            )
                                           )))
               (om-make-menu "Windows" (default-windows-menu-items self))
               (om-make-menu "Help" (default-help-menu-items self))
               )))

;;; sequencer-editor is its own container
;;; this is needed by the multi-editor click-system
;;; => requires a check in the report-modification process !
(defmethod container-editor ((self sequencer-editor)) self)

(defmethod get-editor-class ((self OMSequencer)) 'sequencer-editor)
(defmethod get-obj-to-play ((self sequencer-editor)) (object self))

(defmethod is-playing ((self sequencer-editor))
  (equal (player-get-object-state (player self) (get-obj-to-play self)) :play))

(defmethod get-editor-view-for-action ((self sequencer-editor))
  (if (equal (view-mode self) :maquette)
      (get-g-component self :maq-view)
    (selected-view self))) ;; selected = the last clicked track

(defmethod get-view-from-mode ((self sequencer-editor))
  (if (equal (view-mode self) :maquette)
      (get-g-component self :maq-view)
    (get-g-component self :track-views)))


(defmethod get-visible-selected-boxes ((self sequencer-editor))
  (let ((selected-boxes (get-selected-boxes self)))
    (if (equal (view-mode self) :maquette)
        selected-boxes
      (remove-if-not #'group-id selected-boxes))))


(defmethod n-tracks ((self sequencer-editor))
  (max 4
       (or (and (boxes (object self))
                (list-max (remove-if-not #'numberp (mapcar 'group-id (get-all-boxes (object self))))))
           0 )))

(defmethod get-range ((self sequencer-editor)) (range (object self)))

(defmethod editor-get-tempo-automation ((self sequencer-editor))
  (tempo-automation (get-g-component self :metric-ruler)))


(defmethod cursor-panes ((self sequencer-editor))
  (remove nil
          (append (list (get-g-component self :maq-view)
                        (get-g-component self :metric-ruler)
                        (get-g-component self :abs-ruler))
                  (get-g-component self :track-views))))

(defmethod move-editor-selection ((self sequencer-editor) &key (dx 0) (dy 0))
  (loop for tb in (get-visible-selected-boxes self) do
        (move-box-in-sequencer (object self) tb :dx dx :dy dy)))

(defmethod box-at-pos ((editor sequencer-editor) time &optional track)
  (let* ((seq (object editor)))
    (find time (if track (get-track-boxes seq track) (get-all-boxes seq))
          :test #'(lambda (tt tb)
                    (and (> tt (get-box-onset tb))
                         (< tt (get-box-end-date tb)))))))


;;; called from the tracks
(defmethod new-box-in-track-view ((self sequencer-editor) at &optional (track 0))
  (store-current-state-for-undo self)
  (let ((seq (object self))
        (new-box (omng-make-special-box 'patch at)))
    (set-box-duration new-box *temporalbox-def-w*)
    (add-box-in-track seq new-box track)
    new-box))

;;;; !!!!!
(defmethod get-internal-view-components ((self patch-editor))
  (let ((editorview (main-view self)))
    (append (get-boxframes editorview)
            (get-grap-connections editorview))))


;;;========================
;;; EDITOR WINDOW
;;;========================

(defclass sequencer-editor-window (OMEditorWindow) ())
(defmethod editor-window-class ((self sequencer-editor)) 'sequencer-editor-window)


(defmethod update-to-editor ((self sequencer-editor) (from t))
  (om-invalidate-view (window self)))

(defmethod report-modifications ((self sequencer-editor))
  (call-next-method)
  (om-invalidate-view (window self)))

(defmethod editor-window-init-size ((self sequencer-editor)) (om-make-point 800 500))

;;; redefined from patch editor
;(defmethod init-window ((win sequencer-editor-window) editor)
;  (call-next-method)
;  (update-window-name editor)
;  win)


;;;========================
;;; MAQUETTE-VIEW (OM STYLE)
;;;========================

(defclass maquette-view (patch-editor-view x-cursor-graduated-view y-graduated-view om-drop-view)
  ()
  (:default-initargs
   :input-model (om-input-model :touch-pan t)))

(defmethod omng-x ((container maquette-view) pix-x) (round (pix-to-x container pix-x)))
(defmethod omng-y ((container maquette-view) pix-y) (pix-to-y container pix-y))
(defmethod omng-w ((container maquette-view) pix-w) (dpix-to-dx container pix-w))
(defmethod omng-h ((container maquette-view) pix-h) (+ (dpix-to-dy container pix-h)))

(defmethod omg-x ((container maquette-view) s-x) (x-to-pix container s-x))
(defmethod omg-y ((container maquette-view) s-y) (y-to-pix container s-y))
(defmethod omg-w ((container maquette-view) s-w) (dx-to-dpix container s-w))
;;; the ruler of the maquette-view is oriented bottom-up:
(defmethod omg-h ((container maquette-view) s-h) (- (dy-to-dpix container s-h)))

(defmethod resize-handle ((self resize-area) (container maquette-view) frame pos)
  (let ((pp (om-add-points (p0 self) pos)))
    (om-set-view-size frame
                      (om-max-point
                       (om-make-point 10 20)
                       (resize-frame-size self frame pp)))
    ))

(defmethod update-temporalbox ((self maquette-view) frame)
  (let* ((box (object frame))
         (x (x-to-pix self (box-x box)))
         (y (y-to-pix self (box-y box)))
         (w (if (scale-in-x-? box) (max 20 (omg-w self (box-w box))) (box-w box)))
         (h (if (scale-in-y-? box) (max 24 (omg-h self (box-h box))) (box-h box))))
    (om-set-view-position frame (om-point-set (om-view-position frame) :x x :y y))
    (om-set-view-size frame (om-point-set (om-view-size frame) :x w :y h))
    (redraw-connections frame)
    ))

(defmethod update-temporalboxes ((self maquette-view))
  (loop for sv in (get-boxframes self) do
        (update-temporalbox self sv)))

(defmethod update-view-from-ruler ((self x-ruler-view) (view maquette-view))
  (call-next-method)
  (setf (getf (range (object (editor view))) :x1) (x1 self)
        (getf (range (object (editor view))) :x2) (x2 self)))

(defmethod update-view-from-ruler ((self y-ruler-view) (view maquette-view))
  (call-next-method)
  (setf (getf (range (object (editor view))) :y1) (y1 self)
        (getf (range (object (editor view))) :y2) (y2 self)))

(defmethod update-view-from-ruler ((self ruler-view) (view maquette-view))
  (call-next-method)
  (update-temporalboxes view))

(defmethod reinit-y-ranges ((self sequencer-editor))
  (let ((boxes (boxes (object self))))
    (if boxes
        (set-ruler-range (get-g-component self :y-ruler)
                         (- (apply #'min (mapcar #'(lambda (b) (- (box-y b) (if (scale-in-y-? b) (box-h b) 40))) boxes)) 10)
                         (+ (apply #'max (mapcar #'(lambda (b) (box-y b)) boxes)) 10))
      (set-ruler-range (get-g-component self :y-ruler) -10 110)
      )))

(defmethod reinit-y-ranges-from-ruler ((self sequencer-editor) ruler)
  (reinit-y-ranges self))

(defmethod om-view-resized :after ((view maquette-view) new-size)
  (declare (ignore new-size))
  (update-temporalboxes view))

(defmethod draw-patch-grid ((view maquette-view) &optional (d 50))
  (declare (ignore d))
  (om-with-fg-color (om-gray-color 0 0.1)
    (draw-grid-from-ruler view (get-g-component (editor view) :metric-ruler))
    (draw-grid-from-ruler view (get-g-component (editor view) :y-ruler))
    ))


(defmethod allowed-element ((self OMSequencer) (elem OMInOutBox)) nil)


(defmethod om-view-pan-handler ((self maquette-view) position dx dy)
  (shift-time-ruler (get-g-component (editor self) :abs-ruler) (* dx 10)))

(defmethod om-view-zoom-handler ((self maquette-view) position zoom)
  (zoom-time-ruler (get-g-component (editor self) :abs-ruler) (- 1 zoom) position self))


(defmethod om-view-doubleclick-handler ((self maquette-view) position)
  (if (om-add-key-down)
      ;;; add new box etc.
      (call-next-method)
    ;;; set play cursor pos
    (let* ((editor (editor self))
           (time (pix-to-x self (om-point-x position))))

      (when (om-get-clipboard) (set-paste-position position self))
      (editor-set-interval editor (list time time))
      (set-object-current-time (get-obj-to-play editor) time))
    ))


;;; SNAP INTERVAL IF ALT/OPTION KEY IS DOWN
(defmethod editor-set-interval ((self sequencer-editor) interval)
  (when (om-option-key-p)
    (setf interval (list (snap-time-to-grid (get-g-component self :metric-ruler) (car interval))
                         (snap-time-to-grid (get-g-component self :metric-ruler) (cadr interval)))))
  (call-next-method self interval))


;;; used for auto-connecting boxes
(defmethod is-lower (y1 y2 (editor sequencer-editor)) (< y1 y2))


;;;========================
;;; TRACK-VIEW
;;;========================
;;; Note : the sequence-track-view is the 'frame' attribute for temporal boxes in the :tracks view-mode
(defclass sequencer-track-view (multi-view-editor-view x-cursor-graduated-view omframe om-drop-view om-view)
  ((num :initarg :num :initform 0 :accessor num))
  (:default-initargs
   ;:cursor-interval-lines-color (om-make-color 0.8 0.7 0.7)
   :cursor-interval-fill-color (om-make-color-alpha (om-def-color :white) 0.2)
   :input-model (om-input-model :touch-pan t)
   ))

(defclass sequencer-track-control (om-view)
  ((num :initarg :num :initform 0 :accessor num)))

;;; redraw upon resize
#-macosx
(defmethod om-view-resized :after ((self sequencer-track-view) size)
  (om-invalidate-view self))

(defmethod om-draw-contents ((self sequencer-track-control))
  (om-with-font
   (om-make-font "Arial" 80 :style '(:bold))
   (om-with-fg-color (om-make-color 1 1 1 0.1)
     (om-draw-string (- (round (w self) 2) 20) (+ (round (h self) 2) 24)
                     (number-to-string (num self))))))


(defmethod om-draw-contents-area ((self sequencer-track-view) x y w h)
  (let* ((editor (editor (om-view-window self)))
         (seq (object editor))
         (xmax (+ x w))
         (t1 (pixel-to-time self x))
         (t2 (pixel-to-time self xmax))
         (ruler (get-g-component editor :metric-ruler)))

    ;;;MARKERS
    (when (and ruler (markers-p ruler))
      (loop for marker in (remove-if #'(lambda (mrk) (or (< mrk t1) (> mrk t2))) (get-all-time-markers ruler)) ;;;PAS OPTIMAL
            do
            (let ((pos (time-to-pixel ruler marker)))
              (om-with-fg-color (om-make-color  0.9 0.7 0 (if (find marker (selected-time-markers ruler)) 1 0.45))
                (om-draw-line pos 0 pos (h self))))
            ))

    ;;;GRID
    (when ruler
      (om-with-fg-color (om-gray-color 0 0.1)
        (om-with-line '(2 2)
          (loop for beat in (remove-if #'(lambda (pt) (or (< (car pt) t1) (> (car pt) t2))) (point-list ruler))
                do
                (draw-grid-line-from-ruler self ruler (ruler-value-to-pix ruler (car beat)))))))

    ;;; will call 'om-draw-contents'
    (call-next-method)

    ;;;CONTENT
    (loop for tb in (get-track-boxes seq (num self))
          do
          (let ((x1 (x-to-pix self (get-box-onset tb)))
                (x2 (if (scale-in-x-? tb)
                        (x-to-pix self (get-box-end-date tb))
                      (+ (x-to-pix self (box-x tb)) (box-w tb)))))
            (when (selected tb)
              (om-with-fg-color (om-make-color-alpha (om-def-color :black) 0.5)
                (om-draw-rect x1 0 (- x2 x1) (h self) :fill t)))

            (unless (frame tb) (setf (frame tb) self))
            (when (and (<= x1 xmax) (> x2 x))
              (draw-temporal-box tb self x1 0 (- x2 x1) (h self) (- (pix-to-x self x) (get-box-onset tb)))
              )))
    ))


(defmethod resizable-box? ((self OMBox)) t)
(defmethod resizable-box? ((self OMBoxEditCall)) nil)

(defmethod update-view-from-ruler ((self x-ruler-view) (view sequencer-track-view))
  (call-next-method)
  (setf (getf (range (object (editor view))) :x1) (x1 self)
        (getf (range (object (editor view))) :x2) (x2 self)))


(defmethod om-view-pan-handler ((self sequencer-track-view) position dx dy)
  (shift-time-ruler (get-g-component (editor self) :abs-ruler) (* dx 10)))

(defmethod om-view-zoom-handler ((self sequencer-track-view) position zoom)
  (zoom-time-ruler (get-g-component (editor self) :abs-ruler) (- 1 zoom) position self))


(defmethod om-view-click-handler ((self sequencer-track-view) position)
  (let* ((editor (editor (om-view-window self)))
         (time (round (pix-to-x self (om-point-x position))))
         (selected-box (box-at-pos editor time (num self)))
         (p0 position))

    (when (om-get-clipboard) (set-paste-position (omp time (num self)) self))

    (editor-box-selection editor selected-box)

    (om-invalidate-view (om-view-window self))
    ;; (when selected-box (move-selection-in-track-action self editor selected-box position)) ;; ???


    (cond
     ((and selected-box (not (edit-lock editor)))

      (let ((selected-end-time-x (and (scale-in-x-? selected-box) ;;; otherwise we just don't rescale in tracks view
                                      (time-to-pixel self (get-box-end-date selected-box)))))

        (if (and (resizable-box? selected-box) (scale-in-x-? selected-box)
                 (<= (om-point-x position) selected-end-time-x) (>= (om-point-x position) (- selected-end-time-x 5)))
            ;;; resize the box
            (progn
              (store-current-state-for-undo editor :action :resize :item selected-box)
              (om-init-temp-graphics-motion
               self position nil
               :motion #'(lambda (view pos)
                           (declare (ignore view))
                           (when (> (- (om-point-x pos) (x-to-pix self (get-box-onset selected-box))) 10)

                             (if (scale-in-x-? selected-box)
                                 (set-box-duration selected-box
                                                   (- (round (pix-to-x self (om-point-x pos)))
                                                      (get-box-onset selected-box)))
                               (setf (box-w selected-box) (- (om-point-x pos) (x-to-pix self (box-x selected-box)))))
                             (om-invalidate-view self)))
               :release #'(lambda (view pos)
                            (declare (ignore view pos))
                            (notify-scheduler (object editor))
                            (report-modifications editor)
                            (om-invalidate-view self))
               :min-move 4)
              )

          ;;; move the selection
          (let* ((visible-selected-boxes (get-visible-selected-boxes editor))
                 (copy? (when (om-option-key-p) (mapcar 'om-copy visible-selected-boxes)))
                 (init-tracks (mapcar 'group-id visible-selected-boxes)))

            (when copy?
              (store-current-state-for-undo editor)
              (select-unselect-all editor nil)
              (mapcar #'(lambda (b)
                          (setf (group-id b) NIL)
                          (select-box b t))
                      copy?)
              (setf visible-selected-boxes copy?))

            (store-current-state-for-undo editor :action :move :item selected-box)

            (om-init-temp-graphics-motion
             self position nil
             :motion #'(lambda (view pos)
                         (declare (ignore view))
                         (let ((dx (round (dpix-to-dx self (- (om-point-x pos) (om-point-x p0)))))
                               (py (om-point-y pos)))

                           (when copy?
                             (mapcar #'(lambda (b)
                                         (unless (group-id b)
                                           (add-box-in-track (object editor) b (num self))
                                           (setf (frame b) self)))
                                     copy?))

                           (let ((diff-track-id (floor py (h self))))
                             (loop for tb in visible-selected-boxes
                                   for init-track in init-tracks do
                                   (let ((new-box-id (+ init-track diff-track-id)))
                                     (when (and (> new-box-id 0) (<= new-box-id (n-track-views editor)))
                                       (update-inspector-for-object tb) ;; here ?
                                       (setf (group-id tb) new-box-id)
                                       ))))

                           (move-editor-selection editor :dx dx)

                           (setf p0 pos)
                           (om-invalidate-view (om-view-window self))
                           ))

             :release #'(lambda (view pos)
                          (declare (ignore view pos))
                          (notify-scheduler (object editor))
                          (report-modifications editor)
                          (om-invalidate-view (om-view-window self)))

             :min-move 4)
            )
          )
        ))

     ((and (om-add-key-down) (not (edit-lock editor)))
      (let ((box (new-box-in-track-view editor (omp time 0) (num self))))
        (setf (frame box) self)
        (om-set-view-cursor self (om-get-cursor :h-size))
        (set-box-duration box nil) ;;; will set a default duration
        (om-init-temp-graphics-motion
         self position nil
         :motion #'(lambda (view pos)
                     (declare (ignore view))
                     (when (> (- (om-point-x pos) (x-to-pix self (get-box-onset box))) 10)
                       (set-box-duration box
                                         (- (round (pix-to-x self (om-point-x pos)))
                                            (get-box-onset box)))
                       (om-invalidate-view self)))
         :release #'(lambda (view pos)
                      (declare (ignore view pos))
                      (notify-scheduler (object editor))
                      (report-modifications editor)
                      (om-invalidate-view self))
         :min-move 4)
        (report-modifications editor)
        ))

     (t (call-next-method))
     )))


(defmethod om-view-mouse-motion-handler :around ((self sequencer-track-view) position)
  (let* ((ed (editor (om-view-window self)))
         (mouse-x (om-point-x position))
         (end-times-x (mapcar
                       #'(lambda (box) (time-to-pixel self (get-box-end-date box)))
                       (remove-if-not #'resizable-box? (get-track-boxes (object ed) (num self))))))
    (if (and (find mouse-x end-times-x :test #'(lambda (a b) (and (<= a b) (>= a (- b 5)))))
             (not (edit-lock ed)))
        (om-set-view-cursor self (om-get-cursor :h-size))
      (progn (om-set-view-cursor self (om-view-cursor self))
        (call-next-method)
        ))))


;; not used at the moment...
(defmethod move-selection-in-track-action ((self sequencer-track-view) editor orig-box position)
  (om-init-temp-graphics-motion
   self position nil :min-move 4
   :motion #'(lambda (view pos)
               (declare (ignore view))
               (let* ((dx (round (dpix-to-dx self (- (om-point-x pos) (om-point-x position)))))
                      (selected-box-onset (get-box-onset orig-box))
                      (snap-delta 200)
                      (new-dx (if (snap-to-grid editor)
                                  (adapt-dt-for-grid-and-markers (get-g-component editor :metric-ruler) selected-box-onset dx snap-delta)
                                dx)))
                 (when (not (equal new-dx 0))
                   (setf position pos)
                   (move-editor-selection editor :dx new-dx)
                   (update-to-editor editor self)
                   )))
   :release #'(lambda (view pos)
                (declare (ignore view pos))
                (report-modifications editor)
                (update-to-editor editor self))))


(defmethod om-view-doubleclick-handler ((self sequencer-track-view) position)
  (let* ((editor (editor (om-view-window self)))
         (time (pix-to-x self (om-point-x position)))
         (selected-box (box-at-pos editor time (num self))))
    (if selected-box
        (open-editor selected-box)
      (progn
        (when (om-get-clipboard) (set-paste-position (omp time (num self)) self))
        (editor-set-interval editor (list time time))
        (set-object-current-time (get-obj-to-play editor) time)))))


(defmethod om-drag-receive ((self sequencer-track-view) (dragged-view OMBoxFrame) position &optional (effect nil))
  (let ((editor (editor (om-view-window self)))
        (dragged-obj (object dragged-view)))
    (when (allowed-element (object editor) dragged-obj) ; (get-box-value dragged-obj)
      (let ((new-box (om-copy dragged-obj)))
        (setf (box-x new-box) (round (pix-to-x self (om-point-x position)))
              (box-y new-box) 0 (box-h new-box) 10)
        (add-box-in-track (object editor) new-box (num self))
        (setf (frame new-box) self)
        (update-to-editor editor self)
        t))))


(defmethod paste-command-for-view ((editor sequencer-editor) (view sequencer-track-view))
  (unless (edit-lock editor)
    (let* ((boxes (car (get-om-clipboard)))
           (connections (cadr (get-om-clipboard)))
           (paste-info (get-paste-position view))
           (paste-x-pos (if paste-info (om-point-x paste-info)
                          (list-max (mapcar #'get-box-end-date boxes))))
           (ref-x-pos (list-min (mapcar #'box-x boxes))))

      (select-unselect-all editor nil)
      (set-paste-position nil)

      (loop for new-box in boxes do
            (let ((x-pos (+ paste-x-pos (- (box-x new-box) ref-x-pos))))
              (setf (box-x new-box) x-pos
                    (box-y new-box) 0 (box-h new-box) 10)
              (add-box-in-track (object editor) new-box
                                (if paste-info (om-point-y paste-info)
                                  (group-id new-box)))
              (setf (frame new-box) view)
              (update-to-editor editor view)
              (set-om-clipboard (list (mapcar 'om-copy boxes) connections))
              )))))


;;;===============================
;;; DISPLAY BOXES IN SEQUENCER TRACKS
;;;==============================-

;;; to be redefined by objects if they have a specific miniview for the sequencer
(defmethod draw-sequencer-mini-view ((object t) (box OMBox) x y w h &optional time)
  (ensure-cache-display-draw box object)
  (draw-mini-view object box x y w h time))

(defmethod draw-sequencer-mini-view ((object OMBoxEditCall) (box OMBox) x y w h &optional time)
  (om-draw-rect (+ x 2) (+ y 4) (- w 4) (- h 16) :fill t :color (om-def-color :white))
  (draw-sequencer-mini-view (get-box-value object) box (+ x 8) (+ y 4) (- w 12) (- h 16) nil))


(defmethod draw-temporal-box ((self OMBox) view x y w h &optional (time 0))
  (let ((bgcolor (box-draw-color self)))
    (unless (om-color-null-p bgcolor)
      (om-with-fg-color (cond ((selected self)
                               (om-make-color-alpha (color-color bgcolor) 1.0)) ;; 0.0 is also nice :)
                              ((> (om-color-a (color-color bgcolor)) 0.6)
                               (om-make-color-alpha (color-color bgcolor) 0.6))
                              (t (color-color bgcolor)))
        (om-draw-rect x y w h :fill t))))
  (om-with-fg-color (om-def-color :white)
    (om-draw-rect x y w h :fill nil))
  (om-with-fg-color (om-def-color :white)
    (om-draw-string (+ x 2) (+ y h -2) (number-to-string (get-box-onset self)))))


(defmethod draw-temporal-box ((self OMBoxPatch) view x y w h &optional (time 0))
  (call-next-method)

  (case (display self)
    (:mini-view
     (draw-sequencer-mini-view (reference self) self (+ x 20) y (- w 40) h time))
    (:text ;; not called
     (draw-values-as-text self x y))
    (:value
     (let ((dur (or (get-obj-dur (get-box-value self)) (box-w self))))
       (om-with-clip-rect view x y w h
         (draw-sequencer-mini-view (get-box-value self) self x y
                                   (if (get-box-value self)
                                       (dx-to-dpix view dur)
                                     w)
                                   h time)
         (draw-mini-arrow (+ x 24) (+ y 9) 3 10 7 1)
         )))
    (:hidden  (om-with-font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))
                            (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
                              (om-draw-string (+ x (/ w 2) -30) (max 22 (+ 6 (/ h 2))) "PATCH"))))

    )

  (draw-patch-icon self x y)
  (draw-eval-buttons view self x y x 12)
  (draw-temporal-box-name self view x y w h)

  (when (find-if 'reactive (outputs self))
    (om-draw-rect x y w h :line 2 :color (om-def-color :dark-red)))

  (if (plusp (pre-delay self))
      (om-with-fg-color (om-def-color :red)
        (om-draw-circle (- x (dx-to-dpix view (pre-delay self))) (/ h 2) 3 :fill t)
        (om-draw-dashed-line x (/ h 2)
                             (- x (dx-to-dpix view (pre-delay self))) (/ h 2)))))


(defmethod draw-temporal-box ((self omboxeditcall) view x y w h &optional (time 0))
  (call-next-method)
  (case (display self)
    (:mini-view (draw-sequencer-mini-view (get-box-value self) self x y w h time))
    (:text (draw-mini-text (get-box-value self) self x y w h nil))
    (:hidden  (om-with-font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))
                            (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
                              (om-draw-string (+ x 10) (max 22 (+ 6 (/ h 2)))
                                              (string-upcase (type-of (get-box-value self))))))))
  (draw-temporal-box-name self view x y w h))


(defmethod draw-temporal-box-name ((self OMBox) view x y w h)

  (let ((name (and (show-name self)
                   (or (name self) (default-name (get-box-value self)))))
        (font (box-draw-font self)))

    (when name
      (om-with-clip-rect view x y w h
        (multiple-value-bind (tw th) (om-string-size name font)
          (declare (ignore th))
          (om-with-fg-color (box-draw-text-color self)
            (om-with-font
             font
             (om-draw-string (- (+ x w) tw 4) (- (+ y h) 4)
                             name
                             :selected nil
                             :wrap (- w 10)
                             )
             )))))))

;;; !! this is a special case : the frame of the object must change
;;; + the 'update' reference of the inspector window (= self) becomes the wrong one
;;; 1 solution = re-create the inspector if track is changed
;;; other solution = invalidate all tracks all the time
(defmethod update-after-prop-edit ((self sequencer-track-view) (object OMBox))
  (let ((editor (editor (om-view-window self))))
    ;;; sets the right frame for the box
    (unless (or (equal :none (group-id object))
                (and (frame object) (equal (num self) (group-id object))))
      (setf (frame object) (find (group-id object) (get-g-component editor :track-views) :key 'num :test '=)))
    (mapcar 'om-invalidate-view (get-g-component editor :track-views))
    ))

(defmethod update-frame-connections-display ((self sequencer-track-view)) nil)

;;;========================
;;; KEYBOARD ACTIONS
;;;========================

(defmethod editor-key-action ((editor sequencer-editor) key)
  (let ((seq (object editor)))
    (case key
      (:om-key-left
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-visible-selected-boxes editor))
         (with-schedulable-object
          seq
          (move-editor-selection editor :dx (- (get-units (get-g-component editor :metric-ruler) (if (om-shift-key-p) 100 10)))))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-right
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-visible-selected-boxes editor))
         (with-schedulable-object
          seq
          (move-editor-selection editor :dx (get-units (get-g-component editor :metric-ruler) (if (om-shift-key-p) 100 10))))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-up
       (unless (or (edit-lock editor)
                   (equal (view-mode editor) :tracks))
         (store-current-state-for-undo editor :action :move :item (get-visible-selected-boxes editor))
         (with-schedulable-object
          seq
          (move-editor-selection editor :dy (if (om-shift-key-p) 10 1)))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-down
       (unless (or (edit-lock editor)
                   (equal (view-mode editor) :tracks))
         (store-current-state-for-undo editor :action :move :item (get-visible-selected-boxes editor))
         (with-schedulable-object
          seq
          (move-editor-selection editor :dy (if (om-shift-key-p) -10 -1)))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))

      (:om-key-esc
       (select-unselect-all editor nil))

      (#\v
       (eval-editor-boxes editor (get-visible-selected-boxes editor))
       (report-modifications editor))

      (#\c
       ;;; don't allow inserting comments, nor connecting in tracks view
       (when (and (get-selected-boxes editor)
                  (equal :maquette (view-mode editor)))
         (call-next-method)))

      (#\C
       ;;; don't allow connecting in tracks view
       (when (equal :maquette (view-mode editor))
         (call-next-method)))

      (#\r (unless (edit-lock editor)
             (loop for tb in (get-visible-selected-boxes editor) do (set-reactive-mode tb))
             (om-invalidate-view (window editor))))
      (otherwise
       (call-next-method)
       (om-invalidate-view (window editor))
       nil)
      )))


(defmethod eval-editor-boxes ((editor sequencer-editor) boxes)
  (let ((seq (object editor)))
    (with-schedulable-object seq
                             (loop for tb in boxes do
                                   (eval-box tb)
                                   (reset-cache-display tb)
                                   (contextual-update tb seq)))
    (om-invalidate-view (window editor))
    (clear-ev-once seq)))


;;; not supported for now...
(defmethod align-selected-boxes ((editor sequencer-editor)) nil)


(defmethod select-unselect-all ((self sequencer-editor) val)
  (if (and (selected-view self)
           (not (equal self (editor (selected-view self)))))
      (select-unselect-all (editor (selected-view self)) val)
    (if (equal (view-mode self) :tracks)
        (progn (mapc #'(lambda (x) (select-box x val))
                     (remove-if-not 'group-id (boxes (object self))))
          (om-invalidate-view (main-view self)))
      (call-next-method))))

(defmethod remove-selection ((self sequencer-editor))
  (if (and (selected-view self)
           (not (equal self (editor (selected-view self)))))
      (remove-selection (editor (selected-view self)))
    (call-next-method)))

(defmethod copy-command-for-view ((editor sequencer-editor) (view t))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (copy-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))

(defmethod cut-command-for-view ((editor sequencer-editor) (view t))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (cut-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))

(defmethod paste-command-for-view ((editor sequencer-editor) (view patch-editor-view))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (paste-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))


;;;========================
;;; TIME MARKERS API
;;;========================

(defmethod get-timed-objects-with-markers ((self sequencer-track-view))
  (remove-if-not #'show-markers
                 (get-track-boxes (get-obj-to-play (editor self)) (num self))))

(defmethod select-elements-at-time ((self sequencer-track-view) marker-time)
  (let* ((editor (editor (om-view-window self)))
         (box (box-at-pos editor marker-time (num self))))
    (when box (select-box box t))
    (update-to-editor editor self)))


;;==============================
;; MARKER API SPECIAL SEQUENCER
;;==============================

(defmethod translate-elements-from-time-marker ((self OMBox) elems dt)
  "translates elements from a time marker with dt"
  (when (get-box-value self)
    (translate-elements-from-time-marker (get-obj-to-play self) elems dt)
    (reset-cache-display self)
    (contextual-update self (container self))
    (when (editor self)
      (update-to-editor (editor self) self))
    ))


;;;========================
;;; INSPECTOR IN SEQUENCER...
;;;========================

(defmethod update-inspector-for-editor ((self sequencer-editor) &optional obj)
  (if (and (selected-view self) (not (equal self (editor (selected-view self)))))
      (update-inspector-for-editor (editor (selected-view self)) obj)
    (call-next-method)))

(defmethod default-editor-help-text ((self sequencer-editor))
  "
This is a sequencer editor window.

Switch between the 'tracks' and the classic 'maquette' view with the icons of the toolbar at the top.
Open the 'control patch' with the icon in the bottom-left corner.
CMD-click to add boxes. Play contents, etc.
")


;;;========================
;;; CONTROL PATCH
;;;========================

(defmethod editor-close ((self sequencer-editor))
  (player-stop-object (player self) (metronome self))
  (editor-close (editor (ctrlpatch (object self))))
  (call-next-method))


(defun make-control-patch-view (sequencer-editor)
  (let ((ctrlpatch (ctrlpatch (object sequencer-editor))))

    (unless (editor ctrlpatch)
      (setf (editor ctrlpatch)
            (make-instance 'patch-editor
                           :object ctrlpatch
                           :container-editor sequencer-editor)))

    (let ((pl (om-make-layout 'om-simple-layout))
          (pv (cadr (multiple-value-list (make-editor-window-contents (editor ctrlpatch))))))

      (om-add-subviews pl pv)
      (setf (main-view (editor ctrlpatch)) pv)

      (put-patch-boxes-in-editor-view ctrlpatch pv)

      ;;; so that the inspector calls are passed through
      (set-g-component (editor ctrlpatch) :inspector
                       (get-g-component sequencer-editor :inspector))

      (update-inspector-for-editor sequencer-editor)

      pl)))


(defun show-hide-control-patch-editor (sequencer-editor show)

  (unless (equal (show-control-patch sequencer-editor) show)
    (setf (show-control-patch sequencer-editor) show)
    (build-editor-window sequencer-editor)
    (init-editor-window sequencer-editor)
    ))


(defun make-control-patch-buttons (editor)
  (om-make-view
   'om-view
   :size (omp *track-control-w* *ruler-view-h*)
   :subviews
   (list
    (om-make-graphic-object
     'om-icon-button
     :position (omp 0 0)
     :size (omp 16 16)
     :icon :ctrlpatch-open :icon-pushed :ctrlpatch-close
     :lock-push t :enabled t
     :pushed (show-control-patch editor)
     :action #'(lambda (b)
                 (show-hide-control-patch-editor editor (pushed b))
                 ))

    (om-make-graphic-object
     'om-icon-button
     :position (omp 20 0)
     :size (omp 16 16)
     :icon :eval-black :icon-pushed :eval-gray
     :lock-push nil :enabled t
     :action #'(lambda (b)
                 (declare (ignore b))
                 (let ((seq (get-obj-to-play editor)))
                   (eval-sequencer seq)
                   (om-invalidate-view (get-g-component editor :main-sequencer-view))
                   )))
    )))


;;;================================
;;; Metronome / Tempo
;;;================================

(defclass tempo-view (om-view)
  ((metronome :accessor metronome :initarg :metronome :initform nil)))

(defmethod om-draw-contents ((self tempo-view))
  (let* ((fontsize 10)
         (color (om-def-color :black)))
    (om-draw-char 2 14 (tempo-note :quater)
                  :font (om-make-font *score-font* fontsize)
                  :color color)
    (om-draw-string 7 15
                    "="
                    :font (om-def-font :font1 :size fontsize)
                    :color color)
    ))

(defmethod initialize-instance ((self tempo-view) &rest args)
  (call-next-method)
  (om-add-subviews
   self
   (om-make-graphic-object
    'numbox
    :value (tempo (metronome self))
    :border nil
    :decimals 0
    :position (om-make-point 13 5)
    :size (om-make-point 40 10)
    :fg-color (om-def-color :black)
    :font (om-def-font :font1 :size 9)
    :min-val 20 :max-val 400
    :change-fun #'(lambda (item)
                    (set-tempo (metronome self) (value item))))))


(defclass metro-view (om-item-view)
  ((metronome :accessor metronome :initarg :metronome :initform nil)))

(defmethod om-draw-contents ((self metro-view))
  (let* ((fontsize 11)
         (color (if (metronome-on (metronome self))
                    (om-def-color :dark-blue)
                  (om-def-color :black))))
    (om-draw-string 10 12
                    "Metro"
                    :font (om-def-font :font1 :size fontsize)
                    :color color)
    (om-draw-rect 0 4 8 8
                  :color color
                  :fill (metronome-on (metronome self)))
    ))

(defmethod om-view-click-handler ((self metro-view) position)
  (declare (ignore position))
  (metronome-on/off (metronome self)
                    (not (metronome-on (metronome self))))
  (om-invalidate-view self))


;;;========================
;;; GENERAL CONSTRUCTOR
;;;========================

(defmethod build-editor-window :before ((editor sequencer-editor))
  (mapc #'stop-cursor (cursor-panes editor)))

(defmethod init-editor :after ((editor sequencer-editor))
  (setf (play-interval editor) (interval (get-obj-to-play editor))))

(defmethod make-editor-window-contents ((editor sequencer-editor))

  (let* ((tracks-or-maq-view
          (if (equal (view-mode editor) :maquette)
              (make-maquette-view editor)
            (make-tracks-view editor)))

         (ctrl-view
          (om-make-layout
           'om-row-layout
           :scrollbars :nil
           :delta 2
           :bg-color +track-color-2+
           :ratios '(1 1000)
           :align :center
           :subviews

           (list

            (om-make-view
             'om-view
             :size (omp *track-control-w* nil)
             :subviews (list
                        (om-make-graphic-object
                         'lock-view-area
                         :locked-icon :lock-dark :unlocked-icon :unlock-dark
                         :position (omp 0 4)
                         :size (omp 16 14)
                         :editor editor)
                        ))

            (om-make-layout
             'om-row-layout
             :delta 30
             :align :center
             :ratios '(1 100 1 100 1)
             :subviews
             (list

              (om-make-view
               'om-view ;; needed to position the layout inside..
               :subviews
               (list
                (om-make-layout
                 'om-row-layout
                 :delta 5
                 :position (omp 30 3)
                 :subviews (list (make-play-button editor :enable t :size (omp 14 14))
                                 (make-pause-button editor :enable t :size (omp 14 14))
                                 (make-stop-button editor :enable t :size (omp 14 14))
                                 (make-previous-button editor :enable t :size (omp 14 14))
                                 (make-next-button editor :enable t :size (omp 14 14))
                                 (make-repeat-button editor :enable t :size (omp 14 14))
                                 ))
                ))

              (make-time-monitor editor
                                 :h 16
                                 :font (om-def-font :font2b)
                                 :background +track-color-2+
                                 :color (om-def-color :white)
                                 :time 0)

              nil

              (om-make-view
               'om-view
               :size (omp 80 nil)
               :subviews (list
                          (om-make-graphic-object
                           'om-icon-button
                           :size (omp 14 14)
                           :position (omp 0 3)
                           :icon :mute-off :icon-pushed :mute-on
                           :lock-push t :enabled t
                           :action #'(lambda (b)
                                       (declare (ignore b))
                                       (let ((seq (get-obj-to-play editor)))
                                         (with-schedulable-object
                                          seq
                                          (setf (no-exec seq)
                                                (not (no-exec seq)))))))

                          (om-make-graphic-object
                           'metro-view
                           :metronome (metronome editor)
                           :size (omp 60 16)
                           :position (omp 24 2))
                          ))

              nil

              (om-make-view
               'om-view
               :subviews
               (let (b1 b2)
                 (setq b1 (om-make-graphic-object
                           'om-icon-button
                           :position (omp 0 2)
                           :size (omp 16 16)
                           :icon :maqview-black :icon-disabled :maqview-gray
                           :lock-push nil :enabled (equal (view-mode editor) :tracks)
                           :action #'(lambda (b)
                                       (unless (equal (view-mode editor) :maquette)
                                         (button-disable b) (button-enable b2)
                                         (set-main-view editor :maquette)
                                         ))))
                 (setq b2 (om-make-graphic-object
                           'om-icon-button
                           :position (omp 20 2)
                           :size (omp 16 16)
                           :icon :trackview-black :icon-disabled :trackview-gray
                           :lock-push nil :enabled (equal (view-mode editor) :maquette)
                           :action #'(lambda (b)
                                       (unless (equal (view-mode editor) :tracks)
                                         (button-disable b) (button-enable b1)
                                         (set-main-view editor :tracks)
                                         ))))
                 (list b1 b2)))
              )))))

         (bottom-view
          (om-make-layout
           'om-simple-layout
           :subviews (list
                      (om-make-di 'om-multi-text
                                  :size (omp 200 nil)
                                  :text "select a box to display contents..."
                                  :fg-color (om-def-color :gray)
                                  )
                      )))

         (inspector-pane nil))

    (set-g-component editor :bottom-view bottom-view)
    (set-g-component editor :ctrl-view ctrl-view)
    (set-g-component editor :main-sequencer-view (om-make-layout 'om-simple-layout))

    ;;; the inspector must be created first because
    ;;; it is used in the control-patch-view creation
    ;;; the control patch constructor will update the inspector if necessary
    (when (equal (editor-window-config editor) :inspector)
      (setf inspector-pane (make-inspector-pane editor)))

    (when (show-control-patch editor)
      (set-g-component editor :left-view (make-control-patch-view editor)))

    (om-add-subviews (get-g-component editor :main-sequencer-view) tracks-or-maq-view)

    (update-cursor-pane-intervals editor)
    (when (is-playing editor)
      (mapcar #'start-cursor (cursor-panes editor)))

    (om-make-layout
     'om-row-layout :delta 2 :ratios (append
                                      (when (show-control-patch editor) '(40 nil))
                                      '(100)
                                      (when (equal (editor-window-config editor) :inspector) '(nil 1)))
     :subviews (append
                ;;; LEFT (PATCH)
                (when (show-control-patch editor)
                  (list (get-g-component editor :left-view)
                        :divider))
                ;;; MAIN
                (list (om-make-layout
                       'om-column-layout :delta 2 :ratios '(nil 100 nil 1)
                       :subviews (list
                                  (get-g-component editor :ctrl-view)
                                  (get-g-component editor :main-sequencer-view)
                                  :divider
                                  (get-g-component editor :bottom-view))))
                ;;; RIGHT (INSPECTOR)
                (when  (equal (editor-window-config editor) :inspector)
                  (list :divider
                        (om-make-layout
                         'om-column-layout :delta nil :ratios '(nil 1) :align :right
                         :subviews (list (om-make-graphic-object
                                          'om-icon-button :icon :xx :icon-pushed :xx-pushed
                                          :size (omp 12 12)
                                          :action #'(lambda (b)
                                                      (declare (ignore b))
                                                      (patch-editor-set-window-config editor nil))
                                          )

                                         (om-make-di 'om-simple-text :size (omp 230 18)
                                                     :font (om-def-font :font2b) :text "info and properties"
                                                     :fg-color (om-def-color :dark-gray))
                                         :separator
                                         inspector-pane))))
                ))
    ))


(defmethod editor-invalidate-views ((editor sequencer-editor))
  (mapc #'om-invalidate-view
        (list
         (get-g-component editor :main-sequencer-view)
         (get-g-component editor :ctrl-view)
         (get-g-component editor :bottom-view)
         )))


(defun set-main-view (editor mode)
  (setf (view-mode editor) mode)
  (om-remove-all-subviews (get-g-component editor :main-sequencer-view))
  (mapcar #'stop-cursor (cursor-panes editor))
  (om-add-subviews
   (get-g-component editor :main-sequencer-view)
   (if (equal mode :maquette)
       (make-maquette-view editor)
     (make-tracks-view editor)))
  ;;; needs to be done once the views are in place...
  (update-cursor-pane-intervals editor)
  (when (is-playing editor)
    (mapcar #'start-cursor (cursor-panes editor)))
  (mapc 'update-connections (boxes (object editor))))


(defun make-maquette-view (sequencer-editor)

  (let* ((ruler-maquette (om-make-view 'time-ruler
                                       :size (om-make-point 30 *ruler-view-h*)
                                       :x1 (or (getf (get-range sequencer-editor) :x1) 10000)
                                       :x2 (or (getf (get-range sequencer-editor) :x2) 10000)
                                       :scrollbars nil :bg-color +track-color-1+))
         (metric-ruler (om-make-view 'metric-ruler
                                     :tempo (tempo (metronome sequencer-editor))
                                     :size (om-make-point 30 *ruler-view-h*)
                                     :scrollbars nil :bg-color +track-color-1+))
         (y-ruler (om-make-view 'y-ruler-view
                                :y1 (or (getf (get-range sequencer-editor) :y1) 100)
                                :y2 (or (getf (get-range sequencer-editor) :y2) 0)
                                :size (om-make-point *track-control-w* 20)
                                :scrollbars nil :bg-color +track-color-1+))
         (maq-view (om-make-view 'maquette-view :editor sequencer-editor :scrollbars nil :bg-color +track-color-1+))
         layout)

    (set-g-component sequencer-editor :track-views nil)
    (set-g-component sequencer-editor :maq-view maq-view)
    (set-g-component sequencer-editor :metric-ruler metric-ruler)
    (set-g-component sequencer-editor :y-ruler y-ruler)
    (set-g-component sequencer-editor :abs-ruler ruler-maquette)

    (attach-view-to-ruler ruler-maquette metric-ruler)
    (attach-view-to-ruler metric-ruler ruler-maquette)
    (attach-view-to-ruler ruler-maquette maq-view)
    (attach-view-to-ruler metric-ruler maq-view)
    (attach-view-to-ruler y-ruler maq-view)

    (update-span metric-ruler)

    (setf layout
          (om-make-layout
           'om-column-layout
           :delta 2
           :ratios '(1 100 1)
           :subviews
           (list

            (om-make-layout
             'om-row-layout
             :delta 0
             :ratios '(1 100)
             :subviews
             (list
              (om-make-view
               'tempo-view :metronome (metronome sequencer-editor)
               :bg-color +track-color-1+
               :size (omp (+ 2 *track-control-w*) *ruler-view-h*))
              metric-ruler))

            (om-make-layout
             'om-row-layout
             :delta 2
             :ratios '(1 100)
             :subviews (list y-ruler maq-view))

            (om-make-layout
             'om-row-layout
             :delta 2
             :ratios '(1 100)
             :subviews
             (list
              (make-control-patch-buttons sequencer-editor)
              ruler-maquette))
            )))

    (put-patch-boxes-in-editor-view (object sequencer-editor) maq-view)

    layout
    ))


(defmethod play-editor-get-ruler-views ((self sequencer-editor))
  (list (get-g-component self :abs-ruler)
        (get-g-component self :metric-ruler)))


(defun make-track-control (n editor)
  (declare (ignore editor))
  (om-make-view
   'sequencer-track-control :num n
   :size (om-make-point *track-control-w* *track-h*)
   :bg-color (nth (mod n 2) (list +track-color-1+ +track-color-2+))))


(defun n-track-views (sequencer-editor)
  (length (get-g-component sequencer-editor :track-views)))


(defun make-tracks-view (sequencer-editor)

  (let* ((ruler-tracks (om-make-view 'time-ruler :size (om-make-point 30 *ruler-view-h*)
                                     :x1 (or (getf (get-range sequencer-editor) :x1) 0)
                                     :x2 (or (getf (get-range sequencer-editor) :x2) 10000)
                                     :scrollbars nil :bg-color +track-color-1+
                                     :bottom-p nil :markers-p t))
         (track-views (loop for n from 1 to (n-tracks sequencer-editor) collect
                            (om-make-view 'sequencer-track-view :num n :size (omp nil *track-h*)
                                          :scrollbars nil :editor sequencer-editor
                                          :bg-color (nth (mod n 2) (list +track-color-1+ +track-color-2+)))))
         (metric-ruler (om-make-view 'metric-ruler
                                     :size (om-make-point 30 *ruler-view-h*)
                                     :scrollbars nil :bg-color +track-color-1+
                                     :tempo (tempo (metronome sequencer-editor))
                                     :markers-p t)))  ;;; enable/disable markers here

    (set-g-component sequencer-editor :track-views track-views)
    (set-g-component sequencer-editor :maq-view nil)
    (set-g-component sequencer-editor :metric-ruler metric-ruler)
    (set-g-component sequencer-editor :abs-ruler ruler-tracks)
    (attach-view-to-ruler ruler-tracks metric-ruler)
    (attach-view-to-ruler metric-ruler ruler-tracks)
    (mapcar #'(lambda (v) (attach-view-to-ruler ruler-tracks v)) track-views)
    (mapcar #'(lambda (v) (attach-view-to-ruler metric-ruler v)) track-views)

    (update-span metric-ruler)

    ;;; set the track view as 'frame' for each box
    (loop for track-view in track-views do
          (loop for box in (get-track-boxes (object sequencer-editor) (num track-view)) do
                (setf (frame box) track-view)))

    (om-make-layout
     'om-column-layout :delta 2 :ratios '(1 99 1)
     :subviews (list
                ;;; the ruler bar
                (om-make-layout
                 'om-row-layout :delta 0 :ratios '(1 99)
                 :subviews (list
                            (om-make-view
                             'tempo-view :metronome (metronome sequencer-editor)
                             :bg-color +track-color-1+
                             :size (omp (+ 2 *track-control-w*) *ruler-view-h*))
                            metric-ruler))

                ;;; allows to scroll the sub-layout
                (om-make-layout
                 'om-simple-layout
                 :subviews
                 (list
                  (om-make-layout
                   'om-column-layout :delta 2 :scrollbars :v
                   :subviews
                   (loop for n from 1 to (n-tracks sequencer-editor) collect
                         (om-make-layout
                          'om-row-layout :delta 2 :ratios '(1 99)
                          :subviews (list
                                     (make-track-control n sequencer-editor)
                                     (nth (1- n) track-views)))))))

                (om-make-layout
                 'om-row-layout :delta 2 :ratios '(1 99)
                 :subviews (list
                            (make-control-patch-buttons sequencer-editor)
                            ruler-tracks))
                ))))


;;; when a box changes track
(defmethod update-container-groups ((seq OMSequencer))
  (when (and (editor seq)
             (equal (view-mode (editor seq)) :tracks)
             (not (= (n-tracks (editor seq)) (n-track-views (editor seq)))))
    ;;; will update the number of tracks
    (set-main-view (editor seq) :tracks)))


;;; called at init:
(defmethod add-lock-item ((editor sequencer-editor) view) nil)


;;;=====================
;;; PLAYER INTERFACE
;;;=====================

(defmethod editor-make-player ((self sequencer-editor))
  ;;; create a metronome
  (setf (metronome self) (make-instance 'metronome :editor self))
  ;;; return the default player
  (call-next-method))


(defmethod update-to-editor ((self sequencer-editor) (from metronome))

  (let ((m-ruler (get-g-component self :metric-ruler)))
    (setf (tempo m-ruler) (tempo from))
    (update-from-tempo m-ruler)
    (om-invalidate-view m-ruler))

  (call-next-method))


(defmethod editor-next-step ((self sequencer-editor))
  (let* ((object (get-obj-to-play self))
         (step (get-units (cadr (cursor-panes self))))
         (time (get-obj-time object)))
    (set-object-current-time object (+ step (- time (mod time step))))
    (set-object-current-time (metronome self) (+ step (- time (mod time step))))))

(defmethod editor-previous-step ((self sequencer-editor))
  (let* ((object (get-obj-to-play self))
         (step (get-units (cadr (cursor-panes self))))
         (time (get-obj-time object)))
    (set-object-current-time object (max 0 (- (- time (mod time step)) step)))
    (set-object-current-time (metronome self) (max 0 (- (- time (mod time step)) step)))))


;;;========================
;;; PLAYER
;;;========================

(defmethod play-editor-callback ((self sequencer-editor) time)

  (set-time-display self time)

  ;;; draw cursor lines (does not work so well with the rulers...)
  (mapcar #'(lambda (view)
              (when view
                (update-cursor view time)))
          (cursor-panes self))

  ;;; update range to play position ("turn pages")
  (let* ((x-ruler (get-g-component self :abs-ruler))
         (m-ruler (get-g-component self :metric-ruler))
         (x-range (round (- (v2 x-ruler) (v1 x-ruler)))))
    (cond ((> time (v2 x-ruler))
           (set-ruler-range x-ruler (+ (v1 x-ruler) x-range) (+ (v2 x-ruler) x-range))
           (update-span m-ruler)
           )
          ((< time (v1 x-ruler))
           (set-ruler-range x-ruler time (+ time x-range))
           (update-span m-ruler))
          (t nil)))

    ;(let ((t-auto (editor-get-tempo-automation self)))
    ; (if (not (getf (beat-info self) :next-date))
    ;    (setf (getf (beat-info self) :next-date) (tempo-automation-get-beat-date t-auto (getf (beat-info self) :beat-count))))
    ;  (loop while (>= time (getf (beat-info self) :next-date))
    ;        do
    ;        (om-set-dialog-item-text (cadr (om-subviews (tempo-box self))) (format nil "~$" (tempo-automation-tempo-at-beat t-auto (getf (beat-info self) :beat-count))))
    ;        (incf (getf (beat-info self) :beat-count) 0.1)
    ;        (setf (getf (beat-info self) :next-date) (tempo-automation-get-beat-date t-auto (getf (beat-info self) :beat-count))))
  )

(defmethod stop-editor-callback ((self sequencer-editor))
  (setf (getf (beat-info self) :beat-count) 0
        (getf (beat-info self) :next-date) nil)
  (when (get-g-component self :tempo-box) ;; see editor-play-mixin
    (set-value (cadr (om-subviews (get-g-component self :tempo-box)))
               (float (tempo-automation-tempo-at-beat (editor-get-tempo-automation self) 0))))
  (reset-boxes (object self))
  (call-next-method))


(defmethod get-interval-to-play ((self sequencer-editor))
  (let ((sb (get-selected-boxes self)))
    (if sb
        (list (reduce 'min sb :key 'get-box-onset)
              (reduce 'max sb :key 'get-box-end-date))
      (call-next-method))))


;;;=======================================
;;; UNDO / REDO INTERFACE
;;;=======================================

(defmethod update-after-state-change ((self sequencer-editor))

  (let* ((seq (object self)))

    (when (equal (view-mode self) :maquette)

      (let ((view (get-g-component self :maq-view)))
        (om-remove-all-subviews view)
        (put-patch-boxes-in-editor-view seq view)
        (update-temporalboxes view)
        (om-invalidate-view view)
        ))

    (mapcar 'om-invalidate-view (get-g-component self :track-views))
    ))


;;; forward to control-patch editor if active/selected

(defmethod undo-command ((self sequencer-editor))
  (let ((ed (editor (selected-view self))))
    (if (or (null ed) (equal ed self))
        (call-next-method)
      (when (undo-stack ed)
        #'(lambda () (do-undo ed))))))


(defmethod redo-command ((self sequencer-editor))
  (let ((ed (editor (selected-view self))))
    (if (or (null ed) (equal ed self))
        (call-next-method)
      (when (redo-stack ed)
        #'(lambda () (do-redo ed))))))


#|
;;; Future Box (Box maker selection rectangle)
(defclass future-box (selection-rectangle) ())

(defmethod om-draw-contents ((self future-box))
  (let ((x (if (plusp (w self)) 0 -2))
        (y (if (plusp (h self)) 0 -2))
        (w (- (w self) (if (plusp (w self)) 1 -4)))
        (h (- (h self) (if (plusp (h self)) 1 -4))))
    (om-draw-rect x y w h :fill t :color (om-make-color 0.5 0.5 0.5 0.7))
    (om-with-fg-color (om-make-color 0.6 0.2 0.2 0.7)
      (om-with-line-size 2
        (om-with-line '(4 4)
          (om-draw-rect x y w h)))
      (om-with-font (om-def-font :font4b)
                    (om-draw-string w h "+")))))

(defmethod om-view-click-handler ((self maquette-view) pos)
  (let ((p0 pos))
    (if (and (om-command-key-p))
        (om-init-temp-graphics-motion self pos
                                      (om-make-graphic-object 'future-box :position pos :size (om-make-point 4 4)
                                                              :fg-color (om-def-color :green))

                                      :release #'(lambda (view position)
                                                   (print (list (om-point-x p0) (om-point-y p0)
                                                                (om-point-x position) (om-point-y position)))))
      (call-next-method))))
|#



