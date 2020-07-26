;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; MAQUETTE WINDOW
;=========================================================================


(in-package :om)

(defparameter *track-control-w* 30)
(defparameter *track-h* 40)
(defparameter +maq-bg-color+ (om-gray-color 0.65))
(defparameter +track-color-1+ (om-gray-color 0.5))
(defparameter +track-color-2+ (om-gray-color 0.6))
(defparameter +font-color+ (om-gray-color 1))


(defclass maquette-editor (multi-view-editor patch-editor play-editor-mixin)
  ((view-mode :accessor view-mode :initarg :view-mode :initform :tracks)     ;;; :tracks or :maquette
   (show-control-patch :accessor show-control-patch :initarg :show-control-patch :initform nil)
   (snap-to-grid :accessor snap-to-grid :initarg :snap-to-grid :initform t)
   (beat-info :accessor beat-info :initarg :beat-info :initform (list :beat-count 0 :prevtime 0 :nexttime 1))))


(defmethod om-menu-items ((self maquette-editor))
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

;;; maquette-editor is its own container
;;; this is needed by the multi-editor click-system 
;;; => requires a check in the report-modification process ! 
(defmethod container-editor ((self maquette-editor)) self)

(defmethod get-editor-class ((self OMMaquette)) 'maquette-editor)
(defmethod get-obj-to-play ((self maquette-editor)) (object self))

(defmethod is-playing ((self maquette-editor))
  (equal (player-get-object-state (player self) (get-obj-to-play self)) :play))

(defmethod get-editor-view-for-action ((self maquette-editor))
  (if (equal (view-mode self) :maquette)
      (get-g-component self :maq-view)
    (selected-view self))) ;; selected = the last clicked track

(defmethod get-view-from-mode ((self maquette-editor))
  (if (equal (view-mode self) :maquette)
      (get-g-component self :maq-view)
    (get-g-component self :track-views)))

(defmethod n-tracks ((self maquette-editor)) 
  (max 4
       (or (and (boxes (object self))
                (list-max (remove-if-not #'numberp (mapcar 'group-id (get-all-boxes (object self))))))
           0 )))

(defmethod get-range ((self maquette-editor)) (range (object self)))

(defmethod editor-get-tempo-automation ((self maquette-editor))
  (tempo-automation (get-g-component self :metric-ruler)))


(defmethod cursor-panes ((self maquette-editor)) 
  (remove nil
          (append (list (get-g-component self :maq-view)
                        (get-g-component self :metric-ruler)
                        (get-g-component self :abs-ruler))
                  (get-g-component self :track-views))))

(defmethod move-editor-selection ((self maquette-editor) &key (dx 0) (dy 0))
  (loop for tb in (get-selected-boxes self) do
        (move-box-in-maquette (object self) tb :dx dx :dy dy)))

(defmethod box-at-pos ((editor maquette-editor) time &optional track)
  (let* ((maquette (object editor)))
    (find time (if track (get-track-boxes maquette track) (get-all-boxes maquette))
          :test #'(lambda (tt tb)
                    (and (> tt (get-box-onset tb)) 
                         (< tt (get-box-end-date tb)))))))


;;; called from the tracks
(defmethod new-box-in-track-view ((self maquette-editor) at &optional (track 0))
  (store-current-state-for-undo self)
  (let ((maq (object self))
        (new-box (omng-make-special-box 'patch at)))
    (set-box-duration new-box *temporalbox-def-w*)  
    (add-box-in-track maq new-box track)  
    new-box))

;;;; !!!!!
(defmethod get-internal-view-components ((self patch-editor))
  (let ((editorview (main-view self)))
    (append (get-boxframes editorview)
            (get-grap-connections editorview))))
    

;;;========================
;;; EDITOR WINDOW
;;;========================

(defclass maquette-editor-window (OMEditorWindow) ())
(defmethod editor-window-class ((self maquette-editor)) 'maquette-editor-window)


(defmethod update-to-editor ((self maquette-editor) (from t))
  (om-invalidate-view (window self)))

(defmethod report-modifications ((self maquette-editor))
  (call-next-method)
  (om-invalidate-view (window self)))

(defmethod editor-window-init-size ((self maquette-editor)) (om-make-point 800 500))

;;; redefined from patch editor
;(defmethod init-window ((win maquette-editor-window) editor)
;  (call-next-method)
;  (update-window-name editor)
;  win)


;;;========================
;;; MAQUETTE-VIEW
;;;========================

(defclass maquette-view (patch-editor-view x-cursor-graduated-view y-graduated-view om-drop-view) ())

(defmethod omng-x ((container maquette-view) pix-x) (round (pix-to-x container pix-x)))
(defmethod omng-y ((container maquette-view) pix-y) (pix-to-y container pix-y))
(defmethod omng-w ((container maquette-view) pix-w) (dpix-to-dx container pix-w))
(defmethod omng-h ((container maquette-view) pix-h) (+ (dpix-to-dy container pix-h)))

(defmethod omg-x ((container maquette-view) s-x) (x-to-pix container s-x))
(defmethod omg-y ((container maquette-view) s-y) (y-to-pix container s-y))
(defmethod omg-w ((container maquette-view) s-w) (dx-to-dpix container s-w))
(defmethod omg-h ((container maquette-view) s-h) (- (dy-to-dpix container s-h)))  ;;; the ruler of the maquette is oriented bottom-up

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

(defmethod reinit-y-ranges ((self maquette-editor))
  (let ((boxes (boxes (object self))))
    (if boxes
        (set-ruler-range (get-g-component self :y-ruler) 
                         (- (apply #'min (mapcar #'(lambda (b) (- (box-y b) (if (scale-in-y-? b) (box-h b) 40))) boxes)) 10)
                         (+ (apply #'max (mapcar #'(lambda (b) (box-y b)) boxes)) 10))
      (set-ruler-range (get-g-component self :y-ruler) -10 110)
      )))

(defmethod reinit-y-ranges-from-ruler ((self maquette-editor) ruler) 
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


(defmethod allowed-element ((self OMMaquette) (elem OMInOutBox)) nil)


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
      (set-object-time (get-obj-to-play editor) time))
    ))



;;; SNAP INTERVAL IF ALT/OPTION KEY IS DOWN
(defmethod editor-set-interval ((self maquette-editor) interval) 
  (when (om-option-key-p)
    (setf interval (list (snap-time-to-grid (get-g-component self :metric-ruler) (car interval))
                         (snap-time-to-grid (get-g-component self :metric-ruler) (cadr interval)))))
  (call-next-method self interval))  
  
                         

;;;========================
;;; TRACK-VIEW
;;;========================
;;; Note : the sequence-track-view is the 'frame' attribute for temporal boxes in the :tracks view-mode
(defclass sequencer-track-view (multi-view-editor-view x-cursor-graduated-view omframe om-drop-view om-view)
  ((num :initarg :num :initform 0 :accessor num))
  (:default-initargs 
   ;:cursor-interval-lines-color (om-make-color 0.8 0.7 0.7)
   :cursor-interval-fill-color (om-make-color-alpha (om-def-color :white) 0.2)
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

#|
(defmethod om-draw-contents ((self sequencer-track-view))
  (let* ((editor (editor (om-view-window self)))
         (maquette (object editor)))
    (when (get-g-component editor :metric-ruler) ;; just in case
     (om-with-fg-color (om-gray-color 0 0.1)
       (draw-grid-from-ruler self (get-g-component editor :metric-ruler))))
    (loop for tb in (get-track-boxes maquette (num self)) 
          when (and (> (+ (get-box-onset tb) (get-box-duration tb)) (x1 self))
                    (< (get-box-onset tb) (x2 self)))
          do
          (let ((x1 (x-to-pix self (get-box-onset tb)))
                (x2 (if (scale-in-x-? tb) 
                               (x-to-pix self (get-box-end-date tb))
                             (box-w tb))))
            (draw-temporal-box tb self x1 0 (- x2 x1) (h self) (- (get-obj-time maquette) (get-box-onset tb)))
            (when (selected tb)
              (om-with-fg-color (om-make-color-alpha (om-def-color :gray) 0.5)
                (om-draw-rect x1 0 (- x2 x1) (h self) :fill t)))))))
|#

(defmethod om-draw-contents-area ((self sequencer-track-view) x y w h)
  (let* ((editor (editor (om-view-window self)))
         (maquette (object editor))
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
    (loop for tb in (get-track-boxes maquette (num self))
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
    
    (unless (edit-lock editor)
      
      (cond 
       (selected-box
        
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
                              (report-modifications editor) 
                              (om-invalidate-view self))
                 :min-move 4)
                )
            
            ;;; move the selection
            (let ((copy? (when (om-option-key-p) (mapcar 'om-copy (get-selected-boxes editor))))
                  (init-tracks (mapcar 'group-id (get-selected-boxes editor))))
              
              (when copy?
                (store-current-state-for-undo editor)
                (select-unselect-all editor nil)
                (mapcar #'(lambda (b) 
                            (setf (group-id b) NIL)
                            (select-box b t))
                        copy?))
              
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
                               (loop for tb in (get-selected-boxes editor) 
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
                            (report-modifications editor) 
                            (om-invalidate-view (om-view-window self)))
               
               :min-move 4)
              )
            )
          ))
       ((om-add-key-down)
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
                        (report-modifications editor)
                        (om-invalidate-view self))
           :min-move 4)
          (report-modifications editor)
          ))
       (t (call-next-method))
       ))
    ))


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
        (set-object-time (get-obj-to-play editor) time)))))


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


(defmethod paste-command-for-view ((editor maquette-editor) (view sequencer-track-view))
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
;;; DISPLAY BOXES IN MAQUETTE TRACKS
;;;==============================-

;;; to be redefined by objects if they have a specific miniview for the maquette
(defmethod draw-maquette-mini-view ((object t) (box OMBox) x y w h &optional time)
  (ensure-cache-display-draw box object)
  (draw-mini-view object box x y w h time))

(defmethod draw-maquette-mini-view ((object OMBoxEditCall) (box OMBox) x y w h &optional time)
  (om-draw-rect (+ x 2) (+ y 4) (- w 4) (- h 16) :fill t :color (om-def-color :white))
  (draw-maquette-mini-view (get-box-value object) box (+ x 8) (+ y 4) (- w 12) (- h 16) nil))


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
     (draw-maquette-mini-view (reference self) self (+ x 20) y (- w 40) h time))
    (:text ;; not called 
     (draw-values-as-text self x y))
    (:value 
     (let ((dur (or (get-obj-dur (get-box-value self)) (box-w self))))
       (om-with-clip-rect view x y w h
         (draw-maquette-mini-view (get-box-value self) self x y 
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
    (:mini-view (draw-maquette-mini-view (get-box-value self) self x y w h time))
    (:text (draw-mini-text (get-box-value self) self x y w h nil))
    (:hidden  (om-with-font (om-def-font :font1 :face "arial" :size 18 :style '(:bold))
                          (om-with-fg-color (om-make-color 0.6 0.6 0.6 0.5)
                            (om-draw-string (+ x 10) (max 22 (+ 6 (/ h 2))) 
                                            (string-upcase (type-of (get-box-value self)))))))))


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

(defmethod editor-key-action ((editor maquette-editor) key)
  (let ((maquette (object editor)))
    (case key
      (:om-key-left
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-selected-boxes editor))
         (move-editor-selection editor :dx (- (get-units (get-g-component editor :metric-ruler) (if (om-shift-key-p) 100 10))))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-right
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-selected-boxes editor))
         (move-editor-selection editor :dx (get-units (get-g-component editor :metric-ruler) (if (om-shift-key-p) 100 10)))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-up 
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-selected-boxes editor))
         (move-editor-selection editor :dy (if (om-shift-key-p) 10 1))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      (:om-key-down 
       (unless (edit-lock editor)
         (store-current-state-for-undo editor :action :move :item (get-selected-boxes editor))
         (move-editor-selection editor :dy (if (om-shift-key-p) -10 -1))
         (om-invalidate-view (main-view editor))
         (report-modifications editor)))
      
      (:om-key-esc
       (select-unselect-all editor nil))

      (#\v (with-schedulable-object maquette
                                    (loop for tb in (get-selected-boxes editor) do 
                                          (eval-box tb)
                                          (reset-cache-display tb)
                                          (contextual-update tb maquette)))
           (om-invalidate-view (window editor))
           (clear-ev-once (object editor))
           (report-modifications editor))

      (#\r (unless (edit-lock editor)
             (loop for tb in (get-selected-boxes editor) do (set-reactive-mode tb))
             (om-invalidate-view (window editor))))
      (otherwise
       (call-next-method)
       (om-invalidate-view (window editor))
       nil)
      )))


(defmethod select-unselect-all ((self maquette-editor) val)
  (if (and (selected-view self)
           (not (equal self (editor (selected-view self)))))
      (select-unselect-all (editor (selected-view self)) val)
    (if (equal (view-mode self) :tracks)
        (progn (mapc #'(lambda (x) (select-box x val))
                     (remove-if-not 'group-id (boxes (object self))))
          (om-invalidate-view (main-view self)))
      (call-next-method))))

(defmethod remove-selection ((self maquette-editor))
  (if (and (selected-view self)
           (not (equal self (editor (selected-view self)))))
      (remove-selection (editor (selected-view self)))
    (call-next-method)))

(defmethod copy-command-for-view ((editor maquette-editor) (view t))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (copy-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))

(defmethod cut-command-for-view ((editor maquette-editor) (view t))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (cut-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))

(defmethod paste-command-for-view ((editor maquette-editor) (view patch-editor-view))
  (if (and (selected-view editor)
           (not (equal editor (editor (selected-view editor)))))
      (paste-command-for-view (editor (selected-view editor)) view)
    (call-next-method)))

;;;========================
;;; TIME MARKERS API
;;;========================

(defmethod get-timed-objects-for-graduated-view ((self sequencer-track-view))
  (get-track-boxes (get-obj-to-play (editor self)) (num self)))

(defmethod select-elements-at-time ((self sequencer-track-view) marker-time)
  (let* ((editor (editor (om-view-window self)))
         (box (box-at-pos editor marker-time (num self))))
    (when box (select-box box t))
    (update-to-editor editor self)))

;;==============================
;; MARKER API SPECIAL MAQUETTE
;;==============================

(defmethod translate-elements-from-time-marker ((self OMBox) elems dt)
  "translates elements from a time marker with dt"
  (when (get-box-value self)
    (with-schedulable-object (container self)
                             (translate-elements-from-time-marker (get-obj-to-play self) elems dt))
    (reset-cache-display self)
    (contextual-update self (container self))))

;;;========================
;;; INSPECTOR IN MAQUETTE...
;;;========================

(defmethod update-inspector-for-editor ((self maquette-editor) &optional obj)
  (if (and (selected-view self) (not (equal self (editor (selected-view self)))))
      (update-inspector-for-editor (editor (selected-view self)) obj)
    (call-next-method)))

(defmethod default-editor-help-text ((self maquette-editor)) 
  "
This is a maquette editor window.

Switch between the 'tracks' and the classic 'maquette' view with the icons of the toolbar at the top.   
Open the 'control patch' with the other icon of the toolbar. 
CMD-click to add boxes. Play contents, etc.
")

;;;========================
;;; CONTROL PATCH
;;;========================

(defmethod editor-close ((self maquette-editor))
  (player-stop-object (player self) (metronome self))
  (editor-close (editor (ctrlpatch (object self))))
  (call-next-method))


(defun make-control-patch-view (maq-editor)
  (let ((ctrlpatch (ctrlpatch (object maq-editor))))
    
    (unless (editor ctrlpatch)
      (setf (editor ctrlpatch)
            (make-instance 'patch-editor 
                           :object ctrlpatch
                           :container-editor maq-editor)))
    
    (let ((pl (om-make-layout 'om-simple-layout))
          (pv (cadr (multiple-value-list (make-editor-window-contents (editor ctrlpatch))))))
      
      (om-add-subviews pl pv)
      (setf (main-view (editor ctrlpatch)) pv)
      
      (put-patch-boxes-in-editor-view ctrlpatch pv)
            
      ;;; so that the inspector calls are passed through
      (set-g-component (editor ctrlpatch) :inspector
                       (get-g-component maq-editor :inspector))

      (update-inspector-for-editor maq-editor)
      
      pl)))
  
(defun show-hide-control-patch-editor (maq-editor show)
  
  (unless (equal (show-control-patch maq-editor) show)
    (setf (show-control-patch maq-editor) show)
    (build-editor-window maq-editor)
    (init-editor-window maq-editor)
    ))


;;;========================
;;; GENERAL CONSTRUCTOR
;;;========================

(defmethod build-editor-window :before ((editor maquette-editor))
  (mapc #'stop-cursor (cursor-panes editor)))

(defmethod init-editor :after ((editor maquette-editor))
  (setf (play-interval editor) (interval (get-obj-to-play editor))))

(defmethod make-editor-window-contents ((editor maquette-editor))
  (let* ((maquette (get-obj-to-play editor))
         
         (tracks-or-maq-view 
          (if (equal (view-mode editor) :maquette)
              (make-maquette-view editor)
            (make-tracks-view editor)))
         
         (ctrl-view 
          (om-make-view 
           'om-view 
           :direct-draw nil
           :scrollbars :nil
           :size (om-make-point nil 20)
           :bg-color +track-color-2+
           :subviews 
           
           (list
              
            (om-make-layout
             'om-row-layout :delta 30 
             :position (om-make-point (+ *track-control-w* 2) 2) :align :top
             :ratios '(1 1 100 1 1 1)
             :subviews 
             (list
              
              (om-make-layout
               'om-simple-layout 
               :align :top
               :subviews (list (make-time-monitor editor 
                                                  :font (om-def-font :font3b) 
                                                  #-macosx :background #-macosx +track-color-2+
                                                  :color (om-def-color :white) 
                                                  :time 0)))

              (om-make-layout
               'om-row-layout 
               :delta 5 
               :position (om-make-point (+ *track-control-w* 2) 2)
               :size (om-make-point 90 15)
               :subviews (list (make-play-button editor :enable t) 
                               (make-pause-button editor :enable t) 
                               (make-stop-button editor :enable t)
                               (make-previous-button editor :enable t) 
                               (make-next-button editor :enable t) 
                               (make-repeat-button editor :enable t)))
              nil
              (om-make-layout
               'om-row-layout 
               :delta 5 
               :subviews 
               (let (b1 b2)
                 (setq b1 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :icon-maqview-black :icon-disabled :icon-maqview-gray
                           :lock-push nil :enabled (equal (view-mode editor) :tracks)
                           :action #'(lambda (b) 
                                       (unless (equal (view-mode editor) :maquette)
                                         (button-disable b) (button-enable b2)
                                         (set-main-maquette-view editor :maquette)
                                         ))))
                 (setq b2 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :icon-trackview-black :icon-disabled :icon-trackview-gray
                           :lock-push nil :enabled (equal (view-mode editor) :maquette)
                           :action #'(lambda (b) 
                                       (unless (equal (view-mode editor) :tracks)
                                         (button-disable b) (button-enable b1)
                                         (set-main-maquette-view editor :tracks)
                                         ))))
                 (list b1 b2)))
                                                        
              (om-make-layout
               'om-row-layout 
               :delta 5 
               :subviews 
               (let (b1 b2 b3 b4)
                 (setq b1 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :ctrlpatch-black :icon-pushed :ctrlpatch-gray
                           :lock-push t :enabled t :pushed (show-control-patch editor)
                           :action #'(lambda (b)
                                       (show-hide-control-patch-editor editor (pushed b))
                                       )))
                 (setq b2 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :eval-black :icon-pushed :eval-gray
                           :lock-push nil :enabled t
                           :action #'(lambda (b)
                                       (declare (ignore b))
                                       (let ((maq (get-obj-to-play editor)))
                                         (eval-maquette maq)
                                         (om-invalidate-view tracks-or-maq-view)
                                         ))))
                 (setq b3 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :icon-trash-black :icon-pushed :icon-trash-gray
                           :lock-push nil :enabled t
                           :action #'(lambda (b) 
                                       (declare (ignore b))
                                       (when (and (boxes (get-obj-to-play editor)) 
                                                  (om-y-or-n-dialog "This will remove all boxes in the maquette."))
                                         (m-flush (get-obj-to-play editor))
                                         (om-invalidate-view tracks-or-maq-view)
                                         ))))
                 (setq b4 (om-make-graphic-object 
                           'om-icon-button :size (omp 16 16) 
                           :icon :icon-mute-black :icon-pushed :icon-mute-gray
                           :lock-push t :enabled t
                           :action #'(lambda (b)
                                       (declare (ignore b))
                                       (with-schedulable-object maquette
                                                                (setf (no-exec maquette) 
                                                                      (not (no-exec maquette)))))))
                 (list b1 b2 b3 b4)))
              (om-make-layout
               'om-row-layout 
               :delta 5 
               :position (om-make-point (+ *track-control-w* 2) 2)
               :align :bottom
               :size (om-make-point 90 15)
               :subviews (when (metronome editor)
                           (list ;(metronome-make-signature-box editor (metronome editor):bg-color +track-color-2+)
                            (metronome-make-tempo-box editor (metronome editor) :bg-color +track-color-2+)
                            ))
               )
              
              
              )))))

         (bottom-view
          (om-make-layout 
           'om-simple-layout
           :subviews (list 
                      (om-make-di 'om-multi-text 
                                  :size (omp 200 nil)
                                  :text "select a box to display contents..."
                                  :fg-color (om-def-color :gray)  ; (om-make-color (/ 215 256) (/ 215 256) (/ 215 256))
                                  )
                      )))
         
         (inspector-pane nil))
         
    (set-g-component editor :bottom-view bottom-view)
    (set-g-component editor :ctrl-view ctrl-view)
    (set-g-component editor :main-maq-view (om-make-layout 'om-simple-layout))
    

    ;;; the inspector must be created first because  
    ;;; it is used in the control-patch-view creation 
    ;;; the control patch constructor will update the inspector if necessary
    (when (equal (editor-window-config editor) :inspector)
      (setf inspector-pane (make-inspector-pane editor)))

    (when (show-control-patch editor) 
      (set-g-component editor :left-view (make-control-patch-view editor)))
        
    (om-add-subviews (get-g-component editor :main-maq-view) tracks-or-maq-view)
    
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
                       'om-column-layout :delta nil :ratios '(nil 100 nil 1)
                       :subviews (list 
                                  (get-g-component editor :ctrl-view)
                                  (get-g-component editor :main-maq-view) 
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


(defun set-main-maquette-view (editor mode)
  (setf (view-mode editor) mode)
  (om-remove-all-subviews (get-g-component editor :main-maq-view))
  (mapcar #'stop-cursor (cursor-panes editor))
  (om-add-subviews 
   (get-g-component editor :main-maq-view)
   (if (equal mode :maquette)
       (make-maquette-view editor)
     (make-tracks-view editor)))
  ;;; needs to be done once the views are in place...
  (update-cursor-pane-intervals editor)
  (when (is-playing editor)
    (mapcar #'start-cursor (cursor-panes editor)))
  (mapc 'update-connections (boxes (object editor))))


(defun make-maquette-view (maq-editor)
  (let* ((ruler-maquette (om-make-view 'time-ruler 
                                       :size (om-make-point 30 20)
                                       :x1 (or (getf (get-range maq-editor) :x1) 10000)
                                       :x2 (or (getf (get-range maq-editor) :x2) 10000)
                                       :scrollbars nil :bg-color +track-color-1+))
         (metric-ruler (om-make-view 'metric-ruler 
                                     :tempo (tempo (metronome maq-editor))
                                     :size (om-make-point 30 20)
                                     :scrollbars nil :bg-color +track-color-1+))
         (y-ruler (om-make-view 'y-ruler-view 
                                 :y1 (or (getf (get-range maq-editor) :y1) 100)
                                 :y2 (or (getf (get-range maq-editor) :y2) 0)
                                 :size (om-make-point 30 20)
                                 :scrollbars nil :bg-color +track-color-1+))
         (maq-view (om-make-view 'maquette-view :editor maq-editor :scrollbars nil :bg-color +track-color-1+))
         layout)
    (set-g-component maq-editor :track-views nil)
    (set-g-component maq-editor :maq-view maq-view)
    (set-g-component maq-editor :metric-ruler metric-ruler)
    (set-g-component maq-editor :y-ruler y-ruler)
    (set-g-component maq-editor :abs-ruler ruler-maquette)
    
    (attach-view-to-ruler ruler-maquette metric-ruler)
    (attach-view-to-ruler metric-ruler ruler-maquette)
    (attach-view-to-ruler ruler-maquette maq-view)
    (attach-view-to-ruler metric-ruler maq-view)
    (attach-view-to-ruler y-ruler maq-view)
    
    (update-span metric-ruler)
    
    (setf layout (om-make-layout 
                  'om-grid-layout
                  :delta 2 :dimensions '(2 3) :ratios '((1 99) (1 99 1))
                  :subviews
                  (list
                   
                   ;;; lock here
                   (om-make-graphic-object 
                    'lock-view-area 
                    :size (omp *track-control-w* 18)
                    :editor maq-editor)
                   
                   metric-ruler
                   y-ruler ; (om-make-view 'om-view :bg-color +track-color-1+)
                   maq-view
                   (om-make-di 'om-simple-text 
                               :size (om-make-point *track-control-w* 20) 
                               :text "" ;"ms"
                               :font (om-def-font :font1) 
                               :fg-color (om-def-color :black)
                               :bg-color +track-color-2+)
                   ruler-maquette)))
    (put-patch-boxes-in-editor-view (object maq-editor) maq-view)
    layout
    ))

(defmethod play-editor-get-ruler-views ((self maquette-editor)) 
  (list (get-g-component self :abs-ruler)
        (get-g-component self :metric-ruler)))

(defun make-track-control (n editor)
  (declare (ignore editor))
  (om-make-view 
   'sequencer-track-control :num n
   :size (om-make-point *track-control-w* *track-h*)
   :bg-color (nth (mod n 2) (list +track-color-1+ +track-color-2+))))

(defun n-track-views (maquette-editor)
  (length (get-g-component maquette-editor :track-views)))

(defun make-tracks-view (maq-editor)

  (let* ((ruler-tracks (om-make-view 'time-ruler :size (om-make-point 30 20) 
                                     :x1 (or (getf (get-range maq-editor) :x1) 0) 
                                     :x2 (or (getf (get-range maq-editor) :x2) 10000)
                                     :scrollbars nil :bg-color +track-color-1+
                                     :bottom-p nil :markers-p t))
         (track-views (loop for n from 1 to (n-tracks maq-editor) collect
                            (om-make-view 'sequencer-track-view :num n :size (omp nil *track-h*)
                                          :scrollbars nil :editor maq-editor
                                          :bg-color (nth (mod n 2) (list +track-color-1+ +track-color-2+)))))
         (metric-ruler (om-make-view 'metric-ruler 
                                      :size (om-make-point 30 20)
                                      :scrollbars nil :bg-color +track-color-1+
                                      :tempo (tempo (metronome maq-editor))
                                      :markers-p t)))  ;;; enable/disable markers here

    (set-g-component maq-editor :track-views track-views)
    (set-g-component maq-editor :maq-view nil)
    (set-g-component maq-editor :metric-ruler metric-ruler)
    (set-g-component maq-editor :abs-ruler ruler-tracks)
    (attach-view-to-ruler ruler-tracks metric-ruler)
    (attach-view-to-ruler metric-ruler ruler-tracks)
    (mapcar #'(lambda (v) (attach-view-to-ruler ruler-tracks v)) track-views)
    (mapcar #'(lambda (v) (attach-view-to-ruler metric-ruler v)) track-views)
    
    (update-span metric-ruler)
    
    ;;; set the track view as 'frame' for each box
    (loop for track-view in track-views do 
          (loop for box in (get-track-boxes (object maq-editor) (num track-view)) do
                (setf (frame box) track-view)))
    
    (om-make-layout 
     'om-column-layout :delta 2 :ratios '(1 99 1)
     :subviews (list 
                ;;; the ruler bar
                (om-make-layout 
                 'om-row-layout :delta 2 :ratios '(1 99)
                 :subviews (list 

                            (om-make-graphic-object 
                             'lock-view-area 
                             :size (omp *track-control-w* 18)
                             :editor maq-editor)

                            metric-ruler))
                ;;; allows to scroll he sub-layout
                (om-make-layout 
                 'om-simple-layout :subviews 
                 (list 
                  (om-make-layout 
                   'om-column-layout :delta 2 :scrollbars :v
                   :subviews
                   (loop for n from 1 to (n-tracks maq-editor) collect 
                         (om-make-layout 
                          'om-row-layout :delta 2 :ratios '(1 99)
                          :subviews (list 
                                     (make-track-control n maq-editor)
                                     (nth (1- n) track-views)))))))
                (om-make-layout 
                 'om-row-layout :delta 2 :ratios '(1 99)
                 :subviews (list (om-make-di 'om-simple-text 
                                             :size (om-make-point *track-control-w* 20) 
                                             :text "" ; "ms"
                                             :font (om-def-font :font1) 
                                             :fg-color (om-def-color :black)
                                             :bg-color +track-color-2+)
                                 ruler-tracks))))))


;;; when a box changes track
(defmethod update-container-groups ((maquette OMMaquette)) 
  (when (and (editor maquette) 
             (equal (view-mode (editor maquette)) :tracks)
             (not (= (n-tracks (editor maquette)) (n-track-views (editor maquette)))))
    ;;; will update the number of tracks
    (set-main-maquette-view (editor maquette) :tracks)))


;;; called at init: 
(defmethod add-lock-item ((editor maquette-editor) view) nil)


;;;=====================
;;; PLAYER INTERFACE
;;;=====================
(defmethod editor-make-player ((self maquette-editor))
  ;;; create a metronome
  (setf (metronome self) (make-instance 'metronome :editor self))
  ;;; return the default player
  (call-next-method))


(defmethod update-to-editor ((self maquette-editor) (from metronome))
  
  (let ((m-ruler (get-g-component self :metric-ruler)))
    (setf (tempo m-ruler) (tempo from))
    (update-from-tempo m-ruler)
    (om-invalidate-view m-ruler))
     
  (call-next-method))



(defmethod editor-repeat ((self maquette-editor) t-or-nil)
  (if t-or-nil
      (loop-object (get-obj-to-play self))
    (unloop-object (get-obj-to-play self))))

(defmethod editor-next-step ((self maquette-editor))
  (let* ((object (get-obj-to-play self))
         (step (get-units (cadr (cursor-panes self))))
         (time (get-obj-time object)))
    (set-object-time object (+ step (- time (mod time step))))
    (set-object-time (metronome self) (+ step (- time (mod time step))))))

(defmethod editor-previous-step ((self maquette-editor))
  (let* ((object (get-obj-to-play self))
         (step (get-units (cadr (cursor-panes self))))
         (time (get-obj-time object)))
    (set-object-time object (max 0 (- (- time (mod time step)) step)))
    (set-object-time (metronome self) (max 0 (- (- time (mod time step)) step)))))






;;;========================
;;; PLAYER
;;;========================

(defmethod play-editor-callback ((self maquette-editor) time)
  
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

(defmethod stop-editor-callback ((self maquette-editor))
  (setf (getf (beat-info self) :beat-count) 0
        (getf (beat-info self) :next-date) nil)
  (when (get-g-component self :tempo-box) ;; see editor-play-mixin
    (set-value (cadr (om-subviews (get-g-component self :tempo-box))) 
               (float (tempo-automation-tempo-at-beat (editor-get-tempo-automation self) 0))))
  (reset-boxes (object self))
  (call-next-method))


(defmethod get-interval-to-play ((self maquette-editor))
  (let ((sb (get-selected-boxes self)))
    (if sb
        (list (reduce 'min sb :key 'get-box-onset)
              (reduce 'max sb :key 'get-box-end-date))
      (call-next-method))))


;;;=======================================
;;; UNDO / REDO INTERFACE
;;;=======================================

(defmethod update-after-state-change ((self maquette-editor))
  
  (let* ((maq (object self)))
    
    (when (equal (view-mode self) :maquette) 
      
      (let ((view (get-g-component self :maq-view)))
        (om-remove-all-subviews view)
        (put-patch-boxes-in-editor-view maq view)
        (update-temporalboxes view)
        ))
    
    (mapcar 'om-invalidate-view (get-g-component self :track-views))
    ))


;;; forward to control-patch editor if active/selected

(defmethod undo-command ((self maquette-editor)) 
  (let ((ed (editor (selected-view self))))
    (if (or (null ed) (equal ed self))
        (call-next-method)
      (when (undo-stack ed) 
        #'(lambda () (do-undo ed))))))


(defmethod redo-command ((self maquette-editor)) 
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



