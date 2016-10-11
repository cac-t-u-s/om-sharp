;;;====================================
;;; SPAT EDITOR
;;;====================================

(in-package :om)

;;;===============================
;;; OSC binding for Spat viewer
;;;===============================

; (setq *spat-debug* nil)

(defun viewer-osc-command (viewerhdl messages)
  (when *spat-debug* (print messages))
  (let ((ob (make-o.bundle (make-instance 'osc-bundle :messages messages))))
    (spat::OmSpatProcessOscCommands viewerhdl (o.pointer-ptr (bundle_s ob)))
    ))



;;;===============================
;;; Spat editor
;;;===============================

(defclass spat-editor (OMEditor play-editor-mixin) 
  ((view-mode :accessor view-mode :initform :spat)
   (active-items :accessor active-items :initform nil)
   (timeline-editor :accessor timeline-editor :initform nil)
   (source-editors :accessor source-editors :initform nil)
   (source-picts :accessor source-picts :initform nil)
   ))

(defmethod object-has-editor ((self spat-scene)) t)
(defmethod get-editor-class ((self spat-scene)) 'spat-editor)

;; from play-editor-mixin
(defmethod cursor-panes ((self spat-editor)) (cursor-panes (timeline-editor self)))
(defmethod get-obj-to-play ((self spat-editor)) (object-value self))

(defmethod set-time-display ((self spat-editor) time)
  (set-time-display (timeline-editor self) time)
  (call-next-method))

(defmethod init-editor ((self spat-editor))
  (setf (timeline-editor self) (make-instance 'timeline-editor :object (object self) :container-editor self))
  (set-cursor-time (timeline-editor self) (spat-scene-min-time (object-value self)))
  (init-3dc-actions self))

(defmethod close-source-editors ((self spat-editor))
  (loop for s-ed in (source-editors self) do 
        (when (window (cadr s-ed)) (om-close-window (window (cadr s-ed)))))
  (setf (source-editors self) nil))

(defmethod init-3dc-actions ((self spat-editor))
  (let ((3dc-list (trajectories (object-value self))))
    (loop for curve in 3dc-list
          for n = 1 then (+ n 1) do
          ; (setf (action curve) 'osc-3dc-to-spat)
          (setf (name curve) (number-to-string n)))))

;;; called from a child-editor when the value is edited
(defmethod update-to-editor ((editor spat-editor) (from t)) 
  (call-next-method)
  (time-sequence-update-internal-times (object-value editor))
  (update-interpol-settings-for-trajs (object-value editor))
  (when (window editor) (editor-invalidate-views editor)))

;;; called when the box updates its value
(defmethod update-to-editor ((editor spat-editor) (from OMBoxEditCall)) 
  (close-source-editors editor)
  (call-next-method)
  (make-timeline-view (timeline-editor editor))
  (enable-play-controls editor t))

(defmethod editor-invalidate-views ((self spat-editor))
  (editor-invalidate-views (timeline-editor self))
  (when (get-g-component self :spat-view)
    (update-spat-view-selection self)
    (update-spat-display self)))

;;======== Editor Window ========

(defmethod editor-get-all-time-sequences ((self spat-editor)) 
  (trajectories (object-value self)))

(defmethod editor-get-time-sequence ((self spat-editor) id)
  (when id (nth id (trajectories (object-value self)))))

;;; TRANSPORT 
(defmethod build-transport-view ((editor spat-editor))
  (let ((button-size (omp 17 17)))
    (om-make-layout 'om-row-layout 
                    :subviews (list 
                               (make-time-monitor (timeline-editor editor) :time 0) 
                               :separator 
                               (make-play-button editor :size button-size) 
                               (make-pause-button editor :size button-size) 
                               (make-stop-button editor :size button-size)
                               (make-previous-button editor :size button-size) 
                               (make-next-button editor :size button-size) ))))


;;; Si les attributs de spat-window sont transférés dans spat-editor, alors
;;; une grand partie de init-window pourra venir ici.
(defmethod make-editor-window-contents ((editor spat-editor))
  (let ((timeline (om-make-layout 'om-row-layout))
        (control-view (om-make-layout 'om-column-layout))
        (spat-view (om-make-view 'spat-scene-view  :size (omp 200 200))))
    
    (set-g-component editor :spat-view spat-view)
    (set-g-component editor :control-view control-view)
    
    (om-add-subviews control-view 
                     (om-make-di 'om-button :text "+" 
                                 :size (omp 40 24)
                                 :di-action #'(lambda (b) 
                                                (declare (ignore b))
                                                (add-source editor)))
                                               
                     (om-make-di 'om-button :text "-" 
                                 :size (omp 40 24)
                                 :di-action #'(lambda (b) 
                                                (declare (ignore b))
                                                (remove-source editor)))
                     nil
                     (om-make-di 'om-popup-list :items '("Spat view" "3DC view" "Eq") 
                                 :size (omp 100 24)
                                 :di-action #'(lambda (b) 
                                                (declare (ignore b))
                                                (case (om-get-selected-item-index b)
                                                  (0 (set-spat-view-mode editor))
                                                  (1 (set-3D-view-mode editor))
                                                  (2 (set-spat-view-mode editor :eq))
                                                  ))))
    
    (set-g-component (timeline-editor editor) :main-panel timeline)
    
    (set-g-component editor :spat-view-container (om-make-layout 'om-simple-layout :subviews (list spat-view)))
    
    (om-make-layout 
     'om-column-layout
     :ratios '(0.9 0.1)
     :subviews (list 
                (om-make-layout 'om-row-layout
                                :ratios '(98 1 1)
                                :subviews
                                (list (get-g-component editor :spat-view-container) 
                                      control-view 
                                      (call-next-method)))
                (get-g-component (timeline-editor editor) :main-panel)
                ))
    ))

(defmethod editor-window-init-size ((self spat-editor)) (om-make-point 500 600))

(defmethod init-window ((win OMEditorWindow) (editor spat-editor))
  (call-next-method)
  (attach-spat-scene-view-to-spat (get-g-component editor :spat-view))
  (make-timeline-view (timeline-editor editor))
  (enable-play-controls editor t)
  (update-sources editor)
  (init-spat-viewer editor)
  (update-spat-display editor))


(defun set-spat-view-mode (editor &optional mode)
  (unless (equal (view-mode editor) :spat)
    (setf (view-mode editor) :spat)
    (set-g-component editor :spat-view (om-make-view 'spat-scene-view :size (omp 200 200)))
    (om-remove-all-subviews (get-g-component editor :spat-view-container))
    (set-g-component editor :3d-view nil)
    (om-add-subviews 
     (get-g-component editor :spat-view-container)
     (get-g-component editor :spat-view))
    (attach-spat-scene-view-to-spat 
     (get-g-component editor :spat-view)
     (if (equal mode :eq) "spat.equalizer" "spat.viewer"))
    (init-spat-viewer editor)
    (update-spat-display editor)))

    
(defun set-3D-view-mode (editor)
  (unless (equal (view-mode editor) :3dc)
    (setf (view-mode editor) :3dc)
    (om-remove-all-subviews (get-g-component editor :spat-view-container))
    (free-spat-viewer editor)
    (set-g-component editor :spat-view nil)
    (set-g-component editor :3D-view 
                     (om-make-view 'om-opengl-view
                                   :title "3D view" :editor editor
                                   :bg-color (om-def-color :dark-gray)
                                   :g-objects (create-GL-objects editor)
                                   ))
    (om-add-subviews 
     (get-g-component editor :spat-view-container)
     (get-g-component editor :3D-view))
    (om-invalidate-view (get-g-component editor :3D-view))
    ))

(defmethod create-GL-objects ((self spat-editor))
  (let ((obj-list (trajectories (object-value self))))
    (mapcar #'(lambda (obj) 
                (make-instance 
                 '3D-curve 
                 :points (format-3d-points obj) :color (color obj) 
                 :draw-style :draw-all :line-width 3
                 ))
            obj-list)))


;;;===============================
;;; SOURCES

;tentative de mettre un prgress bar mais ne se met pas bien à jour...
; Besoin à mon avis car charger les waverform est long...

;(capi:define-interface progress-bar-spat ()
;  ()
;  (:panes
;   (progress-bar
;    capi:progress-bar
;    :start 0
;    :end 100
;    ))
;  (:layouts
;   (progress-layout
;    capi:switchable-layout
;    '(progress-bar)))
;  (:default-initargs
;   :title "Loading SpatScene"
;   :best-width 300
;   :layout 'progress-layout))

(defmethod update-sources ((editor spat-editor))
  (setf (source-picts editor) nil)
  (when (sources (object-value editor))
      ;(let ((progress-bar (make-instance 'capi:progress-bar))
      ;      (size (length (sources (object-value editor)))))
      ;  (capi:contain progress-bar)
    ;(set-spat-processor (object-value editor))
    (setf (source-picts editor)
          (loop for src in (sources (object-value editor))
                  ;  for i from 0 to size
                  ;  do 
                  ;  (setf (capi:range-slug-start progress-bar) (* (/ 100 (1- size)) i))
                collect
                (if (subtypep (type-of src) 'sound)
                    (let ((*def-sound-color* (om-gray-color 0.7)))
                      (list (cadr (get-cache-display-for-draw src))
                            (get-obj-dur src))
                      ))))
        ;(setf (capi:range-slug-start progress-bar) 100))
        )
    )



   
;;;=============================
;;; ADD / REMOVE SOURCES
;;;=============================

(defmethod add-source ((self spat-editor))
  (let* ((ss (object-value self))
         (last-traj-index (1- (length (trajectories ss))))
         (last-col (and (trajectories ss) (color (nth last-traj-index (trajectories ss))))))

    (setf (sources ss) (append (sources ss) (list nil))) ; (number-to-string (+ last-traj-index 2)))))
    (setf (trajectories ss) (append (trajectories ss) 
                                    (list (om-init-instance 
                                           (make-instance '3DC :x-points 0 :y-points 0 :z-points 0 :times 0
                                                          :color (and last-col (find-next-color-in-golden-palette last-col)))))))
    (make-timeline-view (timeline-editor self))
    (enable-play-controls self t)
    (init-3dc-actions self)
    (update-sources self)
    (set-spat-processor (object-value self))
    (update-spat-display self)
    (report-modifications self)))

(defmethod remove-source ((self spat-editor))
  (let* ((ss (object-value self))
         (ns (1- (length (trajectories ss))))
         (ed (find ns (source-editors self) :test '= :key 'car)))
    (when ed
      (when (window (cadr ed)) (om-close-window (window (cadr ed))))
      (setf (source-editors self) (remove ed (source-editors self))))
    (when (>= ns 0)
      (setf (sources ss) (butlast (sources ss))
            (trajectories ss) (butlast (trajectories ss)))
      (make-timeline-view (timeline-editor self))
      (enable-play-controls self t)
      (om-set-layout-ratios (main-view self) '(0.9 0.1))
      (update-spat-display self)
      (update-sources self)
      (set-spat-processor (object-value self))
      (report-modifications self)
      )))


(defmethod spat-source-dbclicked ((self spat-editor) n)
  (let ((ed? (find n (source-editors self) :test '= :key 'car)))
    (if ed? (open-editor-window (cadr ed?))
      (let* ((traj (nth n (trajectories (object-value self))))
             (ed (make-instance (get-editor-class traj) 
                                :object (make-instance 'omabstractcontainer :contents traj)
                                :related-editors (list self))))
        (setf (source-editors self) (cons (list n ed) (source-editors self)))
        (init-editor ed)
        (open-editor-window ed)
        ))))

(defmethod editor-delete-contents ((self spat-editor) timeline-id sel)
  (let ((traj (nth timeline-id (trajectories (object-value self)))))
    (mapcar #'(lambda (point) (remove-timed-point-from-time-sequence traj point)) sel)
    (time-sequence-update-internal-times traj))
  (editor-invalidate-views self)
  (report-modifications self))

;(defmethod open-trajectory-collection ((self spat-editor))
;  (let* ((collection (make-instance 'collection :obj-list (trajectories (object-value self))))
;         (ed (make-instance (get-editor-class collection) 
;                            :object (make-instance 'omabstractcontainer :contents collection)
;                            :related-editors (list self))))
;    (init-editor ed)
;   (open-editor-window ed)))


;;;====================================
;;; EDITOR WINDOW
;;;====================================

;;======= spat scene view =========
;;directly controlled by the spat object using a pointer to the view.

(defclass spat-scene-view (om-view) 
  ((spat-view-handler :accessor spat-view-handler :initform nil)
   (id :accessor id :initform nil :initarg :id)))

(defmethod get-spat-view-id ((self t)) -1)
(defmethod get-spat-view-id ((self spat-editor)) 
  (if (get-g-component self :spat-view)
      (id (get-g-component self :spat-view))
    -2))

(defun find-window-with-spat-view (id)
  (find id (capi::collect-interfaces 'OMEditorWindow)
        :test #'(lambda (id win) 
                  (= id (get-spat-view-id (editor win))))))


(defun free-spat-viewer (editor)
  (let ((spat-view (get-g-component editor :spat-view)))
    (when (and spat-view (spat-view-handler spat-view))
      (spat::OmSpatFreeComponent (spat-view-handler spat-view)))))

(defmethod editor-close ((self spat-editor))
  (free-spat-viewer self)
  (call-next-method))

(defmethod om-view-resized ((self spat-scene-view) size) 
  (call-next-method)
  (when (spat-view-handler self) 
    (viewer-osc-command 
     (spat-view-handler self) 
     `(("/om/window/size" ,(w self) ,(h self))))
    ))

(defmethod init-spat-viewer ((editor spat-editor))
  (when (get-g-component editor :spat-view)
    (viewer-osc-command (spat-view-handler (get-g-component editor :spat-view))
                        `(("/om/window/size" ,(w (get-g-component editor :spat-view)) ,(h (get-g-component editor :spat-view)))
                          ("/layout" "single")))))

;;; UPDATE EVERYTHING
;;; (not optimal !!)
(defmethod update-spat-display ((self spat-editor)) 
  (when (window self)
    (let* ((ss (object-value self))
           (spatview (get-g-component self :spat-view))
           (spatviewhandler (and spatview (spat-view-handler spatview))))
      (when spatviewhandler
        (viewer-osc-command spatviewhandler
                            (append 
                             (list (list "/source/number" (length (sources ss)))
                                   (list "/speaker/number" (length (speakers ss))))
                             (loop for spk in (speakers ss) for n = 1 then (1+ n) collect
                                   (cons (format nil "/set/speaker/~D/xyz" n) spk))))
        (loop for source in (sources ss) 
              for traj in (trajectories ss)
              for n = 1 then (+ n 1) do
              (let ((col (color traj))
                    (pt (get-active-point-at-time traj (or (get-cursor-time (timeline-editor self)) 0))))
                (viewer-osc-command spatviewhandler
                                    (remove 
                                     nil 
                                     (list (when col (list (format nil "/set/source/~D/color" n) (coerce (om-color-r col) 'single-float)
                                                           (coerce (om-color-g col) 'single-float) 
                                                           (coerce (om-color-b col) 'single-float) 
                                                           (coerce (om-color-a col) 'single-float)))
                                           (list (format nil "/set/source/~D/name" n) (format nil "~A" n))
                                           (and pt (list (format nil "/set/source/~D/xyz" n) (om-point-x pt) (om-point-y pt) (om-point-z pt)))))
                                    ))))
      (om-invalidate-view (get-g-component (timeline-editor self) :main-panel)))))

;; source selection
(defmethod update-spat-view-selection ((self spat-editor))
  (when (window self)
    (let* ((spatview (get-g-component self :spat-view))
           (spatviewhandler (and spatview (spat-view-handler spatview)))
           (ids (selected-timelines (timeline-editor self))))
      (when (and spatviewhandler ids)
        (viewer-osc-command spatviewhandler 
                            (loop for s in (sources (object-value self))
                                  for n = 0 then (+ n 1) collect
                                  (list (format nil "/set/source/~D/select" (1+ n)) (if (find n ids) 1 0))))
        ))))

;;=======
;;SpatVIEW CALLBACK (UPDATES)
;;=======

(defmethod update-spat-source-selection-from-SpatView ((self spat-editor) spatview)
  (let* ((nb_sources (length (sources (object-value self)))))
    (let ((selection (spat::spat-get-selected-sources spatview nb_sources)))
      (loop for i = 0 then (+ i 1) 
            for sel-i in selection do
            (timeline-set-selected (timeline-editor self) i sel-i)
            ))))

(defmethod update-position-at-time ((self time-sequence) new-p time)
  (let ((p (point-exists-at-time self time)))
    (if p (om-point-set-values-from-point p new-p)
      (progn
        (om-point-set new-p :time time) 
        (insert-timed-point-in-time-sequence self new-p)))))

(defmethod update-spat-source-position-from-SpatView ((self spat-editor) spatview sourcenb)
  (let* ((ss (object-value self))
         (traj (nth sourcenb (trajectories ss)))
         (src-pos (spat::spat-get-source-position spatview sourcenb))
         )
    (update-position-at-time traj (make-3Dpoint :x (nth 0 src-pos) :y (nth 1 src-pos) :z (nth 2 src-pos))
                             (or (get-cursor-time (timeline-editor self)) 0))
    ;(editor-invalidate-views (timeline-editor self))
    ))

(defmethod  update-spat-speaker-position-from-SpatView ((self spat-editor) spatview n)
  (let ((ss (object-value self)))
    (setf (speakers ss) 
          (loop for spk from 0 to (1- (length (speakers ss))) collect
                (spat::spat-get-speaker-position spatview spk)))))


;;; MAIN EDITOR CALLBACK
(defmethod spat-view-changed ((editor spat-editor) param-type n)
  (let* ((spatview (get-g-component editor :spat-view))
         (spatviewhandler (and spatview (spat-view-handler spatview))))
    (when spatviewhandler
      ;(print param-type)
      (cond
       ((= param-type spat::ksourceselectionchanged)
        (update-spat-source-selection-from-SpatView editor spatviewhandler))
       ((= param-type spat::ksourcepositionchanged)
        (update-spat-source-position-from-SpatView editor spatviewhandler n)
        (report-modifications editor))
       ((= param-type spat::kspeakerpositionchanged)
        (update-spat-speaker-position-from-SpatView editor spatviewhandler n)
        (report-modifications editor))
       ((= param-type spat::kSourceDoubleClicked)
        (spat-source-dbclicked editor n))
       (t     
        (when *spat-debug* (print (list "Unknow event" param-type  "N" n )))))
      )))

;;; not called anymore
;;; redefinition of the spat-callback
(defun spat::spat-view-changed-callback (id param-type n)  
  (when *spat-debug*
    (print (list "spat_view_changed callback" "id" id "param" param-type "n" n)))
  (let ((view (find-window-with-spat-view id)))
    (spat-view-changed (editor view) param-type n)
    ))

; (OmSpatGetCurrentStateAsOscBundle component-ptr)
(defun spat::spat-component-handle-callback (component-ptr bundle-ptr)
  (let ((messages (om::decode-bundle-s-pointer-data bundle-ptr)))
    (print messages)
    (odot::osc_bundle_s_deepFree bundle-ptr)))




(defun attach-spat-scene-view-to-spat (ssview &optional (type "spat.viewer"))
  (setf (id ssview) (read-from-string (subseq (string (gensym)) 1)))
  (let ((spat-ptr (spat::OmSpatCreateComponentWithType type)))
    (print (spat::omspatgetcomponenttype spat-ptr))
    (when spat-ptr
      (spat::spat-component-register-callback spat-ptr)
      (spat::OmSpatInstallComponentInNSView spat-ptr (spat::spat-get-view-pointer ssview))
      (setf (spat-view-handler ssview) spat-ptr)
      (spat::spat-component-register-callback spat-ptr)
      ))
  ssview)


;;============== 
;; Playing functions
;;===================

; FUNCTIONS from PLAY-EDITOR-MIXIN
(defmethod editor-next-step ((self spat-editor))
  (let ((next-time (find (get-cursor-time (timeline-editor self))
                         (get-all-sorted-times(object-value self))
                         :test '<)))
    (if next-time
        (set-cursor-time (timeline-editor self) next-time)
    (om-beep))))
    
(defmethod editor-previous-step ((self spat-editor)) 
  (let ((previous-time (find (get-cursor-time (timeline-editor self))
                         (get-all-sorted-times (object-value self))
                         :test '> :from-end t)))
    (if previous-time
        (set-cursor-time (timeline-editor self) previous-time)
      (om-beep))))

(defmethod play-editor-callback ((self spat-editor) time)
  (update-spat-display self)
  (call-next-method))


;;;;;;;;;;;;;;;;;;;
;;; TIMELINE;;;;;;;
;;;;;;;;;;;;;;;;;;;

(defmethod timeline-item ((self spat-editor) id)
  (om-make-graphic-object 'om-icon-button :size (omp 15 15) :position (omp 0 0)
                          :icon 'folder :icon-pushed 'folder-pushed
                          :lock-push nil
                          :action #'(lambda (b)
                                      (let ((snd (om-init-instance (objFromObjs :choose-file (make-instance 'sound)))))
                                        (when snd 
                                          (let* ((ss (object-value self))
                                                 (traj (nth id (trajectories ss)))
                                                 (point (make-default-tpoint-at-time traj (get-obj-dur snd))))
                                            (setf (nth id (sources ss)) snd)
                                            (insert-timed-point-in-time-sequence traj point))
                                          (update-sources self)
                                          ; (mapcar #'om-invalidate-view (timeline-views (timeline-editor self)))
                                          (reinit-ranges (time-ruler (timeline-editor self))))))
                          ))

(defmethod draw-timeline-background ((self spat-editor) view id)
  (when (car (nth id (source-picts self)))
    (let* ((pict (car (nth id (source-picts self))))
           (dur (cadr (nth id (source-picts self))))
           (pw (om-pict-width pict))
           (x (* (/ pw dur) (x1 view)))
           (w (* (/ pw dur) (- (x2 view) (x1 view)))))
      (om-draw-picture pict :x 0 :y 0 :w (w view) :h (h view)
                       :src-x x :src-w w))))

