(in-package :om)

(defparameter *spat-debug* nil)

(defclass* spat-scene (om-cleanup-mixin named-object schedulable-object object-with-action time-sequence)
  ((sources :accessor sources :initform nil :initarg :sources) ;list of sounds
   (trajectories :accessor trajectories :initform nil :initarg :trajectories) ;list of 3DC
   (speakers :accessor speakers :initform '((-1 1 0) (1 1 0)) :initarg :speakers)    ;  (1 -1 0) (-1 -1 0)
   (spat-processor :accessor spat-processor :initform nil)
   (in-buffer :accessor in-buffer :initform nil)
   (out-buffer :accessor out-buffer :initform nil)
   (buffer-player :accessor buffer-player :initform nil)
   (current-pos :accessor current-pos :initform 0) ;;; not used
   (audio-sr :accessor audio-sr :initform 44100)))

(defmethod om-cleanup ((self spat-scene))
  (when (spat-processor self)
    (when *spat-debug*
      (om-print (format nil "Free pointer ~A in ~A" (spat-processor self) self) "SPAT_DEBUG")) 
    (spat::OmSpatFreeSpatPan (spat-processor self)))
  (when (in-buffer self) (spat::OmSpatFreeAudioBuffer (in-buffer self)))
  (when (out-buffer self) (spat::OmSpatFreeAudioBuffer (out-buffer self)))
  (when (buffer-player self) (free-buffer-player (buffer-player self)))
  )
  
(defmethod play-obj? ((self spat-scene)) t)

(defmethod get-obj-dur ((self spat-scene)) 
  (reduce 'max (cons (get-last-time-point self)
                     (remove nil (mapcar 'get-obj-dur (sources self))))))

(defmethod time-sequence-update-internal-times ((self spat-scene) 
                                                &optional (interpol-mode :constant-speed) 
                                                (duration 10000) (modif-time nil))
  (with-schedulable-object 
   self
   (loop for traj in (trajectories self) do
         (time-sequence-update-internal-times traj))))


(defmethod set-spat-processor ((self spat-scene) &optional (panning-type "angular"))
  ;(when (buffer-player self) (print (bp-buffer (buffer-player self))))
  (om-cleanup self)
  (let* ((buffersize (ms->samples (time-window self) (audio-sr self)))
         (nch (length (speakers self)))
         (nsrc (length (sources self)))
         (outptr (fli:allocate-foreign-object :type :pointer :nelems nch))
         (inptr (fli:allocate-foreign-object :type :pointer :nelems nsrc)))
    ;(print (list "spat-scene buffer size:" buffersize))
    (dotimes (ch nch)
      (setf (fli:dereference outptr :index ch :type :pointer);(cffi::mem-aref outptr :pointer ch) 
            (fli:allocate-foreign-object :type :float :nelems buffersize :initial-element 0.0)))
    (dotimes (src nsrc)
      (setf (fli:dereference inptr :index src :type :pointer);(cffi::mem-aref inptr :pointer src) 
            (fli:allocate-foreign-object :type :float :nelems buffersize :initial-element 0.0)))
 
    (setf (spat-processor self) (spat::OmSpatCreateSpatPan nsrc nch panning-type)
          (out-buffer self) (spat::allocate-spat-audiobuffer :channels nch :size buffersize :data outptr)
          (in-buffer self) (spat::allocate-spat-audiobuffer :channels nsrc :size buffersize :data inptr))

    (set-play-buffer self)
    ))

;;; spat-scene can play up to 1 minute of audio
(defparameter *spat-scene-player-size* 60000)

(defmethod set-play-buffer ((self spat-scene))
  (let* ((nch (length (speakers self)))
         (size (ms->samples *spat-scene-player-size* (audio-sr self)))
         (audio-buffer (fli::allocate-foreign-object 
                        :type :pointer :nelems nch
                        :initial-contents (loop for c from 0 to (1- nch) collect
                                                (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))
    (setf (buffer-player self) 
          (make-player-from-buffer audio-buffer size nch (audio-sr self)))
    ))

(defmethod initialize-instance :after ((self spat-scene) &rest args)
  (let* ((srcs (om-copy (list! (sources self))))
         (trajs (print (format-trajectories-as-3DC-list self)))
         (max-len (max (length (trajectories self)) (length (sources self)))))
    (setf (trajectories self) 
          (loop for i from 1 to max-len collect
                (or (nth (1- i) trajs) 
                    (om-init-instance (make-instance '3DC :x-points 0 :y-points 0 :z-points 0 :times 0)))))
    (setf (sources self) 
          (loop for i from 1 to max-len collect
                (or (nth (1- i) (sources self)) i)))
    (time-sequence-update-internal-times self)
    (set-object-time-window self 100)
    (set-spat-processor self)
    self))


(defmethod format-trajectories-as-3DC-list ((self spat-scene))
  (let ((trajs (trajectories self)))
    (if trajs
        (loop for traj in (list! trajs) collect (format-traj-as-3DC traj))
      (create-default-trajectories-from-sources (sources self)))))

(defmethod format-traj-as-3DC ((self list))
  (let* ((tranformed-lists (mat-trans self))
        (obj (make-instance '3DC 
                   :x-points (nth 0 tranformed-lists) 
                   :y-points (nth 1 tranformed-lists) 
                   :z-points (nth 2 tranformed-lists) 
                   :times (nth 3 tranformed-lists))))
    (om-init-instance obj)))

;new version used for the range functions
(defmethod time-sequence-get-timed-item-list ((self spat-scene))
  (loop for traj in (trajectories self) append (time-sequence-get-timed-item-list traj)))

;not used anymore
(defmethod get-all-times ((self spat-scene)) (get-all-sorted-times self))

(defmethod format-traj-as-3DC ((self 3DC)) (clone self))
(defmethod format-traj-as-3DC ((self bpf)) (objFromObjs self (make-instance '3DC)))

(defmethod create-default-trajectories-from-sources (sources)
  (loop for s in sources collect (make-instance '3DC)))
  
(defmethod get-all-traj-points ((self spat-scene))
  (loop for traj in (trajectories self)
        append (point-pairs traj)))
  
(defmethod get-all-sorted-times ((self spat-scene))
  (sort (loop for traj in (trajectories self) append (get-all-times traj)) '<))

(defmethod spat-scene-min-time ((self spat-scene))
  (if (trajectories self)
      (car (get-all-sorted-times self))
    0))

(defmethod get-last-time-point ((self spat-scene))
  (or (car (last (get-all-sorted-times self))) 0))

(defmethod update-interpol-settings-for-trajs ((self spat-scene))
  (loop for 3dc in (trajectories self) do 
        (setf (interpol 3dc) (interpol self)
              (interpol-time 3dc) (interpol-time self))))

;;;=========================================
;;; TIME MARKERS METHODS
;;;=========================================

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-time-markers ((self spat-scene))
  "returns a list of time markers"
  (flat (loop for traj in (trajectories self) 
        collect (get-all-master-points-times traj))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod get-elements-for-marker ((self spat-scene) marker)
  "returns a list of elements matching the marker"
  (loop for traj in (trajectories self) 
        collect
        (list traj (point-exists-at-time traj marker))))

;;;TIME MARKERS TO REDEFINE FOR YOUR SUBCLASS
(defmethod translate-elements-from-time-marker ((self spat-scene) elems dt)
  "translates elements from a time marker with dt"
  (loop for elem in elems
        do
        (when (not (member nil (cdr elem)))
          (temporal-translate-points (car elem) (cdr elem) dt))))

;;;====================================
;;; MINIVIEW
;;;====================================

(defmethod display-modes-for-object ((self spat-scene))
  '(:hidden :text :mini-view))

;;; to be redefined by objects if they have a specific draw/cache strategy
(defmethod get-cache-display-for-draw ((self spat-scene)) 
  (list 
   ;;; just record a zoom factor for drawing
   (loop for p in (append (get-all-traj-points self) (speakers self))
         maximize (if p (sqrt (+ (* (car p) (car p)) (* (cadr p) (cadr p)))) 0))))
   

(defvar *spat-mini-view-source-radius* 3)   

(defmethod draw-spat-scene ((self spat-scene) x y w h max at)
  (flet ((relpos (coord ref) (+ ref (* coord (/ (- ref 15) max)))))
    (let ((x0 (/ w 2))
          (y0 (/ h 2)))
    (om-with-fg-color (om-def-color :gray)
      (om-with-line '(2 2)
        (om-draw-ellipse (+ x x0) (+ y y0) (- x0 5) (- y0 10))
        (om-draw-line (+ x x0) (+ y y0 -10) (+ x x0) (+ y y0 10))
        (om-draw-line (+ x x0 -10) (+ y y0) (+ x x0 10) (+ y y0)))
    (loop for spk in (speakers self) do
          (when spk (om-draw-rect (+ x (- (relpos (car spk) x0) 4))
                                  (+ y (- (relpos (- (cadr spk)) y0) 4))
                        8 8 :fill t)))
    )
    (loop for traj in (trajectories self) do
          (when traj 
            (let ((pt (get-point-at-time traj at)))
              (when pt
                (om-with-fg-color (color traj)
                  (om-draw-circle (+ x (relpos (om-point-x pt) x0)) ; - *spat-mini-view-source-radius*)
                                  (+ y (relpos (- (om-point-y pt)) y0)) ; - *spat-mini-view-source-radius*)
                                  (/ h 30) ;(* 2 *spat-mini-view-source-radius*) 
                                  :fill t)
                  )))))
    )))

(defmethod draw-mini-view ((self spat-scene) (box t) x y w h &optional time)
  (ensure-cache-display-draw box self)
  (let ((time (or time 0))
        (max (car (get-display-draw box))))
    (draw-spat-scene self x y w h max time)))


#|
;;; showing all the traj as lists of x y and z
(defmethod draw-maquette-mini-view ((self spat-scene) (box t) x y w h &optional time) 
  (let ((num-traj (length (trajectories self)))
        (x-col (om-def-color :red))
        (y-col (om-def-color :green))
        (z-col (om-def-color :blue))
        (min-t (get-first-time self))
        (max-t (get-obj-dur self)))
    (when (> num-traj 0)
      (loop for traj in (trajectories self)
            for i from 0 to (1- num-traj)
            do
            (let* ((ranges (nice-bpf-range traj))
                   (x-t-list (mat-trans (list (get-all-times traj) (x-points traj))))
                   (x-t-ranges (list min-t max-t (car ranges) (cadr ranges)))
                   (y-t-list (mat-trans (list (get-all-times traj) (y-points traj))))
                   (y-t-ranges (list min-t max-t (caddr ranges) (cadddr ranges)))
                   (z-t-list (mat-trans (list (get-all-times traj) (z-points traj))))
                   (z-t-ranges (list min-t max-t (nth 5 ranges) (nth 6 ranges)))
                   (h-new (/ h num-traj))
                   (y-new (* i h-new)))
              ;draw x = f(t)
              (om-draw-line x y-new (+ x w) y-new)
              (draw-bpf-points-in-rect x-t-list
                                       x-col
                                       x-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       )
              ;draw y = f(t)
              (draw-bpf-points-in-rect y-t-list
                                       y-col
                                       y-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       )
              ;draw z = f(t)
              (draw-bpf-points-in-rect z-t-list
                                       z-col
                                       z-t-ranges
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                       x (+ y-new 5) w (- h-new 5)
                                       ) 
              t)))))
|#

(defparameter *min-key-frames-size* 100)

;;; second version using keyframes ;)
;;need to add the time parameters and it should animate the good keyframe...
(defmethod draw-maquette-mini-view ((self spat-scene) (box t) x y w h &optional time)
  (let* ((dur (get-obj-dur self)) 
         (frames (max 1 (round (/ w *min-key-frames-size*))))
         (frame-width (round (/ w frames))))
    (when (> dur 0)
      (loop for frame from 0 to (1- frames) do
            (let ((frame-time (round (* frame (/ dur frames))))
                  (frame-end-time (round (* (1+ frame) (/ dur frames))))
                  (frame-x (+ x (* frame frame-width  ))))
              (when (not (equal frame 0))
                (om-draw-line frame-x y frame-x (+ y h)))
              (draw-mini-view self box frame-x y frame-width h 
                              (if (and time (> time frame-time) (< time frame-end-time)) time frame-time)))))))

;;;=========================================
;;; METHODS CALLED FROM OUTSIDE
;;;=========================================

(defmethod get-properties-list ((self spat-scene))
  '(("" ;"BPF attributes" 
     (:name "Name" :text name)
     (:action "Action" :action action)
     (:interpol "Interpolation" :bool interpol)
     (:interpol-time "Interpol Time (ms)" :number interpol-time (20 1000)))))

(defmethod get-def-action-list ((object spat-scene))
  '(print send-source-as-osc render-audio))

(defmethod arguments-for-action ((fun (eql 'send-source-as-osc)))
  '((:string address "/source/*/xyz")
    (:string host "localhost")
    (:int port 3000)))

(defmethod arguments-for-action ((fun (eql 'render-audio)))
  '((:string panning-type "angular")))

(defun send-source-as-osc (src-point &optional (address "/source/*/xyz") (host "localhost") (port 3000))
  (osc-send (list (substitute  (elt (number-to-string (car src-point)) 0) #\* address)
                  (3dpoint-x (cadr src-point)) (3dpoint-y (cadr src-point)) (3dpoint-z (cadr src-point)))
            host port))
    
(defmethod spat-scene-point-actions ((self spat-scene) interval)
  (when (action self)
    (sort 
     (if (interpol self)
        
         (let* ((root (get-active-interpol-time self (car interval))))
           (loop for interpolated-time in (arithm-ser root (1- (cadr interval)) (interpol-time self)) 
                 append (loop for 3dc in (trajectories self) 
                              for i = 1 then (+ i 1) 
                              collect (list 
                                       interpolated-time 
                                       #'(lambda (pt) (funcall (action-fun self) pt)) 
                                       (list (list i (make-default-tpoint-at-time 3dc interpolated-time))) ;; (traj-id point)
                                       ))))
      
       (loop for 3dc in (trajectories self) 
             for i = 1 then (+ i 1) append
             (loop for pt in (filter-list (point-list 3dc) (car interval) (cadr interval) :key 'tpoint-internal-time)
                   collect
                   (list (tpoint-internal-time pt)
                         #'(lambda (ptmp) (funcall (action-fun self) ptmp))
                         (list (list i pt))  ;; (traj-id point)
                         ))) 
       )
     '< :key 'car))
  )


;;;===============
;;; PLAYER
;;;===============

;;; dummy action, just to be accepted by object-with-action
(defun render-audio ())

(defmethod get-computation-list ((self spat-scene) &optional interval)
  (when (equal (action self) 'render-audio)
    (spat-scene-audio-actions self (car interval) (cadr interval))))

(defmethod get-action-list-for-play ((self spat-scene) interval &optional parent)
  (when (action self)
    (if (equal (action self) 'render-audio)
        (get-external-control-action-list self interval parent)
      (spat-scene-point-actions self interval)
      )))

;;;===============
;;; SPECIAL AUDIO RENDERING USING SPAT 
;;;===============


(defmethod player-play-object ((self scheduler) (object spat-scene) caller &key parent interval)
  (call-next-method)
  (when (equal (action object) 'render-audio)
    (setf (current-pos object) 0)
    (jump-to-frame (buffer-player object) 0)
    (start-buffer-player (buffer-player object) 
                         :start-frame (if (car interval) (round (* (car interval) (/ (audio-sr object) 1000.0))) 0))))

(defmethod player-stop-object ((self scheduler) (object spat-scene))
  (when (equal (action object) 'render-audio)
    (stop-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object spat-scene))
  (when (equal (action object) 'render-audio)
    (pause-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object spat-scene))
  (when (equal (action object) 'render-audio)
    (continue-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod set-object-time ((self spat-scene) time)
  (when (equal (action self) 'render-audio)   
    (jump-to-time (buffer-player self) time))
  (call-next-method))


;;; !! TO-FROM must be <= WINDOW-SIZE !!
(defun spat-scene-spatialize (self from-smp to-smp)
  ;(om-print (format nil "Preparing audio from ~D to ~D" from-smp to-smp) "SPAT-SCENE")
  ;;; FILL IN-BUFFER WITH SOURCE SECTION
  (let ((n-samples (- to-smp from-smp))
        (inptr (spat::spat-audiobuffer-get-data (in-buffer self))))
    (when (< (ms->samples (time-window self) (audio-sr self)) n-samples)
      (error "Window [~D-~D] is too big for the SPAT-SCENE buffer size (~D)" 
             from-smp to-smp (ms->samples (time-window self) (audio-sr self))))
    ;(om-print (format nil "Get ~D samples from ~D" n-samples from-smp) "SPAT-SCENE-PLAYER")
    ;;; FILL THE SPAT IN-POINTER WITH SOURCES
    (when (>= from-smp 0)
      (loop for src in (sources self) 
            for n = 0 then (+ n 1) do
            (when src 
              (with-audio-buffer (b src)
                (dotimes (i n-samples)
                  (setf (fli:dereference (fli:dereference inptr :index n :type :pointer) :index i :type :float)
                        (if (< (+ from-smp i) (n-samples src))
                            (fli:dereference 
                             (fli:dereference (om-sound-buffer-ptr b) :index 0 :type :pointer)
                             :index (+ from-smp i) :type :float)
                          0.0)))))))
    
    ;(spat::omspatprintaudiobuffer (in-buffer self) nil)
    ;(spat::omspatprintaudiobuffer (out-buffer self) nil)
    
    (handler-bind ((error #'(lambda (e) 
                              (print (format nil "~A" e))
                              (print (spat::OmSpatGetLastError))
                              (spat::OmSpatClearLastError)
                              (abort e))))
      ;;; SEND SPAT CONTROL COMMANDS
      ;;; FOR THE MOMENT WE CONSIDER JUST ONE POSITION FOR EACH SOURCE (STATIC)
      (let* ((time-ms (samples->ms from-smp (audio-sr self)))
             (messages (remove nil 
                               (loop for traj in (trajectories self) 
                                     for i = 1 then (+ i 1) collect
                                     (let ((p (find time-ms (point-list traj) :test '<= :key 'tpoint-internal-time)))
                                       (when p (list (format nil "/source/~D/xyz" i) (om-point-x p) (om-point-y p) (om-point-z p)))))))
             (ob (make-o.bundle (make-instance 'osc-bundle :messages messages))))
        (unless (or (null ob)
                    (spat::OmSpatPanProcessOSCCommands (spat-processor self) (o.bundle-ptr ob) (o.bundle-size ob)))
          (error "ERROR IN SPAT CONTROL PROCESSING")))
      
      ;;; PROCESS
      (if (spat::OmSpatPanProcess (spat-processor self) (out-buffer self) (in-buffer self) n-samples)
          (spat::spat-audiobuffer-get-data (out-buffer self))
        (error "ERROR IN SPAT AMPLITUDE PANNING:")))
    "done"))
    

(defun spat-scene-copy-output-to-buffer (self audio-buffer n-samples at)
  (let ((out-buffer-data (spat::spat-audiobuffer-get-data (out-buffer self))))
    (dotimes (c (length (speakers self)))
      (dotimes (smp n-samples)
        (setf (fli:dereference (fli:dereference audio-buffer :index c :type :pointer)
                               :index (+ at smp) :type :float)
              (fli:dereference (fli:dereference out-buffer-data :index c :type :pointer) :index smp :type :float)
              )))))
  
(defmethod spat-scene-audio-actions ((self spat-scene) t1 t2)
  (if (>= t2 *spat-scene-player-size*) (print "out of memory !!!!")
    (let* ((smp1 (ms->samples t1 (audio-sr self)))
           (smp2 (ms->samples t2 (audio-sr self)))
           (out-buffer-data ))
      (list
       (list (- t1 (time-window self))
             t1
             #'(lambda ()
                 (when (spat-scene-spatialize self smp1 smp2)
                   ;;; COPY IN BUFFER-PLAYER BUFFER
                   (spat-scene-copy-output-to-buffer self 
                                                     (bp-buffer (buffer-player self))
                                                     (- smp2 smp1) smp1))))))))




;;;===============================================
;;; Offline synth
;;;===============================================

(defmethod* spat-synth ((self spat-scene) &optional to-file)  
            (let* ((window-size (ms->samples (time-window self) (audio-sr self)))
                   (total-size (ms->samples (get-obj-dur self) (audio-sr self)))
                   (out-buffer (make-om-sound-buffer-gc 
                                :nch (length (speakers self))
                                :ptr (make-audio-buffer (length (speakers self)) total-size))))
              (loop for smp = 0 then (+ smp window-size)
                    while (< smp total-size) do
                    (let ((smp2 (min total-size (+ smp window-size))))
                      (spat-scene-spatialize self smp smp2)
                      (spat-scene-copy-output-to-buffer 
                       self 
                       (om-sound-buffer-ptr out-buffer)
                       (- smp2 smp) smp)
                      ))
              (om-init-instance (make-instance 'sound :buffer out-buffer
                                               :n-samples total-size
                                               :n-channels (length (speakers self)) 
                                               :sample-rate (audio-sr self))
                                `((:file ,to-file)))))

;;;===============================================
;;; SVG export
;;;===============================================

(defun omcol2svgcolorstr (color)
  (format nil "rgb(~D, ~D, ~D)" 
          (round (* 255 (om-color-r color)))
          (round (* 255 (om-color-g color)))
          (round (* 255 (om-color-b color)))))

;to improve to adapt to the size
(defmethod export-keyframe-as-svg ((self spat-scene) file-path &key  (time 0) (w 300) (h 300) (margins 20) (speakers t))
  :icon 908
  :indoc '("a spat-scene object" "a pathname" "time to draw" "image width" "image height" "margins size" "show speakers" )
  :initvals '(nil nil 0 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New SVG file"
                                                       :types '("SVG Files" "*.svg")))))
    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
             (center (omp (/ w 2) (/ h 2)))
             (scale (/ (min (/ (- h (* 2 margins)) 2) (/ (- w (* 2 margins)) 2)) 2) ))

        ;draw the frame
        (svg::draw scene 
                   (:rect :x margins :y margins :height (- h (* margins 2)) :width (- w (* margins 2)))
                   :fill "none" :stroke "rgb(0, 0, 0)" :stroke-width 1 )

        ;draw a cross ath the center
        (svg::draw scene 
                   (:line
                    :x1 (- (om-point-x center) (* 0.1 scale)) :y1 (om-point-y center) 
                    :x2  (+ (om-point-x center) (* 0.1 scale)) :y2 (om-point-y center) 
                    :stroke "rgb(0, 0, 0)"))
        (svg::draw scene 
                   (:line
                    :x1 (om-point-x center) :y1 (- (om-point-y center) (* 0.1 scale))
                    :x2 (om-point-x center) :y2 (+ (om-point-y center) (* 0.1 scale))
                    :stroke "rgb(0, 0, 0)"))
        
        ;draw a unit circle
        (svg::draw scene 
                   (:circle :cx (om-point-x center) :cy (om-point-y center) :r (* 1 scale)
                    :stroke "rgb(0, 0, 0)"
                    :fill "none"))
      
        ;draw the speakers
        (when speakers
          (let ((speak_w (* 0.1 scale)))
            (loop for spk in (speakers self) do
                  (when spk
                    (svg::draw scene 
                               (:rect 
                                :x  (- (- (om-point-x center) (* (car spk) scale)) (/ speak_w 2))  
                                :y (- (- (om-point-x center) (* (cadr spk) scale)) (/ speak_w 2)) 
                                :height speak_w
                                :width speak_w)
                               :fill "rgb(0, 0, 0)" :stroke "rgb(0, 0, 0)" :stroke-width 1 )
                    ))))

        ;draw the sources at time
        (loop for traj in (trajectories self)
              for n = 1 then (+ n 1) do
          (when traj 
            (let ((pt (if (interpol traj)
                          (make-default-tpoint-at-time traj time)
                        (get-active-point-at-time traj time))))
              (when pt
                (let ((cx (+ (om-point-x center) (* (om-point-x pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill col))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                )))))

        ;draw the time
        (svg:text scene (:x (+ margins 4) :y (+ margins 14))  (format nil "~D" time ) " ms")
  
             (with-open-file (s pathname :direction :output :if-exists :supersede)
               (svg::stream-out s scene)))
        pathname
        )))


;to improve to adapt to the size
(defmethod export-interval-as-svg ((self spat-scene) file-path &key  (start-time 0) (end-time nil) (w 300) (h 300) (margins 20) (speakers t))
  :icon 908
  :indoc '("a spat-scene object" "a pathname" "start-time" "end-time" "image width" "image height" "margins size" "show speakers" )
  :initvals '(nil nil nil nil 300 300 20 1)
  :doc "
Exports <self> to SVG format.
"
  (let* ((pathname (or file-path (om-choose-new-file-dialog :directory (def-save-directory) 
                                                       :prompt "New SVG file"
                                                       :types '("SVG Files" "*.svg")))))
    (unless end-time
      (setf end-time (get-obj-dur self)))

    (when pathname
      (setf *last-saved-dir* (make-pathname :directory (pathname-directory pathname)))
      (let* ((scene (svg::make-svg-toplevel 'svg::svg-1.1-toplevel :height h :width w))
             (center (omp (/ w 2) (/ h 2)))
             (scale (/ (min (/ (- h (* 2 margins)) 2) (/ (- w (* 2 margins)) 2)) 2) ))

        ;draw the frame
        (svg::draw scene 
                   (:rect :x margins :y margins :height (- h (* margins 2)) :width (- w (* margins 2)))
                   :fill "none" :stroke "rgb(0, 0, 0)" :stroke-width 1 )

        ;draw a cross ath the center
        (svg::draw scene 
                   (:line
                    :x1 (- (om-point-x center) (* 0.1 scale)) :y1 (om-point-y center) 
                    :x2  (+ (om-point-x center) (* 0.1 scale)) :y2 (om-point-y center) 
                    :stroke "rgb(0, 0, 0)"))
        (svg::draw scene 
                   (:line
                    :x1 (om-point-x center) :y1 (- (om-point-y center) (* 0.1 scale))
                    :x2 (om-point-x center) :y2 (+ (om-point-y center) (* 0.1 scale))
                    :stroke "rgb(0, 0, 0)"))
        
        ;draw a unit circle
        (svg::draw scene 
                   (:circle :cx (om-point-x center) :cy (om-point-y center) :r (* 1 scale)
                    :stroke "rgb(0, 0, 0)"
                    :fill "none"))
      
        ;draw the speakers
        (when speakers
          (let ((speak_w (* 0.1 scale)))
            (loop for spk in (speakers self) do
                  (when spk
                    (svg::draw scene 
                               (:rect 
                                :x  (- (- (om-point-x center) (* (car spk) scale)) (/ speak_w 2))  
                                :y (- (- (om-point-x center) (* (cadr spk) scale)) (/ speak_w 2)) 
                                :height speak_w
                                :width speak_w)
                               :fill "rgb(0, 0, 0)" :stroke "rgb(0, 0, 0)" :stroke-width 1 )
                    ))))

        ;draw the motions at start-time
        (loop for traj in (trajectories self)
              for n = 1 then (+ n 1) do
          (when traj 
            (let ((start-pt (if (interpol traj)
                          (make-default-tpoint-at-time traj start-time)
                              (get-active-point-at-time traj start-time)))
                  (end-pt (if (interpol traj)
                              (make-default-tpoint-at-time traj end-time)
                            (get-active-point-at-time traj end-time))))

              ;draw a line between start and end
              (when (and start-pt end-pt)
                (let ((spos (find-active-position-at-time traj start-time))
                      (epos (find-active-position-at-time traj end-time)))
                  (loop for idx = spos then (+ idx 1)
                         while (< idx epos) do
                         
                         (let* ((p1 (get-nth-point traj idx))
                               (p2 (get-nth-point traj (1+ idx)))
                               (csx (+ (om-point-x center) (* (om-point-x p1) scale)))
                               (csy (-  (om-point-y center) (* scale (om-point-y p1))))
                               (cex (+ (om-point-x center) (* (om-point-x p2) scale)))
                               (cey (-  (om-point-y center) (* scale (om-point-y p2))))
                               (col (omcol2svgcolorstr (or (color traj) (om-def-color :black)))))
                           (svg::draw scene 
                                      (:line
                                       :x1 csx :y1 csy)
                                      :x2 cex :y2 cey
                                      :stroke col)
                           ))))

              ;draw start point
              (when start-pt
                (let ((cx (+ (om-point-x center) (* (om-point-x start-pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y start-pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke col
                            :fill "none"))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke col
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                ))
              ;draw end-point
              (when end-pt
                (let ((cx (+ (om-point-x center) (* (om-point-x end-pt) scale)))
                      (cy (-  (om-point-y center) (* scale (om-point-y end-pt))))
                      (col (omcol2svgcolorstr (or (color traj) (om-def-color :black))))
                      (rad  (* 0.1 scale))
                      (txt (format nil "~D" n )))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill col))
                (svg::draw scene 
                           (:circle :cx cx :cy cy :r rad
                            :stroke "rgb(0, 0, 0)"
                            :fill "none"))
                (svg:text scene (:x (- cx 2) :y (+ cy 3))  (svg:tspan (:font-size "10") txt))
                )))))

        ;draw the time
        (svg:text scene (:x (+ margins 4) :y (+ margins 14))  (format nil "[~D, ~D]" start-time end-time ) " ms")
  
             (with-open-file (s pathname :direction :output :if-exists :supersede)
               (svg::stream-out s scene)))
        pathname
        )))