(in-package :om)

(declaim (optimize (speed 3) (safety 0) (debug 0)))


;(om-midi::portmidi-restart)
;(om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil nil))

;;;====================================================================================
;;;===================== MIDI RENDERER ================================================
;;;====================================================================================

;;;=====
;;;CLASS
(defclass midi-improvisation-renderer (piano-roll)
  ((time-markers :initform nil :accessor time-markers)))

(defmethod initialize-instance ((self midi-improvisation-renderer) &rest initargs)
  (call-next-method)
  (setf (time-window self) 100))

;;;=============
;;;REDEFINITIONS
;*********
;Scheduler
(defmethod player-stop-object ((self scheduler) (object midi-improvisation-renderer))
  (call-next-method)
  (if (car (time-markers object))
      (funcall (cadr (car (time-markers object))))))

(defmethod get-action-list-for-play ((object midi-improvisation-renderer) interval &optional parent)
  (filter-list (sort (append (mapcan #'(lambda (n)
                                         (list (list (midinote-onset n)
                                                     #'(lambda (note) (om-midi::midi-send-evt 
                                                                       (om-midi:make-midi-evt 
                                                                        :type :keyOn
                                                                        :chan (or (midinote-channel note) 1) :port 0
                                                                        :fields (list (midinote-pitch note) (midinote-vel note)))))
                                                     (list n))
                                               (list (midinote-end n)
                                                     #'(lambda (note) (om-midi::midi-send-evt 
                                                                       (om-midi:make-midi-evt 
                                                                        :type :keyOff
                                                                        :chan (or (midinote-channel note) 1) :port 0
                                                                        :fields (list (midinote-pitch note) 0))))
                                                     (list n))))
                                     (midi-notes object))
                             (copy-tree (time-markers object)))
                     '< :key 'car)
               (car interval) (cadr interval) :key 'car))

;********
;Graphics
(defmethod draw-mini-view ((self midi-improvisation-renderer) (box t) x y w h &optional time)
  (let ((display-cache (get-display-draw box)))
    (multiple-value-bind (fx ox) 
        (conversion-factor-and-offset 0 (* 128 330) w x)
      (multiple-value-bind (fy oy) 
          (conversion-factor-and-offset 100 30 (- h 20) (+ y 10))
        (om-with-line-size 8
          (loop for n in (midi-notes self) do
                (om-with-fg-color (nth (mod (midinote-channel n) (length (car display-cache))) (car display-cache))
                  (om-draw-line (+ ox (* fx (midinote-onset n))) (+ oy (* fy (midinote-pitch n)))
                                (+ ox (* fx (midinote-end n))) (+ oy (* fy (midinote-pitch n)))))))
        (om-with-font (om-make-font "Helvetica" 24)
                      (om-draw-string 20 40
                                      "Midi Renderer"))))))

(defmethod draw-cursor-on-box ((object midi-improvisation-renderer) frame pos)
  (when pos
    (om-with-fg-color (om-make-color 0.73 0.37 0.42)
      (let ((x (* (w frame) 
                  (/ pos (* 128 330)))))
        (om-draw-polygon (list (- x 5) 4 x 9 (+ x 5) 4) :fill t)
        (om-with-line '(2 2)
        (om-draw-line x 4 x (- (h frame) 6))
        )))
    t))

;;;==============
;;;OBJECT METHODS
;***********************************
;Output method for data from handler
(defmethod output-sequence-of ((self list) (whencontent midiharmbeat) index time handler (target midi-improvisation-renderer))
  (let ((durs (om+ (dx->x 0 (loop for harmbeat in self collect (duration harmbeat)))  time))
         (midisets (mapcar #'(lambda (beat)
                               (format-midi-list (midiset beat)))
                           (thread-midiharmbeats self)))
         (diff (duration (last-elem self)))
         markers)

    (loop for noteset in midisets
          for offset in durs
          for i from 0
          do
          (loop for note in noteset do
                (incf (car note) offset))
          (let ((ind (+ index i)))
            (push (list offset #'(lambda () (setf (perf-time handler) ind))) markers)))
        
    (loop for n from 1 to 127;(- (- (length (scenario handler)) index) (length markers))
          for off from (+ (caar markers) diff) by diff
          for i from (+ index (length self))
          do
          (let ((ind (+ index i)))
            (if (> ind 0)
                (push (list off #'(lambda () (setf (perf-time handler) ind))) markers)
              (push (list -1 #'(lambda () (setf (perf-time handler) 0))) markers))))
    
    (tile-markers&notes target (reverse markers) (delete nil (sort (flat midisets 1) '< :key 'car)))))

;**********************
;Tile markers and notes
(defmethod tile-markers&notes ((object midi-improvisation-renderer) (markers list) (notes list))
  (let ((new-markers (copy-list (time-markers object)))
        (new-notes (copy-list (midi-notes object))))
    (if markers
        (if new-markers
            (let ((p (position (caar markers) new-markers :test '<= :key 'car)))
              (if p
                  (if (= p 0)
                      (setq new-markers markers)
                    (setf (nthcdr p new-markers) markers))
                (nconc new-markers markers)))
          (setq new-markers markers)))
    (if notes
        (if new-notes
            (let* ((start-date (midinote-onset (car notes)))
                   (p (position start-date new-notes :test '<= :key 'car)))
              (if p
                  (if (= p 0)
                      (setq new-notes notes)
                    (progn
                      ;(loop while (> (midinote-end (nth j new-notes)) start-date)
                      ;      do
                      ;      (setq j (1- j)))
                      (setf (nthcdr p new-notes) notes)))
                (nconc new-notes notes)))
          (setq new-notes notes)))
    (with-schedulable-object object
                             (setf (time-markers object) new-markers)
                             (setf (midi-notes object) new-notes))))

;************************
;For 
(defmethod output-sequence-of ((self list) (whencontent midiharmbeat) index time handler (target ommaquette))
  (let ((notetrak (num handler))
        (durs (om+ (dx->x 0 (loop for harmbeat in self collect (duration harmbeat)))  time))
        (midisets (mapcar #'(lambda (beat)
                              (format-midi-list (midiset beat)))
                          (thread-midiharmbeats self)))
        (diff (duration (last-elem self)))
        ;markers
        )

    (loop for noteset in midisets
          for offset in durs
          for i from 0
          do
          (loop for note in noteset do
                (progn
                  (incf (car note) offset)
                  (setf (nth 4 note) notetrak))) ;;;Channel midi = track
          ;(let ((ind (+ index i)))
          ;  (push (list offset #'(lambda () (setf (perf-time handler) ind))) markers))
          )
    
    ;(loop for n from 1 to 10;(- (- (length (scenario handler)) index) (length markers))
    ;      for off from (+ (caar markers) diff) by diff
    ;      for i from (+ index (length self))
    ;      do
    ;      (let ((ind (+ index i)))
    ;        (if (> ind 0)
    ;            (push (list off #'(lambda () (setf (perf-time handler) ind))) markers)
    ;          (push (list -1 #'(lambda () (setf (perf-time handler) 0))) markers))))
    
    ;(setq markers (reverse markers))
    (setq midisets (delete nil (sort (flat midisets 1) '< :key 'car)))
    (setq diff (caar midisets))
    ;(print (list "HEY OH" (mapcar 'midiset (mapcar 'data self))))
    (when midisets
      (let ((box (last-elem (get-track-boxes target notetrak)))
            ;(boxmark (last-elem (get-track-boxes target marktrak)))
            )
        (when box
          ;;;SLICE MARKERS TOO
          (prune target (get-box-value box) 
                        0 
                        (- diff (get-box-onset box)))
          (set-box-duration box (get-obj-dur (get-box-value box))))
        ;(when boxmark
        ;  ;;;SLICE MARKERS TOO
        ;  (prune target (get-box-value boxmark) 
        ;                0 
        ;                (- diff (get-box-onset boxmark)))
        ;  (set-box-duration boxmark (get-obj-dur (get-box-value boxmark))))
        (add-object-in-maquette target 
                                (make-instance 'piano-roll
                                               :midi-notes (mapcar #'(lambda (note)
                                                                       (decf (car note) diff)
                                                                       note) midisets))
                                :tracknum notetrak
                                :time diff)))
    
    ;(add-object-in-maquette target 
    ;                        (make-instance 'data-stream
    ;                                       :frames (loop for elem in markers 
    ;                                                     collect
    ;                                                     (make-act-bundle (- (car elem) (caar markers)) (list (cadr elem)))))
    ;                        :tracknum marktrak
    ;                        :time (caar markers))
    ))

(defmethod prune-box ((self OMBox) t1-ms t2-ms)
  (set-box-onset self (+ (box-x self) t1-ms))
  (set-box-duration self (- t2-ms t1-ms)))

;;;====================================================================================
;;;===================== AUDIO RENDERER ===============================================
;;;====================================================================================

;;;=====
;;;CLASS
(defclass audio-improvisation-renderer (sound)
  ((time-markers :initform nil :accessor time-markers)
   (source :initform nil :accessor source :initarg :source)
   (box :initform nil :accessor box)
   (crossfade :initform 150 :accessor crossfade :initarg :crossfade)
   (pos-display :initform nil :accessor pos-display :initarg :pos-display)
   ;(display-array :initform nil :accessor display-array)
   ;(display-array-size :initform nil :accessor display-array-size)
   ))

;;;=============
;;;REDEFINITIONS
;**************
;Initialization
(defmethod initialize-instance :after ((self audio-improvisation-renderer) &rest initargs)
  (shared-initialize self t)
  (let* ((nch 2)
         (sr 44100)
         (dur 90)
         (buf (fli:allocate-foreign-object :nelems 2 :type :pointer)))
    (dotimes (n nch)
      (setf (fli:dereference buf :index n :type :pointer)
            (fli:allocate-foreign-object :nelems (* sr dur) :type :float :initial-element 0.0)))
    (setf (buffer self) (make-om-sound-buffer :ptr buf
                                              :count 1 
                                              :nch nch)
          (n-samples self) (* sr dur)
          (n-channels self) nch
          (sample-rate self) sr
          (smpl-type self) :float
          (buffer-player self) (make-player-from-buffer (oa::om-pointer-ptr (buffer self)) (* sr dur) nch sr))))

;*********
;Scheduler
(defmethod player-stop-object ((self scheduler) (object audio-improvisation-renderer))
  (call-next-method)
  (if (car (time-markers object))
      (funcall (cadr (car (time-markers object)))))
  (when (pos-display object)
    (setf (current-time (pos-display object)) 0)
    (om-invalidate-view (box (pos-display object)))))
  
(defmethod get-action-list-for-play ((self audio-improvisation-renderer) interval &optional parent)
  (declaim (optimize (speed 3)))
  (let* ((dur (get-obj-dur self))
         (tmin (car interval))
         (tmax (cadr interval))
         (dt (- tmax tmin))
         (sr (/ (sample-rate self) 1000.0)))
    (if (buffer self)
        (if (< tmin dur)
            (append
             (list 
              (list (- tmin dt)
                    #'(lambda (buf size)
                        (setf (last-bp-pos self)
                              (play-buffer *audio-player*
                                           buf
                                           size
                                           (n-channels self)
                                           :overwrite t
                                           :last-pos (last-bp-pos self)
                                           :buffer-bounds (list (round (* tmin sr)) (min (round (* tmax sr)) size))
                                           :delay dt)))
                    (list (oa::om-pointer-ptr (buffer self)) (n-samples self))))
             (filter-list (copy-tree (time-markers self)) tmin tmax :key 'car))
          (progn
            (setf (last-bp-pos self) nil)
            (list (list nil nil nil))))
      (progn
        (print "this sound has no buffer!")
        (list (list nil nil nil))))))

;********
;Graphics
(defmethod draw-mini-view ((self audio-improvisation-renderer) (box t) x y w h &optional time)
  (setf (box self) box)
  (call-next-method)
  (om-with-font (om-make-font "Helvetica" 24)
                    (om-draw-string 20 40
                                    "Audio Renderer: original file")))

(defmethod get-cache-display-for-draw ((object audio-improvisation-renderer)) 
  (get-cache-display-for-draw (source object)))

(defmethod play-editor-callback ((self t) time) nil)
;  (if (not (eq (type-of (get-obj-to-play self)) 'audio-improvisation-renderer))
;      (box-player-callback self time)
;    (box-player-callback (box (pos-display (get-obj-to-play self))) (current-time (pos-display (get-obj-to-play self))))))

;;;==============
;;;OBJECT METHODS
;***********
;Information
(defmethod get-obj-dur ((self audio-improvisation-renderer))
  (get-obj-dur (source self)))

(defmethod get-real-dur ((self audio-improvisation-renderer))
  (if (and (sample-rate self) (n-samples self)) (* 1000.0 (/ (n-samples self) (sample-rate self))) 0))

;***********************************
;Output method for data from handler
(defmethod output-sequence-of ((self list) (whencontent audiodescrbeat) index time handler target)
  (let* ((durs (om+ (dx->x 0 (loop for descrbeat in self collect (duration descrbeat))) time))
        (dates (mapcar #'(lambda (beat)
                           (datesinbuffer (data beat)))
                       self))
        (packs (list (car dates)))
        (start-index (round (* (/ (sample-rate target) 1000.0) time)))
        (diff (duration (last-elem self)))
        markers
        (pos-in-sound '()))
    ;;;Merge contiguous time-bounds
    (loop for d in (nthcdr 1 dates)
          do
          (if (= (cadr (last-elem packs)) (car d))
              (setf (cadr (last-elem packs)) (cadr d))
            (nconc packs (list d))))

     ;;;Build markers
    (loop for offset in durs
          for i from 0
          for times in dates
          do
          (push (list offset (car times)) pos-in-sound) ;;;prepare points for BPF
          (let ((ind (+ index i))
                (ti (car times)))
            (push (list offset #'(lambda () 
                                   (setf (perf-time handler) ind)
                                   (box-player-callback (box target) ti)
                                   (osc-send (list (format nil "~A" ti)) nil 3000)
                                   (when (pos-display target) 
                                     (setf (current-time (pos-display target)) ind)
                                     (box-player-callback (box (pos-display target)) (current-time (pos-display target))))
                                   )) markers)))

    ;;;Extend markers
    (loop for n from 1 to 30
          for off from (+ (caar markers) diff) by diff
          for i from (+ index (length self))
          do
          (let ((ind (+ index i)))
            (push (list off #'(lambda () (setf (perf-time handler) ind))) markers)))
    ;;;Tile markers
    (tile-markers target (reverse markers))
    ;;;Tile BPF display
    (push `(90000 ,(get-obj-dur (source target))) pos-in-sound)
    (tile-display target (reverse pos-in-sound))
    ;;;Write packs in rederer buffer (tile audio)
    (loop for pack in packs 
          do
          (om-ignore&print-error
           (setq start-index
                 (write-to-audio-renderer target
                                          :time-bounds pack
                                          :start-index start-index
                                          :crossfade-ms (crossfade target)))))
    (print "Chunk to buffer : terminated")))

;**************
;Tiling methods
(defmethod tile-markers ((object audio-improvisation-renderer) (markers list))
  (let ((new-markers (copy-list (time-markers object))))
    (if markers
        (if new-markers
            (let ((p (position (caar markers) new-markers :test '<= :key 'car)))
              (if p
                  (if (= p 0)
                      (setq new-markers markers)
                    (setf (nthcdr p new-markers) markers))
                (nconc new-markers markers)))
          (setq new-markers markers)))
    (with-schedulable-object object
                             (setf (time-markers object) new-markers))))

(defmethod tile-display ((object audio-improvisation-renderer) (points list))
  (let* (;(max (/ 200.0 (expt 10 (decimals (pos-display object)))))
         ;(xcoeff (/ max 90000))
         ;(ycoeff (/ max (get-obj-dur (source object))))
         (start-date (caar points))
         (oldp (loop for x in (point-list (pos-display object))
                     while (< (om-point-x x) start-date)
                     when (>= (om-point-x x) 0)
                     collect x))
         (newp (mapcar #'(lambda (xy) (omp (car xy) (cadr xy))) (sort points '< :key 'car)))
         (newpl (append oldp newp)))
    (setf (point-list (pos-display object)) '())
    (setf (point-list (pos-display object)) newpl)
    (if (box (pos-display object))
        (set-cache-display (box (pos-display object)) (pos-display object)))))

;************************************
;Write a chunk to the renderer buffer
(defmethod write-to-audio-renderer ((object audio-improvisation-renderer) &key (start-index 0) (time-bounds '(0 0)) (crossfade-ms 0))
  (let* ((source-buffer (oa::om-pointer-ptr (buffer (source object))))
         (source-size (n-samples (source object)))
         (nch (n-channels object))
         (sr (/ (sample-rate object) 1000.0))
         (impro-buffer (oa::om-pointer-ptr (buffer object)))
         (impro-size (n-samples object))
         (added-cross-samples (round (* crossfade-ms sr) 2))
         (cross-samples (* added-cross-samples 2))
         (p1-source (max 0 (round (* sr (car time-bounds)))))
         (p2-source (min source-size (round (* sr (cadr time-bounds)))))
         (p1b-source (max (- p1-source added-cross-samples) 0))
         (p2b-source (min (+ p2-source added-cross-samples) source-size))
         (outcross1 (max (- start-index added-cross-samples) 0))
         (outcross2 (min (+ start-index added-cross-samples) impro-size)))
    ;;;Erase previsouly written audio samples after crossfade
    (loop for i from outcross2 to impro-size
          do
          (dotimes (n nch)
            (setf (fli:dereference (fli:dereference impro-buffer :index n :type :pointer) 
                                   :index i :type :float)
                  0.0)))
    ;;;Fade out previous data
    (loop for i from outcross1 to outcross2
          for j from 0
          do
          (dotimes (n nch)
            (setf (fli:dereference (fli:dereference impro-buffer :index n :type :pointer) 
                                   :index i :type :float)
                  (* (cosinus-crossfade j cross-samples)
                     (fli:dereference (fli:dereference impro-buffer :index n :type :pointer) 
                                      :index i :type :float)))))
    (loop for i from p1b-source to p2b-source 
          for j from outcross1
          do
          (dotimes (n nch)
            (incf (fli:dereference (fli:dereference impro-buffer :index n :type :pointer) 
                                   :index j :type :float)
                  (* (if (< j outcross2) 
                         (sinus-crossfade (- j outcross1) cross-samples)
                       1.0)
                     (fli:dereference (fli:dereference source-buffer :index n :type :pointer) 
                                      :index i :type :float)))))
    (+ start-index (- p2-source p1-source))))

;***************
;Crossfade tools
(defun sinus-crossfade (x xmax)
  (coerce (abs (sin (* x (/ (/ pi 2) xmax)))) 'single-float))

(defun cosinus-crossfade (x xmax)
  (coerce (abs (cos (* x (/ (/ pi 2) xmax)))) 'single-float))




;;;====================================================================================
;;;===================== BPF DISPLAYER ================================================
;;;====================================================================================

;;;=====
;;;CLASS 
(defclass bpf-display (bpf)
  ((box :initform nil :accessor box)
   (current-time :initform 0 :accessor current-time)))

;;;=============
;;;REDEFINITIONS
;************************
;Initialization displayer
(defmethod initialize-instance :after ((self bpf-display) &rest args)
  (setf (decimals self) 3)
  (set-bpf-points self
                  :x '()
                  :y '())
  self)

;*************************
;Initialization BPF (hack)
(defmethod initialize-instance :after ((self bpf) &rest args)
  ;(setf (decimals self) 0)
  (set-bpf-points self
                  :x (or (find-value-in-arg-list args :x-points) '(0 200))
                  :y (or (find-value-in-arg-list args :y-points) '(0 200)))
  self)

;********
;Graphics
(defmethod draw-mini-view ((self bpf-display) (box t) x y w h &optional time)
  (setf (box self) box)
  (om-with-font (om-make-font "Helvetica" 24)
                    (om-draw-string 20 40
                                    "Audio Renderer: generated anticipation"))
  (let ((display-cache (get-display-draw box)))
    (draw-bpf-display-points-in-rect (cadr display-cache)
                                     (color self) 
                                     (car display-cache)
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                                     x (+ y 10) w (- h 20)
                                     )
    t)
  (let ((x (* (or (nth (+ 0 (current-time self)) (x-points self)) 0) (/ w (get-obj-dur self)))))
    (om-draw-line x 4 x (- h 4))))

(defun draw-bpf-display-points-in-rect (points color ranges x y w h)
  (multiple-value-bind (fx ox) 
      (conversion-factor-and-offset (car ranges) (cadr ranges) w x)
    (multiple-value-bind (fy oy) 
        ;;; Y ranges are reversed !! 
        (conversion-factor-and-offset (cadddr ranges) (caddr ranges) h y)
      (when points 
        (om-with-fg-color (om-def-color :gray)
        ;draw first point
        (om-draw-circle (+ ox (* fx (car (car points))))
                        (+ oy (* fy (cadr (car points))))
                        3 :fill t)
        (loop for pt in points
              do
              (om-draw-circle (+ ox (* fx (car pt)))
                              (+ oy (* fy (cadr pt)))
                              3 :fill t)))))))

;**************
;BPF range hack
;(defmethod set-ruler-range ((self ruler-view) v1 v2)
;  (let* ((v11 (min v1 v2))
;         (v22 (max v1 v2)))
;    (when (>= (- v22 v11) 10)
;      (setf (v1 self) 0)
;      (setf (v2 self) 200)))
;  (set-shift-and-factor self)
 ; (om-with-delayed-redraw
 ;     (om-invalidate-view self)
 ;   (update-views-from-ruler self)))

;*************************
;Initialization BPF (hack)
(defmethod draw-mini-view ((self bpf) (box t) x y w h &optional time)
  ;(om-with-font (om-make-font "Helvetica" 24)
  ;                  (om-draw-string 20 40
  ;                                  "Scenario: spectral centroid profile"))
  (let ((display-cache (get-display-draw box)))
    (draw-bpf-points-in-rect (cadr display-cache)
                             (color self) 
                             (car display-cache)
                             ;(+ x 7) (+ y 10) (- w 14) (- h 20)
                             x (+ y 10) w (- h 20)
                             )
    t))




;;;====================================================================================
;;;===================== COMMENT BOX   ================================================
;;;====================================================================================
(defclass comment-box (omobject)
  ((text :initform nil :initarg :test :accessor text)))

(defmethod display-modes-for-object ((self comment-box))
  '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self comment-box) (box t) x y w h &optional time)
  (when (text self)
    (om-with-font (om-make-font "Helvetica" 24)
                  (om-draw-string 10 30 (text self)))))



