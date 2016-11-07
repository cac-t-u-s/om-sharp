(in-package :om)

(defvar *iae-sr* 44100)

(defclass* iae (om-cleanup-mixin data-stream)
 ((iaeengine-ptr :accessor iaeengine-ptr :initform nil)
  (sounds :initarg :sounds :accessor sounds :initform nil)
  (channels :initarg :channels :accessor channels :initform 1)
  (max-dur :initarg :max-dur :accessor max-dur :initform 10000)
  (grains :initarg :grains :accessor grains :initform nil)
  (descriptors :accessor descriptors :initform nil)
  (desc-tracks :accessor desc-tracks :initform nil)
  ;;; needed for play
  (buffer-player :accessor buffer-player :initform nil)
  )
 (:default-initargs :default-frame-type 'IAE-grain))

(defmethod data-stream-frames-slot ((self iae)) 'grains)

(defmethod play-obj? ((self iae)) t)
(defmethod get-obj-dur ((self iae)) (max-dur self))

(defmethod om-cleanup ((self iae))
  (when (iaeengine-ptr self)
    (om-print (format nil "deleting engine of ~A [~A]" self (iaeengine-ptr self)) "GC")
    (iae::iae_delete (iaeengine-ptr self))
    (when (buffer-player self) (free-buffer-player (buffer-player self)))
  ))

;been modified to 100 to 0 for test...
(defmethod initialize-instance :after ((self iae) &rest initargs)
  (om-print (format nil "new engine for ~A" self) "OM")
  (setf (iaeengine-ptr self) (iae::iae_new *iae-sr* 512 (channels self) 1))
  (set-object-time-window self 0)
  (let* ((size (round (* (max-dur self) *iae-sr*) 1000))
         (audio-buffer (fli::allocate-foreign-object 
                        :type :pointer :nelems (channels self)
                        :initial-contents (loop for c from 1 to (channels self)
                                                collect
                                                (fli::allocate-foreign-object :type :float :nelems size :initial-element 0.0)))))
    
    (setf (buffer-player self) 
          (make-player-from-buffer audio-buffer size (channels self) *iae-sr*))))


(defmethod om-init-instance ((self iae) &optional args)
  (let ((temp-single-sound (if (listp (sounds self)) (car (sounds self)) (sounds self))))
    (setf (sounds self) (list! (sounds self)))
    ;;; can be called several times for the different sources
    (loop for s in (sounds self) do
          (iae::iae_read (iaeengine-ptr self) (namestring (get-sound-file s)) (cffi-sys::null-pointer)))
    ;(iae::iae_set_MarkerTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
    ;(iae::iae_set_DescriptorTrackSdif (iaeengine-ptr self) -1 "XCUD" "XCUD")
    ;;;
    (init-pipo self "basic")     ;;  options: "basic" "ircamdescriptor" "slice:fft"  "...:chop"
    (print (iae::iae_info_get_string (iaeengine-ptr self) (cffi-sys::null-pointer)))
    ;;;
    (iae::iae_update_kdtree (iaeengine-ptr self) T)
    (print "KDTree updated!")
    (iae::iae_set_SynthMode (iaeengine-ptr self) 1)
    self))

(defun init-pipo (iae module)
  (let ((*iae (iaeengine-ptr iae)))
    (if (= 1 (iae::iae_pipo_create *iae module))   
        (let ()
          (set-pipo-module iae)
          
          (setf (desc-tracks iae) 
                (loop for i from 0 to (1- (length (sounds iae))) 
                      collect (iae::iae_pipo_run *iae i)))
          
          (let ((num (iae::iae_get_numdescriptors *iae)))
            (print "=================================")
            (setf (descriptors iae)
                  (loop for i from 0 to (1- num) 
                        ; do (print i)
                        collect (print (iae::iae_get_descriptorname *iae i))))))
 
      (print "Error initializing PiPo"))))


(defparameter *ircamdescriptors*
  '("TotalEnergy"
    "FundamentalFrequency"
    "SpectralCentroid"
    "Loudness"
    "Sharpness"
    "Spread"
    "HarmonicEnergy"
    "Inharmonicity"
    "Noisiness"))
    

(defun set-pipo-module (iae)
  (let* ((*iae (iaeengine-ptr iae))
         (nparams (iae::iae_pipo_param_num *iae)))
    (loop for param-i from 0 to (1- nparams) do
          (let ((name (iae::iae_pipo_param_get_name *iae param-i)))
            (print (string+ "--> " (iae::iae_pipo_param_get_description *iae param-i)))
            (when (string-equal name "ircamdescriptor.descriptors")
            ;(print (iae::iae_pipo_param_get_type *iae param-i))
            ;(let ((numdesc (iae::iae_pipo_param_enum_get_num *iae name)))
            ;   (loop for d from 0 to (1- numdesc) do
            ;      (print  (iae::iae_pipo_param_enum_get_element *iae name d))))
              (loop for desc in *ircamdescriptors* 
                    for d = 0 then (+ d 1) do
                    (iae::iae_pipo_param_set_string *iae name d desc)
                    )
              )
            ))
    ))
		


(defun get-sound-descriptors (iae src-index)
  (let* ((*iae (iaeengine-ptr iae))
         (numdesc (length (descriptors iae)))
         (framedescbuffer (fli::allocate-foreign-object :type :float :nelems numdesc))
         (size (iae::iae_get_track_size *iae src-index (nth src-index (desc-tracks iae)))))
    (unwind-protect 
        (loop for i from 0 to (1- size) collect
              (let ((time (iae::iae_get_descriptor_data *iae src-index i framedescbuffer)))
                (cons time
                      (loop for x from 0 to (1- numdesc) collect 
                            (fli:dereference framedescbuffer :index x :type :float)))))
      (fli:free-foreign-object framedescbuffer))
    ))
        
 
(defun iae-info (iae)
  (iae::iae_info_get_string (iaeengine-ptr iae) (oa::om-make-null-pointer)))

(defun iae-play-grain (snd)
  (player-play-object *general-player* snd nil))

(defmethod iae-synth ((self iae) source pos dur)
  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* dur *iae-sr* 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (make-audio-buffer (channels self) nsamples)))

;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae::iae_set_SynthMode *iae 0)

      (when (< source (length (sounds self)))
        (iae::iae_set_sourceindex *iae source))
      (iae::iae_set_Cyclic *iae nil)
      (iae::iae_set_CenteredGrains *iae nil)
      (iae::iae_set_Attack *iae 0.0d0 0.5d0)
      (iae::iae_set_Attack *iae 0.0d0 0.5d0)
      (iae::iae_set_position *iae (coerce pos 'double-float) 0.0d0)
      ;(iae::iae_set_positionvar *iae 1000.0d0)
      (iae::iae_set_period *iae -0.0d0 0.0d0)
      (iae::iae_set_duration *iae (coerce dur 'double-float) 0.0d0)
      (iae::iae_trigger *iae)
      (iae::iae_synth *iae nsamples **samples (channels self))
      (setf (om::buffer omsnd) (om::make-om-sound-buffer :ptr **samples :count 1 :nch (channels self)))
      omsnd)))

(defmethod iae-synth-desc ((self iae) descriptor value dur)
  (when (iaeengine-ptr self)
    (let* ((*iae (iaeengine-ptr self))
           (nsamples (ceiling (* (max dur 500) *iae-sr* 0.001)))
           (omsnd (make-instance 'om::om-internal-sound :n-channels (channels self) :smpl-type :float
                                 :n-samples nsamples :sample-rate 44100))
           (**samples (make-audio-buffer (channels self) nsamples))
           (framedescbuffer (fli::allocate-foreign-object :type :float :nelems (length (descriptors self)))))
      
;   Granular = 0,    asynchronous granular synthesis
;   Segmented = 1,   concatenative synthesis (needs at least 1 marker)
;   Synchronous = 2  synchronous granular synthesis (needs at least 2 markers)
      (iae::iae_set_SynthMode *iae 1)

      (iae::iae_set_Cyclic *iae nil)
      (iae::iae_set_CenteredGrains *iae nil)
      (iae::iae_set_Attack *iae 10.0d0 0.0d0)
      (iae::iae_set_period *iae -0.0d0 0.0d0)
      ;(iae::iae_set_positionvar *iae 0.0d0)
      (iae::iae_set_duration *iae (coerce dur 'double-float) 1.0d0)
      
      (when (descriptors self)
        (let* ((n (length (descriptors self)))
               (vals (make-list n :initial-element 0.0))
               (weights (make-list n :initial-element 0.0)))
          (setf (nth descriptor vals) (float value))
          (setf (nth descriptor weights) 1.0)
          
          (iae::iae_set_target *iae n (cffi::foreign-alloc :float :initial-contents vals))
          (iae::iae_set_weight *iae n (cffi::foreign-alloc :float :initial-contents weights))
          ))
      
      (iae::iae_set_k *iae 3)
      
      (iae::iae_select_new *iae T)
      ;(let* ((selind (iae::iae_get_selectedsegmentindex *iae 0))
      ;       (selbuf (iae::iae_get_SelectedSourceIndex *iae 0))
      ;       (seltime (iae::iae_get_descriptor_data *iae selbuf selind framedescbuffer)))
      ;  (print (format nil "VALUE: ~D found in buffer ~D - index ~D - time=~D" value selbuf selind seltime))
      ;  )
      ;(iae::iae_trigger *iae)
      (iae::iae_synth *iae nsamples **samples (channels self))

      (setf (om::buffer omsnd) (om::make-om-sound-buffer :ptr **samples :count 1 :nch (channels self)))
      omsnd)))



(defun iae-add-grain (iae audiobuffer dur at)
  (when (buffer-player iae)
    (let ((pos (round (* at *iae-sr*) 1000))
          (size (round (* dur *iae-sr*) 1000)))
      (dotimes (c (channels iae))
        (dotimes (i size)
          (unless (>= i (bp-size (buffer-player iae)))
            (setf (fli:dereference (fli:dereference (bp-buffer (buffer-player iae)) :index c :type :pointer) :index (+ pos i) :type :float)
                  (+ (fli:dereference (fli:dereference (bp-buffer (buffer-player iae)) :index c :type :pointer) :index (+ pos i) :type :float)
                     (fli:dereference (fli:dereference (om-sound-buffer-ptr (om::buffer audiobuffer)) :index c :type :pointer) :index i :type :float)))
            ))))))

(defmethod iae-reset ((self iae))
  (when (buffer-player self)
    (dotimes (c (channels self))
      (dotimes (i (bp-size (buffer-player self)))
        (setf (fli:dereference (fli:dereference (bp-buffer (buffer-player self)) :index c :type :pointer) :index i :type :float) 0.0)))))


; (gc-all)


;;;============================================
;;; DATA-STREAM ACTIONS FOR IAE
;;;============================================

(defclass* IAE-grain (data-frame)
   ((date :accessor date :initarg :date :initform 0 :documentation "date/time of the grain")
    (source :accessor source :initarg :source :initform 0 :documentation "source num inside IAE")
    (pos :accessor pos :initarg :pos :initform 0 :documentation "position in source")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")))

(defclass* IAE-request (data-frame)
   ((date :accessor date :initarg :date :initform 0 :documentation "date/time of the grain")
    (descriptor :accessor descriptor :initarg :descriptor :initform 0 :documentation "the descriptor inside IAE/pipo")
    (value :accessor value :initarg :value :initform 0 :documentation "the value of the descriptor")
    (duration :accessor duration :initarg :duration :initform 100 :documentation "duration of the grain")))

(defmethod item-duration ((self iae-grain)) (duration self))

(defun make-IAE-grains (n &key (nsources 1) (maxpos 1000) (durtot 10000) (mindur 50) (maxdur 300))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-grain :date (om-random 0 durtot)
                        :source (om-random 0 (1- nsources))
                        :pos (om-random 0 maxpos)
                        :duration (om-random mindur maxdur)))
   '< :key 'date))

(defun make-IAE-requests (n &key (descriptor 0) (minval 100) (maxval 1000) (durtot 10000) (mindur 50) (maxdur 300))
  (sort 
   (loop for i from 1 to n collect
         (make-instance 'IAE-request :date (om-random 0 durtot)
                        :descriptor descriptor
                        :value (om-random minval maxval)
                        :duration (om-random mindur maxdur)))
   '< :key 'date))

(defmethod y-range-for-object ((self iae)) '(1100 0))

(defmethod set-frame-attributes ((f iae-grain) editor) 
  (setf (getf (attributes f) :color) (nth (source f) +midi-colors+)
        (getf (attributes f) :posy) (pos f)
        (getf (attributes f) :sizey) 100
        ))

(defmethod set-frame-attributes ((f iae-request) editor) 
  (setf (getf (attributes f) :color) (nth (descriptor f) +midi-colors+)
        (getf (attributes f) :posy) (value f)
        (getf (attributes f) :sizey) 100
        ))

;;;============================================
;;;Methods redefinitions using the slot "data" of schedulable object to bypass actions rendering and store the las pointer
;;;============================================

(defmethod add-random-descriptor-grain ((self IAE) descriptor)
  (let ((durtot (max-dur self))
        (mindur 50)
        (maxdur 300)
        (source (random (length (sounds self)))))
    (push (make-instance 'IAE-grain 
                               :date (om-random 0 durtot)
                               :source (om-random 0 (1- (length (sounds self))))
                               :pos (om-random 0 (sound-dur-ms (nth source (sounds self))))
                               :duration (om-random mindur maxdur)) (grains self))

   ; (push (make-instance 'IAE-request :date (om-random 0 durtot)
   ;                      :descriptor 0
   ;                      :value (om-random 100 1000)
   ;                      :duration (om-random mindur maxdur)) (grains self))
    
    (sort (grains self) '< :key 'date)))

(defmethod make-grain-from-frame ((self IAE) (frame IAE-grain))
  (iae-synth self (source frame) (pos frame) (duration frame)))

(defmethod make-grain-from-frame ((self IAE) (frame IAE-request))
  (iae-synth-desc self (descriptor frame) (value frame) (duration frame)))

(defmethod get-computation-list ((object iae) &optional interval)
  (mapcar 
   #'(lambda (frame) 
       (list (- (date frame) (time-window object))
             (date frame)
             #'(lambda ()
                 (iae-add-grain object
                                (make-grain-from-frame object frame)
                                (duration frame) (date frame)))))
   (remove-if #'(lambda (date) (or (< date (car interval)) (>= date (cadr interval))))
              (data-stream-get-frames object) 
              :key 'date)))

(defmethod get-action-list-for-play ((object iae) interval &optional parent)
  (get-external-control-action-list object interval parent))


(defmethod player-play-object ((self scheduler) (object iae) caller &key parent interval)
  (start-buffer-player (buffer-player object) 
                       :start-frame (if (car interval)
                                        (round (* (car interval) (/ *iae-sr* 1000.0)))
                                      (or (car interval) 0)))
  (call-next-method))

(defmethod player-stop-object ((self scheduler) (object iae))
  (stop-buffer-player (buffer-player object))
  (iae-reset object)
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object iae))
  (pause-buffer-player (buffer-player object))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object iae))
  (continue-buffer-player (buffer-player object))
  (call-next-method))

(defmethod set-object-time ((self iae) time)
  (jump-to-time (buffer-player self) time)
  (call-next-method))


