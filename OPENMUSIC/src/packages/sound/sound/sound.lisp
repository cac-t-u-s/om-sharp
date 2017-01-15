
;;;=================
;;; SOUND OBJECT
;;;=================

(in-package :om)

;;==============================
;; SOUND BUFFER
;;==============================

;;; use as :ptr for OM-SOUND-BUFFER
(defun make-audio-buffer (nch size)
  (fli::allocate-foreign-object :nelems nch
                                :type :pointer 
                                :initial-contents (loop for c from 0 to (1- nch) 
                                                        collect (fli::allocate-foreign-object :type :float :nelems size
                                                                 :initial-element 0.0))))

(defparameter *default-internal-sample-size* :float)

(defstruct (om-sound-buffer (:include oa::om-pointer))
  (nch 1 :type integer))

(defun make-om-sound-buffer-GC (&key ptr (count 1) (nch 1))
  (om-create-with-gc (make-om-sound-buffer :ptr ptr :count count :nch nch)))
    
(defmethod omng-save ((self om-sound-buffer)) nil)

;;; not useful if cleanup buffer works
(defmethod oa::om-release ((ptr om-sound-buffer))
  ;(om-print (format nil "Release audio buffer ~A in ~A" (oa::om-pointer-ptr ptr) ptr) "SOUND_DEBUG")
  (when (<= (decf (oa::om-pointer-count ptr)) 0)
    ;(om-print (format nil "CAN FREE Audio buffer ~A in ~A !" (oa::om-pointer-ptr ptr) ptr) "SOUND_DEBUG")
    ;(unless (om-null-pointer-p (oa::om-pointer-ptr ptr)) 
    ;  (audio-io::om-free-sound-buffer (oa::om-pointer-ptr ptr) (om-sound-buffer-nch ptr)))
    ))

(defmethod om-cleanup ((self om-sound-buffer))
  ;(om-print (list "AUDIO BUFFER CLEANUP" self) "SOUND_DEBUG")
  (when (and (oa::om-pointer-ptr self) (not (om-null-pointer-p (oa::om-pointer-ptr self))))
    (audio-io::om-free-sound-buffer (oa::om-pointer-ptr self) (om-sound-buffer-nch self))))
  
;;==============================
;; SOUND
;;==============================

;; schedulable-object
(defclass om-internal-sound (om-cleanup-mixin schedulable-object timed-object)
  ((buffer :accessor buffer :initform nil :initarg :buffer :documentation "the buffer of audio samples")
   (n-samples :accessor n-samples :initform nil :initarg :n-samples :type integer)
   (n-channels :accessor n-channels :initform nil :initarg :n-channels :type integer)
   (sample-rate :accessor sample-rate :initform nil :initarg :sample-rate :type integer)
   (smpl-type :accessor smpl-type :initform :float :initarg :smpl-type)
   (sample-size :accessor sample-size :initform 16 :initarg :sample-size :type integer)
   (buffer-player :accessor buffer-player :initform nil :documentation "pointer to a buffer player")))

;; not needed ?
(defmethod om-cleanup ((self om-internal-sound))
  ;(print (list "SOUND CLEANUP" self (buffer self)))
  (when (buffer self) (oa::om-release (buffer self))))


;;; CLONE A SOUND = KEEP THE SAME POINTER AND INCREMENT THE COUNT
(defmethod clone-object ((self om-internal-sound) &optional clone)
  (let ((snd (call-next-method)))
    (setf (buffer snd) (buffer self))
    (when (buffer snd) (oa::om-retain (buffer snd)))
    snd))


(defmethod release-sound-buffer ((self om-internal-sound)) 
  (when (buffer self) 
    (oa::om-release (buffer self))
    (setf (buffer self) nil)))

;;; called by the box at eval
(defmethod release-previous-value ((self om-internal-sound)) 
  (release-sound-buffer self))
 
(defclass* sound (om-internal-sound data-stream named-object)
  ((markers :accessor markers ::initform nil :documentation "a list of markers (s)")
   (file :accessor file :initform nil :documentation "a pathname")
   (gain :accessor gain :initform 1.0 :documentation "gain controller [0.0-1.0]")
   (access-from-file :accessor access-from-file :initform nil :documentation "read from file or allocate buffer"))
  (:icon 'sound)
  (:documentation "Sound object.

A sound can be initialized/attached to a pathname (<file>) corresponding to an audio file on the disk.
If it is unlocked and unconnected, evaluating the box will open a file chooser dialog and allow the selection of a sound file to load.

The other inputs/outputs :
- <gain> = a gain applied to the audio buffer for playback. If a list is supplied, it will be considered as a set of gains to apply to the different channels of the sound.
- <markers> = a list of markers (time in seconds). The markers can also be added/moved/removed from the sound editor.
- <access-from-file> = if this parameter is non-NIL, then this sound will work without an internal audio buffer,that is, referring to a file on disk.
Press 'space' to play/stop the sound file.
"))

;(references-to (find-class 'sound))
;(setf (references-to (find-class 'sound)) nil)

(defmethod additional-class-attributes ((self sound)) '(markers file sample-rate n-channels n-samples gain access-from-file))

(defmethod play-obj? ((self sound)) t)

(defmethod get-obj-dur ((self sound))
  (if (and (sample-rate self) (n-samples self)) (* 1000.0 (/ (n-samples self) (sample-rate self))) 0))

(defmethod object-default-edition-params ((self sound)) '((:player :libaudiostream)))

(defmethod box-def-self-in ((self (eql 'sound))) :choose-file)

;;;===========================
;;; UTILS
;;;===========================

;;; GET A SOUND OBJECT FROM ...

(defmethod get-sound ((self sound)) self)
(defmethod get-sound ((self om-internal-sound)) (om-init-instance (clone-object self (make-instance 'sound)) nil))
(defmethod get-sound ((self pathname)) (when (probe-file self) (om-init-instance (make-instance 'sound) `((:file ,self) (:access-from-file t)))))
(defmethod get-sound ((self string)) (get-sound (pathname self)))
(defmethod get-sound ((self t)) nil)

(defmethod get-sound-name ((self pathname)) (pathname-name self))
(defmethod get-sound-name ((self string)) self)
(defmethod get-sound-name ((self sound)) (if (file self) (pathname-name (file self)) "sound"))
(defmethod get-sound-name ((self t)) "invalid sound")

(defmethod get-sound-file ((self pathname)) self)
(defmethod get-sound-file ((self string)) (pathname self))
(defmethod get-sound-file ((self sound)) (get-sound-file (file self)))
(defmethod get-sound-file ((self t)) nil)


;;;===========================
;;; TIME MARKERS
;;;===========================

(defclass marker-frame (data-frame) ())

(defmethod print-object ((self marker-frame) out)
  (format out "#<marker-at ~D>" (date self)))

(defmethod data-frame-text-description ((self marker-frame))
  (list (format nil "t=~A" (date self))))

(defmethod get-frame-action ((self marker-frame)) nil)

(defmethod frame-graphic-duration ((self marker-frame)) 0)
  
(defmethod compute-frame-color ((self marker-frame) editor) 
  (declare (ignore editor))
  (om-def-color :dark-blue))

(defmethod compute-frame-posy ((self marker-frame) editor) 100)
(defmethod compute-frame-sizey ((self marker-frame) editor) 200)


(defmethod get-time-markers ((self sound)) (markers self))

;;; for the user markers are just numbers
;;; (in fact they are data-frames)
(defmethod markers ((self sound)) (mapcar 'date (data-stream-get-frames self)))
(defmethod (setf markers) (markers (self sound))
  (data-stream-set-frames 
   self
   (mapcar #'(lambda (m) (make-instance 'marker-frame :date m)) markers)))

;;;===========================
;;; INITIALIZATION...
;;;===========================

#|
;; NOT USED FOR THE MOMENT
(defmacro om-sound-protect (sound &body body)
  `(if (equal (loaded ,sound) :error)
       (progn 
         (print (format nil "sound ~s is disabled because of previous errors" (namestring (filename ,sound))))
         nil)
     (or (ignore-errors ,@body)
         (progn
           (print (format nil "error in sound ~s" (namestring (filename ,sound))))
           (setf (loaded ,sound) :error)
           nil))))
|#

;; DOES THIS RETAIN THE BUFFER FROM BEING GARBAGE-COOLECTED ???
;(defun make-player-from-buffer (buffer size channels sample-rate) nil)

(defmethod clone-object ((self sound) &optional clone)
  (let ((snd (call-next-method)))
    (when (buffer snd)
      ;; instanciate a player for this sound
      (setf (buffer-player snd) (make-player-from-buffer 
                                 (oa::om-pointer-ptr (buffer snd)) 
                                 (n-samples snd) (n-channels snd) (sample-rate snd))))
    snd))


(defmethod objFromObjs ((model om-internal-sound) (target sound))
  (clone-object model target))

(defmethod objFromObjs ((model sound) (target sound))
  (clone-object model target))

(defmethod objFromObjs ((model t) (target sound))
  (when (get-sound-file model)
    (objFromObjs (get-sound-file model) target)))

(defmethod objFromObjs ((model pathname) (target sound))
  (setf (file target) model)
  target)

(defmethod objFromObjs ((model (eql :choose-file)) (target sound))
  (objFromObjs (om-choose-file-dialog :prompt "Choose an audio file AIFF/WAV..."
                                      :types '("AIFF files" "*.aiff;*.aif" "WAV files" "*.wav;*.wave"))
               target))

;;;=================================
;;; SOUND INIT RULES:
;;;=================================

;;; IF <SELF> + NO <FILE>
;;; - access-from-file = T => SET FILE and NO BUFFER
;;; - access-from-file = NIL => SET BUFFER and FILE = NIL
;;; IF <SELF> + <FILE> (same)
;;; - access-from-file = T => NO BUFFER, JUST SET SOUND INFO
;;; - access-from-file = NIL => SET BUFFER
;;; IF <SELF> + <FILE> (different)
;;; - access-from-file = T => SAVE INCOMING DATA TO FILE, NO BUFFER
;;; - access-from-file = NIL => SAVE INCOMING DATA TO FILE, KEEP BUFFER
;;; IF NO <SELF> AND <FILE> ONLY
;;; - access-from-file = T => NO BUFFER - JUST SET SOUND INFO
;;; - access-from-file = NIL => SET BUFFER


(defmethod om-init-instance ((self sound) &optional args)
  (call-next-method)
  ;;; these are given to om-init-instance
  ;;; the sound may OR NOT already contain FILE and BUFFER
  (let ((FILE-IN (find-value-in-kv-list args :file))
        (access-from-file (find-value-in-kv-list args :access-from-file)))
    (unless (or (null FILE-IN) (valid-pathname-p FILE-IN))
      (om-beep-msg "Wrong path as sound input: ~D" FILE-IN) 
      (setf FILE-IN nil))
    
    (cond ((and FILE-IN (file self) (string-equal (namestring (file self)) (namestring FILE-IN)))
           ;;; FILE-IN maches the current file
           (if access-from-file 
               ;; We want to keep working with this file (and no buffer)
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self FILE-IN))
             ;;; else we set the buffer (if needed)
             (unless (buffer self)
               (set-sound-data self FILE-IN))))
         
          ((and FILE-IN (file self))
           ;;; there was already a file but different 
           (om-copy-file (file self) FILE-IN)
           (setf (file self) FILE-IN)
           (if access-from-file
               ;; We want to keep working with this file (and no buffer)
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self FILE-IN))
             ;;; create new buffer
             (set-sound-data self FILE-IN)))
         
          ((and FILE-IN (buffer self))
           ;;; There was no file in the sound but already a buffer
           (save-sound-data self FILE-IN)
           (setf (file self) FILE-IN)
           (if access-from-file
               (release-sound-buffer self)))
         
          (FILE-IN 
           ;;; there was nothing before...
           (if access-from-file
               (progn (setf (file self) FILE-IN)
                 (set-sound-info self FILE-IN))
             (set-sound-data self FILE-IN)))
      
          ((file self)
           ;;; No FILE-IN given but a file was in
           (if access-from-file
               (if (buffer self)
                   (release-sound-buffer self)
                 (set-sound-info self (file self)))
             (unless (buffer self) ;;; create the buffer if needed
               (set-sound-data self (file self)))))

          (t 
           ;;; there was no file and no in-file is given
           (if access-from-file
               (om-beep-msg "Cannot use ACCESS-FROM-FILE without a file !!"))
            ;;; => if there was a buffer we just keep it
            )
          )
         
    ;;; SET A PLAYER IN ANYCASE !
    (if access-from-file
        (setf (buffer-player self) (make-player-from-file (namestring (file self))))
      (when (buffer self) ;;; in principle at that point there should be a buffer..
        (if (and (n-samples self) (n-channels self) (sample-rate self))
            (setf (buffer-player self) (make-player-from-buffer 
                                        (oa::om-pointer-ptr (buffer self)) 
                                        (n-samples self) (n-channels self) (sample-rate self)))
          (om-beep-msg "Incomplete info in SOUND object. Could not instanciate the player !!")
          )))
    self))

;(defmethod initialize-instance :after ((self sound) &rest initargs)
;  (shared-initialize self t initargs))

(defun set-sound-data (sound path)
  (if (probe-file path)
  (multiple-value-bind (buffer format channels sr ss size skip)
      (audio-io::om-get-sound-buffer (namestring path) *default-internal-sample-size* nil)
    (unwind-protect 
        (progn
          (when (buffer sound) (oa::om-release (buffer sound)))
          (setf (buffer sound) (make-om-sound-buffer-GC :ptr buffer :count 1 :nch channels)
                (smpl-type sound) *default-internal-sample-size*
                (n-samples sound) size
                (n-channels sound) channels
                (sample-rate sound) sr
                (sample-size sound) ss)
          ;(om-print (format nil "Allocated buffer ~A for ~A" (buffer sound) sound) "SOUND_DEBUG")
          sound)))
    (om-beep-msg "Wrong pathname for sound: ~s" path)))

(defun set-sound-info (sound path)
  (multiple-value-bind (format channels sr ss size skip)
      (audio-io::om-get-sound-info (namestring path))
    (setf (n-samples sound) size
          (n-channels sound) channels
          (sample-rate sound) sr
          (sample-size sound) ss)
     sound))

(defun allocate-split-buffer (samples channels type)
  (let ((buffer (om-alloc-memory channels :type :pointer :clear t)))
    (dotimes (n channels)
      (setf (fli:dereference buffer :index n :type :pointer)
            (om-alloc-memory samples :type type :clear t)))
    buffer))

(defun interleave-buffer (in out samples channels)
  (dotimes (smp samples)
    (dotimes (ch channels)
      (setf (cffi::mem-aref out :float (+ (* smp channels) ch))
            (cffi::mem-aref (cffi::mem-aref in :pointer ch) :float smp))))
  out)

(defun split-buffer (in out samples channels)
  (dotimes (smp samples)
    (dotimes (ch channels)
      (setf (cffi::mem-aref (cffi::mem-aref out :pointer ch) :float smp)
            (cffi::mem-aref in :float (+ (* smp channels) ch)))))
  out)

(defun save-sound-data (sound path)
  (when (buffer sound)
    (let* ((nch (n-channels sound))
           (nsmp (n-samples sound))
           (itl-buffer (fli:allocate-foreign-object :type :float :nelems (* nsmp nch))))
      (unwind-protect 
          (progn ;;; PROTECTED
            (interleave-buffer (oa::om-pointer-ptr (buffer sound)) itl-buffer nsmp nch)
            (audio-io::om-save-buffer-in-file itl-buffer (namestring path) 
                                             nsmp nch (sample-rate sound) 
                                             24 ; *audio-res* 
                                             :aiff ; *def-snd-format*   
                                             ;;; FOR BETTER CONTROL ON FORMAT AND RESOLUTION, USE SAVE-SOUND (?)
                                             )
            (probe-file path))
        ;;; CLEANUP
        (om-free-memory itl-buffer)
        ))))

;;; executes its body with buffer-name bound to a valid audio buffer
;;; this bufer can be found in sound or produced from the filename
;;; in the second case, it is freed at the end 

(defmacro with-audio-buffer ((buffer-name sound) &body body)
  `(let* ((snd (get-sound ,sound))
          (tmp-buffer (unless (buffer snd) 
                        (when (and (valid-pathname-p (file snd)) (n-channels snd) (n-samples snd))
                          (make-om-sound-buffer-GC :count 1 :nch (n-channels snd)
                                                   :ptr (audio-io::om-get-sound-buffer (namestring (file snd)) *default-internal-sample-size*)))))
          (,buffer-name (or tmp-buffer (buffer snd))))
     (unwind-protect
         (progn ,@body)
       (when tmp-buffer (oa::om-release tmp-buffer))
       )))

;;;===========================
;;; BOX
;;;===========================

;(defclass OMBoxSound (omboxeditcall) ())
;(defmethod special-box-type ((self (eql 'sound))) 'OMBoxSound)

(defmethod display-modes-for-object ((self sound))
  '(:hidden :text :mini-view))

(defun resample-2D-array (array start-pos end-pos nbpix)
  (let* ((array-size (cadr (array-dimensions array)))
         (n-channels (car (array-dimensions array)))
         (slice-size (- end-pos start-pos))
         (maxi 0.0)
         result)
    (cond ((= nbpix slice-size)
           (setq result (make-array (list n-channels slice-size) :element-type 'single-float))
           (dotimes (i nbpix)
             (dotimes (n n-channels)
               (setf (aref result n i) (aref array n (+ start-pos i))))))
          ((< nbpix slice-size)
           (let ((step (/ slice-size nbpix 1.0)))
             (setq result (make-array (list n-channels nbpix) :element-type 'single-float))
             (dotimes (c n-channels)
               (dotimes (i nbpix)
                 (dotimes (j (floor step))
                   (setq maxi (max maxi (aref array c (round (+ start-pos j (* i step)))))))
                 (setf (aref result c i) maxi)
                 (setq maxi 0.0)))))
          (t (om-print "ERROR: ARRAY IS SMALLER THAN NBPIX") nil))
    (values result (if result (< (cadr (array-dimensions result)) nbpix) t))))



;;; CREATES An INTERNAL PICTURE FROM MAX DETECTION OVER DOWNSAMPLED BUFFER

(defvar *def-sound-color* (om-make-color 0.41 0.54 0.67))

(defun create-waveform-pict (array &optional color)
  (when array
    (let* ((pict-h 256)
           (nch (car (array-dimensions array)))
           (array-size (cadr (array-dimensions array)))
           (channels-h (round pict-h nch))
           (offset-y (round channels-h 2))
           (prevstream oa::*curstream*)
           pixpoint pixpointprev)
      
      (om-record-pict array-size 256
        
        (dotimes (i nch)  
          (om-draw-line 0 (+ (* i channels-h) offset-y) array-size (+ (* i channels-h) offset-y)))
        (om-with-fg-color (or color *def-sound-color*)
          (dotimes (c nch)
            (setq pixpointprev (* offset-y (* 0.99 (aref array c 0))))
            (loop for i from 1 to (1- array-size) do
                  (setf pixpoint (* offset-y (* 0.99 (aref array c (min i (1- array-size))))))
                  (om-draw-polygon `(,(om-make-point (1- i) (+ offset-y (* c channels-h) pixpointprev))
                                     ,(om-make-point i (+ offset-y (* c channels-h) pixpoint)) 
                                     ,(om-make-point i (+ offset-y (* c channels-h) (- pixpoint))) 
                                     ,(om-make-point (1- i) (+ offset-y (* c channels-h) (- pixpointprev))))
                                   :fill t)
                  (setq pixpointprev pixpoint))))
        )
      )))
   

;;;==========================
;;; METHODS FROM BOX API:

(defmethod get-cache-display-for-text ((object sound)) ;(call-next-method))
  (append (call-next-method) 
          (list (list :sound-buffer (buffer object)))))



;;;============================================
;;; DISPLAY ARRAY
;;;============================================

;;;Fill a sound's display array (for waveform)
;;;Inspired from OM6's display-array (to do)
(defun fill-sound-display-array (audio-ptr audio-ptr-size array-ptr array-size n-channels)
  ;(print (list audio-ptr audio-ptr-size (* audio-ptr-size n-channels) array-ptr array-size (* array-size n-channels) n-channels))
  (let* ((sample-ratio (/ audio-ptr-size array-size))
         (window (floor sample-ratio))
         (maxindx (1- (* n-channels audio-ptr-size)))
         (maxi 0.0)
         (pos-in-buffer 0))
    (if (or (fli:null-pointer-p audio-ptr) (fli:null-pointer-p array-ptr))
        (print "ERROR BUILDING DISPLAY ARRAY: NULL POINTER")
        ;(print (list window maxindx))
      (dotimes (array-frame array-size)
          ;(print (list "::array-cell" array-frame)) 
        (setq pos-in-buffer (floor (* sample-ratio array-frame)))
        (dotimes (chan n-channels)
            ;(print (list "::::ch" chan))
          (dotimes (i window)
              ;(print (list "POS IN SOUND" (+ (* pos-in-buffer n-channels) (+ chan (* n-channels i)))))
            (setq maxi (max (abs 
                               ;(fli:dereference audio-ptr :type :float :index (min maxindx (+ (* pos-in-buffer n-channels) (+ chan (* n-channels i)))))
                             (fli:dereference 
                              (fli:dereference audio-ptr :type :pointer :index chan)
                              :type :float :index (min maxindx (+ pos-in-buffer i)))
                            ) maxi)))
          (setf (fli:dereference array-ptr :index (+ array-frame (* chan array-size))) maxi)
            ;(print (list "POS IN ARRAY" (+ array-frame (* chan array-size)) maxi))
          (setq maxi 0.0))))))


(defmethod get-cache-display-for-draw ((self sound))
  (when (and (n-samples self) (> (n-samples self) 0) (n-channels self)
             (or (buffer self) (file self)))
    (let* ((window 128)
           (pictsize 512)
           (array-size (floor (n-samples self) window))
           (array (make-array (list (n-channels self) array-size) :element-type 'single-float :initial-element 0.0 :allocation :static))
           (soundbuffer nil))

      (unless (buffer self)
        (let ((ab (audio-io::om-get-sound-buffer (file self) *default-internal-sample-size*)))
          (when ab (setf soundbuffer (make-om-sound-buffer-GC :count 1 :nch (n-channels self) :ptr ab)))))
      
      (when (or soundbuffer (buffer self))
        (unwind-protect 
            (fli:with-dynamic-lisp-array-pointer 
                (ptr array :type :float)
              (fill-sound-display-array (om-sound-buffer-ptr (or soundbuffer (buffer self)))
                                        (n-samples self) ptr array-size (n-channels self)))
          (when soundbuffer (oa::om-release soundbuffer)))
        
        (list array (create-waveform-pict (resample-2D-array array 0 array-size (min array-size pictsize))))
        ))))
            


(defmethod draw-mini-view ((self sound) (box t) x y w h &optional time) 
  (let ((pict (cadr (get-display-draw box))))
    (when pict
      (om-draw-picture pict :x x :y (+ y 4) :w w :h (- h 8)))
    (when (markers self)
      (let ((fact (/ w (get-obj-dur self))))
        (loop for mrk in (markers self) do
              (om-with-fg-color (om-def-color :dark-gray)
                (om-draw-dashed-line (+ x (* mrk fact)) 4 (+ x (* mrk fact)) (- h 4))))))))

;;;====================
;;; PLAYER
;;;====================
;;;Methods redefinitions using the slot "data" of schedulable object to bypass actions rendering and store the las pointer
(defmethod get-action-list-for-play ((self sound) time-interval &optional parent)
  (get-external-control-action-list self time-interval parent))

(defmethod player-play-object ((self scheduler) (object sound) caller &key parent interval)
  ;(juce::setgainreader (bp-pointer (buffer-player object)) 0.1)
  (when (buffer-player object)
    (start-buffer-player (buffer-player object) :start-frame (if (car interval)
                                                                 (round (* (car interval) (/ (sample-rate object) 1000.0)))
                                                               (or (car interval) 0)))
  (call-next-method)))

(defmethod player-stop-object ((self scheduler) (object sound))
  (if (buffer-player object) (stop-buffer-player (buffer-player object)))
  (call-next-method))

(defmethod player-pause-object ((self scheduler) (object sound))
  (pause-buffer-player (buffer-player object))
  (call-next-method))

(defmethod player-continue-object ((self scheduler) (object sound))
  (continue-buffer-player (buffer-player object))
  (call-next-method))

(defmethod set-object-time ((self sound) time)
  (jump-to-time (buffer-player self) time)
  (call-next-method))


;;;====================
;;; EDITOR
;;;====================

(defclass sound-editor (stream-editor) ())
(defclass sound-panel (stream-panel) ())
(defmethod get-editor-class ((self sound)) 'sound-editor)
(defmethod editor-view-class ((self sound-editor)) 'sound-panel)

(defmethod frame-display-modes-for-object ((self sound-editor) (object sound))
  '((:lines "lines")))

(defmethod om-draw-contents ((self sound-panel))
  (call-next-method)  
  (let* ((editor (editor self))
         (sound (object-value editor)))
    (if (or (buffer sound)
            (and (access-from-file sound) (valid-pathname-p (file sound))))
        (let ((pict (cadr (ensure-cache-display-draw (object editor) (object-value editor))))
              (view-at (/ (x1 self) (get-obj-dur sound)))
              (view-dur (/ (- (x2 self) (x1 self)) (get-obj-dur sound))))
          (when pict (om-draw-picture pict :w (w self) :h (h self) 
                                      :src-x (* (om-pict-width pict) view-at) :src-w (* (om-pict-width pict) view-dur))))
      (om-with-fg-color (om-def-color :light-gray)
        (om-with-font (om-make-font "Arial" (round (h self) 4) :style '(:bold))
                      (om-draw-string 10 (+ (round (h self) 2) (round (h self) 8)) "No sound loaded..")
                      ))
      )
    ))



      
         

#|

(defmethod om-draw-waveform ((self soundPanel))
  (multiple-value-bind (data smplevel)
      (om-get-display-slice self)
    (when (and (> (car (array-dimensions data)) 0) (> (cadr (array-dimensions data)) 0))
      (let* ((thesound (object (om-view-container self)))
           (window-v-size (* 0.99 (om-point-v (om-view-size self))))
           (system-etat (get-system-etat self))
           (xmin (car (rangex self)))
           (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
           (xmax (cadr (rangex self)))
           (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
           (xtime (- xmax xmin))
           (nbpts (cadr (array-dimensions data)))
           (timestep (/ xtime (coerce nbpts 'single-float)))
           (nch (om-sound-n-channels thesound))
           (channels-h (round window-v-size nch))
           (offset-y (round channels-h 2))
           pixpoint
           pixtime
           pixprev pixtprev)
      (when data
        (om-with-fg-color nil *om-steel-blue-color*
          ;(when smplevel    ;;;Use this "when" only when HQ display is used below
          (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 0.5) ;;;=*om-steel-blue-color* with 50% transparency
            (dotimes (i nch)  
              (om-draw-line pixmin (- (+ (* i channels-h) offset-y) 10) pixmax (- (+ (* i channels-h) offset-y) 10))));)
          (dotimes (c nch)
            (setq pixprev (round (* offset-y (* 0.99 (aref data c 0)))))
            (setq pixtprev (om-point-h (point2pixel self (om-make-point (round xmin) 0) system-etat)))
            (loop for i from 1 to (1- (cadr (array-dimensions data))) do
                  (let ((val (aref data c i)))
                    (setf pixtime (om-point-h (point2pixel self (om-make-point (+ xmin (* i timestep)) 0) system-etat)))
                    (setf pixpoint (* offset-y (* 0.99 val))) ; scaled 0-1 --> 0 -->256/2
                    (if (not smplevel) ;(= nbpts nbpix)
                        (progn
                          ;;;Use this for HQ display : requires a bit more computation to display middle lines with dynamic intensity
                          ;(when (not smplevel)
                          ;  (let ((colorval (- 1 (log (1+ (* (1- (exp 1)) (expt (abs val) 0.1)))))));(- 1 (expt (abs val) 0.2))));(expt (abs val) 0.2)))
                          ;    (om-with-fg-color nil (om-make-color-alpha 0.41 0.54 0.67 colorval)
                          ;      (om-draw-line pixtprev (- (+ (* c channels-h) offset-y) 10) pixtime (- (+ (* c channels-h) offset-y) 10)))))
                          (om-fill-polygon `(,(om-make-point pixtprev (- (+ offset-y (* c channels-h) pixprev) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) pixpoint) 9))
                                             ,(om-make-point pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 9))
                                             ,(om-make-point pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 9)))))
                      (om-draw-line pixtprev (- (+ offset-y (* c channels-h) (- pixprev)) 10) pixtime (- (+ offset-y (* c channels-h) (- pixpoint)) 10)))
                    (setq pixprev pixpoint pixtprev pixtime))))))))))



(defmethod om-get-display-slice ((self soundpanel))
  (let* ((snd (object (om-view-container self)))
         (system-etat (get-system-etat self))
         (xmin (car (rangex self)))
         (pixmin (om-point-h (point2pixel self (om-make-point xmin 0) system-etat)))
         (xmax (cadr (rangex self)))
         (pixmax (om-point-h (point2pixel self (om-make-point xmax 0) system-etat)))
         (nbpix (- pixmax pixmin)))
    (sound-get-display-array-slice snd nbpix xmin xmax)))

|#


;;;===========================
;;; OM METHODS 
;;;===========================


(defmethod* sound-points ((self sound) (num integer) &optional channel)
  :initvals '(nil 1000 1)
  :indoc '("a sound object" "number of points" "channel number")
  :doc "Reurns <num> sampled points from the audio waveform of channel <channel> in <self>."
  :icon 221
  (with-audio-buffer (b self)
    (let ((numdat (n-samples self))
          (numchan (n-channels self))
          (ch (or channel 1)))
      (if (or (> ch numchan) (> num numdat)) 
          (om-beep-msg "SOUND-POINTS: out-of-range input values !!")
        (let ((channel-ptr (om-read-ptr (om-sound-buffer-ptr b) (1- ch) :pointer)))
          (loop for i from 0 to numdat by (round numdat num) collect
                (om-read-ptr channel-ptr i :float)))
        ))))


(defmethod* sound-dur ((sound sound))
  :icon 221
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in seconds."
   (if (and (n-samples sound) (sample-rate sound)
            (> (sample-rate sound) 0))
       (float (/ (n-samples sound) (sample-rate sound)))
     0))

(defmethod* sound-dur ((sound pathname))
  (sound-dur (namestring sound)))

(defmethod* sound-dur ((sound string))
  (if (probe-file sound)
      (multiple-value-bind (format channels sr ss size skip)
          (audio-io::om-get-sound-info sound)
        (if (and size sr (> sr 0)) (float (/ size sr)) 0))
    (progn (om-beep-msg "File not found: ~s" sound) 0)))

(defmethod* sound-dur-ms ((sound t))
  :initvals '(nil)
  :indoc '("a sound object or file pathname")
  :doc "Returns the duration of <sound> in milliseconds."
  :icon 221
  (round (* 1000 (sound-dur sound))))

