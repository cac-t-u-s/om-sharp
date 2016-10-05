;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE
;;;===========================================

(in-package :om)


(defmethod player-name ((self (eql :libaudiostream))) "LibAudioStream")
(defmethod player-desc ((self (eql :libaudiostream))) "internal OM audio player")

#+libaudiostream(add-player-for-object 'sound :libaudiostream)
#+libaudiostream(enable-player :libaudiostream)

(defvar *las-player* nil)
(defvar *las-player-n-channels* 2)
(defvar *las-channels-selector* (fli:allocate-foreign-object :type :long :initial-contents '(0 1)))

; (las::libversion)

; (las::OpenAudioPlayer 2 *las-player-n-channels* 44100 512 (* 65536 4) (* 44100 60 20) las::kCoreAudioRenderer 1)

(defun open-LAS-player (&optional (sr 44100))
  (progn
    (setf *las-player* (las::OpenAudioPlayer 0 *las-player-n-channels* sr 512 (* 65536 4) (* sr 60 20) 
                                             #+macosx las::kCoreAudioRenderer 
                                             #-macosx las::kPortAudioRenderer
                                             1))
    (las::StartAudioPlayer *las-player*)))


(defun close-LAS-player ()
  (las::StopAudioPlayer *las-player*)
  (las::CloseAudioPlayer *las-player*)
  (setf *las-player* nil))

; (las::SetAudioLatencies 1024 1024)
; (open-LAS-player)
; (close-LAS-player)
(om-add-init-fun 'open-las-player) 

; (las::StartAudioPlayer *las-player*)

;; Apply a same volume on all channels
(defun apply-master-volume (sound)
 (let* ((channels (las::getchannelssound sound))
        (effect (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume\",0.5,0,1,0.01));" channels) "" ""))
        (json (las::GetJsonEffect effect))
        (name (las::GetNameEffect effect)))
   (print (list channels name json))
   (las::MakeEffectSound sound effect 0 0)))

;; Apply separated volume on each channel
(defun apply-separated-volume (sound)
 (let* ((channels (las::getchannelssound sound))
        (effect (las::MakeFaustAudioEffect (format nil "process = par(i,~S,_*hslider(\"Volume%2i\",0.5,0,1,0.01));" channels) "" ""))
        (json (las::GetJsonEffect effect))
        (name (las::GetNameEffect effect)))
   (print (list channels name json))
   (las::MakeEffectSound sound effect 0 0)))


(defun print-date ()
  (cffi:with-foreign-object (renderer-info '(:struct las::rendererinfo))
    (las::get-audio-renderer-info (las::getaudioplayerrenderer *las-player*) renderer-info)
    (print (format nil "frame = ~A / usec = ~A / sec = ~A" 
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::rendererinfo) 'las::fCurFrame) :unsigned-long-long)
                   (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::rendererinfo) 'las::fCurUsec) :unsigned-long-long)
                   (/ (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::rendererinfo) 'las::fCurUsec) :unsigned-long-long) 1000000.0)))
   ))

(defun get-date ()
  (cffi:with-foreign-object (renderer-info '(:struct las::rendererinfo))
    (las::get-audio-renderer-info (las::getaudioplayerrenderer *las-player*) renderer-info) 
    (cffi::mem-aref (cffi:foreign-slot-pointer renderer-info '(:struct las::rendererinfo) 'las::fCurFrame) :unsigned-long-long)))

(defun gen-current-date ()
  (las::GenRealDate om::*las-player* (las::GetAudioPlayerDateInFrame *las-player*)))
;(las::GetAudioRendererInfo (las::getaudioplayerrenderer *las-player*))
; (get-date)

(defun get-date-from-info ()
  (nth 4 (las::getaudiorendererinfo (las::getaudioplayerrenderer *las-player*))))


; fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
(defun test-time ()
  (loop for i from 0 to 5 do 
        (print (las::getaudiorendererinfo (las::getaudioplayerrenderer *las-player*)))
        (sleep 1)))

; (test-time)

;; (las::GetLastLibError)


#|
;;; SEQUENCE DE TEST
;; charge les bindings
(las::libaudiostream-framework)

(defvar gAudioPlayer (openaudioplayer 2 2 SR 512 (* 65536 4) (* SR 60 20) kcoreaudiorenderer 1))

;(CloseAudioPlayer gAudioPlayer)
;;;kcoreaudiorenderer marche, ou kjackrenderer si serveur Jack démarré

;Démarre le player
(startaudioplayer gaudioplayer)


; (get-date)
;;; (test-time)

;(stopaudioplayer gaudioplayer)

;(las::getdevicecount (las::getaudioplayerrenderer *las-player*)) ;;;renvoie bien 1
;(GetDeviceInfo (getaudioplayerrenderer gaudioplayer) 0)

(probe-file filename1)
(probe-file filename2)

(setq s1 (makeregionsound filename1 (* SR 0) (* SR 5)))  
(setq s2 (makeregionsound filename2 (* SR 0) (* SR 20)))  


;;; Problème dans la récupération des arguments côté LAS?

(startsound gaudioplayer (makeregionsound filename1 (* SR 2) (* SR 20)) (las::genrealdate *las-player* (get-date)))
(startsound gaudioplayer (makeregionsound filename2 (* SR 2) (* SR 20)) (las::genrealdate *las-player* 0))

(startsound gaudioplayer s1 (genrealdate gaudioplayer (getdatefrominfo)))  
(startsound gaudioplayer s2 (genrealdate gaudioplayer 0))

(resetsound s1)
(resetsound s2)

(stopsound gaudioplayer s1 (genrealdate gaudioplayer 0)) 
(stopsound gaudioplayer s2 (genrealdate gaudioplayer 0)) 


(setq s3 (apply-master-volume (makeregionsound filename1 (* SR 2) (* SR 20))))
(setq s4 (apply-separated-volume (makeregionsound filename1 (* SR 2) (* SR 20))))


|#



#|
(defmethod prepare-to-play ((engine (eql :libaudiostream)) (player omplayer) object at interval params)
  (when (loaded object)
  (let* ((newinterval (om- (interval-intersec interval (list at (+ at (real-dur object)))) at))
         (from (car newinterval))
         (to (cadr newinterval))
         newptr)
    (setf (player-data object)
          (make-instance 'las-player-sound :filename (om-sound-file-name object)))
    (if (and (or (null interval) newinterval) (las-sound-sndlasptr-current (player-data object)))
        (progn
          (setf newptr (if (> (om-sound-n-channels object) 1) 
                           (las-sound-sndlasptr-current (player-data object)) 
                         (las-make-stereo-sound (las-sound-sndlasptr-current (player-data object)))))
          (if (or from to)
              (let ((begin (if from (round (* from (/ las-srate 1000.0)))))
                    (end (if to (round (* to (/ las-srate 1000.0)))))
                    (max (las-sound-n-samples-current (player-data object))))
                (if (and begin (or (< begin 0) (not begin)))
                    (setf begin 0))
                (if (and end (or (> end max) (not end)))
                    (setf end max))
                (las-sound-set-sndlasptr-to-play (player-data object) (las-slice-sample-cut newptr begin end)))
            (las-sound-set-sndlasptr-to-play (player-data object) newptr))
          (las-sound-update-las-infos (player-data object))
          (call-next-method engine player object at newinterval params))))))
|#

; (las::GetAudioPlayerDateInFrames *las-player*)


;(defmethod player-start ((engine (eql :libaudiostream)) &optional play-list)
;  ;(las::StartAudioPlayer *las-player*)
;  )

;;; we need to maintain a list of "live" sounds in order to void garbage collection
(defparameter *live-las-pointers* nil)

;;; PLAY (NOW)

(defmethod las-player-play-object ((engine (eql :libaudiostream)) (object sound) &key interval params)
  (let ((start-t (las::GetAudioPlayerDateInUsec *las-player*))
        (las-snd (if (buffer object)
                     (progn
                       ;(om-print "PLAY BUFFER" "LAS PLAYER")
                       (las::MakeBufferSound (om-sound-buffer-ptr (buffer object)) (n-samples object) (n-channels object)))
                   (progn 
                     ;(om-print "PLAY FILE" "LAS PLAYER")
                     (las::MakeReadSound (namestring (file object)))))
                 )
        (las-snd-cut nil) (reworked-snd nil))
    
    (when interval
      (setq las-snd-cut (las::MakeCutSound las-snd 
                                           (round (* (or (car interval) 0) (sr object) 0.001))
                                           (round (* (or (cadr interval) (get-obj-dur object)) (sr object) 0.001)))))

    (when (> (las::GetChannelsSound las-snd) *las-player-n-channels*)
      (om-print "MODIFY CHANNELS" "LAS PLAYER")
      (let ((selection (fli:allocate-foreign-object 
                        :type :long 
                        ;:initial-contents '(1 2)
                        ))) ; (loop for i from 0 to (1- *las-player-n-channels*) collect i))))
        (setf (fli::dereference selection :index 0 :type :long) 1)
        (setf (fli::dereference selection :index 1 :type :long) 2)
        
        (setq reworked-snd (las::MakeSelectSound (or las-snd-cut las-snd) selection *las-player-n-channels*))
        (fli::free-foreign-object selection)
        ))
    
    ;(om-print (- (las::GetAudioPlayerDateInUsec *las-player*) start-t) "MAKE SOUND TIME =")
    (push (list object (or reworked-snd las-snd-cut las-snd)) *live-las-pointers*)
    
    ;(print (list reworked-snd (and reworked-snd (las::GetChannelsSound reworked-snd)) las-snd (las::GetChannelsSound las-snd)))
    (las::StartSound 
     *las-player* 
     (or reworked-snd las-snd-cut las-snd)
     (las::GenRealDate *las-player* (las::GetAudioPlayerDateInFrame *las-player*)))))


#|
(let ((date (las::GenSymbolicDate *las-player*)))
  (las::SetSymbolicDate *las-player* date (las::GetAudioPlayerDateInFrames *las-player*))
  date)





(let ((snd1 (las::MakeReadSound "/xxxxx/xxx.aiff"))
      (snd2 nil))
  
    (when (> (las::GetChannelsSound snd1) *las-player-n-channels*)
      (let ((selection (fli:allocate-foreign-object 
                        :type :long 
                        ;:initial-contents '(0 1)
                        )))
        
        (setf (fli::dereference selection :index 0 :type :long) 1)
        (setf (fli::dereference selection :index 1 :type :long) 2)
        
        (setf snd2 (las::MakeSelectSound las-snd selection *las-player-n-channels*))
        (fli::free-foreign-object selection)
        ))
    
    (print (list  snd1 (las::GetChannelsSound snd1)
                  snd2 (and reworked-snd (las::GetChannelsSound snd2))))
    
    )



|#

; (las::MakeBufferSound (get-temp-audio-buffer object nil) (size object) (nch object))

;;; PAUSE
;(defmethod player-pause ((engine (eql :libaudiostream)) &optional play-list)
;  (if play-list
;      (loop for i from 0 to (1- (length play-list)) do
;            (player-pause-object engine (nth i play-list)))
;    (las-pause-all-players)))

;;; CONTINUE
;(defmethod player-continue ((engine (eql :libaudiostream)) &optional play-list)
;  (if play-list
;      (loop for i from 0 to (1- (length play-list)) do
;            (player-continue-object engine (nth i play-list)))
;    (las-cont-all-players)))

;;; STOP
(defmethod player-stop-object-rendering ((engine (eql :libaudiostream)) &optional play-list)
  (if play-list
      (progn 
        (loop for snd in play-list do 
              (let ((ptr (cadr (find snd *live-las-pointers* :test 'equal :key 'car))))
                (when ptr
                  (las::StopSound *las-player* ptr (las::GenRealDate *las-player* (las::GetAudioPlayerDateInFrame *las-player*))))))
        (setf *live-las-pointers* (remove-if #'(lambda (item) (find (car item) play-list)) *live-las-pointers*)))
    (progn
      (las::StopAudioPlayer *las-player*)
      (setf *live-las-pointers* nil))
    ))

;;; will LAS free the pointers ?

;(defmethod player-loop ((self (eql :libaudiostream)) player &optional play-list)
;  (declare (ignore player))
;  (if play-list
;      (loop for i from 0 to (1- (length play-list)) do
;            (let ((thesound (nth i play-list)))
;              (las-stop thesound (tracknum thesound))
;              (las-loop-play thesound (tracknum thesound))))))



;;; NOT IN OM PLAYER API

;;; PAUSE ONLY ONE OBJECT
(defmethod player-pause-object-rendering ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-pause object (tracknum object)))

;;; RESTART ONLY ONE OBJECT
(defmethod player-continue-object-rendering ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-play object (car interval) (cadr interval) (tracknum object)))

;;; STOP ONLY ONE OBJECT
(defmethod player-stop-one-object ((engine (eql :libaudiostream)) (object sound) &key interval)
  (las-stop object (tracknum object)))

;(defclass las-player (omplayer) 
;  ((sound-to-play :initform nil :initarg :sound-to-play :accessor sound-to-play))
;  ())

;;; TODO
;;; called when a box or editor attached to player is removed/closed
(defmethod player-cleanup ((player (eql :libaudiostream)) snd)
  (let* ((status-list (if (= (tracknum snd) 0)
                          *audio-player-hidden-tracks-info*
                        *audio-player-visible-tracks-info*))
         (chan (if (eq player *audio-player-hidden*)
                   (tracknum-sys (player-data snd))
                 (tracknum snd)))
         (loadedsnd (car (gethash chan status-list)))
         (status (cadr (gethash chan status-list))))
    (if (eq snd loadedsnd)
        (let () 
           (if (= (tracknum snd) 0)
               (las-stop snd)
             (las-stop snd (tracknum snd)))
          (setf (car (gethash chan status-list)) nil)))))


