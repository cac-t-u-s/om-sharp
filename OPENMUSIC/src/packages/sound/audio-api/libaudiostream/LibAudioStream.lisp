
;; ============================================================================================
;; The LibAudioStream Library is Copyright (c) Grame, Computer Music Research Laboratory 03-05
;;
;; Grame : Computer Music Research Laboratory
;; Web : http://www.grame.fr/Research
;; ============================================================================================

;;; USE CFFI 0.11
;; This file contains definitions for entry points of the LibAudioStream library
;; It must be used with the LibAudioStream.Framework located in /System/Library/Frameworks

(in-package :cl-user)

(defpackage "LibAudioStream"
  (:nicknames "LAS")
  (:use common-lisp))

(in-package :las)

(push :libaudiostream *features*)

;; LIBSNDFILE TYPES

(defparameter SF_FORMAT_WAV     #x010000)
(defparameter SF_FORMAT_AIFF	#x020000)
(defparameter SF_FORMAT_AU	#x030000)
(defparameter SF_FORMAT_RAW	#x040000)
(defparameter SF_FORMAT_PAF	#x050000)
(defparameter SF_FORMAT_SVX	#x060000)
(defparameter SF_FORMAT_NIST	#x070000)
(defparameter SF_FORMAT_VOC	#x080000)
(defparameter SF_FORMAT_IRCAM	#x0A0000)
(defparameter SF_FORMAT_W64	#x0B0000)
(defparameter SF_FORMAT_MAT4	#x0C0000)
(defparameter SF_FORMAT_MAT5	#x0D0000)	
(defparameter SF_FORMAT_PCM_S8	#x0001)
(defparameter SF_FORMAT_PCM_16	#x0002)
(defparameter SF_FORMAT_PCM_24	#x0003)
(defparameter SF_FORMAT_PCM_32	#x0004)
(defparameter SF_FORMAT_PCM_U8	#x0005)


;; LAS FOREIGN TYPES
(cffi:defctype sound-ptr :pointer)


;;;==============================================
;;  GENERAL
;;;==============================================

(defparameter no_err 0)
(defparameter open_err -1)
(defparameter load_err -2)
(defparameter file_not_found_err -3)
(defparameter state_err -4)

;................................................................................: LibVersion
(cffi:defcfun ("LibVersion" libversion) :long)
;;; (LibVersion)

;................................................................................: GetLastLibError
(cffi:defcfun  ("GetLastLibError" GetLastLibError) :string)
;;; (GetLastLibError)


;;;==============================================
;;  POINTERS AND MEMORY MANAGEMENT
;;;==============================================

;;; We use intermediate structs to encapsulate LAS pointers
;;; This allows us to differenciate them in the cleanup procedure
(defstruct las-sound (ptr nil))

;;; This counter allows us to force GC when too many pointers are open
(defparameter *ptr-counter* 0)

;;; test utils for null or equal pointers
(defmethod las-null-ptr-p ((obj las-sound))
  (cffi:null-pointer-p (las-sound-ptr obj)))
(defmethod las-eql-ptr ((obj1 las-sound) (obj2 las-sound))
  (cffi-sys:pointer-eq (las-sound-ptr obj1) (las-sound-ptr obj2)))

;................................................................................: DeleteSound
(cffi:defcfun  ( "DeleteSoundPtr" delete-sound) :void (sound sound-ptr))
(defun DeleteSound (sound)
  (delete-sound (las-sound-ptr sound)))

; AUDIO-CLEANUP is added as a 'special free action
; use (hcl::remove-special-free-action 'audio-cleanup) to remove it
(hcl::add-special-free-action 'audio-cleanup)

(defmethod audio-cleanup ((obj t)) nil)

(defmethod audio-cleanup ((obj las-sound))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteSound obj))

;;; When a new LAS pointer is created the poiner count is incremented
;;; and the object is flagged for CG special cleanup
(defun register-for-cleanup (obj)
  ;(print (list "GC-REGISTERING_PTR" obj))
  (incf *ptr-counter*)
  (when (> *ptr-counter* 100) (sys::gc-all))
  (hcl::flag-special-free-action obj)
  obj)


;;;==============================================
;;  MAKE SOUND PTRS
;;;==============================================

;................................................................................: MakeNullSound
(cffi:defcfun ("MakeNullSoundPtr" make-null-sound-ptr) sound-ptr (length :long))

(defun MakeNullSound (length)
  (register-for-cleanup 
   (make-las-sound :ptr (make-null-sound-ptr length))))

;................................................................................: MakeMultiNullSound
(cffi:defcfun ("MakeMultiNullSoundPtr" make-multi-null-sound-ptr) sound-ptr (length :long) (channels :long))
(defun MakeMultiNullSound (length channels)
  (register-for-cleanup 
   (make-las-sound :ptr (make-multi-null-sound-ptr length channels))))

;................................................................................: MakeConstantSound
(cffi:defcfun ("MakeConstantSoundPtr" make-constant-sound-ptr) sound-ptr (channels :long) (length :long) (value :float))
(defun MakeConstantSound (channels length value)
  (register-for-cleanup
   (make-las-sound :ptr (make-constant-sound-ptr channels length value))))

;................................................................................: MakeBufferSound
(cffi:defcfun ("MakeBufferSoundPtr" make-buffer-sound-ptr) sound-ptr (buffer :pointer) (length :long) (channels :long) (clear :boolean))
(defun MakeBufferSound (buffer length channels &key (clear nil))
  (register-for-cleanup
   (make-las-sound :ptr (make-buffer-sound-ptr buffer length channels clear))))


;................................................................................: MakeReadSound

#-lispworks
(cffi:defcfun  ("MakeReadSoundPtr" make-read-sound-ptr) sound-ptr (s :pointer))

#-lispworks
(defun MakeReadSound (name)
  (cffi:with-foreign-string (s name)
    (register-for-cleanup
     (make-las-sound :ptr (make-read-sound-ptr s)))))
  
#+lispworks
(fli:define-foreign-function (make-read-sound-ptr "MakeReadSoundPtr")
    ((str (:reference-pass (:ef-mb-string :external-format #+cocoa :macos-roman #-cocoa :latin-1))))
  :result-type :pointer)

#+lispworks
(defun MakeReadSound (name)
  (register-for-cleanup
   (make-las-sound :ptr (make-read-sound-ptr name))))

;................................................................................: MakeRegionSound

#-lispworks
(cffi:defcfun  ("MakeRegionSoundPtr" make-region-sound-ptr) sound-ptr (s :pointer) (begin :long) (end :long))

#-lispworks
(defun MakeRegionSound (name begin end)
  (cffi:with-foreign-string (s name)
    (register-for-cleanup
     (make-las-sound :ptr (make-region-sound-ptr s begin end)))))
  
#+lispworks
(fli:define-foreign-function (make-region-sound-ptr "MakeRegionSoundPtr")
    ((str (:reference-pass (:ef-mb-string :external-format #+cocoa :macos-roman #-cocoa :latin-1)))
     (begin :long) (end :long))
  :result-type :pointer)

#+lispworks
(defun MakeRegionSound (name begin end)
  (register-for-cleanup 
   (make-las-sound :ptr (make-region-sound-ptr name begin end))))


;................................................................................: MakeCopySound
(cffi:defcfun ("MakeCopySoundPtr" make-copy-sound-ptr) sound-ptr (sound sound-ptr))
(defun MakeCopySound (sound)
  (register-for-cleanup 
   (make-copy-sound-ptr (las-sound-ptr sound))))
  

;;;==============================================
;;  INSPECT SOUND PTRS
;;;==============================================

;................................................................................: GetLengthSound
(cffi:defcfun  ( "GetLengthSoundPtr" get-lenght-sound-ptr) :long (sound sound-ptr))

(defun GetLengthSound (sound)
  (get-lenght-sound-ptr (las-sound-ptr sound)))

;................................................................................: GetChannelsSound
(cffi:defcfun  ( "GetChannelsSoundPtr" get-channel-sound-ptr) :long (sound sound-ptr))

(defun GetChannelsSound (sound)
  (get-channel-sound-ptr (las-sound-ptr sound)))


;;;==============================================
;;  TRANSFORM SOUND PTRS
;;;==============================================

;................................................................................: MakStereoSound
;(cffi:defcfun ( "MakeStereoSoundPtr" make-stereo-sound) sound-ptr (sound sound-ptr))
;(defun MakeStereoSound (sound)
;   (let ((snd (make-las-sound :ptr (make-stereo-sound (las-sound-ptr sound)))))
;     (register-rsrc snd)
;    snd))

;................................................................................: MakeFadeSound
(cffi:defcfun ("MakeFadeSoundPtr" make-fade-sound-ptr) sound-ptr (sound sound-ptr) (fadein :long) (fadeout :long))

(defun MakeFadeSound (sound fadein fadeout)
   (register-for-cleanup
    (make-las-sound :ptr (make-fade-sound-ptr (las-sound-ptr sound) fadein fadeout))))

;................................................................................: MakeLoopSound
(cffi:defcfun ("MakeLoopSoundPtr" make-loop-sound-ptr) sound-ptr (sound sound-ptr) (len :long))

(defun MakeLoopSound (sound len)
   (register-for-cleanup
    (make-las-sound :ptr (make-loop-sound-ptr (las-sound-ptr sound) len))))

;................................................................................: MakeCutSound
(cffi:defcfun  ("MakeCutSoundPtr" make-cut-sound-ptr) sound-ptr (sound sound-ptr) (begin :long) (end :long))

(defun MakeCutSound (sound begin end)
   (register-for-cleanup
    (make-las-sound :ptr (make-cut-sound-ptr (las-sound-ptr sound) begin end))))

;................................................................................: MakeSeqSound
(cffi:defcfun  ("MakeSeqSoundPtr" make-seq-sound-ptr) sound-ptr (s1 sound-ptr) (s2 sound-ptr) (crossfade :long))

(defun MakeSeqSound (s1 s2 crossfade)
   (register-for-cleanup
    (make-las-sound :ptr (make-seq-sound-ptr (las-sound-ptr s1) (las-sound-ptr s2) crossfade))))

;................................................................................: MakeMixSound
(cffi:defcfun  ("MakeMixSoundPtr" make-mix-sound-ptr) sound-ptr (s1 sound-ptr) (s2 sound-ptr))

(defun MakeMixSound (s1 s2)
   (register-for-cleanup
    (make-las-sound :ptr (make-mix-sound-ptr (las-sound-ptr s1) (las-sound-ptr s2)))))


;................................................................................: MakeParSound
(cffi:defcfun ("MakeParSoundPtr" make-par-sound-ptr) sound-ptr (s1 sound-ptr) (s2 sound-ptr))
(defun MakeParSound (s1 s2)
  (register-for-cleanup
   (make-las-sound :ptr (make-par-sound-ptr (las-sound-ptr s1) (las-sound-ptr s2)))))
   

;................................................................................: MakeSelectSound
(cffi:defcfun ("MakeSelectSoundPtr" make-select-sound-ptr) sound-ptr (s sound-ptr) (selection :pointer) (n-channels :long))
;;;;;;/!/!/!/ selection type = const std::vector<int>& selection

;;; test if foreign array is ok
(defun MakeSelectSound (s selection nch) 
  (register-for-cleanup 
   (make-las-sound :ptr (make-select-sound-ptr (las-sound-ptr s) selection nch))))


;................................................................................: MakePitchSchiftTimeStretchSound
(cffi:defcfun ("MakePitchSchiftTimeStretchSoundPtr" make-pitchshift-timestretch-sound-ptr) sound-ptr 
  (s sound-ptr) (pitch :double) (stretch :double))

(defun MakePitchSchiftTimeStretchSound (s pitch stretch)
  (register-for-cleanup 
   (make-las-sound :ptr (make-pitchshift-timestretch-sound-ptr (las-sound-ptr s) pitch stretch))))


;;;==============================================
;; SPECIAL STREAM I/O
;;;==============================================

; Create a stream writer (File output)
;................................................................................: MakeWriteSound
(cffi:defcfun  ( "MakeWriteSoundPtr" make-write-sound-ptr) sound-ptr (s :pointer) (sound sound-ptr) (format :long))

(defun MakeWriteSound (name sound format)
   (cffi:with-foreign-string (s name)
     (register-for-cleanup 
      (make-las-sound :ptr (make-write-sound-ptr s (las-sound-ptr sound) format)))))

; Create a stream writer (File output)
;................................................................................: WriteSound
(cffi:defcfun  ( "WriteSound" write-sound) :long (sound sound-ptr) (buffer :pointer) (size :long))

(defun WriteSound (sound buffer size)
  (write-sound (las-sound-ptr sound) buffer size))

; Create a renderer "wrapper" on a stream, to be used for direct access to the stream content.
;................................................................................: MakeRendererSound
(cffi:defcfun  ( "MakeRendererSoundPtr" make-renderer-sound-ptr) sound-ptr (sound sound-ptr))

(defun MakeRendererSound (sound)
  (register-for-cleanup
   (make-las-sound :ptr (make-renderer-sound-ptr (las-sound-ptr sound)))))

;................................................................................: ReadSound
(cffi:defcfun  ("ReadSoundPtr" read-sound-ptr) :long (sound sound-ptr) (buffer :pointer) (buffer-size :long))

(defun ReadSound (sound buffer buffer_size)
  (read-sound-ptr (las-sound-ptr sound) buffer buffer_size))

;................................................................................: ReadSoundPos
(cffi:defcfun  ("ReadSoundPosPtr" read-sound-pos-ptr) :long (sound sound-ptr) (buffer :pointer) (buffer-size :long) (frames :long) (pos :long))

(defun ReadSoundPos (sound buffer buffer_size frames pos)
  (read-sound-pos-ptr (las-sound-ptr sound) buffer buffer_size frames pos))

;................................................................................: SetSoundPos
(cffi:defcfun  ("SetPosSoundPtr" set-pos-sound-ptr) :long (sound sound-ptr) (frames :long))

(defun SetPosSound (sound frames)
  (set-pos-sound-ptr (las-sound-ptr sound) frames))


;................................................................................: ResetSound
(cffi:defcfun  ( "ResetSoundPtr" reset-sound-ptr) :void (sound sound-ptr))

(defun ResetSound (sound)
  (reset-sound-ptr (las-sound-ptr sound)))


;................................................................................: MakeInputSound
(cffi:defcfun  ( "MakeInputSoundPtr" make-input-sound-ptr) sound-ptr)

(defun MakeInputSound ()
   (register-for-cleanup
    (make-las-sound :ptr (make-input-sound-ptr))))

;................................................................................: MakeSharedInputSound
(cffi:defcfun  ( "MakeSharedInputSoundPtr" make-shared-input-sound-ptr) sound-ptr)

(defun MakeSharedInputSound ()
   (register-for-cleanup
    (make-las-sound :ptr (make-shared-input-sound-ptr))))



;;;==============================================
;; EFFECTS
;;;==============================================
(cffi:defctype effect-ptr :pointer)

;;; EFFECT POINTERS BEHAVE JUST LIKE AUDIO STREAM POINTERS
(defstruct las-effect (ptr nil))

(defmethod las-null-ptr-p ((obj las-effect))
  (cffi:null-pointer-p (las-effect-ptr obj)))
(defmethod las-eql-ptr ((obj1 las-effect) (obj2 las-effect))
  (cffi-sys:pointer-eq (las-effect-ptr obj1) (las-effect-ptr obj2)))

;................................................................................: DeleteEffect
(cffi:defcfun  ("DeleteEffectPtr" delete-effect-ptr) :void  (effect effect-ptr))
(defun DeleteEffect (effect)
  (delete-effect-ptr (las-effect-ptr effect)))

;;; CALLED BY GC
(defmethod audio-cleanup ((obj las-effect))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteEffect obj))

;................................................................................: GetControlCountEffect
(cffi:defcfun ("GetControlCountEffectPtr" get-control-count-effect-ptr) :long (effect effect-ptr))
(defun GetControlCountEffect (effect)
  (get-control-count-effect-ptr (las-effect-ptr effect)))

;................................................................................: GetNameEffect
(cffi:defcfun ("GetNameEffectPtr" get-name-effect-ptr) :string (effect effect-ptr))
(defun GetNameEffect (effect)
  (get-name-effect-ptr (las-effect-ptr effect)))

;................................................................................: GetJsonEffect
(cffi:defcfun ("GetJsonEffectPtr" get-json-effect-ptr) :string (effect effect-ptr))

(defun GetJsonEffect (effect)
  (get-json-effect-ptr (las-effect-ptr effect)))

;................................................................................: GetControlParamEffect
(cffi:defcfun ("GetControlParamEffectPtr" get-control-param-effect-ptr) :void  
  (effect effect-ptr) (control :long) (name :pointer) (min :pointer) (max :pointer) (init :pointer))

(defun GetControlParamEffect (effect control)
  (let ((name (cffi::%foreign-alloc 64))
        (min (cffi::%foreign-alloc 4))
        (max (cffi::%foreign-alloc 4))
        (init (cffi::%foreign-alloc 4) ) str minrep maxrep initrep)
 (get-control-param-effect-ptr (las-effect-ptr effect) control name min max init)
 (setf str (cffi::foreign-string-to-lisp name))
 (setf minrep (cffi::mem-ref min :float))
 (setf maxrep (cffi::mem-ref max :float))
 (setf initrep (cffi::mem-ref init :float))
 (cffi::foreign-free name)
 (cffi::foreign-free min)
 (cffi::foreign-free max)
 (cffi::foreign-free init)
 (list str minrep maxrep initrep)))

;................................................................................: SetControlValueEffect
(cffi:defcfun ("SetControlValueEffectPtr" set-control-value-effect-ptr) :void (effect effect-ptr) (control :long) (value :float))
(defun SetControlValueEffect (effect control value)
 (set-control-value-effect-ptr (las-effect-ptr effect) control value))

;................................................................................: GetControlValueEffect
(cffi:defcfun ("GetControlValueEffectPtr" get-control-value-effect-ptr) :float (effect effect-ptr) (control :long))
(defun GetControlValueEffect (effect control)
  (get-control-value-effect-ptr (las-effect-ptr effect) control))

;................................................................................: SetStateEffect
(cffi:defcfun ("SetStateEffectPtr" set-state-effect-ptr) :void (effect effect-ptr) (state :long))
(defun SetStateEffect (effect state)
  (set-state-effect-ptr (las-effect-ptr effect) state))

;................................................................................: GetStateEffect
(cffi:defcfun ("GetStateEffectPtr" get-state-effect-ptr) :long (effect effect-ptr))
(defun GetStateEffect (effect)
  (get-state-effect-ptr (las-effect-ptr effect)))

;................................................................................: ResetEffect
(cffi:defcfun ("ResetEffectPtr" reset-effect-ptr) :void (effect effect-ptr))
(defun ResetEffect (effect)
  (reset-effect-ptr (las-effect-ptr effect)))

;................................................................................: MakeCopyEffect
(cffi:defcfun ("MakeCopyEffectPtr" make-copy-effect-ptr) effect-ptr (effect effect-ptr))
(defun MakeCopyEffect (effect)
  (register-for-cleanup
   (make-las-effect 
    :ptr (make-copy-effect-ptr (las-effect-ptr effect)))))

#|
;;; NOT ANYMORE IN THE LASMC 
(cffi:defcfun  ("MakeVolAudioEffectPtr" make-vol-audio-effect-ptr) effect-ptr  (gain :float))
(defun MakeVolAudioEffect (gain)
  (register-for-cleanup
   (make-las-effect :ptr (make-vol-audio-effect-ptr gain))))

(cffi:defcfun  ("MakeMonoPanAudioEffectPtr" make-mono-pan-audio-effect-ptr) effect-ptr (pan :float))
(defun MakeMonoPanAudioEffect (pan)
  (register-for-cleanup
   (make-las-effect :ptr (make-mono-pan-audio-effect-ptr pan))))

(cffi:defcfun  ("MakeStereoPanAudioEffectPtr" make-stereo-pan-audio-effect-ptr) effect-ptr (panl :float) (panr :float))
(defun MakeStereoPanAudioEffect (panLeft panRight)
  (register-for-cleanup
   (make-las-effect :ptr (make-stereo-pan-audio-effect-ptr panLeft panRight))))
|#


#|
;;; THESE FUNCTIOSN ARE (APPARENTLY) ABTRACTED WITH MakeDispatchFaustAudioEffectPtr
;................................................................................: MakeFaustAudioEffect
(cffi:defcfun  ("MakeFaustAudioEffect" make-faust-audio-effect) effect-ptr (s0 :pointer) (s1 :pointer) (s2 :pointer))
(defun MakeFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s0 name)
    (cffi:with-foreign-string (s1 library_path)
      (cffi:with-foreign-string (s2 draw_path)
        (let ((effect (make-las-effect :ptr (make-faust-audio-effect s0 s1 s2))))
          (register-rsrc effect)
          effect)))))

;................................................................................: MakeRemoteFaustAudioEffect
(cffi:defcfun  ("MakeRemoteFaustAudioEffect" make-remote-faust-audio-effect) effect-ptr (s0 :pointer) (s1 :pointer) (s2 :pointer))
(defun MakeRemoteFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s0 name)
    (cffi:with-foreign-string (s1 library_path)
      (cffi:with-foreign-string (s2 draw_path)
        (let ((effect (make-las-effect :ptr (make-remote-faust-audio-effect s0 s1 s2))))
          (register-rsrc effect)
          effect)))))
|#

(cffi:defcfun  ("MakeDispatchFaustAudioEffectPtr" make-dispatch-faust-audio-effect-ptr) effect-ptr (s0 :pointer) (s1 :pointer) (s2 :pointer))

(defun MakeFaustAudioEffect (name library_path draw_path)
  (cffi:with-foreign-string (s0 name)
    (cffi:with-foreign-string (s1 library_path)
      (cffi:with-foreign-string (s2 draw_path)
        (register-for-cleanup
         (make-las-effect :ptr (make-dispatch-faust-audio-effect-ptr s0 s1 s2))))
      )))



;................................................................................: MakeEffectSound
(cffi:defcfun  ( "MakeEffectSoundPtr" make-effect-sound-ptr) sound-ptr 
  (sound sound-ptr) (effect effect-ptr) (fadein :long) (fadeout :long))
(defun MakeEffectSound (sound effect fadein fadeout )
   (register-for-cleanup
    (make-las-sound :ptr (make-effect-sound-ptr 
                          (las-sound-ptr sound) 
                          (las-effect-ptr effect) fadein fadeout))))

;................................................................................: ProcessEffect
(cffi:defcfun ("ProcessEffectPtr" process-effect-ptr) :void (effect effect-ptr) (input-buffer :pointer) (output-buffer :pointer) (framesnum :long))

(defun ProcessEffect (effect input-buffer output-buffer framesnum)
  (process-effect-ptr (las-effect-ptr effect) input-buffer output-buffer framesnum))


;;;==============================================
;; PLAYER
;;;==============================================

(defparameter kPortAudioRenderer 0)
(defparameter kJackRenderer 1)
(defparameter kCoreAudioRenderer 2)
(defparameter kOffLineAudioRenderer 3)


(cffi:defctype uint64_t :unsigned-long-long)
(cffi:defctype audio-renderer :pointer)
(cffi:defctype audio-client :pointer)
(cffi:defctype audio-player :pointer)
(cffi:defctype audio_frame_t :unsigned-long-long)
(cffi:defctype audio_usec_t :unsigned-long-long)

(cffi:defcstruct DeviceInfo 
  (fName :char :count 64)
  (fMaxInputChannels :long)
  (fMaxOutputChannels :long)
  (fDefaultBufferSize :long)
  (fDefaultSampleRate :double))

(cffi:defctype TDeviceInfo (:struct deviceinfo))

(cffi:defcstruct RendererInfo 
  (fInput :long)
  (fOutput :long)
  (fSampleRate :long)
  (fBufferSize :long)
  (fCurFrame uint64_t)
  (fCurUsec uint64_t)
  (fOutputLatencyFrame :long)
  (fOutputLatencyUsec :long)
  (fInputLatencyFrame :long)
  (fInputLatencyUsec :long))

(cffi:defctype TRendererInfo (:struct RendererInfo))


;................................................................................: AudioGlobalsInit
(cffi:defcfun ("AudioGlobalsInit" audio-globals-init) :void
  (inchan :long) 
  (outchan :long) 
  (samplerate :long) 
  (buffersize :long) 
  (streambuffersize :long) 
  (rtstreamduration :long)
  (threadnum :long))
(defun AudioGlobalsInit (inchan outchan samplerate buffersize streambuffersize rtstreamduration threadnum)
  (audio-globals-init inchan outchan samplerate buffersize streambuffersize rtstreamduration threadnum))

;................................................................................: AudioGlobalsDestroy
(cffi:defcfun ("AudioGlobalsDestroy" audio-globals-destroy) :void)
(defun AudioGlobalsDestroy ()
  (audio-globals-destroy))



;;; RENDERER
;................................................................................: MakeAudioRenderer
(cffi:defcfun ("MakeAudioRenderer" make-audio-renderer) audio-renderer (renderer :long))
(defun MakeAudioRenderer (renderer)
  (make-audio-renderer renderer))

;................................................................................: DeleteAudioRenderer
(cffi:defcfun ("DeleteAudioRenderer" delete-audio-renderer) :void (renderer audio-renderer))
(defun DeleteAudioRenderer (renderer)
  (delete-audio-renderer renderer))

;................................................................................: OpenAudioRenderer
(cffi:defcfun ("OpenAudioRenderer" open-audio-renderer) :long 
  (renderer audio-renderer) 
  (inputdevice :long) 
  (outputdevice :long) 
  (inchan :long) 
  (outchan :long) 
  (buffersize :long) 
  (samplerate :long))
(defun OpenAudioRenderer (renderer inputdevice outputdevice inchan outchan buffersize samplerate)
  (open-audio-renderer renderer inputdevice outputdevice inchan outchan buffersize samplerate))

;................................................................................: CloseAudioRenderer
(cffi:defcfun ("CloseAudioRenderer" close-audio-renderer) :void (renderer audio-renderer))
(defun CloseAudioRenderer (renderer)
  (close-audio-renderer renderer))

;................................................................................: GetAudioRendererInfo
(cffi:defcfun ("GetAudioRendererInfo" get-audio-renderer-info) :void (renderer audio-renderer) (info RendererInfo))

#|


;(let ((info (cffi:foreign-alloc '(:POINTER (:STRUCT RENDERERINFO)))))
;  (unwind-protect 
;      (progn
;        (get-audio-renderer-info om::*las-player* info)
;      ;(cffi::convert-from-foreign info '(:STRUCT RENDERERINFO))
;        )
;    (cffi::foreign-free info)))


(defun GetAudioRendererInfo (renderer)
  (let ((info (cffi:foreign-alloc '(:STRUCT RENDERERINFO))))
    (get-audio-renderer-info renderer info)
    (let ((fInput (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fInput))
          (fOutput (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fOutput))
          (fSampleRate (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fSampleRate))
          (fBufferSize (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fBufferSize))
          (fCurFrame (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fCurFrame))
          (fCurUsec (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fCurUsec))
          (fOutputLatencyFrame (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fOutputLatencyFrame))
          (fOutputLatencyUsec (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fOutputLatencyUsec))
          (fInputLatencyFrame (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fInputLatencyFrame))
          (fInputLatencyUsec (cffi:foreign-slot-value info '(:STRUCT RENDERERINFO) 'fInputLatencyUsec)))
      (cffi::foreign-free info)
      (list fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
      )))


(defun GetAudioRendererInfo (renderer)
  (let ((info (cffi:foreign-alloc '(:POINTER (:STRUCT RENDERERINFO)))))
    (get-audio-renderer-info renderer info)
    (let ((fInput (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fInput) :long))
          (fOutput (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fOutput) :long))
          (fSampleRate (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fSampleRate) :long))
          (fBufferSize (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fBufferSize) :long))
          (fCurFrame (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fCurFrame) :unsigned-long-long))
          (fCurUsec (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fCurUsec) :unsigned-long-long))
          (fOutputLatencyFrame (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fOutputLatencyFrame) :long))
          (fOutputLatencyUsec (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fOutputLatencyUsec) :long))
          (fInputLatencyFrame (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fInputLatencyFrame) :long))
          (fInputLatencyUsec (cffi::mem-aref (cffi:foreign-slot-pointer info 'TRendererInfo 'fInputLatencyUsec) :long)))
      (cffi::foreign-free info)
      (list fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
      )))
|#

(defun GetAudioRendererInfo (renderer)
  (let ((info (cffi:foreign-alloc '(:POINTER (:STRUCT RENDERERINFO)))))
    (get-audio-renderer-info renderer info)
    (let ((fInput (cffi:foreign-slot-value info 'TRendererInfo 'fInput))
          (fOutput (cffi:foreign-slot-value info 'TRendererInfo 'fInput))
          (fSampleRate (cffi:foreign-slot-value info 'TRendererInfo 'fSampleRate))
          (fBufferSize (cffi:foreign-slot-value info 'TRendererInfo 'fBufferSize))
          (fCurFrame (cffi:foreign-slot-value info 'TRendererInfo 'fCurFrame))
          (fCurUsec (cffi:foreign-slot-value info 'TRendererInfo 'fCurUsec))
          (fOutputLatencyFrame (cffi:foreign-slot-value info 'TRendererInfo 'fOutputLatencyFrame))
          (fOutputLatencyUsec (cffi:foreign-slot-value info 'TRendererInfo 'fOutputLatencyUsec))
          (fInputLatencyFrame (cffi:foreign-slot-value info 'TRendererInfo 'fInputLatencyFrame))
          (fInputLatencyUsec (cffi:foreign-slot-value info 'TRendererInfo 'fInputLatencyUsec)))
      (cffi::foreign-free info)
      (list fInput fOutput fSampleRate fBufferSize fCurFrame fCurUsec fOutputLatencyFrame fOutputLatencyUsec fInputLatencyFrame fInputLatencyUsec)
      )))



;................................................................................: AddAudioClient
(cffi:defcfun ("AddAudioClient" add-audio-client) :void (renderer audio-renderer) (client audio-client))
(defun AddAudioClient (renderer client)
  (add-audio-client renderer client))

;................................................................................: RemoveAudioClient
(cffi:defcfun ("RemoveAudioClient" remove-audio-client) :void (renderer audio-renderer) (client audio-client))
(defun RemoveAudioClient (renderer client)
  (remove-audio-client renderer client))

;................................................................................: StartAudioRenderer
(cffi:defcfun ("StartAudioRenderer" start-audio-renderer) :long (renderer audio-renderer))
(defun StartAudioRenderer (renderer)
  (start-audio-renderer renderer))

;................................................................................: StopAudioRenderer
(cffi:defcfun ("StopAudioRenderer" stop-audio-renderer) :long (renderer audio-renderer))
(defun StopAudioRenderer (renderer)
  (stop-audio-renderer renderer))

;................................................................................: GetDeviceCount
(cffi:defcfun ("GetDeviceCount" get-device-count) :long (renderer audio-renderer))
(defun GetDeviceCount (renderer)
  (get-device-count renderer))

;................................................................................: GetDeviceInfo
(cffi:defcfun ("GetDeviceInfo" get-device-info) :void (renderer audio-renderer) (device-num :long) (info :pointer))
(defun GetDeviceInfo (renderer device-num)
  (let ((info (cffi:foreign-alloc 'DeviceInfo)))
    (get-device-info renderer device-num info)
    (let ((fName (cffi::foreign-string-to-lisp (cffi:foreign-slot-value info 'DeviceInfo 'fName)))
          (fMaxInputChannels (cffi:foreign-slot-value info 'DeviceInfo 'fMaxInputChannels))
          (fMaxOutputChannels (cffi:foreign-slot-value info 'DeviceInfo 'fMaxOutputChannels))
          (fDefaultBufferSize (cffi:foreign-slot-value info 'DeviceInfo 'fDefaultBufferSize))
          (fDefaultSampleRate (cffi:foreign-slot-value info 'DeviceInfo 'fDefaultSampleRate)))
      (cffi::foreign-free info)
      (list fName fMaxInputChannels fMaxOutputChannels fDefaultBufferSize fDefaultSampleRate))
    ))

;................................................................................: GetDefaultInputDevice
(cffi:defcfun ("GetDefaultInputDevice" get-default-input-device) :long (renderer audio-renderer))
(defun GetDefaultInputDevice (renderer)
  (get-default-input-device renderer))

;................................................................................: GetDefaultOutputDevice
(cffi:defcfun ("GetDefaultOutputDevice" get-default-output-device) :long (renderer audio-renderer))
(defun GetDefaultOutputDevice (renderer)
  (get-default-output-device renderer))



;;; CLIENT
;................................................................................: OpenAudioClient
(cffi:defcfun ("OpenAudioClient" open-audio-client) :pointer (manager audio-renderer))
(defun OpenAudioClient (manager)
  (open-audio-client manager))

;................................................................................: CloseAudioClient
(cffi:defcfun ("CloseAudioClient" close-audio-client) :void (player :pointer))
(defun CloseAudioClient (player)
  (close-audio-client player))


;;; PLAYER

; Set global input/output audio latencies. This calls has to be done BEFORE OpenAudioPlayer 
; and will take effect only when PortAudio is used.
;................................................................................: SetAudioLatencies
(cffi:defcfun ("SetAudioLatencies" set-audio-latencies) :void (input-latency :long) (output-latency :long))
(defun SetAudioLatencies (input-latency output-latency)
  (set-audio-latencies input-latency output-latency))

;................................................................................: OpenAudioPlayer
(cffi:defcfun ("OpenAudioPlayer" open-audio-player) :pointer
  (inchan :long)
  (outchan :long)
  (sr :long)
  (bs :long)
  (sbs :long)
  (rtbs :long)
  (renderer :long)
  (thread_num :long))

(defun OpenAudioPlayer (inchan outchan sr bs sbs rtbs renderer thread_num)
  (open-audio-player inchan outchan sr bs sbs rtbs renderer thread_num))
; (setq test-plater (las::OpenAudioPlayer 2 2 44100 512 (* 65536 4) (* 44100 60 20) las::kCoreAudioRenderer 1))

;;................................................................................: CloseAudioPlayer
(cffi:defcfun  ( "CloseAudioPlayer" close-audio-player) :void (player audio-player))
(defun CloseAudioPlayer (player)
  (close-audio-player player))

;................................................................................: GetAudioPlayerRenderer
(cffi:defcfun ("GetAudioPlayerRenderer" get-audio-player-renderer) audio-renderer (player audio-player))
(defun GetAudioPlayerRenderer (player)
  (get-audio-player-renderer player))

;................................................................................: StartAudioPlayer
(cffi:defcfun  ("StartAudioPlayer" start-audio-player) :void (player audio-player) )

(defun StartAudioPlayer (player)
  (start-audio-player player))

;................................................................................: StartAudioPlayer
(cffi:defcfun  ("StopAudioPlayer" stop-audio-player) :void (player audio-player) )

(defun StopAudioPlayer (player)
  (stop-audio-player player))

;................................................................................: ClearAudioPlayer
(cffi:defcfun ("ClearAudioPlayer" clear-audio-player) :long (player audio-player))
(defun ClearAudioPlayer (player)
  (clear-audio-player player))


;;; PLAY TOOLS
;................................................................................: GetAudioPlayerDateInUsec
(cffi:defcfun ("GetAudioPlayerDateInUsec" GetAudioPlayerDateInUsec) audio_usec_t (player audio-player))
;................................................................................: GetAudioPlayerDateInFrames
(cffi:defcfun ("GetAudioPlayerDateInFrame" GetAudioPlayerDateInFrame) audio_frame_t (player audio-player))



;;;==============================================
;;; DATE
;;;==============================================
(cffi:defctype symbolic-date-ptr :pointer)

;;; DATE POINTERS BEHAVE JUST LIKE AUDIO STREAM POINTERS
(defstruct las-date (ptr nil))

(defmethod las-null-ptr-p ((obj las-date))
  (cffi:null-pointer-p (las-effect-ptr obj)))
(defmethod las-eql-ptr ((obj1 las-date) (obj2 las-date))
  (cffi-sys:pointer-eq (las-effect-ptr obj1) (las-effect-ptr obj2)))

;................................................................................: DeleteEffect
(cffi:defcfun  ("DeleteSymbolicDatePtr" delete-date-ptr) :void  (effect symbolic-date-ptr))
(defun DeleteSymbolicDate (date)
  (delete-date-ptr (las-date-ptr date)))

;;; CALLED BY GC
(defmethod audio-cleanup ((obj las-date))
  ;(print (list "GC-AUDIO_CLEANUP" obj))
  (decf *ptr-counter*)
  (DeleteSymbolicDate obj))


;................................................................................: GenSymbolicDate
(cffi:defcfun ("GenSymbolicDatePtr" gen-symbolic-date-ptr) symbolic-date-ptr (player audio-player))

(defun GenSymbolicDate (player) 
  (register-for-cleanup
   (make-las-date :ptr (gen-symbolic-date-ptr player))))

;................................................................................: GenRealDate
(cffi:defcfun ("GenRealDatePtr" gen-real-date-ptr) symbolic-date-ptr (player audio-player) (date audio_frame_t))

(defun GenRealDate (player date) 
  (register-for-cleanup
   (make-las-date :ptr (gen-real-date-ptr player date))))

;................................................................................: GetSymbolicDate
(cffi:defcfun ("GetSymbolicDatePtr" get-symbolic-date-ptr) audio_frame_t (player audio-player) (symb-date symbolic-date-ptr))

(defun GetSymbolicDate (player symb-date) 
  (get-symbolic-date-ptr player (las-date-ptr symb-date)))

;................................................................................: SetSymbolicDate
(cffi:defcfun ("SetSymbolicDatePtr" set-symbolic-date-ptr) :long (player audio-player) (symb-date symbolic-date-ptr) (real-date audio_frame_t))

(defun SetSymbolicDate (player symb-date real-date) 
  (set-symbolic-date-ptr player (las-date-ptr symb-date) real-date))

;................................................................................: GetPosSound
(cffi:defcfun ("GetPosSoundPtr" get-pos-sound-ptr) :long (sound sound-ptr))

(defun GetPosSound (sound) 
  (get-pos-sound-ptr (las-sound-ptr sound)))


;;;==============================================
;;; TIMED ACTIONS
;;;==============================================

;................................................................................: StartSound
(cffi:defcfun ("StartSoundPtr" start-sound-ptr) :long (player audio-player) (s sound-ptr) (date symbolic-date-ptr))

(defun StartSound (player s date) 
  (start-sound-ptr player (las-sound-ptr s) (las-date-ptr date)))

;................................................................................: StopSound
(cffi:defcfun ("StopSoundPtr" stop-sound-ptr) :long (player audio-player) (s sound-ptr) (date symbolic-date-ptr))

(defun StopSound (player s date) 
  (stop-sound-ptr player (las-sound-ptr s) (las-date-ptr date)))


; Set the effect control value at a specific date in frames.
;................................................................................: SetTimedControlValueEffect
(cffi:defcfun ("SetTimedControlValueEffectPtr" set-timed-control-value-effect-ptr) :long
  (player :pointer) (effect :pointer) (path :pointer) (value :float) (date symbolic-date-ptr))

(defun SetTimedControlValueEffect (player effect path value date)
  (cffi:with-foreign-string (s1 effect)
    (cffi:with-foreign-string (s2 path)
      (set-timed-control-value-effect-ptr player s1 s2 value (las-date-ptr date)))))
      
      

;;===============================================
;; TESTS

; (fli::register-module :las :real-name "C:\\Program Files (x86)\\LispWorks\\LibAudioStream2.dll" :connection-style :immediate)
; (las::libversion)
; (las::OpenAudioPlayer 2 2 44100 512 (* 65536 4) (* 44100 60 20) las::kPortAudioRenderer 1)



