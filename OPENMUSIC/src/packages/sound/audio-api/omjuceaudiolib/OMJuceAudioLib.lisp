(in-package :cl-user)

(defpackage :juce)

;(fli:register-module 
; "OMJuceAudioLib" 
; :real-name "/Users/bouche/Documents/GIT/om7/OPENMUSIC/resources/lib/mac/OMJuceAudioLib.dylib"
; :connection-style :immediate)

(push :omjuceaudiolib *features*)

(in-package :juce)

;;;==============================================
;;  PLAYER
;;;==============================================
(cffi:defcfun ("OpenAudioPlayer" OpenAudioPlayer) :pointer)

(cffi:defcfun ("GetAvailableInputDevices" GetAvailableInputDevices) :pointer (player :pointer))
(cffi:defcfun ("GetAvailableOutputDevices" GetAvailableOutputDevices) :pointer (player :pointer))
(cffi:defcfun ("getInputDevicesCount" getInputDevicesCount) :int (player :pointer))
(cffi:defcfun ("getOutputDevicesCount" getOutputDevicesCount) :int (player :pointer))
(cffi:defcfun ("setAudioDevice" setAudioDevice) :void 
  (player :pointer) (inputdevicename :pointer) (outputdevicename :pointer) (inchan :int) (outchan :int) (sr :int))

;(cffi:foreign-string-to-lisp (fli:dereference (scandevices) :index 1 :type :pointer))

(cffi:defcfun ("CloseAudioPlayer" CloseAudioPlayer) :void (player :pointer))

(cffi:defcfun ("ChangeSampleRate" ChangeSampleRate) :void (player :pointer) (SR :int))

;;;==============================================
;;  BUFFER
;;;==============================================

(cffi:defcfun ("MakeDataReader" MakeDataReader) :pointer (buffer :pointer) (channels :int) (size :int) (sr :int))
(cffi:defcfun ("MakeFileReader" MakeFileReader) :pointer (file :string))

(cffi:defcfun ("FreeReader" FreeReader) :void (reader :pointer))

(cffi:defcfun ("StartReader" StartReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("PauseReader" PauseReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("StopReader" StopReader) :void (player :pointer) (reader :pointer))

(cffi:defcfun ("SetPosReader" SetPosReader) :void (reader :pointer) (pos :long))

(cffi:defcfun ("GetPosReader" GetPosReader) :long (reader :pointer))

(cffi:defcfun ("LoopReader" LoopReader) :void (reader :pointer) (looper :boolean))

(cffi:defcfun ("GetGainReader" GetGainReader) :float (reader :pointer))

(cffi:defcfun ("SetGainReader" SetGainReader) :float (reader :pointer) (gain :float))


