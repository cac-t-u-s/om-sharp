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
  (player :pointer) (inputdevicename :pointer) (outputdevicename :pointer) (inchan :int) (outchan :int) (sr :int) (buffsize :int))
;;; todo : use cffi :string type
(cffi:defcfun ("GetAvailableSampleRates" GetAvailableSampleRates) :pointer (player :pointer))
(cffi:defcfun ("GetAvailableSampleRatesCount" GetAvailableSampleRatesCount) :int (player :pointer))
(cffi:defcfun ("GetAvailableBufferSizes" GetAvailableBufferSizes) :pointer (player :pointer))
(cffi:defcfun ("GetAvailableBufferSizesCount" GetAvailableBufferSizesCount) :int (player :pointer))
(cffi:defcfun ("GetDefaultBufferSize" GetDefaultBufferSize) :int (player :pointer))
(cffi:defcfun ("getInputChannelsCount" GetInputChannelsCount) :int (player :pointer))
(cffi:defcfun ("getOutputChannelsCount" GetOutputChannelsCount) :int (player :pointer))

;(cffi:foreign-string-to-lisp (fli:dereference (scandevices) :index 1 :type :pointer))

(defun getinputchannelslist (player)
  (or (loop for i from 1 to (juce::GetInputChannelsCount player) collect i) '(0)))

(defun getoutputchannelslist (player)
  (or (loop for i from 1 to (juce::GetOutputChannelsCount player) collect i) '(0)))

(defun getinputdevicenames (player)
  (loop for i from 0 to (1- (juce::getinputdevicescount player))
        collect
        (cffi:foreign-string-to-lisp (fli:dereference (juce::getavailableinputdevices player)
                                                      :index i :type :pointer))))
(defun getoutputdevicenames (player)
  (loop for i from 0 to (1- (juce::getoutputdevicescount player))
        collect
        (cffi:foreign-string-to-lisp (fli:dereference (juce::getavailableoutputdevices player)
                                                      :index i :type :pointer))))

(defun getsamplerates  (player)
  (loop for i from 0 to (1- (juce::getavailablesampleratescount player))
      collect 
      (fli:dereference (juce::getavailablesamplerates player) :index i :type :int)))

(defun getbuffersizes  (player)
  (loop for i from 0 to (1- (juce::getavailablebuffersizescount player))
      collect 
      (fli:dereference (juce::getavailablebuffersizes player) :index i :type :int)))

(defun setdevices (player input-device-name inch output-device-name outch sample-rate buffer-size)
  (cffi::with-foreign-pointer-as-string (str 255)
    (juce::setaudiodevice player 
                          (cffi::lisp-string-to-foreign input-device-name str (1+ (length input-device-name)))
                          (cffi::lisp-string-to-foreign output-device-name str (1+ (length output-device-name)))
                          inch 
                          outch
                          sample-rate
                          buffer-size)))


(cffi:defcfun ("CloseAudioPlayer" CloseAudioPlayer) :void (player :pointer))


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

(cffi:defcfun ("SetGainReader" SetGainReader) :void (reader :pointer) (gain :float))


