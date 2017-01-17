;;;===========================================
;;; IMPLEMENTATION OF AN AUDIO PLAYER FOR OM 
;;; USING THE LAS ARCHITECTURE JUCE LIB
;;;===========================================

(in-package :om)


(defmethod player-name ((self (eql :omjuceaudiolib))) "OMJuceAudioLib")
(defmethod player-desc ((self (eql :omjuceaudiolib))) "OMJuce audio player")

#+omjuceaudiolib(add-player-for-object 'sound :omjuceaudiolib)
#+omjuceaudiolib(enable-player :omjuceaudiolib)

(defvar *juce-player* nil)
(defvar *audio-driver* "CoreAudio")

;(defvar *juce-player-in-channels* 0)
;(defvar *juce-player-out-channels* 2)
;(defvar *juce-sample-rate* 44100)
;(defvar *juce-input-devices* nil)
;(defvar *juce-output-devices* nil)

(setf *juce-input-devices* nil)

(add-preference-module :audio "Audio")
(add-preference-section :audio "Devices")
;(add-preference :audio :input "Input device" *juce-input-devices* (car *juce-input-devices*))
(add-preference :audio :output "Output device" nil nil nil 'set-audio-device) ;; will be set at player startup
(add-preference-section :audio "Configuration")
;(add-preference :audio :in-channels "Input Channels" '(0) 0)
(add-preference :audio :out-channels "Output Channels" '(2) 2 nil 'apply-audio-device-config)
(add-preference :audio :samplerate "Sample Rate" '(44100) 44100 nil 'apply-audio-device-config)
(add-preference :audio :buffersize "Buffer Size" '(256 512 1024) 512 nil 'apply-audio-device-config)

; (add-preference :audio :apply "Apply" :action 'apply-audio-prefs)

(defun default-audio-input-device ()
  (or (and *juce-player* (car (audio-driver-input-devices *juce-player* *audio-driver*))) ""))

(defun default-audio-output-device ()
  (or (and *juce-player* (car (audio-driver-output-devices *juce-player* *audio-driver*))) ""))

; (juce::get-audio-drivers *juce-player*)

(defun set-audio-device ()  
  ;; scan for available devices (just in case)
  (let ((out-devices (juce::audio-driver-output-devices *juce-player* *audio-driver*)))
    (add-preference :audio :output "Output device" out-devices (car out-devices) nil 'set-audio-device)
    (unless (and (get-pref-value :audio :output)
                 (find (get-pref-value :audio :output) out-devices :test 'string-equal))
      (when (get-pref-value :audio :output) 
        (om-beep-msg "Audio output device: ~S not found. restoring default."  (get-pref-value :audio :output)))
      (put-default-value (get-pref :audio :output)))

    (let ((out (or (position (get-pref-value :audio :output) out-devices :test 'string-equal) -1)))
  
      (juce::setdevices *juce-player* "" 0 ;; default/0 channels for input
                        (get-pref-value :audio :output) ;; this has changed
                        (get-pref-value :audio :out-channels) ;; this might be invalidated
                        (get-pref-value :audio :samplerate) ;; this might be invalidated
                        (get-pref-value :audio :buffersize)) ;; this might be invalidated
      ))
  
  (let ((device-supported-out-channels (juce::getoutputchannelslist *juce-player*))
        (device-supported-sample-rates (juce::getsamplerates *juce-player*))
        (device-supported-buffer-sizes (juce::getbuffersizes *juce-player*)))
    
    ;;; update the config preferences for new device
    (add-preference :audio :out-channels "Output Channels" device-supported-out-channels 2
                    nil 'apply-audio-device-config)     
    (unless (find (get-pref-value :audio :out-channels) device-supported-out-channels :test '=)
        (put-default-value (get-pref :audio :out-channels)))
    
    (add-preference :audio :samplerate "Sample Rate" device-supported-sample-rates (car device-supported-sample-rates)
                    nil 'apply-audio-device-config)
    (unless (find (get-pref-value :audio :samplerate) device-supported-sample-rates :test '=)
        (put-default-value (get-pref :audio :samplerate)))
    
    (add-preference :audio :buffersize "Buffer Size" device-supported-buffer-sizes (juce::getdefaultbuffersize *juce-player*)
                    nil 'apply-audio-device-config)
    (unless (find (get-pref-value :audio :buffersize) device-supported-buffer-sizes :test '=)
      (put-default-value (get-pref :audio :buffersize)))

    (update-preferences-window)
    (apply-audio-device-config)))
    
    
(defun apply-audio-device-config ()
  (om-print (format nil "Setting audio out on ~s with ~D channels at ~D Hz." 
                    (get-pref-value :audio :output)
                    (get-pref-value :audio :out-channels)
                    (get-pref-value :audio :samplerate))
            "AUDIO SETUP")
  (juce::setdevices  *juce-player* 
                     "" 0
                     (get-pref-value :audio :output)
                     (get-pref-value :audio :out-channels)
                     (get-pref-value :audio :samplerate)
                     (get-pref-value :audio :buffersize)
                     ))

;;; when this function si called the preferences are set to their default or saved values
(defun open-juce-player ()
  (setq *juce-player* (juce::openAudioManager))
  (juce::setDeviceType *juce-player* *audio-driver*)
  (set-audio-device))

(om-add-init-fun 'open-juce-player)

(defun close-juce-player ()
  (juce::closeAudioManager *juce-player*)
  (setf *juce-player* nil))

(defun configure-audio-channels (list)
  (juce::setoutputchannels *juce-player* list))


#|
(defun set-juce-devices (input-device-index output-device-index sample-rate)
  (juce::setdevices 
   *juce-player* (nth input-device-index *juce-input-devices*)
   *juce-player-in-channels* 
   (nth output-device-index *juce-output-devices*)   
   sample-rate))
|#


;(set-juce-devices 0 0 44100) A APPELER
;(listen *terminal-io*) 
;(defun testgetmono (a) (setq *testbp1* (bp-pointer (buffer-player a))))
;(defun testgetstereo (a) (setq *testbp2* (bp-pointer (buffer-player a))))
;(juce::setgainreader *testbp1* 0.2)
;(juce::setgainreader *testbp2* 0.2)

;(defun testgainmono (a n)  (juce::setgainreader *testbp1* n))
;(defun testgainstereo (a n)  (juce::setgainreader *testbp2* n))



