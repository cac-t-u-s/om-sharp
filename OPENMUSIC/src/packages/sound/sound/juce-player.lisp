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
(defvar *juce-player-in-channels* 0)
(defvar *juce-player-out-channels* 2)
(defvar *juce-sample-rate* 44100)
(defvar *juce-input-devices* nil)
(defvar *juce-output-devices* nil)

(setf *juce-input-devices* nil)

(add-preference-module :audio "Audio")
(add-preference-section :audio "Devices")
(add-preference :audio :input "Input device" *juce-input-devices* (car *juce-input-devices*))
(add-preference :audio :output "Output device" *juce-output-devices* (car *juce-output-devices*))
(add-preference-section :audio "Configuration")
(add-preference :audio :inchan "Input Channels" '(0) 0)
(add-preference :audio :outchan "Output Channels" '(1 2) *juce-player-out-channels*)
(add-preference :audio :samplerate "Sample Rate" '(44100) 44100)
(add-preference :audio :buffersize "Buffer Size" '(256 512 1024) 512)

(add-preference :audio :apply "Apply" :action 'apply-audio-prefs)

(defun open-juce-player ()
  (setq *juce-player* (juce::OpenAudioPlayer))
  ;;; update the preferences
  (when *juce-player*
    (let ((in-devices (juce::getinputdevicenames *juce-player*))
          (out-devices (juce::getoutputdevicenames *juce-player*))
          in-channels
          out-channels
          rates
          buffer-sizes)
        
      (add-preference :audio :input "Input device" in-devices (car in-devices))
      (add-preference :audio :output "Output device" out-devices (car out-devices))
     
      
      (juce::setdevices 
       *juce-player* 
       (car in-devices) *juce-player-in-channels* 
       (car out-devices) *juce-player-out-channels*
       *juce-sample-rate*
       512)  
      
      (setq rates (juce::getsamplerates *juce-player*)
            in-channels (juce::getinputchannelslist *juce-player*)
            out-channels (juce::getoutputchannelslist *juce-player*)
            buffer-sizes (juce::getbuffersizes *juce-player*))
      
      (add-preference :audio :outchan "Output Channels" out-channels (last-elem out-channels))
      (add-preference :audio :inchan "Input Channels" in-channels (car in-channels))
      
      (add-preference :audio :samplerate "Sample Rate" rates  (car rates))
      (add-preference :audio :buffersize "Buffer Size" buffer-sizes (juce::getdefaultbuffersize *juce-player*))
      )))

(defun apply-audio-prefs ()
  (juce::setdevices 
       *juce-player* 
       (get-pref-value :audio :input) (get-pref-value :audio :inchan) 
       (get-pref-value :audio :output) (get-pref-value :audio :outchan)
       (get-pref-value :audio :samplerate)
       (get-pref-value :audio :buffersize)
       ))

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

(defun close-juce-player ()
  (juce::closeaudioplayer *juce-player*)
  (setf *juce-player* nil))

(om-add-init-fun 'open-juce-player)