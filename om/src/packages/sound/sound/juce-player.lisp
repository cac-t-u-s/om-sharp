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
(defvar *audio-driver* nil)

(add-preference-module :audio "Audio")
(add-preference-section :audio "Devices")
(add-preference :audio :output "Output device" nil nil nil 'setup-audio-device) ;; will be set at player startup
(add-preference-section :audio "Configuration")
(add-preference :audio :out-channels "Output Channels" '(2) 2 nil 'apply-audio-device-config)
(add-preference :audio :samplerate "Sample Rate" '(44100) 44100 nil 'apply-audio-device-config)
(add-preference :audio :buffersize "Buffer Size" '(256 512 1024) 512 nil 'apply-audio-device-config)
(add-preference :audio :channels-config "Output channels routing list" :list nil nil 'apply-audio-device-config)


(defun default-audio-input-device ()
  (or (and *juce-player* (car (juce::audio-driver-input-devices *juce-player* *audio-driver*))) ""))

(defun default-audio-output-device ()
  (or (and *juce-player* (car (juce::audio-driver-output-devices *juce-player* *audio-driver*))) ""))


; (juce::get-audio-drivers *juce-player*)
(defun setup-audio-device ()  
  ;; scan for available devices (just in case)
  (let ((out-devices (juce::audio-driver-output-devices *juce-player* *audio-driver*)))
    (add-preference :audio :output "Output device" out-devices (car out-devices) nil 'setup-audio-device)
    (unless (and (get-pref-value :audio :output)
                 (find (get-pref-value :audio :output) out-devices :test 'string-equal))
      (when (get-pref-value :audio :output) 
        (om-beep-msg "Audio output device: ~S not found. restoring default."  (get-pref-value :audio :output)))
      (put-default-value (get-pref :audio :output)))
   
    ;;; apply current params
    (apply-audio-device-config)
      
    ;;; update preference choices
    ;;; update pref window
    ;;; check for conformuity of current settings
    ;;; reset defaults if needed
    (let ((device-supported-out-channels (juce::getoutputchannelslist *juce-player*))
          (device-supported-sample-rates (juce::getsamplerates *juce-player*))
          (device-supported-buffer-sizes (juce::getbuffersizes *juce-player*)))
      
      (add-preference :audio :out-channels "Output Channels" device-supported-out-channels 2
                      nil 'apply-audio-device-config)
      (add-preference :audio :samplerate "Sample Rate" device-supported-sample-rates (car device-supported-sample-rates)
                      nil 'apply-audio-device-config)
      (add-preference :audio :buffersize "Buffer Size" device-supported-buffer-sizes (juce::getdefaultbuffersize *juce-player*)
                      nil 'apply-audio-device-config)

      (unless (find (get-pref-value :audio :out-channels) device-supported-out-channels :test '=)
        (put-default-value (get-pref :audio :out-channels))
        ;;; need to reapply the config with the defautl value
        (apply-audio-device-config))
           
      (unless (find (get-pref-value :audio :samplerate) device-supported-sample-rates :test '=)
        (put-default-value (get-pref :audio :samplerate))
        (juce::setsamplerate *juce-player* (get-pref-value :audio :samplerate)))
      
      
      (unless (find (get-pref-value :audio :buffersize) device-supported-buffer-sizes :test '=)
        (put-default-value (get-pref :audio :buffersize))
        (juce::setsamplerate *juce-player* (get-pref-value :audio :buffersize)))
      
      (update-preference-window-item :audio :out-channels)
      (update-preference-window-item :audio :samplerate)
      (update-preference-window-item :audio :buffersize)
      
      )))
    
;;; applies the current prefs
(defun apply-audio-device-config ()
  (om-print (format nil "Setting audio out on ~s with ~D channels at ~D Hz." 
                    (get-pref-value :audio :output)
                    (get-pref-value :audio :out-channels)
                    (get-pref-value :audio :samplerate))
            "AUDIO SETUP")
  (juce::setdevices *juce-player* 
                    "" 0 ;; default/0 channels for input
                    (get-pref-value :audio :output)
                    (get-pref-value :audio :out-channels)
                    (get-pref-value :audio :samplerate)
                    (get-pref-value :audio :buffersize)
                    )
  (when (listp (get-pref-value :audio :channels-config))
    (configure-audio-channels (get-pref-value :audio :channels-config)))
  )

; (juce::getCurrentDeviceType *juce-player*)
; (juce::audio-driver-output-devices *juce-player* "CoreAudio")
;;; when this function si called the preferences are set to their default or saved values
(defun open-juce-player ()
  (setq *juce-player* (juce::openAudioManager))
  (unless *audio-driver* 
    (setf *audio-driver* (car (juce::get-audio-drivers *juce-player*))))
  (if *audio-driver*
      (progn 
        (om-print (format nil "Selecting default audio driver: \"~A\"." *audio-driver*) "AUDIO SETUP")
        (juce::setDeviceType *juce-player* *audio-driver*)
        (setup-audio-device))
    (om-beep-msg "ERROR OPENING AUDIO: Could not find any audio driver."))
  )

(defun close-juce-player ()
  (juce::closeAudioManager *juce-player*)
  (setf *juce-player* nil))

(defun configure-audio-channels (list)
  (when list 
    (om-print (format nil "Output channels:") "AUDIO SETUP")
    (let* ((available-channels (juce::getoutputchannelslist *juce-player*))
           (checked-list
            (loop for to in list for from = 1 then (+ from 1) collect
                 (cond ((not (find from available-channels :test '=))
                        ;;; from channel not available
                        (om-print (format nil "~D => ERROR there is no chanel ~D" from from) "AUDIO SETUP")
                        nil)
                       ((not (find to available-channels :test '=))                        
                        ;;; to channel not available => don't route
                        (om-print (format nil "~D => ERROR: channel ~D not available => ~D" from to from) "AUDIO SETUP")
                        from)
                       (t 
                        (om-print (format nil "~D => ~D" from to) "AUDIO SETUP")
                        to)))))
      (juce::setoutputchannels *juce-player* (remove nil checked-list)))))

(add-om-init-fun 'open-juce-player)
(add-om-exit-action 'close-juce-player)

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



