;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

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
(add-preference :audio :channels-config "Output channels routing" :list nil 
                '("format : out-for-ch1 out-for-ch2 ..." 
                  "NIL/- = mute channel"
                  "Channels beyond the list will be routed to their direct output" 
                  "This will NOT apply on sounds that are accessed from file")
                'apply-audio-device-config)


(defun default-audio-input-device ()
  (or (and *juce-player* (car (juce::audio-driver-input-devices *juce-player* *audio-driver*))) ""))

(defun default-audio-output-device ()
  (or (and *juce-player* (car (juce::audio-driver-output-devices *juce-player* *audio-driver*))) ""))

;(default-audio-output-device)

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
      (add-preference :audio :samplerate "Sample Rate" device-supported-sample-rates 44100
                      nil 'apply-audio-device-config)
      (add-preference :audio :buffersize "Buffer Size" device-supported-buffer-sizes 512
                      nil 'apply-audio-device-config)

      (unless (find (get-pref-value :audio :out-channels) device-supported-out-channels :test '=)
        (put-default-value (get-pref :audio :out-channels))
        ;;; need to reapply the config with the default value
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
  (om-print "=============================================" "AUDIO")
  (om-print (format nil "Setting audio out on ~s with ~D channels at ~D Hz." 
                    (get-pref-value :audio :output)
                    (get-pref-value :audio :out-channels)
                    (get-pref-value :audio :samplerate))
            "AUDIO")
  (juce::setdevices *juce-player* 
                    "" 0 ;; default/0 channels for input
                    (get-pref-value :audio :output)
                    (get-pref-value :audio :out-channels)
                    (get-pref-value :audio :samplerate)
                    (get-pref-value :audio :buffersize)
                    )
  (configure-audio-channels (get-pref-value :audio :channels-config))
  )

; (juce::getCurrentDeviceType *juce-player*)
; (juce::audio-driver-output-devices *juce-player* "Windows Audio")
;;; when this function si called the preferences are set to their default or saved values
(defun open-juce-player ()
  (setq *juce-player* (juce::openAudioManager))

  (unless *audio-driver* 
    (loop for ad in (juce::get-audio-drivers *juce-player*)
          while (not *audio-driver*)
          do (setf *audio-driver* ad))
    )

  (if *audio-driver*
      (progn 
        (om-print (format nil "Selecting default audio driver: \"~A\"." *audio-driver*) "AUDIO")
        (juce::setDeviceType *juce-player* *audio-driver*)
        (setup-audio-device))
    (om-beep-msg "ERROR OPENING AUDIO: Could not find any audio driver."))
  )

(defun close-juce-player ()
  (juce::closeAudioManager *juce-player*)
  (setf *juce-player* nil))

;;; CONVENTION: 
;;; (nth n list) = target for channel n in the sounds :
;;; c >= 1 : play on channel c
;;; c = 0 : play on its original channel
;;; c = -1 : mute
(defun configure-audio-channels (list)
  (om-print (format nil "Routing channels:") "AUDIO")
  (unless list (om-print "[reset]" "AUDIO"))
  (let* ((available-channels (juce::getoutputchannelslist *juce-player*))
         (checked-list
            (loop for to in list 
                  for from = 1 then (+ from 1) collect
                  (cond 
                   
                   ((or (null to) (not (integerp to))) 
                    (om-print (format nil "   ~D => X" from) "AUDIO")
                    -1) ;; don't play this channel
                   
                   ((and (find to available-channels :test '=)
                         (<= to (get-pref-value :audio :out-channels)))
                    (om-print (format nil "   ~D => ~D" from to) "AUDIO")
                    to) ;; ok
                   
                   (t ;;; 'to' channel not available 
                      (let ((to2 (if (and (find from available-channels :test '=)
                                          (<= from (get-pref-value :audio :out-channels)))
                                     from -1) ;; keep on same channel ('from') if available or mute
                                 ))
                        (om-print-format 
                         "~D => ERROR: channel ~D not available/enabled => ~A" 
                         (list from to (if (minusp to2) "X" to2))
                         "AUDIO SETUP")
                        to2)
                      ))))
         )
      (juce::setoutputchannels *juce-player* checked-list)))

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
;(juce::setAudioSourceGain *testbp1* 0.2)
;(juce::setAudioSourceGain *testbp2* 0.2)

;(defun testgainmono (a n)  (juce::setAudioSourceGain *testbp1* n))
;(defun testgainstereo (a n)  (juce::setAudioSourceGain *testbp2* n))



