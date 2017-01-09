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
(defvar *juce-input-devices* nil)
(defvar *juce-output-devices* nil)

(setf *juce-input-devices* nil)

(add-preference-module :audio "Audio")
(add-preference :audio :input "Input device" *juce-input-devices* (car *juce-input-devices*))
(add-preference :audio :input "Output device" *juce-output-devices* (car *juce-output-devices*))
(add-preference :audio :apply "Apply" :action 'om-beep)

(get-pref-value :audio :input)

(defun open-juce-player ()
  (setq *juce-player* (juce::OpenAudioPlayer)
        *juce-input-devices* (loop for i from 0 to (1- (juce::getinputdevicescount *juce-player*))
                                   collect
                                   (cffi:foreign-string-to-lisp (fli:dereference (juce::getavailableinputdevices *juce-player*)
                                                                                 :index i :type :pointer)))
        *juce-output-devices* (loop for i from 0 to (1- (juce::getoutputdevicescount *juce-player*))
                                    collect
                                    (cffi:foreign-string-to-lisp (fli:dereference (juce::getavailableoutputdevices *juce-player*)
                                                                                  :index i :type :pointer)))))

(defun set-juce-device (input-device-index output-device-index sample-rate)
  (cffi::with-foreign-pointer-as-string (str 255)
    (juce::setaudiodevice *juce-player* 
                          (cffi::lisp-string-to-foreign (nth input-device-index *juce-input-devices*)
                                                        str (1+ (length (nth input-device-index *juce-input-devices*))))
                          (cffi::lisp-string-to-foreign (nth output-device-index *juce-output-devices*)
                                                        str (1+ (length (nth output-device-index *juce-output-devices*)))) 
                          *juce-player-in-channels* 
                          *juce-player-out-channels*
                          sample-rate)))





;(set-juce-device 0 0 44100) A APPELER



;(listen *terminal-io*) (defun testget (a) (setq *testbp* (bp-pointer (buffer-player a))))

;(juce::setgainreader *testbp* 1.0)

(defun close-juce-player ()
  (juce::closeaudioplayer *juce-player*)
  (setf *juce-player* nil))

(om-add-init-fun 'open-juce-player)