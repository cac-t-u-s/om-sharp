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
(defvar *juce-player-n-channels* 2)

(defun open-juce-player (&optional (sr 44100))
  (progn
    (setf *juce-player* (juce::OpenAudioPlayer *juce-player-n-channels* *juce-player-n-channels* sr))))

(defun close-juce-player ()
  (juce::closeaudioplayer *juce-player*)
  (setf *juce-player* nil))

(om-add-init-fun 'open-juce-player)