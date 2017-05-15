
(in-package :om)

(defun midi-setup ()
  (let ((newsetup (om-midi::portmidi-setup (get-pref-value :midi :ports))))
    (when newsetup 
      (om-midi::portmidi-connect-ports newsetup)
      (set-pref :midi :ports newsetup))))

(add-preference-module :midi "MIDI")
(add-preference :midi :ports "Ports configuration" :action 'midi-setup)


