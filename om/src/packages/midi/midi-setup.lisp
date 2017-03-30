
(in-package :om)

(defun show-midi-setup ()
  (om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil nil)))
