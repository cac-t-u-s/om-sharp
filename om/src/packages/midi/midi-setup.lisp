
(in-package :om)

(defun midi-get-ports-settings ()
  (let ((newsetup (om-midi::portmidi-setup (get-pref-value :midi :ports))))
    (when newsetup 
      (set-pref :midi :ports newsetup)
      t)))

(defun midi-apply-ports-settings ()
  (om-midi::portmidi-connect-ports (get-pref-value :midi :ports)))

(defun midi-setup ()
  (when (midi-get-ports-settings)
    (midi-apply-ports-settings)))


(add-preference-module :midi "MIDI")

(add-preference-section :midi "In/Out Ports")
(add-preference :midi :ports "Configuration" :action 'midi-setup)

(add-preference-section :midi "Channel colors")
(add-preference :midi :midi-ch1-color " 1" :color (om-random-color))
(add-preference :midi :midi-ch2-color " 2" :color (om-random-color))
(add-preference :midi :midi-ch3-color " 3" :color (om-random-color))
(add-preference :midi :midi-ch4-color " 4" :color (om-random-color))
(add-preference :midi :midi-ch5-color " 5" :color (om-random-color))
(add-preference :midi :midi-ch6-color " 6" :color (om-random-color))
(add-preference :midi :midi-ch7-color " 7" :color (om-random-color))
(add-preference :midi :midi-ch8-color " 8" :color (om-random-color))
(add-preference :midi :midi-ch9-color " 9" :color (om-random-color))
(add-preference :midi :midi-ch10-color "10" :color (om-def-color :gray))
(add-preference :midi :midi-ch11-color "11" :color (om-random-color))
(add-preference :midi :midi-ch12-color "12" :color (om-random-color))
(add-preference :midi :midi-ch13-color "13" :color (om-random-color))
(add-preference :midi :midi-ch14-color "14" :color (om-random-color))
(add-preference :midi :midi-ch15-color "15" :color (om-random-color))
(add-preference :midi :midi-ch16-color "16" :color (om-random-color))

(defun get-midi-channel-color (n)
  (case n  
    (1 (get-pref-value :midi :midi-ch1-color))
    (2 (get-pref-value :midi :midi-ch2-color))
    (3 (get-pref-value :midi :midi-ch3-color))
    (4 (get-pref-value :midi :midi-ch4-color))
    (5 (get-pref-value :midi :midi-ch5-color))
    (6 (get-pref-value :midi :midi-ch6-color))
    (7 (get-pref-value :midi :midi-ch7-color))
    (8 (get-pref-value :midi :midi-ch8-color))
    (9 (get-pref-value :midi :midi-ch9-color))
    (10 (get-pref-value :midi :midi-ch10-color))
    (11 (get-pref-value :midi :midi-ch11-color))
    (12 (get-pref-value :midi :midi-ch12-color))
    (13 (get-pref-value :midi :midi-ch13-color))
    (14 (get-pref-value :midi :midi-ch14-color))
    (15 (get-pref-value :midi :midi-ch15-color))
    (16 (get-pref-value :midi :midi-ch16-color))
    ))
