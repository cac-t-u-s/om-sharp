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


(let ((colors (loop for h from 0 to 1 by (/ 1 32) collect (om-make-color-hsv h .8 .78))))

  (add-preference :midi :midi-ch1-color " 1" :color (nth 0 colors))
  (add-preference :midi :midi-ch2-color " 2" :color (nth 3 colors))
  (add-preference :midi :midi-ch3-color " 3" :color (nth 5 colors))
  (add-preference :midi :midi-ch4-color " 4" :color (nth 7 colors))
  (add-preference :midi :midi-ch5-color " 5" :color (nth 16 colors))
  (add-preference :midi :midi-ch6-color " 6" :color (nth 18 colors))
  (add-preference :midi :midi-ch7-color " 7" :color (nth 21 colors))
  (add-preference :midi :midi-ch8-color " 8" :color (nth 24 colors))
  (add-preference :midi :midi-ch9-color " 9" :color (nth 29 colors))
  (add-preference :midi :midi-ch10-color "10" :color (om-def-color :lightsteelblue4))
  (add-preference :midi :midi-ch11-color "11" :color (om-def-color :darkgreen))
  (add-preference :midi :midi-ch12-color "12" :color (om-def-color :yellow4))
  (add-preference :midi :midi-ch13-color "13" :color (om-def-color :burlywood4))
  (add-preference :midi :midi-ch14-color "14" :color (om-def-color :lightpink4))
  (add-preference :midi :midi-ch15-color "15" :color (om-def-color :saddlebrown))
  (add-preference :midi :midi-ch16-color "16" :color (om-def-color :dark-gray))
  
  )

;; (color::get-all-color-names)

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
