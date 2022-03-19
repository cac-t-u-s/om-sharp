;============================================================================
; om#: visual programming language for computer-assisted music composition
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


;;; table is one of teh global variables below
(defun name-to-number (name table)
  (cadr (find name table :key 'car :test 'string-equal)))

(defun number-to-name (num table)
  (or (car (find num table :key 'cadr :test '=)) "Undefined"))


;======================
; LSB/MSP UTILS
;======================

;=== tests if a controller num corresponds
;=== to LSB value of another one
(defun lsb-controller (ctrlNum)
  (and (>= ctrlNum 32) (<= ctrlNum 63)))


;=== gets MSB from a 14bits value
(defun msb (value)
  (floor (/ value 128)))

;=== gets LSB from a 14bits value
(defun lsb (value)
  (- value (* (msb value) 128)))

;=== decomposes a value in two 7 bytes blocks
(defun val2lsbmsb (value)
  (let ((msb (msb value)))
    (list (- value (* msb 128)) msb)))

;=== Converts msb lsb to a value
(defun msb-lsb2value (msb lsb)
  (+ lsb (* 128 msb)))

;; 7 bits to 14 bits
;; 7b  = 0-127
;; 14b = 0-16383
(defun 7b-to-14b (v)
  (* v 128))
;(round (* (/ pb 127) 16383)))


;======================
; PITCHBEND/WHEEL
;======================

(defmethod* mc-to-pitchwheel ((midic number) &optional (pw-range 200))
  :initvals '(0 200)
  :indoc '("value in midicents" "variation range")
  :doc "Outputs a MIDI PitchWheel value corresponding to a variation of <midic> (midicents).

<pw-range> is the maximum pitch-wheel range (can vary depending on synthesizers)."
  :icon :midi
  (cond ((zerop midic) 8192)
        ((minusp midic) (round (+ 8192 (* (/ midic pw-range) 8192))))
        (t (floor (+ 8192 (* (/ midic pw-range) 8191))))))

; (mc-to-pitchwheel 50)

(defmethod* mc-to-pitchwheel ((midic list) &optional (pw-range 200))
  (mapcar #'(lambda (n) (mc-to-pitchwheel n pw-range)) midic))


;;; pb = (0-127)
;;; total range = +/- 200 midicents
(defun pitchbend-to-mc (pb)
  (- (round (* pb 400) 127) 200))

;;; pw = (0-16383)
;;; total range = +/- 200 midicents
(defun pitchwheel-to-mc (pw)
  (- (round (* pw 400) 16383) 200))


;======================
; CONTINUOUS CONTROLLERS
;======================

(defvar *midi-controllers*
  '(("BankSelect" 0)
    ("ModulationWheel" 1)
    ("BreathController" 2)
    ("Undefined" 3)
    ("FootController" 4)
    ("PortamentoTime" 5)
    ("DataEntryMSB" 6)
    ("ChannelVolume" 7)
    ("Balance" 8)
    ("Undefined" 9)
    ("Pan" 10)
    ("ExpressionController" 11)
    ("EffectControl1" 12)
    ("EffectControl2" 13)
    ("Undefined" 14)
    ("Undefined" 15)
    ("GeneralPurposeController1" 16)
    ("GeneralPurposeController2" 17)
    ("GeneralPurposeController3" 18)
    ("GeneralPurposeController4" 19)
    ("BankSelectFine" 32)
    ("ModulationWheelFine" 33)
    ("BreathControllerFine" 34)
    ("Ctrl03 (Undefined) Fine" 35)
    ("FootControllerFine" 36)
    ("PortamentoTimeFine" 37)
    ("DataEntryMSBLSB" 38)
    ("ChannelVolumeFine" 39)
    ("BalanceFine" 40)
    ("Ctrl09 (Undefined) Fine" 41)
    ("PanFine" 42)
    ("ExpressionControllerFine" 43)
    ("EffectControl1Fine" 44)
    ("EffectControl2Fine" 45)
    ("Ctrl14 (Undefined) Fine" 46)
    ("Ctrl15 (Undefined) Fine" 47)
    ("GeneralPurposeController1Fine" 48)
    ("GeneralPurposeController2Fine" 49)
    ("GeneralPurposeController3Fine" 50)
    ("GeneralPurposeController4Fine" 51)
    ("DamperPedal" 64)
    ("Portamento" 65)
    ("Sustenuto" 66)
    ("SoftPedal" 67)
    ("LegatoFootswitch" 68)
    ("Hold2" 69)
    ("SoundController1" 70)
    ("SoundController2" 71)
    ("SoundController3" 72)
    ("SoundController4" 73)
    ("SoundController5" 74)
    ("SoundController6" 75)
    ("SoundController7" 76)
    ("SoundController8" 77)
    ("SoundController9" 78)
    ("SoundController10" 79)
    ("PortamentoControl" 84)
    ("Effects1Depth" 91)
    ("Effects2Depth" 92)
    ("Effects3Depth" 93)
    ("Effects4Depth" 94)
    ("Effects5Depth" 95)
    ("DataIncrement" 96)))



(defmethod* midi-control-change (ctrl)
  :initvals '(nil)
  :indoc '("control name")
  :menuins `((0 ,*midi-controllers*))
  :doc "Outputs the MIDI ControlChange number corresponding to <ctrl>."
  :icon :midi
  (cond ((stringp ctrl)
         (name-to-number ctrl *midi-controllers*))
        ((numberp ctrl) ;; eg just coming from teh menuins
         ctrl)
        (t nil)))



;==============================
; GENERAL MIDI
;==============================

;=== General MIDI programs with program numbers
(defvar *midi-gm-programs* '(("0 - Acoustic Grand Piano" 0)
                             ("1 - Bright Acoustic Piano" 1)
                             ("2 - Electric Grand Piano" 2)
                             ("3 - Honki Tonk Piano" 3)
                             ("4 - Electric Piano 1" 4)
                             ("5 - Electric Piano 2" 5)
                             ("6 - Harpsichord" 6)
                             ("7 - Clavinet" 7)
                             ("8 - Celesta" 8)
                             ("9 - Glockenspiel" 9)
                             ("10 - Music Box" 10)
                             ("11 - Vibraphone" 11)
                             ("12 - Marimba" 12)
                             ("13 - Xylophone" 13)
                             ("14 - Tubular bells" 14)
                             ("15 - Dulcimer" 15)
                             ("16 - Drawbar Organ" 16)
                             ("17 - Percussive Organ" 17)
                             ("18 - Rock Organ" 18)
                             ("19 - Church Organ" 19)
                             ("20 - Reed Organ" 20)
                             ("21 - Accordion" 21)
                             ("22 - Harmonica" 22)
                             ("23 - Tango Accordion" 23)
                             ("24 - Nylon Acoustic Guitar" 24)
                             ("25 - Steel Acoustic Guitar" 25)
                             ("26 - Jazz Electric Guitar" 26)
                             ("27 - Clean Electric Guitar" 27)
                             ("28 - Muted Electric Guitar" 28)
                             ("29 - Overdrive Guitar" 29)
                             ("30 - Distorted Guitar" 30)
                             ("31 - Guitar Harmonics" 31)
                             ("32 - Acoustic Bass" 32)
                             ("33 - Electric Fingered Bass" 33)
                             ("34 - Electric Picked Bass" 34)
                             ("35 - Fretless Bass" 35)
                             ("36 - Slap Bass 1" 36)
                             ("37 - Slap Bass 2" 37)
                             ("38 - Synth Bass 1" 38)
                             ("39 - Synth Bass 2" 39)
                             ("40 - Violin" 40)
                             ("41 - Viola" 41)
                             ("42 - Cello" 42)
                             ("43 - Contrabass" 43)
                             ("44 - Tremolo Strings" 44)
                             ("45 - Pizzicato Strings" 45)
                             ("46 - Orchestral Harp" 46)
                             ("47 - Timpani" 47)
                             ("48 - String Ensemble 1" 48)
                             ("49 - String Ensemble 2" 49)
                             ("50 - Synth Strings 1" 50)
                             ("51 - Synth Strings 2" 51)
                             ("52 - Choir Aahs" 52)
                             ("53 - Voice Oohs" 53)
                             ("54 - Synth Voice" 54)
                             ("55 - Orchestra Hit" 55)
                             ("56 - Trumpet" 56)
                             ("57 - Trombone" 57)
                             ("58 - Tuba" 58)
                             ("59 - Muted Trumpet" 59)
                             ("60 - French Horn" 60)
                             ("61 - Brass Section" 61)
                             ("62 - Synth Brass 1" 62)
                             ("63 - Synth Brass 2" 63)
                             ("64 - Soprano Sax" 64)
                             ("65 - Alto Sax" 65)
                             ("66 - Tenor Sax" 66)
                             ("67 - Baritone Sax" 67)
                             ("68 - Oboe" 68)
                             ("69 - English Horn" 69)
                             ("70 - Bassoon" 70)
                             ("71 - Clarinet" 71)
                             ("72 - Piccolo" 72)
                             ("73 - Flute" 73)
                             ("74 - Recorder" 74)
                             ("75 - Pan Flute" 75)
                             ("76 - Bottle Blow" 76)
                             ("77 - Shakuhachi" 77)
                             ("78 - Whistle" 78)
                             ("79 - Ocarina" 79)
                             ("80 - Syn Square Wave" 80)
                             ("81 - Syn Sawtooth Wave" 81)
                             ("82 - Syn Calliope" 82)
                             ("83 - Syn Chiff" 83)
                             ("84 - Syn Charang" 84)
                             ("85 - Syn Voice" 85)
                             ("86 - Syn Fifths Sawtooth w-Wave" 86)
                             ("87 - Syn Brass and Lead" 87)
                             ("88 - New Age Syn Pad" 88)
                             ("89 - Warm Syn Pad" 89)
                             ("90 - Polysynth Syn Pad" 90)
                             ("91 - Choir Syn Pad" 91)
                             ("92 - Bowed Syn Pad" 92)
                             ("93 - Metal Syn Pad" 93)
                             ("94 - Halo Syn Pad" 94)
                             ("95 - Sweep Syn Pad" 95)
                             ("96 - FX Rain" 96)
                             ("97 - FX Soundtrack" 97)
                             ("98 - FX Crystal" 98)
                             ("99 - FX Atmosphere" 99)
                             ("100 - FX Brightness" 100)
                             ("101 - FX Goblins" 101)
                             ("102 - FX Echoes" 102)
                             ("103 - FX Sci-fi" 103)
                             ("104 - Sitar" 104)
                             ("105 - Banjo" 105)
                             ("106 - Shamisen" 106)
                             ("107 - Koto" 107)
                             ("108 - Kalimba" 108)
                             ("109 - Bag Pipe" 109)
                             ("110 - Fiddle" 110)
                             ("111 - Shanai" 111)
                             ("112 - Tinkle Bell" 112)
                             ("113 - Agogo" 113)
                             ("114 - Steel Drums" 114)
                             ("115 - Woodblock" 115)
                             ("116 - Taiko Drum" 116)
                             ("117 - Melodic Tom" 117)
                             ("118 - Syn Drum" 118)
                             ("119 - Reverse Cymbal" 119)
                             ("120 - Guitar Fret Noise" 120)
                             ("121 - Breath Noise" 121)
                             ("122 - Seashore" 122)
                             ("123 - Bird Tweet" 123)
                             ("124 - Telephone Ring" 124)
                             ("125 - Helicopter" 125)
                             ("126 - Applause" 126)
                             ("127 - Gun Shot" 127)))



; Pitches for General MIDI drum notes (channel 10)
(defvar *midi-gm-drum-notes* '(("Acoustic Bass Drum" 35)
                               ("Bass Drum 1" 36)
                               ("Side Stick" 37)
                               ("Acoustic Snare" 38)
                               ("Hand Clap" 39)
                               ("Electric Snare" 40)
                               ("Low Floor Tom" 41)
                               ("Closed Hi Hat" 42)
                               ("High Floor Tom" 43)
                               ("Pedal Hi Hat" 44)
                               ("Low Tom" 45)
                               ("Open Hi Hat" 46)
                               ("Low Mid Tom" 47)
                               ("High Mid Tom" 48)
                               ("Crash Cymbal 1" 49)
                               ("High Tom" 50)
                               ("Ride Cymbal 1" 51)
                               ("Chinese Cymbal" 52)
                               ("Ride Bell" 53)
                               ("Tambourine" 54)
                               ("Splash Cymbal" 55)
                               ("Cowbell" 56)
                               ("Crash Cymbal 2" 57)
                               ("Vibraslap" 58)
                               ("Ride Cymbal 2" 59)
                               ("High Bongo" 60)
                               ("Low Bongo" 61)
                               ("Mute High Conga" 62)
                               ("Open High Conga" 63)
                               ("Low Conga" 64)
                               ("High Timbale" 65)
                               ("Low Timbale" 66)
                               ("High Agogo" 67)
                               ("Low Agogo" 68)
                               ("Cabasa" 69)
                               ("Maracas" 70)
                               ("Short Whistle" 71)
                               ("Long Whistle" 72)
                               ("Short Guiro" 73)
                               ("Long Guiro" 74)
                               ("Claves" 75)
                               ("High Wood Block" 76)
                               ("Low Wood Block" 77)
                               ("Mute Cuica" 78)
                               ("Open Cuica" 79)
                               ("Mute Triangle" 80)
                               ("Open Triangle" 81)))



;==================================
; SELECTION BOXES FOR OM

(defmethod* gm-program (program-name)
  :initvals '(nil)
  :indoc '("Instrument name")
  :menuins `((0 ,*midi-gm-programs*))
  :doc "Outputs the General MIDI program number corresponding to <program-name>."
  :icon :synth
  program-name)


(defmethod! GM-DrumNote (drumName &optional (midicents t))
  :initvals '(nil)
  :indoc '("Drum name")
  :menuins `((0 ,*midi-gm-drum-notes*))
  :doc "Outputs key value corresponding to <drumname> (in General MIDI standard)."
  :icon :drum
  (if midicents (* drumName 100) drumName)
  )




