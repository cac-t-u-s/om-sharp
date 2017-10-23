;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed; in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================


(in-package :om)


(defclass* chord-seq (container named-object score-object)   
  ((Lmidic :initform (list 6000) :accessor LMidic :initarg :LMidic :type list :documentation "pitches (mc): list or list of lists")
   (LOnset :initform (list  0 1000) :accessor LOnset :initarg :LOnset :type list :documentation "onsets (ms): list")
   (Ldur :initform (list 1000) :accessor Ldur :initarg :Ldur :type list :documentation "durations (ms): list or list of lists")
   (LVel :initform (list 100) :accessor LVel :initarg :LVel :type list :documentation "velocities (0-127): list or list of lists")
   (LOffset :initform (list 0) :accessor LOffset :type list :documentation "offsets (ms): list or list of lists")
   (Lchan :initform (list 1) :accessor Lchan :type list :documentation "MIDI channels (1-16): list or list of lists")
   (legato :initform 0 :accessor legato :type integer :documentation "relative chords duration (0-100)"))
  (:icon 138)
  (:documentation "
A sequence of chords.

Time is expressed in absolute dates and durations. (For rhytmic structures see the VOICE object.)

CHORD-SEQ is defined with:

- <lmidic>: a list of list of pitches (midicents: 100 = 1 half-tone - 6000 = C3); e.g. '((6000 6500) (6100 6700 7100)). Each pitch list is considered as a chord in teh sequence. Single-note sequences can be specified with simple list of pitches, e.g. (6000 61000 6800) ; in this case each pitch will be considered as a single-note chord.
- <lonsets>: a list of onsets in milliseconds (one for each chord). If the list is shorter than the list of pitch, the last interval is repeated.
- <ldur>: a list or list of lists values (in milliseconds) expressing chords durations or note durations inside each chord.
- <lvel>: a list or list of lists of values (MIDI velocity from 0 to 127) expressing chords velocities or note durations inside each chord.
- <loffset>: a list or list of lists of values (milliseconds) expressing note offsets for the notes inside each chord.
- <lchan>: a list or list of lists of values (1-16) expressing MIDI channels for each chord or notes inside each chord.
- <legato>: a number between 0 and 100, indicating the duration of chords as a percentage of inter-onsets time intervals. If different from 0 (the default), the ldur and loffset inputs are ignored, and notes in the chords are formatted with regard to the legato value.

All values (excepted onsets and legato) are returned (in the box outputs) as list of lists (one value for each note, missing values computed from previous notes or chords).

")
  )