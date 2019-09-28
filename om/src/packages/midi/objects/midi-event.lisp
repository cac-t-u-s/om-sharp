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

(defvar *midi-event-types* '(("Note" :Note)
			     ("KeyOn " :KeyOn)
			     ("KeyOff" :KeyOff)
			     ("KeyPress" :KeyPress)
			     ("CtrlChange" :CtrlChange)
			     ("ProgChange" :ProgChange)
			     ("ChanPress" :ChanPress)
			     ("PitchWheel/PitchBend" :PitchBend)
			     ("SongPos" :SongPos)
			     ("SongSel" :SongSel)
			     ("Clock" :Clock)
			     ("Start" :Start)
			     ("Continue" :Continue)
			     ("Stop" :Stop)
			     ("Tune" :Tune)
			     ("ActiveSens" :ActiveSens)
			     ("Reset" :Reset)
			     ("SysEx" :SysEx)
			     ("Stream" :Stream)
			     ("Private" :Private)
			     ("Process" :Process)
			     ("DProcess" :DProcess)
			     ("QFrame" :QFrame)
			     ("Ctrl14b" :Ctrl14b)
			     ("NonRegParam" :NonRegParam)
			     ("RegParam" :RegParam)
			     ("SeqNum" :SeqNum)
			     ("Textual" :Textual)
			     ("Copyright" :Copyright)
			     ("SeqName" :SeqName)
			     ("InstrName" :InstrName)
			     ("Lyric" :Lyric)
			     ("Marker" :Marker)
			     ("CuePoint" :CuePoint)
			     ("ChanPrefix" :ChanPrefix)
			     ("EndTrack" :EndTrack)
			     ("Tempo" :Tempo)
			     ("SMPTEOffset" :SMPTEOffset)
			     ("TimeSign" :TimeSign)
			     ("KeySign" :KeySign)
			     ("Specific" :Specific)
                             ))

;;;============================
;;; MIDI-EVENT AS A DATA-FRAME (see data-stream container)
;;; A high-lev / graphical class in OM, representing a MIDI event from the MIDI-API
;;;============================

(defclass* midievent (data-frame)
  ((onset :accessor onset :initform 0 
          :initarg :onset :initarg :date :initarg :ev-date ;;; different possible initargs (for compatibility)
          :documentation "date/time of the object")
   (ev-type :accessor ev-type :initarg :ev-type :initform :keyon :documentation "type of event")
   (ev-chan :accessor ev-chan :initarg :ev-chan :initform 1 :documentation "MIDI channel (1-16)")
   (ev-track :accessor ev-track :initarg :ev-track :initform 0 :documentation "Track of the MIDI evevnt")
   (ev-value :accessor ev-value 
             :initarg :ev-value :initarg :ev-fields 
             :initform 0 :documentation "value(s)")
   (ev-port :accessor ev-port :initarg :ev-port :initform 0 :documentation "Target MIDI port")))


(defmethod get-frame-action ((self midievent))
  #'(lambda () (om-midi::midi-send-evt 
                (om-midi:make-midi-evt 
                 :type (ev-type self) 
                 :chan (ev-chan self) 
                 :fields (ev-value self)
                 :port (or (ev-port self) (get-pref-value :midi :out-port))
                 ))
      ))

(defmethod data-frame-text-description ((self midievent))
  (list "MIDI EVENT" (format nil "~A (~A): ~A" (ev-type self) (ev-chan self) (ev-value self))))


;======================================
; MIDI-IMPORT
;======================================

;;; converts a list of MIDI-EVT struct to MIDIEVENTS instance
(defmethod* get-midievents ((evtlist list) &optional test)
  (remove nil
          (loop for event in evtlist collect 
                (let ((om-event
                       (cond ((om-midi::midi-evt-p event)
                              (make-instance 'MIDIEvent 
                                             :onset (om-midi::midi-evt-date event)
                                             :ev-type (om-midi::midi-evt-type event) 
                                             :ev-chan (om-midi::midi-evt-chan event)
                                             :ev-value (om-midi:midi-evt-fields event) 
                                             :ev-port (om-midi:midi-evt-port event) 
                                             :ev-track (om-midi:midi-evt-ref event)))
                             ((typep event 'MIDIEvent)
                              (om-copy event))
                             (t (om-beep-msg "ERROR Unknown event: ~A" event)))))
                  (when (and om-event 
                             (or (null test) 
                                 (funcall test om-event)))
                    om-event)))
          ))

;======================================
; Test functions for MIDI events
;======================================

(defmethod* test-date ((self midievent) tmin tmax)
  :initvals '(nil nil nil)
  :indoc '("a MIDIevent" "min date" "max date")
  :doc "Tests if <self> falls between <tmin> (included) and <tmax> (excluded)."
  :icon :midi-filter 
  (and (or (not tmin) (>= (onset self) tmin))
       (or (not tmax) (< (onset self) tmax))))
  

(defmethod* test-midi-channel ((self MidiEvent) channel)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "MIDI channel number (1-16) or channel list")
  :doc "Tests if <self> is in channel <channel>."
  :icon :midi-filter 
  (or (not channel) (member (ev-chan self) (list! channel))))

(defmethod* test-midi-port ((self MidiEvent) port)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "output port number (or list)")
  :doc "Tests is <self> ouputs to <port>."
  :icon :midi-filter 
  (or (not port) (member (ev-port self) (list! port))))

(defmethod* test-midi-track ((self MidiEvent) track)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a track number or list")
  :doc "Tests <self> is on <track>."
  :icon :midi-filter 
  (or (not track) (member (ev-track self) (list! track))))


(defmethod* test-midi-type ((self MidiEvent) type)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a MIDI event type") 
  :menuins (list (list 1 *midi-event-types*))
  :doc "Tests if <self> is of type <type>.

(see function MS-EVENT for a list of valid MIDI event types)
"
  :icon :midi-filter 
  (or (not type)
      (if (symbolp type)
          (= (ev-type self) type)
        (member (ev-type self) (list! type)))))


(defmethod* midi-filter ((self MidiEvent) type track port channel)
  :initvals '(nil nil nil nil nil)
  :indoc '("a MIDIEvent" "event type(s)" "track number(s)" "output port(s)" "MIDI channel(s)")
  :doc "Tests the attributes of <self>.

Returns T if <self> matches <type> (see function MS-EVENT for a list of valid MIDI event types), <ref>, <port> and <channel>.

If a test value is NIL, the test is not performed on this attribute.

"
  :icon :midi-filter 
  (and (or (not type) (member (ev-type self) (list! type)))
       (or (not track) (member (ev-track self) (list! track)))
       (or (not port) (member (ev-port self) (list! port)))
       (or (not channel) (member (ev-chan self) (list! channel)))))



