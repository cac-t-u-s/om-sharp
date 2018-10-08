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

;;;============================
;;; MIDI-EVENT AS A DATA-FRAME (see data-stream container)
;;;============================
(defclass* midi-event (data-frame)
  ((date :accessor date :initarg :date :initform 0 :documentation "time of the frame")
   (ev-type :accessor ev-type :initarg :ev-type :initform nil :documentation "type of event")
   (ev-channel :accessor ev-channel :initarg :ev-channel :initform 1 :documentation "MIDI channel (1-16)")
   (ev-value :accessor ev-value :initarg :ev-value :initform 0 :documentation "value(s)")
   (midi-port :accessor midi-port :initarg :midi-port :initform 0 :documentation "Target MIDI port")))


(defmethod get-frame-action ((self midi-event))
  #'(lambda () (om-midi::midi-send-evt 
                (om-midi:make-midi-evt :type (ev-type self) 
                                       :chan (ev-channel self) 
                                       :fields (ev-value self)
                                       :port (midi-port self)))
      ))

(defmethod data-frame-text-description ((self midi-event))
  (list "MIDI EVENT" (format nil "~A (~A): ~A" (ev-type self) (ev-channel self) (ev-value self))))




#|

;======================================
; Test functions for MIDI events
;======================================
(defmethod* test-date ((self midi-event) tmin tmax)
  :initvals '(nil nil nil)
  :indoc '("a MIDI-event" "min date" "max date")
  :doc "Tests if <self> falls between <tmin> and <tmax>."
  :icon 907 
  (and (or (not tmin)(>= (ev-date self) tmin))
       (or (not tmax)(< (ev-date self) tmax))))
  

(defmethod! test-channel ((self MidiEvent) channel)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "MIDI channel number (1-16) or channel list")
  :doc "Tests if <self> is in channel <channel>."
  :icon 907 
  (or (not channel) (member (ev-chan self) (list! channel))))

(defmethod! test-port ((self MidiEvent) port)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "output port number (or list)")
  :doc "Tests is <self> ouputs to <port>."
  :icon 907 
  (or (not port) (member (ev-port self) (list! port))))

(defmethod! test-ref ((self MidiEvent) ref)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a track number or list")
  :doc "Tests <self> is on track <ref>."
  :icon 907 
  (or (not ref) (member (ev-ref self) (list! ref))))

(defmethod! test-Track ((self MidiEvent) track)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a track number or list")
  :doc "Tests <self> is on track <ref>."
  :icon 907 
  (test-ref self track))

(defmethod! test-Type ((self MidiEvent) type)
  :initvals '(nil nil)
  :indoc '("a MidiEvent" "a MIDI event type") 
  :menuins (list (list 1 *ms-events-symb*))
  :doc "Tests if <self> is of type <type>.

(see function MS-EVENT for a list of valid MIDI event types)
"
  :icon 907 
  (or (not type)
      (if (symbolp type)
        (= (ev-type self) (om-midi-symb2mtype type))
        (member (ev-type self) (list! type)))))


(defmethod! midi-filter ((self MidiEvent) type ref port channel)
  :initvals '(nil nil nil nil nil)
  :indoc '("a MIDIEvent" "event type(s)" "track number(s)" "output port(s)" "MIDI channel(s)")
  :doc "Tests the attributes of <self>.

Returns T if <self> matches <type> (see function MS-EVENT for a list of valid MIDI event types), <ref>, <port> and <channel>.

If a test value is NIL, the test is not performed on this attribute.

"
  :icon 907 
  (and (or (not type) (member (ev-type self) (mapcar 'om-midi-symb2mtype (list! type))))
                            (or (not ref) (member (ev-ref self) (list! ref)))
                            (or (not port) (member (ev-port self) (list! port)))
                            (or (not channel) (member (ev-chan self) (list! channel)))))


;=== converts to string the slot "fields" of a textual MidiEvent

;;; replaced copy-instance with copy-container
(defmethod! me-textinfo ((self MidiEvent))
    :indoc '("a MIDIEvent or list of MIDIEvents")
    :icon 908
    :doc "
Returns the MIDIEvent or list after converting to string the data (ev-field) of all textual events (e.g. types 'textual', 'copyright', 'lyrics', 'instrname', etc.)
"
  (let ((newEvt (copy-container self)))
    (if (istextual (ev-type self))
      (setf (ev-fields newEvt) (list2string (ev-fields self))))
  newEvt))

;=== converts to string the slot "fields" of textual events from a MidiEvent list
(defmethod! me-textinfo ((self list))
  (let ((rep nil))
    (loop for event in self do
          (if (midievent-p event)
            (progn
              ;(me-textinfo event)
              (push (me-textinfo event) rep)
              ))) 
    (reverse rep)))





(defmethod! get-midievents ((self list) &optional test)
  :icon 902
  (let ((evtList nil) event)
    (loop for listitem in self do
          (if (midievent-p listitem)
              (progn
              (setf event (make-instance 'MidiEvent
                                   :ev-date (ev-date listitem)
                                   :ev-type (ev-type listitem)
                                   :ev-chan (ev-chan listitem)     
                                   :ev-ref (ev-ref listitem)
                                   :ev-port (ev-port listitem)
                                   :ev-fields (ev-fields listitem)
                                   ))
              (if (or (not test) (funcall test event))
                (push event evtList)
                ))
            (let ((tmpList (get-midievents listItem)))
              (if tmpList (push tmpList evtList)))
            ))
    (flat (reverse evtList))))



(defmethod! separate-channels ((self eventmidi-seq))
  :indoc '("an EventMIDI-seq object")
  :initvals '(nil)
  :doc "Separates MIDI channels in <self> on diferents tacks (modifies the 'lref' slot)."
  :icon 915
  (loop for ch in (Lchan self)
        for i = 0 then (+ i 1) do
        (setf (nth i (Lref self)) ch))
  self)


;=== Returns a complete midi notes (pitch date dur vel chan track port) list
(defmethod evm-seq2midilist ((self eventmidi-seq))
  (let ((midiList nil))
    (loop for date in (Ldate self)
          for type in (Ltype self)
          for param in (Lfields self)
          for ref in (Lref self)
          for port in (Lport self)
          for chan in (Lchan self) do
          (case type
            (0  (push (list (first param) date (third param) (second param) chan ref port) midiList))
            (1 (if (= (second param) 0)
                 (close-notes-on midiList (first param) chan date ref)
                 (push (list (first param) date (* -1 date) (second param) chan ref port) midiList)))
            (2 (close-notes-on midiList (first param) chan date ref))))
    (reverse midiList)))

;=== Ctreates tracks with a list of notes (pitch date dur vel chan track port)
;=== (grouping notes with same track value)
(defun midiList2trackList (midilist)
  (let ((tracks-list nil) (tracks nil) (rep nil) trackNum pos)
  (loop for note in midilist do
        (if (plusp (third note))
          (progn
            (setf trackNum (sixth note))
            (if (member trackNum tracks)
              (progn
                (setf pos (position trackNum tracks))
                ;(push (list (first note) (second note) (third note) (fourth note) (fifth note)) (nth pos tracks-list))
                (push note (nth pos tracks-list))
                )
              (progn
                ;(push (list (list (first note) (second note) (third note) (fourth note) (fifth note))) tracks-list) 
                (push (list note) tracks-list) 
                (push trackNum tracks)
                )))))
  (loop for trk in tracks-list do
        (push (reverse trk) rep))
  rep))

;=== Returns a list of tracks from th EventMidi-seq object
;=== A track is a list of notes (pitch date dur vel chan) 
(defmethod! get-midi-notes ((self eventmidi-seq))
  :initvals '(nil) 
  :indoc '("a MIDI fiule or sequence") 
  :icon 909
  (let ((trackList (midilist2trackList (evm-seq2midiList self))) tmpList rep)
    (loop for track in trackList do
         (setf tmpList (mat-trans track))
         (push (mat-trans (list (first tmpList) (second tmpList) (third tmpList) (fourth tmpList) (fifth tmpList))) rep))
    (reverse rep)))




;=== Creates a list of MidiEvents 
(defmethod! get-midievents ((self Tempo-Map) &optional test)
  :icon 902
  (let ((evtList nil) evt fields)
  (loop for tempoitem in (tempo-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first tempoitem)
          :ev-type (om-midi-get-num-from-type "Tempo")
          :ev-ref 0
          :ev-fields (second tempoItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (loop for timesignitem in (timeSign-Evts self) do
        (setf event (make-instance 'MidiEvent
          :ev-date (first timesignitem)
          :ev-type (om-midi-get-num-from-type "TimeSign")
          :ev-ref 0
          :ev-fields (second timesignItem)))
        (if (or (not test) (funcall test event))
          (push event evtList)))
  (reverse evtList)))


;=== Extract tempo-map from a simple-container
;=== get-midievent method must be defined for this container
(defmethod! get-TempoMap ((self simple-container))
  :initvals '(nil) 
  :indoc '("a musical object or MIDI sequence") 
  :icon 905
  :doc "Extracts and generates a TEMPO-MAP object from <self>."
  (let ((tempoEvents nil)
        (tempoMap (make-instance 'tempo-Map))
        (tempoList nil) (timeSignList nil))
    (setf tempoEvents (get-midievents self #'(lambda (x) (or (test-type x 'tempo) (test-type x 'timeSign)))))
    (loop for event in tempoEvents do
          (cond
           ((= (ev-type event) (om-midi-get-num-from-type "Tempo"))
            (push (list (ev-date event) (first (ev-fields event))) tempoList))
           ((= (ev-type event) (om-midi-get-num-from-type "TimeSign"))
            (push (list (ev-date event) (ev-fields event)) timeSignList))
           (t nil)))
    
    (setf (tempo-Evts tempoMap) (reverse tempoList))
    (setf (timeSign-Evts tempoMap) (reverse timesignList))
    tempoMap))

|#
