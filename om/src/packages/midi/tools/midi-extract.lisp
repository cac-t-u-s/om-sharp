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


;=================
; MIDI NOTES
;=================

(defmethod* mf-info ((self midi-track) &optional (tracknum nil))
  :initvals '(nil nil)
  :icon :midi
  :indoc '("a MIDI-TRACK object" "a track number or nil")
  :doc "Converts a MIDI-TRACK object into a symbolic description.
 The result of mf-info is a list of tracks. Each track is a list of notes. 
 Each note is a list of parameters in the form :

 (midi-number (pitch) , onset-time(ms), duration(ms), velocity, channel)

 optional <tracknum> (a number in 0-15) allows to choose a single track."
  :icon :midi
  (if tracknum
      (loop for evt in (midi-events self) 
            when (and (equal :note (ev-type evt))
                      (and (numberp (ev-track evt)) (= tracknum (ev-track evt))))
            collect (list (midinote-pitch evt)
                          (midinote-onset evt)
                          (midinote-dur evt)
                          (midinote-vel evt)
                          (midinote-channel evt)))
    (let ((tracks (make-list (1+ (list-max
                                  (loop for evt in (midi-events self) 
                                        when (equal :note (ev-type evt))
                                        collect (or (ev-track evt) 0))))
                             :initial-element (list nil))))
      (loop for evt in (midi-events self) 
            when (equal :note (ev-type evt))
            do (let ((track (or (ev-track evt) 0)))
                 (setf (nth track tracks) 
                       (append (nth track tracks)
                               (list (list (midinote-pitch evt)
                                           (midinote-onset evt)
                                           (midinote-dur evt)
                                           (midinote-vel evt)
                                           (midinote-channel evt))
                                     )))
                 ))
      tracks)
  ))
           
;;; same
(defmethod* get-midi-notes ((self midi-track))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :icon :midi
  :doc "Extracts and returns the notes from <self>.

The result is a list of lists where each first level list represents a MIDI track and contains a list of note values.
Note values are lists of (pitch date dur vel chan).

"
  :icon :midi
  (mf-info self))



;=================
; MIDI LYRICS / TEXT
;=================

(defun isTextual (type)
  (find type '(:Textual 
               :Copyright
               :SeqName
               :InstrName
               :Lyric
               :Marker
               :CuePoint)
        :test 'equal))

;=== converts to string the slot "fields" of a textual MidiEvent

(defmethod! convert-textinfo ((self MidiEvent))
    :indoc '("a MIDIEvent or list of MIDIEvents")
    :icon :midi-filter
    :doc "
Returns the MIDIEvent or list after converting to string the data (ev-field) of all textual events (e.g. types 'textual', 'copyright', 'lyrics', 'instrname', etc.)
"
    (let ((newEvt (om-copy self)))
    (if (istextual (ev-type self))
      (setf (ev-value newEvt) (list2string (ev-fields self))))
  newEvt))


;=== Converts an integer (ascii codes) list into a string
(defun list2string (list)
  (let ((rep ""))
    (loop for item in list do
          (if (not (= 10 item))
          (setf rep (string+ rep (string (code-char item))))))
    rep))


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



(defmethod! get-mf-lyrics ((self midi-track))
  :initvals '(nil) 
  :indoc '("a MIDI file or sequence") 
  :numouts 2
  :doc "Extracts lyrics (event type 'Lyric') from <self>.

The second output returns the corresponding dates"
  :icon :write
  (let ((rep
         (mat-trans (loop for evt in (fileseq self)  
                          when (equal (om-midi::midi-evt-type evt) :Lyric)
                          collect (list (if (stringp (car (om-midi::midi-evt-fields evt))) (om-midi::midi-evt-fields evt) (list2string (om-midi::midi-evt-fields evt)))
                                        (om-midi::midi-evt-date evt)))
                    )))
    (values (car rep) (cadr rep))))
