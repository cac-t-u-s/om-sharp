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


;=================
; MIDI NOTES
;=================

(defmethod* mf-info ((self midi-track) &optional (tracknum nil))
  :initvals '(nil nil)
  :icon :midi
  :indoc '("a MIDI-TRACK object" "a track number or nil")
  :doc "Converts a MIDI-TRACK object into a list of note infos.

The result is a list of tracks.
Each track is a list of notes.
Each note is a list of parameters in the form : (pitch, onset-time, duration, velocity, channel)

- <tracknum> allows choosing a single track."
  :icon :midi
  :outdoc '("list of list of note infos")
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
                             :initial-element nil)))
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
      (remove nil tracks))
    ))


;;; same with MIDI-NOTEs
(defmethod* get-midi-notes ((self midi-track) &optional (tracknum nil))
  :initvals '(nil)
  :indoc '("a MIDI file or sequence")
  :icon :midi
  :doc "Extracts and returns the MIDI-NOTEs from <self> (a MIDI-TRACK)."
  :icon :midi
  :outdoc '("list of MIDI-NOTEs")

  (when (midi-events self)

    (if tracknum

        (loop for evt in (midi-events self)
              when (and (equal :note (ev-type evt))
                        (and (numberp (ev-track evt)) (= tracknum (ev-track evt))))
              collect (make-midinote :onset (midinote-onset evt)
                                     :pitch (midinote-pitch evt)
                                     :vel (midinote-vel evt)
                                     :dur (midinote-dur evt)
                                     :chan (midinote-channel evt)))

      (let ((tracks (make-list (1+ (or (list-max
                                        (loop for evt in (midi-events self)
                                              when (equal :note (ev-type evt))
                                              collect (or (ev-track evt) 0)))
                                       0))
                               :initial-element nil)))
        (loop for evt in (midi-events self)
              when (equal :note (ev-type evt))
              do (let ((track (or (ev-track evt) 0)))
                   (setf (nth track tracks)
                         (append (nth track tracks)
                                 (list (make-midinote :onset (midinote-onset evt)
                                                      :pitch (midinote-pitch evt)
                                                      :vel (midinote-vel evt)
                                                      :dur (midinote-dur evt)
                                                      :chan (midinote-channel evt))
                                       )))
                   ))
        tracks)
      )))



;=================
; LYRICS / TEXT
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

; Converts an integer (ascii codes) list into a string
(defun list2string (list)
  (let ((rep ""))
    (loop for item in list do
          (if (not (= 10 item))
              (setf rep (string+ rep (string (code-char item))))))
    rep))


; converts to string the slot "fields" of a textual MidiEvent
(defmethod* convert-textinfo ((self MidiEvent))
  :indoc '("a MIDIEvent or list of MIDIEvents")
  :icon :midi-filter
  :doc "
Returns the MIDIEvent or list after converting to string the data (ev-field) of all textual events (e.g. types 'textual', 'copyright', 'lyrics', 'instrname', etc.)
"
  (let ((newEvt (om-copy self)))
    (when (istextual (ev-type self))
      (setf (ev-values newEvt) (list2string (ev-values self))))
    newEvt))

(defmethod* convert-textinfo ((self list))
  (mapcar #'convert-textinfo self))


; compat
(defmethod me-textinfo ((self t)) (convert-textinfo self))


(defmethod* get-mf-lyrics ((self midi-track))
  :initvals '(nil)
  :indoc '("a MIDI file or sequence")
  :numouts 2
  :doc "Extracts lyrics (event type 'Lyric') from <self>.

The second output returns the corresponding dates."
  :icon :write
  :outdoc '("list of lyrics texts (strings)" "list of times (ms)")
  (let ((rep
         (mat-trans (loop for evt in (midi-events self)
                          when (equal (ev-type evt) :Lyric)
                          collect (list (if (stringp (car (list! (ev-values evt))))
                                            (ev-values evt)
                                          (list2string (ev-values evt)))
                                        (onset evt)))
                    )))
    (values (car rep) (cadr rep))))


;=================
; TEMPO
;=================

(defmethod! get-tempomap ((self midi-track))
  :initvals '(nil)
  :indoc '("a MIDI-TRACK")
  :icon :midi
  :doc "Extracts the MIDI tempo changes from <self>.

Returns a list of lists (time tempo)."
  :outdoc '("list of (time tempo) pairs")
  (let ((tempo-events (get-midievents self #'(lambda (x) (test-midi-type x :tempo)))))
    (loop for event in tempo-events
          collect (list (onset event) (first (ev-values event))))))

