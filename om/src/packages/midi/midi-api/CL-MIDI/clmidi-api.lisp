;;===========================================================================
;;; midi-api-cl.lisp
;;; Common Lisp Midi API - based on ms:: versions found in midi-api.lisp
;;;
;;; This program is free software;;;  you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation;;;  either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; See file LICENSE for further informations on licensing terms.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY;;;  without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program;;;  if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;;
;;; Author: Anders Vinjar

;;===========================================================================
; DocFile
; MIDI functions called by OpenMusic
; Using lisp-based SMF I/O + events
; Sources at Goldsmiths, Univ. of London: http://www.doc.gold.ac.uk/isms/lisp/midi/
;;===========================================================================


(in-package :om-midi)

;;; FILE OUTPUT: building useful midi-messages, writing SMF's:

;;; smf refs:
;;;
;;; http://acad.carleton.edu/courses/musc108-00-f14/pages/04/04StandardMIDIFiles.html
;;; http://cs.fit.edu/~ryan/cse4051/projects/midi/midi.html
;;;


;;;
;;; BOOKKEEPING, REGISTER SPECIALISED FUNCTIONS TO MAKE MIDI:*MESSAGE FOR EACH MIDI-EVT-TYPE
;;;

;; DEFINE FUNCTION AND REGISTER IN LOOKUP TABLE:

(defvar *event-to-message-functions* '())

(defmacro defevt2msg ((name type) &body body)
  `(prog1
       (defun ,name (ev) ,@body)
     (pushnew (cons ,type #',name) *event-to-message-functions*)))

(defun get-event-to-message-func (type)
  (cdr (assoc type *event-to-message-functions*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  START OF MESSAGE TYPES
;;;
;; voice messages
(defconstant +note-off-opcode+		#x80)
(defconstant +note-on-opcode+		#x90)
(defconstant +key-pressure-opcode+	#xA0)
(defconstant +control-change-opcode+	#xB0)
(defconstant +program-change-opcode+	#xC0)
(defconstant +channel-pressure-opcode+	#xD0)
(defconstant +pitch-bend-opcode+	#xE0)

;; mode messages
(defconstant +mode-messages-opcode+ #xB0)

;; meta messages, all have status-byte=#xFF
(defconstant +meta-messages-opcode+	#xFF)


;;;============================
;;; VOICE MESSAGES
;;;============================

(defmethod midi-channel ((event t)) nil)
(defmethod midi-channel ((event midi::channel-message))
  ;; used where channel isn't set explicit in instance
  (- (slot-value event 'midi::status) (slot-value event 'midi::status-min)))


(defmethod midi-message-time ((msg midi::message)) (midi::message-time msg))

(defmethod midi-message-channel ((msg midi::channel-message)) (midi::message-channel msg))
(defmethod midi-message-channel ((msg t)) -1)

;;; Accessors to define for the different types of MIDI messages
(defmethod midi-message-type ((msg t)) (intern (concatenate 'string "unknown/" (string (type-of msg)))))
(defmethod midi-message-fields ((msg t)) nil)

;; NOTE OFF

(defmethod midi-message-type ((msg midi::note-off-message)) :KeyOff)

(defmethod midi-message-fields ((msg midi::note-off-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

(defevt2msg (event2note-off :KeyOff)
  (let* ((fields (midi-evt-fields ev))
	 (key (first fields))
	 (vel (second fields))
	 (chan (1- (midi-evt-chan ev))))		    ;TODO: find where this goes to 1-based offset
    (make-instance 'midi:note-off-message
		   :time (midi-evt-date ev) :key key :velocity vel
		   :status (logior +note-off-opcode+ chan))))

(defun make-note-off-message (time key vel chan)
  (make-instance 'midi:note-off-message :key key :time time :velocity vel :status (logior +note-off-opcode+ chan)))

;; NOTE ON

(defmethod midi-message-type ((msg midi::note-on-message)) :KeyOn)

(defmethod midi-message-fields ((msg midi::note-on-message))
  (list (midi::message-key msg) (midi::message-velocity msg)))

(defevt2msg (event2note-on :KeyOn)
  (let* ((fields (midi-evt-fields ev))
	 (key (first fields))
	 (vel (second fields))
	 (chan (1- (midi-evt-chan ev))))
    (make-instance 'midi:note-on-message
		   :time (midi-evt-date ev) :key key :velocity vel
		   :status (logior +note-on-opcode+ chan))))

(defun make-note-on-message (time key vel chan)
  (make-instance 'midi:note-on-message :key key :time time :velocity vel :status (logior +note-on-opcode+ chan)))


;; POLY-KEY PRESSURE

(defmethod midi-message-type ((msg midi::polyphonic-key-pressure-message)) :KeyPress)

(defmethod midi-message-fields ((msg midi::polyphonic-key-pressure-message))
  (list (slot-value msg 'midi::key) (slot-value msg 'midi::pressure)))

(defevt2msg (event2keypress :KeyPress)
  (let ((msg (make-instance 'midi:note-on-message
                            :time (midi-evt-date ev)
                            :status (logior +note-on-opcode+ (1- (midi-evt-chan ev))))))
    (setf (slot-value msg 'midi::key) (car (midi-evt-fields ev)))
    (setf (slot-value msg 'midi::pressure) (cadr (midi-evt-fields ev)))
    msg))


;; CONTROL CHANGE

(defmethod midi-message-type ((msg midi::control-change-message)) :CtrlChange)

(defmethod midi-message-fields ((msg midi::control-change-message))
  (list (slot-value msg 'midi::controller) (slot-value msg 'midi::value)))

(defevt2msg (event2control-change :CtrlChange)
  (let* ((fields (midi-evt-fields ev))
	 (controller (first fields))
	 (value (second (midi-evt-fields ev))))
    (make-instance 'midi::control-change-message
		   :time (midi-evt-date ev) :controller controller :value value
		   :status (logior +control-change-opcode+ (1- (midi-evt-chan ev))))))


;; PROGRAM CHANGE

(defmethod midi-message-type ((msg midi:program-change-message)) :ProgChange)

(defmethod midi-message-fields ((msg midi:program-change-message)) (list (midi::message-program msg)))

(defevt2msg (event2program-change-message :ProgChange)
  (make-instance 'midi:program-change-message
		 :time (midi-evt-date ev) :program (first (midi-evt-fields ev))
		 :status (logior +program-change-opcode+ (1- (midi-evt-chan ev)))))


;; CHANNEL PRESSURE

(defmethod midi-message-type ((msg midi::channel-pressure-message)) :ChanPress)

(defmethod midi-message-fields ((msg midi::channel-pressure-message))
  (list (slot-value msg 'midi::pressure)))

(defevt2msg (event2chanpress :ChanPress)
  (let ((msg (make-instance 'midi:note-on-message
                            :time (midi-evt-date ev)
                            :status (logior +note-on-opcode+ (1- (midi-evt-chan ev))))))
    (setf (slot-value msg 'midi::pressure) (car (midi-evt-fields ev)))
    msg))


;; PITCH BEND

(defmethod midi-message-type ((msg midi::pitch-bend-message)) :PitchBend)

(defmethod midi-message-fields ((msg midi::pitch-bend-message))
  (list (midi::message-value msg)))

(defevt2msg (event2pitch-bend :PitchBend)
  (when (midi-evt-fields ev)
    (make-instance 'midi::pitch-bend-message
		   :time (midi-evt-date ev) :value (first (midi-evt-fields ev))
		   :status (logior +pitch-bend-opcode+ (1- (midi-evt-chan ev))))))


;;;============================
;;; MODE MESSAGES
;;;============================

(defmethod midi-message-fields ((msg midi::mode-message))
  (list (slot-value msg 'midi::channel)))

;; :ResetAllControllers
(defmethod midi-message-type ((msg midi::reset-all-controllers-message)) :ResetAllControllers)

(defevt2msg (event2reset-all-controllers-message :ResetAllControllers)
  (make-instance 'midi::reset-all-controllers-message
		 :time (midi-evt-date ev)
		 :status (logior +mode-messages-opcode+ (1- (midi-evt-chan ev)))))

;; :AllNotesOff

(defmethod midi-message-type ((msg midi::all-notes-off-message)) :AllNotesOff)

(defevt2msg (event2all-notes-off-message :AllNotesOff)
  (make-instance 'midi::all-notes-off-message
		 :time (midi-evt-date ev)
		 :status (logior +mode-messages-opcode+ (1- (midi-evt-chan ev)))))


;;;============================
;;; META MESSAGES
;;;============================

;;; for all text messages :
;;; read fileds as a list with a string inside
(defmethod midi-message-fields ((msg midi::text-message)) 
  ;; (map 'list #'char-code (slot-value msg 'midi::text))  ;; restore the list of ASCII.. ?
  (list (slot-value msg 'midi::text))
  )

(defun midi-fields-to-string (fields)
  (if (stringp (car fields)) (car fields)
    (map 'string #'code-char fields)))


;; SEQUENCE NUMBER

(defmethod midi-message-type ((msg midi::sequence-number-message)) :SeqNum)

(defmethod midi-message-fields ((msg midi::sequence-number-message)) 
  (list (slot-value msg 'midi::sequence)))

(defevt2msg (event2seqnum :SeqNum)
  (let ((msg (make-instance 'midi::sequence-number-message
                            :time (midi-evt-date ev)
                            :status +meta-messages-opcode+)))
    (setf (slot-value msg 'midi::sequence) (car (midi-evt-fields ev)))
    ))
  
;; TEXT

(defmethod midi-message-type ((msg midi::general-text-message)) :Textual)

(defevt2msg (event2text :Textual)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::general-text-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;;; COPYRIGHT

(defmethod midi-message-type ((msg midi::copyright-message)) :CopyRight)

(defevt2msg (event2copyright :CopyRight)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::copyright-message :time time
			       :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;;; TRACK NAME

(defmethod midi-message-type ((msg midi:sequence/track-name-message)) :SeqName)

(defevt2msg (event2seqname :SeqName)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::sequence/track-name-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;;; INSTR NAME

(defmethod midi-message-type ((msg midi::instrument-message)) :InstrName)


(defevt2msg (event2instrument :InstrName)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::instrument-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))


;;; LYRICS

(defmethod midi-message-type ((msg midi::lyric-message)) :Lyric)

(defevt2msg (event2lyric :Lyric)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::lyric-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;;; MARKER

(defmethod midi-message-type ((msg midi::marker-message)) :Marker)


(defevt2msg (event2marker :Marker)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::marker-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))

;;; CUE MARKER

(defmethod midi-message-type ((msg midi::cue-point-message)) :CuePoint)

(defevt2msg (event2cue-point :CuePoint)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::cue-point-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))


;;; PROGRAM NAME

(defmethod midi-message-type ((msg midi::program-name-message)) :ProgName)

(defevt2msg (event2progname :ProgName)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::program-name-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))


;;; DEVICE NAME

(defmethod midi-message-type ((msg midi::device-name-message)) :DeviceName)

(defevt2msg (event2devicename :DeviceName)
  (let ((time (midi-evt-date ev))
	(value (midi-fields-to-string (midi-evt-fields ev))))
    (let ((inst (make-instance 'midi::device-name-message :time time :status +meta-messages-opcode+)))
      (setf (slot-value inst 'midi::text) value)
      inst)))


;; CHANNEL PREFIX PORT

(defmethod midi-message-type ((msg midi::channel-prefix-message)) :ChannelPrefix)

(defmethod midi-message-fields ((msg midi::channel-prefix-message))
  (list (slot-value msg 'midi::channel)))

(defevt2msg (event2channel-prefix-msg :MidiPortMsg)
  (let ((inst (make-instance 'midi::channel-prefix-message :time (midi-evt-date ev)
			     :status +meta-messages-opcode+)))
    (setf (slot-value inst 'midi::channel) (car (midi-evt-fields ev)))
    inst))


;; MIDI PORT

(defmethod midi-message-type ((msg midi::midi-port-message)) :MidiPortMsg)

(defmethod midi-message-fields ((msg midi::midi-port-message))
  (list (slot-value msg 'midi::port)))

(defevt2msg (event2midi-port-msg :MidiPortMsg)
  (let ((inst (make-instance 'midi::midi-port-message :time (midi-evt-date ev)
			     :status +meta-messages-opcode+)))
    (setf (slot-value inst 'midi::port) (car (midi-evt-fields ev)))
    inst))

;; END OF TRACK

(defmethod midi-message-type ((msg midi::end-of-track-message)) :EndOfTrackMsg)

(defevt2msg (event2end-of-track-msg :EndOfTrackMsg)
  (let ((inst (make-instance 'midi::end-of-track-message
			     :time (midi-evt-date ev)
			     :status +meta-messages-opcode+)))
    (setf (slot-value inst 'midi::status) (midi-evt-fields ev))
    inst))



;; TEMPO

(defmethod midi-message-type ((msg midi::tempo-message)) :Tempo)

(defmethod midi-message-fields ((msg midi::tempo-message)) (list (midi::message-tempo msg)))

(defevt2msg (event2tempo :Tempo)
  (make-instance 'midi:tempo-message
		 :time (midi-evt-date ev) :tempo (first (midi-evt-fields ev))
		 :status +meta-messages-opcode+))



;; SMPTE OFFSET 
;; => todo: midi:smpte-offset-message


;; TIME SIGNATURE

(defmethod midi-message-type ((msg midi::time-signature-message)) :TimeSign)

(defmethod midi-message-fields ((msg midi::time-signature-message))
  (list (midi::message-numerator msg)
        (midi::message-denominator msg)			    ;power of 2
	(slot-value msg 'midi::cc)			    ;midi clocks pr. metronome click
        (slot-value msg 'midi::bb)))			    ;n 32nd notes notated per quarter note

(defevt2msg (event2time-signature :TimeSign)
  (let ((inst (make-instance 'midi::time-signature-message
			     :time (midi-evt-date ev)
			     :status +meta-messages-opcode+))
	(data (midi-evt-fields ev)))
    (setf (slot-value inst 'midi::nn) (first data)
	  (slot-value inst 'midi::dd) (second data)
	  (slot-value inst 'midi::cc) (third data)
	  (slot-value inst 'midi::bb) (fourth data))
    inst))

;; KEY SIGNATURE

(defmethod midi-message-type ((msg midi::key-signature-message)) :KeySign)

(defmethod midi-message-fields ((msg midi::key-signature-message))
  (list (midi::message-sf msg)
        (midi::message-mi msg)))

(defevt2msg (event2key-signature :KeySign)
  (let ((inst (make-instance 'midi::key-signature-message
			     :time (midi-evt-date ev)
			     :status +meta-messages-opcode+)))
    (setf (slot-value inst 'midi::sf) (first (midi-evt-fields ev))
	  (slot-value inst 'midi::mi) (second (midi-evt-fields ev)))
    inst))



;; SPECIFIC/PROPRIETARY EVENT 
;; => todo : midi::proprietary-event





;;;============================
;;; READ/WRITE
;;;============================


;; takes instances of the various midi:*message classes, returning a list of midi-evt
(defun make-event-from-message (msg ref)
  (make-midi-evt :type (midi-message-type msg)
                 :date (midi::message-time msg)
                 :chan (1+ (midi-message-channel msg))
                 :ref ref
                 :fields (midi-message-fields msg)))

(defun make-messages-from-event (ev)
  (let* ((type (midi-evt-type ev))
	 (func (get-event-to-message-func type)))
    ;; (when (equal type :TimeSign)
    ;;   (print (list (midi-evt-fields ev) func)))
    (if func
	(funcall func ev)
	(progn (print (format nil "(cl-midi) message-type ~A isn't supported yet" type))
	       NIL))))


(defun tracks2seq (tracks)
  (sort (loop for track in tracks
              for ref = 0 then (+ ref 1) append
              (loop for msg in track collect (make-event-from-message msg ref)))
        #'midi-evt-<))


(defun seq2tracks (seq)
  (let ((tracks nil))
    (loop for ev in seq
          for msg = (make-messages-from-event ev)
          do (let* ((tracknum (or (midi-evt-ref ev) 0))
                    (trackpos (position tracknum tracks :key 'car :test '=)))
               (if trackpos
                   (setf (nth trackpos tracks)
                         (list tracknum
                               (append (cadr (nth trackpos tracks)) (if (listp msg) msg (list msg)))))
                 (push (list tracknum (if (listp msg) msg (list msg))) tracks))))
    (mapcar 'cadr (sort tracks '< :key 'car))))



;;; FUNCTION CALLED BY OM
;;; Returns a flat list of midi-evt with :ref = track num
(defun cl-midi-load-file (pathname)
  (let ((f (midi:read-midi-file pathname)))
    (values (tracks2seq (midi:midifile-tracks f))
            (length (midi:midifile-tracks f))
            (midi:midifile-division f)
            (midi:midifile-format f))))

;;; FUNCTION CALLED BY OM
;;; Saves a flat list of midi-evt where :ref = track num
(defun cl-midi-save-file (seq filename fileformat clicks)
  (let ((mf (make-instance 'midi:midifile :format fileformat :division clicks)))
    (setf (slot-value mf 'midi::tracks) (seq2tracks seq))
    #+lispworks(sys::ENSURE-DIRECTORIES-EXIST filename :verbose t) ;;; !!! LW specific
    (midi:write-midi-file mf filename)
    filename))


