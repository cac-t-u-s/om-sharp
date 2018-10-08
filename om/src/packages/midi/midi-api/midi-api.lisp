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

;;===========================================================================
; MIDI functions called by OpenMusic
;;===========================================================================

(in-package :om-midi)

(cl-user::compile&load (merge-pathnames "portmidi/portmidi" *load-pathname*))
(cl-user::compile&load (merge-pathnames "portmidi/portmidi-api" *load-pathname*))
(cl-user::compile&load (merge-pathnames "portmidi/portmidi-setup" *load-pathname*))

(cl-user::compile&load (merge-pathnames "CL-MIDI/midi-20070618/midi" *load-pathname*))
(cl-user::compile&load (merge-pathnames "CL-MIDI/clmidi-api" *load-pathname*))


(export '(make-midi-evt
          midi-evt-date
          midi-evt-type
          midi-evt-chan
          midi-evt-ref
          midi-evt-port
          midi-evt-fields
          copy-midi-evt
          midi-evt-<
          )
        :om-midi)

(defvar om-midi::*libportmidi* nil)
(defun load-midi-lib ()
  (setf om-midi::*libportmidi*
        (om-fi::om-load-foreign-library  
         "PortMidi"
         `((:macosx ,(om-fi::om-foreign-library-pathname "libportmidi.dylib"))
           (:windows (:or ,(om-fi::om-foreign-library-pathname "libportmidi.dll")
                      (:default "libportmidi")))
           (:linux (:or "libportmidi.so" ,(om-fi::om-foreign-library-pathname "libportmidi.dll")))
	   ((:default "libportmidi"))))))
(om-fi::add-foreign-loader 'load-midi-lib)
(om::add-om-init-fun 'om-midi::om-start-portmidi)

;;; Conventions: channels = 1-16
(defstruct midi-evt (date) (type) (chan) (ref) (port) (fields))


(defparameter *key-ons* (make-list 16))

(defun midi-send-evt (evt) 
  (cond 
   ((or (equal (om-midi::midi-evt-type evt) :keyOff)
        (and (equal (om-midi::midi-evt-type evt) :keyOn) (= 0 (cadr (om-midi::midi-evt-fields evt)))))
    (setf (nth (1- (om-midi::midi-evt-chan evt)) *key-ons*) 
          (delete (list (om-midi::midi-evt-port evt) (car (om-midi::midi-evt-fields evt)))
                  (nth (1- (om-midi::midi-evt-chan evt)) *key-ons*)
                  :test 'equal)))
   ((equal (om-midi::midi-evt-type evt) :keyOn)
    (pushnew (list (om-midi::midi-evt-port evt) (car (om-midi::midi-evt-fields evt))) (nth (1- (om-midi::midi-evt-chan evt)) *key-ons*) :test 'equal)))
  (portmidi-send-evt evt))

;(defmethod midi-start () 
;  (portmidi-start))

(defmethod midi-stop () 
  ;(portmidi-stop)
  (loop for ch in *key-ons* for c = 1 then (+ c 1) do
        (loop for note in ch do
              (midi-send-evt (om-midi::make-midi-evt :type :keyOff
                                                    :chan c :date 0 :ref 0 :port (car note)
                                                    :fields (list (cadr note) 0))
                             )))
  (setf *key-ons* (make-list 16)))

;;; A IS BEFORE B IF...
(defun midi-evt-< (a b)
  (or (< (midi-evt-date a) (midi-evt-date b)) ;;; A IS BEFORE B
      (and (= (midi-evt-date a) (midi-evt-date b)) ;;; A IS = B 
           (not (find (midi-evt-type a) (list :KeyOn :KeyOff))))  ;;; BUT A IS NOT A NOTE MESSAGE
      (and (= (midi-evt-date a) (midi-evt-date b))
           (equal (midi-evt-type a) :KeyOff) (equal (midi-evt-type a) :KeyOn))))  ;;; SEND NOTE OFF MESSAGES FIRST

; MIDI event type identifiers
; = list of supported MIDI events
;(export '(Note KeyOn KeyOff KeyPress CtrlChange ProgChange ChanPress PitchBend
;               SongPos SongSel Clock Start Continue Stop Tune ActiveSens Reset 
;               SysEx Stream Private Process DProcess QFrame Ctrl14b NonRegParam
;               RegParam SeqNum Textual Copyright SeqName InstrName Lyric Marker
;               CuePoint ChanPrefix EndTrack Tempo SMPTEOffset TimeSign KeySign
;               Specific PortPrefix RcvAlarm ApplAlarm Reserved dead)
;        :om-midi)

(defun midi-import (&optional filename)
  (let ((file (or filename (om-api:om-choose-file-dialog :types '("MIDI file" "*.mid;*.midi")))))
    (when (probe-file file)
      (cl-midi-load-file file))))





