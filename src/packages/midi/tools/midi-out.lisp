;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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


;===================
; PITCHBEND & PITCHWHEEL
;===================


(defmethod* pitchwheel ((val number) (chans number) &optional port)
   :icon :midi-out
   :indoc '("pitch wheel value(s)" "MIDI channel(s) (1-16)" "output port number")
   :initvals '(8192 1 nil)
   :doc "Sends one or more MIDI pitch wheel message(s) of <vals> in the MIDI channel(s) <chans>.  

<values> and <chans> can be single numbers or lists. 

The range of pitch wheel is between 0 and 16383 (inclusive).  8192 (default) means no bend.
"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event  (om-midi::make-midi-evt :type :PitchBend
                                      :chan chans :port aport
                                      :fields (val2lsbmsb val))))
           (om-midi::midi-send-evt event)
           )))

(defmethod* pitchwheel ((vals number) (chans list) &optional port)
   (loop for item in chans do
         (pitchwheel vals item port)))

(defmethod* pitchwheel ((vals list) (chans list) &optional port)
   (loop for item in chans 
         for item1 in vals do
         (pitchwheel item1 item port)))

(defmethod* pitchwheel ((vals list) (chans number) &optional port)
  (loop
     for chan from chans 
     for val in vals
     do (pitchwheel val chan port)))



(defmethod* pitchbend ((val number) (chan number) &optional port)
   :icon :midi-out
   :indoc '("pitch bend value(s)" "MIDI channel(s) (1-16)" "output port number")
   :initvals '(64 1 nil)
   :doc "Sends one or more MIDI pitch bend message(s) of <vals> in the MIDI channel(s) <chans>.  

<values> and <chans> can be single numbers or lists. 

The range of pitch bend is between 0 and 127. 64 (default) means no bend.
"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event  (om-midi::make-midi-evt :type :PitchBend
                                      :chan chan :port aport
                                      :fields (list 0 val))))
           (om-midi::midi-send-evt event)
           )))

(defmethod* pitchbend ((vals number) (chans list) &optional port)
   (loop for item in chans do
         (pitchbend vals item port)))

(defmethod* pitchbend ((vals list) (chans list) &optional port)
   (loop for item in chans 
         for item1 in vals do
         (pitchbend item1 item port)))



;===================
; PROGRAM CHANGE
;===================

(defmethod* pgmout ((progm integer) (chans integer) &optional port) 
  :icon :midi-out
  :indoc '("program number" "MIDI channel(s)" "output port number")
  :initvals '(2 1 nil)
  :doc "Sends a program change event with program number <progm> to channel(s) <chans>.

<progm> and <chans> can be single numbers or lists."
  (unless port (setf port (get-pref-value :midi :out-port)))
  (setf port (list! port))
  (loop for aport in port do
        (let ((event (om-midi::make-midi-evt :type :ProgChange 
                                             :chan chans
                                             :port aport
                                             :fields (list progm))))
          (when event 
            (om-midi::midi-send-evt event)
            t))))



(defmethod* pgmout ((progm number) (chans list) &optional port)
  (loop for item in chans do
        (pgmout progm item port)))

(defmethod* pgmout ((progm list) (chans list) &optional port)
   (if (or (null port) (integerp port))
     (loop for item in chans 
           for item1 in progm do
           (pgmout item1 item port))
     (loop for item in chans 
           for item1 in progm
           for item2 in port do
           (pgmout item1 item item2))))


;===================
; POLY KEY PRESSURE
;===================

(defmethod* polyKeypres ((val integer) (pitch integer) (chans integer) &optional port) 
   :icon :midi-out
   :indoc '("pressure value" "target pitch" "MIDI channel (1-16)" "output port number")
   :initvals '(100 6000 1 nil)
   :doc "
Sends a key pressure event with pressure <values> and <pitch> on channel <cahns> and port <port>.

Arguments can be single numbers or lists.
"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi::make-midi-evt :type :KeyPress 
					    :chan chans
					    :port aport
					    :fields (list (round pitch 100) val))))
	 (when event (om-midi::midi-send-evt event)))
         ))

(defmethod* polyKeypres ((vals list) (pitch list) (chans list) &optional port)
   (loop for item in pitch
         for val in vals
         for chan in chans do
         (polyKeypres val item chan port)))

(defmethod* polyKeypres ((val integer) (pitch list) (chans integer) &optional port)
   (loop for item in pitch  do
         (polyKeypres val item chans port)))

(defmethod* polyKeypres ((val integer) (pitch list) (chans list) &optional port)
   (loop for item in pitch
         for chan in chans do
         (polyKeypres val item chan port)))

(defmethod* polyKeypres ((vals list) (pitch integer) (chans list) &optional port)
   (loop for val in vals
         for chan in chans do
         (polyKeypres val pitch chan port)))

(defmethod* polyKeypres ((vals list) (pitch integer) (chans integer) &optional port)
   (loop  for val in vals do
          (polyKeypres val pitch chans port)))

(defmethod* polyKeypres ((vals list) (pitch list) (chans integer) &optional port)
   (loop for item in pitch
         for val in vals do
         (polyKeypres val item chans port)))



;===================
; AFTER TOUCH
;===================
(defmethod* aftertouch ((val integer) (chans integer) &optional port) 
   :icon :midi-out
   :indoc '("pressurev value"  "MIDI channel (1-16)" "output port number")
   :initvals '(100 1 nil)
   :doc "Sends an after touch event of <val> to channel <chans> and port <port>.

Arguments can be can be single numbers or lists.
"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi::make-midi-evt :type :ChanPress 
                                              :chan chans
                                              :port aport
                                              :fields (list val))))
           (when event (om-midi::midi-send-evt event)))
         ))

(defmethod* aftertouch ((vals number) (chans list) &optional port)
  (loop for item in chans do
        (aftertouch vals item port)))

(defmethod* aftertouch ((vals list) (chans list) &optional port)
   (if (or (null port) (integerp port))
     (loop for item in vals
           for val in chans do
           (aftertouch item val port))
     (loop for item in vals
           for val in chans
           for item2 in port do
           (aftertouch item val item2))))


;===================
; CONTROL CHANGE 
;===================

(defmethod* ctrlchg ((ctrlnum integer) (val integer) (chans integer) &optional port) 
   :icon :midi-out
   :indoc '("control number"  "value" "MIDI channel (1-16)" "output port number")
   :initvals '(7 100 1 nil)
   :doc "Sends a control change event with control number <ctrlnum> and value <val> to channel <chans> (and port <port>)."
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi::make-midi-evt :type :CtrlChange
					      :chan chans
					      :port aport
					      :fields (list ctrlnum val))))
           (when event (om-midi::midi-send-evt event)))))

(defmethod* ctrlchg ((ctrlnum integer) (val integer) (chans list) &optional port) 
  (loop for item in chans do
        (ctrlchg  ctrlnum val item port)))

(defmethod* ctrlchg ((ctrlnum list) (val list) (chans list) &optional port) 
  (loop for ctrl in ctrlnum
        for item in chans
        for aval in val do
        (ctrlchg  ctrl aval item port)))

(defmethod* ctrlchg ((ctrlnum list) (val list) (chans integer) &optional port) 
  (loop for ctrl in ctrlnum
        for aval in val do
        (ctrlchg  ctrl aval chans port)))

(defmethod* ctrlchg ((ctrlnum list) (val integer) (chans integer) &optional port) 
  (loop for ctrl in ctrlnum do
        (ctrlchg  ctrl val chans port)))



(defmethod* ctrlchg ((ctrlnum list) (val integer) (chans list) &optional port) 
  (loop for ctrl in ctrlnum
        for item in chans do
        (ctrlchg  ctrl val item port)))

(defmethod* ctrlchg ((ctrlnum integer) (val list) (chans list) &optional port) 
  (loop for item in chans
        for aval in val do
        (ctrlchg  ctrlnum aval item port)))


;===================
; VOLUME 
;===================

(defmethod* volume ((vol integer) (chans integer) &optional port) 
   :icon :midi-out
   :indoc '("value" "MIDI channel (1-16)" "output port number")
   :initvals '(100 1 nil)
   :doc "Sends MIDI volume message(s) to channel(s) <chans> and port <port>.

Arguments can be numbers or lists. 

The range of volume values is 0-127.
"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (setf port (list! port))
   (loop for aport in port do
         (let ((event (om-midi::make-midi-evt :type :CtrlChange
					      :chan chans :port aport
					      :fields (list 7 vol))))
             (when event (om-midi::midi-send-evt event)))))

(defmethod* volume ((volume number)  (chans list) &optional port)
  (loop for item in chans do
        (volume volume item port)))

(defmethod* volume ((volume list)  (chans list) &optional port)
   (if (or (null port) (integerp port))
       (loop for item in volume
             for val in chans do
             (volume item val port))
     (loop for item in volume
           for val in chans
           for item2 in port do
           (volume item val item2))))

;===================
; ALL NOTES OFF
;===================
(defmethod* midi-allnotesoff (&optional port) 
   :icon :midi-out
   :indoc '("output port number")
   :initvals '(nil)
   :doc "Turns all notes off on all channels"
   (unless port (setf port (get-pref-value :midi :out-port)))
   (loop for aport in (list! port) do
         (loop for c from 1 to 16 do
               (let ((event (om-midi::make-midi-evt :type :CtrlChange
                                                    :chan c
                                                    :port aport
                                                    :fields (list 120 0))))
                 (when event (om-midi::midi-send-evt event)))
               ))
   t)


;===================
; RESET
;===================

(defmethod* midi-reset (port)
   :icon :midi-out
   :indoc '("ouput MIDI port")
   :initvals '(0)
   :doc "Sends a MIDI Reset message on port <port>."
   (loop for chan from 1 to 16 do 
         (let ((event (om-midi::make-midi-evt :type :CtrlChange
                                              :port (or port (get-pref-value :midi :out-port))
                                              :chan chan
                                              :fields '(121 0))))
             (when event (om-midi::midi-send-evt event))))
   t)


;===================
; SEND FREE BYTES
;===================

;;; THIS FUNCTION WILL PROBABLY NOT WORK
;;; LOW-LEVEL API SHOULD HANDLE THIS TYPE OF EVENTS... OR NOT
(defmethod* midi-o ((bytes list) &optional port)
   :icon :midi-out
   :indoc '("data bytes" "output port number")
   :initvals '((144 60 64) nil)
   :doc "Sends <bytes> out of the port number <port>. 
"
   (when bytes
     
     (unless port (setf port (get-pref-value :midi :out-port)))
     
     (if (list-subtypep bytes 'list)
         ;;; bytes is a list of lists
         (if (integerp port)
             ;;; send all lists one by one to port
             (loop for item in bytes do
                   (midi-o item port))
           ;;; send all lists, one to each port
           (loop for item in bytes 
                 for item1 in port do
                 (midi-o item item1)))
       
       ;;; send the bytes list
       (loop for aport in (list! port) do
             (om-midi::midi-send-bytes bytes aport))
       )
     t))

(defmethod* sysex ((databytes list) &optional port) 
   :icon :midi-out
   :indoc '("data bytes (ID data)" "output port number")
   :initvals '((1 1) nil)
   :doc "Sends a system exclusive MIDI message on <port> with any number of data bytes. The data will be framed between SysEx begin/end messages (F0/F7)."
   (when databytes
     (unless port (setf port (get-pref-value :midi :out-port)))
     (if (list-subtypep databytes 'list)
       (if (integerp port)
         (loop for item in databytes do
               (sysex item port))
         (loop for item in databytes 
               for item1 in port do
               (sysex item item1)))
       (loop for aport in (list! port) do
             (om-midi::midi-send-bytes (cons #xF0 databytes) aport)
             (om-midi::midi-send-bytes (cons #xF7 '(0 0)) aport)
             ))
     t))



;===================
; SEND ONE NOTE
;===================

(defmethod! send-midi-note (port chan pitch vel dur)
  :icon :midi-out
  :initvals '(0 1 60 100 1000)
  (let ((note (make-midinote :port port :chan chan :pitch pitch :vel vel :dur dur))) 
    (funcall (get-frame-action note))))


; (send-midi-note 0 1 60 100 1000)



