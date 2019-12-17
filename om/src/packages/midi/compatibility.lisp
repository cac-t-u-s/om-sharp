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

;;; LOAD OBJECTS AND CODE FROM OM6

(in-package :om)

(defmethod update-reference ((ref (eql 'midifile))) 'midi-track)
(defmethod update-reference ((ref (eql 'eventmidi-seq))) 'midi-track)

(defmethod function-changed-name ((reference (eql 'test-type))) 'test-midi-type)
(defmethod function-changed-name ((reference (eql 'test-channel))) 'test-midi-channel)
(defmethod function-changed-name ((reference (eql 'test-port))) 'test-midi-port)
(defmethod function-changed-name ((reference (eql 'test-track))) 'test-midi-track)


(defun load-midi (path)
  (make-instance 'midi-track 
                 :midi-events (import-midi-notes path)))

