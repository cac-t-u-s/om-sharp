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


;================================================
;=== CHANNEL CONTROLLER                     
;=== a single track controller 
;================================================
(defclass channel-controls () 
  ((midiport :initform nil :initarg :midiport :accessor midiport :type integer)
   (midichannel :initform 1 :initarg :midichannel :accessor midichannel :type integer)
   (program :initform 0 :initarg :program :accessor program :type integer)
   (pan-ctrl :initform 64 :initarg :pan-ctrl  :accessor pan-ctrl :type integer)
   (control1-num :initform 1 :initarg :control1-num :accessor control1-num :type integer)
   (control2-num :initform 2 :initarg :control2-num :accessor control2-num :type integer)
   (control1-val :initform 0 :initarg :control1-val :accessor control1-val :type integer)
   (control2-val :initform 0 :initarg :control2-val :accessor control2-val :type integer)
   (vol-ctrl :initform 100 :initarg :vol-ctrl :accessor vol-ctrl :type integer)
   (pitch-ctrl :initform 8192 :initarg :pitch-ctrl :accessor pitch-ctrl :type integer)))


(defclass* midi-mix-console (data-frame)
  ((midiport :initform nil :accessor midiport :type integer :documentation "output port number")
   (miditrack :initform 0 :accessor miditrack)
   (channels-ctrl :initform nil :accessor channels-ctrl)))
