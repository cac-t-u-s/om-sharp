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

(defmethod update-arg-names ((reference (eql 'synthesize)))
  '(("elements" "obj")))

;; :-(
(defclass audio-mix-console () 
  ((channels-ctrl :accessor channels-ctrl)
   (nbtracks :initarg :nbtracks)))

