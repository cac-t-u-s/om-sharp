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


;;;===================================================
;;; SCORE IS A LIST OF PARALLEL VOICES 
;;; Equivalent to former POLY / MULTI-SEQ + MIXED-TYPES
;;;===================================================

(defclass* poly (score-object collection) 
  ((obj-list :initarg :obj-list :initarg :voices
             :accessor obj-list :initform nil)))

;;; deprecated
(defclass* multi-seq (poly) 
  ((obj-list :initarg :obj-list :initarg :chord-seqs
             :accessor obj-list :initform nil)))


(defmethod score-object-mini-view ((self poly) box x-pix y-pix y-u w h)
  (let ((voice-h (if (obj-list self) (/ h (length (obj-list self))) h)))
    (loop for voice in (obj-list self)
          for i from 0
          do (progn
               ;(draw-staff x-pix (* i voice-h) y-u w voice-h (fontsize box) (get-edit-param box :staff) :margin-l 1 :margin-r 1 :keys t)
               (score-object-mini-view voice box x-pix (+ y-pix (* i voice-h)) 0 w voice-h))
          )))
