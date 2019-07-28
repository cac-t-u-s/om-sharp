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


;;;=================
;;; SOUND PREFS
;;;=================

(in-package :om)

(add-preference :audio :format "Default format" '(:aiff :wav) :aiff
                "Applies as default choice for audio synthesis functions")

(add-preference :audio :resolution "Default resolution" '(16 24 32) 16
                "Applies as default choice for audio synthesis functions")

(add-preference :audio :normalize "Normalization" :bool t
                "Applies as default choice for audio synthesis functions")

;(add-preference :audio :normalize "Normalization level (db)" :bool t
;                "Applies as default choice for audio synthesis functions")
