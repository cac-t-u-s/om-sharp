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


;;;=================
;;; SOUND PREFS
;;;=================

(in-package :om)

(add-preference :audio :format "Default format" '(:aiff :wav) #+macosx :aiff #-macosx :wav
                "Applies as default choice for audio synthesis functions")

(add-preference :audio :normalize "Normalization" :bool t
                "Applies as default choice for audio synthesis functions")

(add-preference :audio :resolution "Default sample size" '(16 24 32) 16
                "Applies as parameter for saving audio buffers")

;(add-preference :audio :normalize "Normalization level (db)" :bool t
;                "Applies as default choice for audio synthesis functions")
