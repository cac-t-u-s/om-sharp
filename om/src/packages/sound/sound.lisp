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
;;; SOUND PROJECT
;;;=================


(in-package :om)

(load (decode-local-path "audio-api/load-audio-api.lisp"))
        
(mapc #'(lambda (filename) (compile&load (decode-local-path filename))) 
      '(
        "sound/audio-tools"
        "sound/sound-object"
        "sound/sound-processing"
        "sound/synthesize"
        "player/juce-player"
        "player/buffer-player"
        "sound-preferences"
        "compatibility"
        "sound-pack"
        ))

