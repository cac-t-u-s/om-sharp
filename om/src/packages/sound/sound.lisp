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
        "sound-pack"
        )
      )