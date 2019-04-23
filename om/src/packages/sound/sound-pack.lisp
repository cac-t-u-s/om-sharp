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
;;; SOUND OBJECT
;;;=================


(in-package :om)

(omNG-make-package 
 "Audio"
 :container-pack *om-package-tree*
 :doc "Sound/DSP objects and support"
 :classes '(sound)
 :functions '(sound-dur sound-dur-ms sound-points save-sound)
 :subpackages (list (omNG-make-package 
                     "Processing"
                     :functions '(sound-silence 
                                  sound-fade sound-loop sound-reverse sound-cut
                                  sound-mix sound-seq
                                  sound-normalize sound-vol 
                                  sound-mono-to-stereo sound-stereo-to-mono sound-stereo-pan
                                  sound-merge sound-split sound-resample 
                                  ))
                    (omNG-make-package 
                     "Conversions"
                     :functions '(lin->db db->lin samples->sec sec->samples ms->sec sec->ms))
                    (omNG-make-package 
                     "Tools"
                     :functions '(adsr)))
 )


