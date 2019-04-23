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
; File author: D. Bouche
;============================================================================

(in-package :om)

(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename)))      
      '(
        "action"
        
        "scheduler"
        "engine"
        "thread-pool"
        "dispatcher"
        "graphics-callback"

        "schedulable-object"
        
        "scheduling-system"
        
        "mode-switcher"))