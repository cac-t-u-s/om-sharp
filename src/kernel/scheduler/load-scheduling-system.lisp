;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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

