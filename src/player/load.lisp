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
; File author: J. Bresson
;============================================================================


(in-package :om)


(mapc #'(lambda (filename)
          (cl-user::compile&load (cl-user::decode-local-path filename)))
      '(
        "scheduler/load-scheduling-system"
        "scheduler/clock"

        "play/timed-object"
        "play/general-player"
        "play/box-player"
        "play/editor-player"
        ))

