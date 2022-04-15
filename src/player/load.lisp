;============================================================================
; om#: visual programming language for computer-assisted music composition
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

        "data/object-with-action"
        "data/time-sequence"
        "data/timeline-editor"
        "data/data-track"
        "data/data-track-editor"
        ))


(omNG-make-package
 "Data"
 :container-pack (get-subpackage *om-package-tree* "Visual Language")
 :classes '(data-track))
