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
          (compile&load (decode-local-path filename)))

      '("sequencer-object"
        "metronome"
        "metric-ruler"
        "sequencer-editor"
        "sequencer-api"
        "sequencer-meta"
        "sequencer-box"
        "maquette-compatibility"
        ))


(omNG-make-package "Sequencer"
                   :container-pack *om-package-tree*
                   :doc "Sequencer manipulation and control"
                   :special-symbols '(thissequencer)
                   :subpackages
                   (list (omNG-make-package
                          "Contents"
                          :doc ""
                          :functions '(get-objects s-add s-remove s-move s-clear))
                         (omNG-make-package
                          "Player"
                          :doc ""
                          :functions '(s-play s-pause s-stop s-loop s-set-time s-get-time))
                         ))

