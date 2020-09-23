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
          (compile&load (decode-local-path filename))) 
      
      '("maquette-object"
        "metronome"
        "metric-ruler"
        "maquette-editor"
        "maquette-api"
        "maquette-meta"
        "maquette-box"
        "maquette-compatibility"
        ))


(omNG-make-package "Maquette/Meta" 
	:container-pack *om-package-tree*
	:doc "Visual program / maquette manipulation"
    :functions '(get-boxes m-add m-remove m-move m-objects m-flush)
    :special-symbols '(mybox mymaquette)
    )
