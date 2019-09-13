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


(in-package :om)                


(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename))) 
      
      '("om-maquette"
        "metronome"
        "om-metric-ruler"
        "om-maquette-editor"
        "om-maquette-api"
        "om-maquette-meta"
        )
      )


(omNG-make-package "Maquette/Meta" 
	:container-pack *om-package-tree*
	:doc "Visual program / maquette manipulation"
    :functions '(get-boxes m-add m-remove m-move m-objects m-flush)
    :special-symbols '(mybox mymaquette)
    )
