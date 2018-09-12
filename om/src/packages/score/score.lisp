;============================================================================
; o7: visual programming language for computer-aided music composition
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
      '("score-objects/score-object"
        "score-objects/chord"
        "score-objects/chord-seq"
        "score-objects/score"
        "editor/score-draw"
        "tools/conversions"
        "import-export/musicxml"))


(omNG-make-package 
 "Score"
 :container-pack *om-package-tree*
 :doc "Score tools and objects"
 :classes nil
 :functions nil
 :subpackages  (list (omNG-make-package 
                      "Utils"
                      :doc "Unit conversion utilities etc."
                      :classes '(chord score)
                      :functions '(approx-m mc->f f->mc mc->n n->mc beats->ms)
                      :subpackages nil)))
