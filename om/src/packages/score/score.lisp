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
      
      '("score-objects/score-object"
        "score-objects/chord"
        "score-objects/chord-seq"
        "score-objects/tree"
        "score-objects/ratios"
        "score-objects/voice"
        "score-objects/score"

        "editor/draw-score-basic"
        "editor/draw-score-rhythm"

        "editor/score-editor"
        "editor/chord-editor"
        "editor/chord-seq-editor"

        "tools/conversions"
        "functions/score-functions"
        "functions/trees"
        "functions/quantify"
        "import-export/musicxml"))


(omNG-make-package 
 "Score"
 :container-pack *om-package-tree*
 :doc "Score tools and objects"
 :classes '(note chord chord-seq voice poly)
 :functions nil
 :subpackages  (list (omNG-make-package 
                      "Score Manipulations"
                      :doc "Manipulation of score objects"
                      :functions '(object-dur get-chords concat)
                      :subpackages nil)
                     (omNG-make-package 
                      "Rhythm"
                      :doc "Operations on rhythm trees and ratios"
                      :functions '(mktree tree2ratio 
                                          pulsemaker maketreegroups
                                          n-pulses group-pulses get-pulse-places get-rest-places get-signatures
                                          reducetree tietree filtertree reversetree rotatetree
                                          remove-rests subst-rhythm invert-rhythm
                                          omquantify)
                      :subpackages nil)
                     (omNG-make-package 
                      "Utils"
                      :doc "Unit conversion utilities etc."
                      :functions '(approx-m mc->f f->mc mc->n n->mc int->symb symb->int beats->ms)
                      :subpackages nil)))


