;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

(add-preference-module :score "Score")

(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename))) 
      
      '("score-objects/score-object"
        "score-objects/chord"
        "score-objects/chord-seq"
        "score-objects/tree"
        "score-objects/ratios"
        "score-objects/voice"
        "score-objects/multiseq-poly"
        "score-objects/extras"
       
        "functions/conversions"
        "functions/score-functions"
        "functions/trees"
        "functions/quantify"

        "draw/draw-score-basic"
        "draw/draw-score-rhythm"
        "draw/draw-score-spacing"

        "editor/scales"
        "editor/score-editor"
        "editor/chord-editor"
        "editor/chord-seq-editor"
        "editor/voice-editor"
        "editor/multiseq-poly-editor"
        "editor/score-boxes"
        "editor/play"

        "import-export/midi"
        "import-export/musicxml-import"
        "compatibility"
        ))

(omNG-make-package 
 "Score"
 :container-pack *om-package-tree*
 :doc "Score tools and objects"
 :classes '(note chord chord-seq voice multi-seq poly)
 :functions nil
 :subpackages  (list (omNG-make-package 
                      "Score Manipulations"
                      :doc "Manipulation of score objects"
                      :functions '(object-dur get-chords concat select insert merger align-chords split-voices)
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
                      "Extras"
                      :doc "Extra elements attached to chords in score editors."
                      :functions '(add-extras remove-extras get-extras)
                      :classes '(score-marker head-extra vel-extra text-extra symb-extra)
                      :subpackages nil)
                     (omNG-make-package 
                      "Utils"
                      :doc "Unit conversion utilities etc."
                      :functions '(approx-m mc->f f->mc mc->n n->mc int->symb symb->int beats->ms)
                      :subpackages nil)
                     (omNG-make-package 
                      "Import/Export"
                      :doc "Import and export utilities"
                      :functions '(import-musicxml import-midi save-as-midi)  ;; export-musicxml
                      :subpackages nil)))


