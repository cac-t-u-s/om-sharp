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

(load (merge-pathnames "cercle/n-cercle" *load-pathname*))
(load (merge-pathnames "cercle/n-cercle-editor" *load-pathname*))


(omNG-make-package 
 "Math"
 :container-pack *om-package-tree*
 :doc "Mathematical tools / Set theory / ..."
 :classes '(n-cercle)
 :functions '(chord2c c2chord c2chord-seq chord-seq2c c2rhythm rhythm2c
              nc-rotate nc-complement nc-inverse)
 :subpackages nil)
