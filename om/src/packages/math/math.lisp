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

(load (merge-pathnames "cercle/n-cercle" *load-pathname*))
(load (merge-pathnames "cercle/n-cercle-editor" *load-pathname*))


(omNG-make-package 
 "Math"
 :container-pack *om-package-tree*
 :doc "Mathematical tools / Set theory / ..."
 :classes '(n-cercle)
 :functions '(chord2c)
 :subpackages nil)
