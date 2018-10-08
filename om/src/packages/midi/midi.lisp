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

(defpackage :om-midi)

(compile&load (merge-pathnames "midi-api/midi-api" *load-pathname*))
(compile&load (merge-pathnames "midi-setup" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-event" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-controllers" *load-pathname*))
(compile&load (merge-pathnames "objects/piano-roll" *load-pathname*))

(omNG-make-package 
 "MIDI"
 :container-pack *om-package-tree*
 :doc "MIDI tools and objects"
 :classes '(piano-roll midi-note)
 :functions '(import-midi-notes)
 :subpackages nil)
 