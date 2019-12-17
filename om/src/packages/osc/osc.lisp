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

(require-om-package "basic")

;;; CL-OSC
(load (merge-pathnames "cl-osc/osc.asd" *load-pathname*))
(asdf:operate 'asdf:load-op 'osc)
(push :osc *features*)

(compile&load (decode-local-path "osc-om/osc-struct"))
(compile&load (decode-local-path "osc-om/osc-send-receive"))
(compile&load (decode-local-path "osc-om/osc-route"))

(omNG-make-package 
 "OSC"
 :container-pack *om-package-tree*
 :doc "Tools for manipulating/communicating with data using the Open Sound Protocol"
 :classes '(osc-bundle)
 :functions '(osc-msg osc-set osc-get osc-delete osc-timetag osc-send osc-receive route-osc))
