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


;;; compatibility for om6 code (e.g. in external libraries)
(defmethod filepathname ((self sdiffile)) (file-pathname self))
(defmethod streamsdesc ((self sdiffile)) (file-map self))

;;; for OM patches
(defmethod function-changed-name ((reference (eql 'f0->pf))) 'sdif->bpf)
