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
; File authors: J. Bresson
;============================================================================

(in-package :om)

(defclass OMBoxMaquette (OMBoxPatch) ())

(defmethod special-box-p ((name (eql 'maquette))) t)

(defmethod get-box-class ((self OMMaquette)) 'OMBoxMaquette)

(defmethod omNG-make-special-box ((reference (eql 'maquette)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMMaquetteInternal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "new-maquette"))
   pos 
   init-args ;; don't need to pass them in principle..
   ))
