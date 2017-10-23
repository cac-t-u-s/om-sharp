;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed; in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

(in-package :om)

(load (om-relative-path '("lib-t") "lib-t"))

(defun libt-call (o.bundle)
  (when (bundle_s o.bundle)
    (let ((newptr (lib-t::t_req_s (oa::om-pointer-ptr (bundle_s o.bundle))))
          (out (make-instance 'o.bundle)))
      (assign-foreign-pointer out newptr)
      out)))
