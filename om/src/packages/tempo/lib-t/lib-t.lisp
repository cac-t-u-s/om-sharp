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

(in-package :cl-user)


(defpackage :lib-t)
(in-package :lib-t)

(cffi:defcfun ("t_req_s" t_req_s) :pointer (b :pointer))
(cffi:defcfun ("t_req_u" t_req_u) :boolean (b :pointer))

(defvar lib-t::*lib-t* nil)

(defun load-lib-t ()
  (when (setf lib-t::*lib-t*
              (om-fi::om-load-foreign-library
               "LIB-T"
               `((:macosx ,(om-fi::om-foreign-library-pathname "libt.dylib"))))))
  (print "LIB-T LOADED!"))

(push :lib-t *features*)

;; load now
;; (load-t-lib)

;; load at OM startup
#+macosx(om-fi::add-foreign-loader 'load-lib-t)

