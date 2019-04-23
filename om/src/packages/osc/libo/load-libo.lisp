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


(in-package :cl-user)

(defpackage :odot
  (:use :common-lisp :cl-user))

(compile&load (merge-pathnames "o" *load-pathname*))
(compile&load (merge-pathnames "networking-utils" *load-pathname*))
(compile&load (merge-pathnames "odot" *load-pathname*))

(defvar odot::libo nil)

(defun load-o-lib ()
  (setf odot::libo
        (om-fi::om-load-foreign-library 
         "O."
         `((:macosx ,(om-fi::om-foreign-library-pathname "libo.dylib"))
           (t (:default "libo"))))))

(push :odot *features*)

;; load now
;; (load-o-lib)

;; load at OM startup
#+macosx(om-fi::add-foreign-loader 'load-o-lib)

; (odot::osc_bundle_u_alloc)