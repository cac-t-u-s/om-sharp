;============================================================================
; om#: visual programming language for computer-assisted music composition
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

;===========================================================================
; loads the SDIF API
;===========================================================================

(in-package :cl-user)

(defpackage "SDIF-PACKAGE"
  (:nicknames "SDIF")
  (:use :common-lisp :cffi))

(defvar sdif::*sdif-library* nil)

(compile&load (merge-pathnames "sdif" *load-pathname*))
(compile&load (merge-pathnames "sdif-api" *load-pathname*))

(pushnew :sdif *features*)

;;;==============================
;;; CHARGEMENT

(defun load-sdif-lib ()
  (setf sdif::*sdif-library*
        (om-fi::om-load-foreign-library
         "SDIF"
         `((:macosx ,(om-fi::om-foreign-library-pathname "libSDIF.dylib"))
           (:windows (:or ,(om-fi::om-foreign-library-pathname "libsdif.dll") (:default "sdif")))
           (:linux (:or "libsdif.so" ,(om-fi::om-foreign-library-pathname "libsdif.so")))
           (t (:default "libsdif")))))
  (setf sdif::*sdif-initialized* NIL))

(om-fi::add-foreign-loader 'load-sdif-lib)





