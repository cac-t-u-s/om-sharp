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

;;===========================================================================
; OM AUDIO API 
;;===========================================================================
(in-package :cl-user)

(compile&load (decode-local-path "libsamplerate/libsamplerate"))
(compile&load (decode-local-path "omaudiolib/omaudiolib"))
(compile&load (decode-local-path "file-access"))


(push :audio *features*)

(defun load-audio-libs ()

  (om-fi::om-load-foreign-library  
   "OMAudioLib"
   `((:macosx ,(om-fi::om-foreign-library-pathname "OMAudioLib.dylib"))
     (:windows ,(om-fi::om-foreign-library-pathname "OMAudioLib.dll"))
     (:linux ,(om-fi::om-foreign-library-pathname "OMAudioLib.so"))))
  
;  (om-fi::om-load-foreign-library 
;   "LIBSNDFILE"
;   `((:macosx ,(om-fi::om-foreign-library-pathname "libsndfile.dylib"))
;     (:unix  (:default "libsndfile"))
;     (:windows (:or ,(om-fi::om-foreign-library-pathname "libsndfile-1.dll")
;                (:default "libsndfile-1")))
;     (t (:default "libsndfile"))))

  (om-fi::om-load-foreign-library 
   "LIBSAMPLERATE"
   `((:macosx ,(om-fi::om-foreign-library-pathname "libsamplerate.dylib"))
     (:unix (:default "libsamplerate"))
     (:windows (:or ,(om-fi::om-foreign-library-pathname "libsamplerate-0.dll")
                (:default "libsamplerate")))
     (t (:default "libsamplerate"))))
  )

(om-fi::add-foreign-loader 'load-audio-libs)





