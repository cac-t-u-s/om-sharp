;;===========================================================================
; OM AUDIO API 
;;===========================================================================
(in-package :cl-user)

(compile&load (decode-local-path "libsndfile/libsndfile"))
(compile&load (decode-local-path "libsndfile/libsndfile-api"))
(compile&load (decode-local-path "file-access"))
(compile&load (decode-local-path "libsamplerate/libsamplerate"))
(compile&load (decode-local-path "libsamplerate/libsamplerate-api"))
(compile&load (decode-local-path "omjuceaudiolib/omjuceaudiolib"))


(push :audio *features*)


(defun load-audio-libs ()

  (om-fi::om-load-foreign-library  
   "OMJuceAudioLib"
   `((:macosx ,(om-fi::om-foreign-library-pathname "OMJuceAudioLib.dylib"))
     (:windows ,(om-fi::om-foreign-library-pathname "OMJuceAudioLib.dll"))
     (:linux ,(om-fi::om-foreign-library-pathname "OMJuceAudioLib.so"))))
  
  (om-fi::om-load-foreign-library 
   "LIBSNDFILE"
   `((:macosx ,(om-fi::om-foreign-library-pathname "libsndfile.dylib"))
     (:unix  (:default "libsndfile"))
     (:windows (:or ,(om-fi::om-foreign-library-pathname "libsndfile-1.dll")
                (:default "libsndfile-1")))
     (t (:default "libsndfile"))))

  (om-fi::om-load-foreign-library 
   "LIBSAMPLERATE"
   `((:macosx ,(om-fi::om-foreign-library-pathname "libsamplerate.dylib"))
     (:unix (:default "libsamplerate"))
     (:windows (:or ,(om-fi::om-foreign-library-pathname "libsamplerate.dll")
                (:default "libsamplerate")))
     (t (:default "libsamplerate"))))
  )

(om-fi::add-foreign-loader 'load-audio-libs)





