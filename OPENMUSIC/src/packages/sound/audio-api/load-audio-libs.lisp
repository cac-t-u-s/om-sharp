;;===========================================================================
; OM AUDIO API 
;;===========================================================================
(in-package :cl-user)

(defpackage :om-audio
  (:nicknames "AU")
  (:use cl-user common-lisp))

(push :om-audio *features*)

(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "libsndfile")) :name "libsndfile"))
(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "libsamplerate")) :name "libsamplerate"))

;(compile&load (make-pathname :directory  (append (pathname-directory *load-pathname*) (list "libportaudio")) :name "load-portaudio"))
;(compile&load (make-pathname :directory  (append (pathname-directory *load-pathname*) (list "libportaudio")) :name "portaudio-api"))

(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "libsndfile")) :name "sndfile-api"))

(compile&load (make-pathname :directory  (pathname-directory *load-pathname*) :name "audio-api"))

#+(or macosx windows)
;(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "libaudiostream")) :name "LibAudioStream"))
#+macosx(compile&load (make-pathname :directory (append (pathname-directory *load-pathname*) (list "omjuceaudiolib")) :name "OMJuceAudioLib"))


(in-package :om-audio)

;(las::libversion)

(defun load-audio-libs ()

 ; #+libaudiostream
 ; (om-fi::om-load-foreign-library  
 ;  "LIBAUDIOSTREAM"
 ;  `((:macosx (:or 
 ;              ,(om-fi::om-foreign-library-pathname "LibAudioStreamMC.framework/LibAudioStreamMC")
 ;              ;(:framework "LibAudioStreamMC")
 ;              ))
 ;    (:windows (:or 
 ;               ,(om-fi::om-foreign-library-pathname "LibAudioStream2.dll")
 ;               (:default "LibAudioStream")))
 ;    ((:default "LibAudioStream"))))


  ;#+omjuceaudiolib
  ;(om-fi::om-load-foreign-library  
  ; "OMJuceAudioLib"
  ; `((:macosx (:or ,(om-fi::om-foreign-library-pathname "OMJuceAudioLib.dylib")))))

  
  #-macosx 
  (om-fi::om-load-foreign-library 
   "LIBSNDFILE"
   (list (list :unix  '(:default "libsndfile"))
	 (list :windows (list :or (om-fi::om-foreign-library-pathname "libsndfile-1.dll")
			      '(:default "libsndfile-1")))
	 (list t '(:default "libsndfile"))))

  #-macosx 
  (om-fi::om-load-foreign-library 
   "LIBSAMPLERATE"
   (list (list :unix  '(:default "libsamplerate"))
	 (list :windows (list :or (om-fi::om-foreign-library-pathname "libsamplerate.dll")
			      '(:default "libsamplerate")))
	 (list t '(:default "libsamplerate"))))
  )

(om-fi::add-foreign-loader 'load-audio-libs)



#|
  (om-load-foreign-library 
   "LIBPORTAUDIO"
   (list (list :macosx (list :or 
                             (oa::om-foreign-library-pathname "libportaudio.dylib")
                             '(:default "libportaudio")))
         (list :windows (list :or (oa::om-foreign-library-pathname "libportaudio.dll")
                              '(:default "libportaudio")))
         (list t '(:default "libportaudio"))))
|#


