(in-package :cl-user)

; (make-ffi (merge-pathnames "iae.txt" *load-pathname*) (merge-pathnames "iae.lisp" *load-pathname*) :iae)
        
(defpackage :IAE 
  (:use :common-lisp :cl-user))

(compile&load (merge-pathnames "iae" *load-pathname*))
(compile&load (merge-pathnames "iae-om-objects" *load-pathname*))

(defvar iae::*iae-lib* nil)

(defun load-iae-lib ()
  (when 
      (setf iae::*iae-lib*
            (om-fi::om-load-foreign-library 
             "IAE"
             `((:macosx ,(om-fi::om-foreign-library-pathname "iaeom.framework/iaeom"))
               (t (:default "libiaeom")))))
    ))

(push :iae *features*)

;; load now
;; (load-iae-lib)

;; load at OM startup
#+macosx
(om-fi::add-foreign-loader 'load-iae-lib)



#|
(in-package :iae)

(probe-file "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet2.aif")
(setf *iae* (iae::iae_new 44100 44100 1 1 1))
(iae::iae_read *iae* "/Users/bresson/_SHARED-FILES/IN-FILES/SOUNDFILES/Bassclarinet2.aif" (oa::om-make-null-pointer))
(iae::iae_get_NumSources *iae*)
(iae::iae_set_position *iae* 5000.0d0 0.0d0)
(iae::iae_set_period *iae* -0.0d0 0.0d0)
(iae::iae_set_duration *iae* 200.0d0 0.0d0)
(iae::iae_set_play *iae* t)
(iae::iae_set_play *iae* nil)
  

(defparameter *audio-buffer* (om::make-audio-buffer 1 44100))

(iae::iae_synth *iae* 44100 *audio-buffer* 1)

(iae::iae_delete *iae*)


(in-package :om)



(defun get-iae-snd (size) 
  (let ((buffer (make-audio-buffer 1 size)))
    (iae::iae_synth iae::*iae* 44100 buffer 1)
    (let ((snd (make-instance 'om::om-internal-sound :n-channels 1 :smpl-type :float
                              :n-samples 44100 :sample-rate 44100)))
      (setf (buffer snd)
            (om::make-om-sound-buffer :ptr buffer :count 1 :nch 1))
      snd)))



|#
