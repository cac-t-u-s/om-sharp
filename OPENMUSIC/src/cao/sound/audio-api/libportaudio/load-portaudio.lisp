;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cl:defpackage "PortAudio-Vars"
  (:nicknames "PAV")
   (:use common-lisp cl-user))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :pav)

;Set the portaudio folder path
(defconstant *portaudio-path* (namestring (make-pathname :directory (pathname-directory *load-pathname*))))

;Dependencies (do not modify)
(defconstant *cl-utilities-subdir* "cl-utilities-1.2.4/")
(defconstant *bind-subdir* "metabang-bind/")
(defconstant *iterate-subdir* "iterate-1.4.3/")
(defconstant *ffa-subdir* "ffa/")

;Load files (do not modify)
(load (concatenate 'string *portaudio-path* *cl-utilities-subdir* "cl-utilities.asd"))
(asdf:operate 'asdf:load-op 'cl-utilities)

(load (concatenate 'string *portaudio-path* *bind-subdir* "metabang-bind.asd"))
(asdf:operate 'asdf:load-op 'metabang-bind)

(load (concatenate 'string *portaudio-path* *iterate-subdir* "iterate.asd"))
(asdf:operate 'asdf:load-op 'iterate)

(load (concatenate 'string *portaudio-path* *ffa-subdir* "ffa.asd"))
(asdf:operate 'asdf:load-op 'ffa)

;(fli:register-module "libportaudio" 
;                     :real-name (concatenate 'string pav::*portaudio-path* "libportaudio.dylib")
;                     :connection-style :immediate)

(load (concatenate 'string *portaudio-path* "cl-portaudio/cl-portaudio.asd"))
(asdf:operate 'asdf:load-op 'cl-portaudio)

; (om::om-beep)
; (portaudio-tests:test-read-write-echo)
; (pa::initialize)
; (pa::terminate)
; (pa::print-devices)




