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

