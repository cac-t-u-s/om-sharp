(in-package :cl-user)

(defpackage :spat)

(compile&load (merge-pathnames "omspat-utils" *load-pathname*))
(compile&load (merge-pathnames "omspat-dsp" *load-pathname*))
(compile&load (merge-pathnames "omspat-gui" *load-pathname*))

(defvar spat::*omspat-lib* nil)

(defun load-omspat-lib ()
  (when 
      (setf spat::*omspat-lib*
            (om-fi::om-load-foreign-library
             "LIBOMSPAT"
             `((:macosx ,(om-fi::om-foreign-library-pathname "OMSpat.framework/OMSpat"))
               (:windows (:or ,(om-fi::om-foreign-library-pathname "libomspat.dll")
                          '(:default "libomspat")))
               (t (:default "libomspat")))))
    (spat::OmSpatInitialize)))

(push :om-spat *features*)

;; load now
;; (load-omspat-lib)

;; load at OM startup
#+macosx(om-fi::add-foreign-loader 'load-omspat-lib)

