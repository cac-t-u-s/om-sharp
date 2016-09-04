;;; Binding TIME functions from MacOSX System framework
;;; D. bouche (2014)

(in-package :cl-user)

(defpackage "Mach"
  (:nicknames "MACH")
  (:use common-lisp fli))

(in-package :mach)

(define-c-typedef Sint32 (:signed :long))
(define-c-typedef Uint32 (:unsigned :long))
(define-c-typedef Uint64 (:unsigned :long :long))

(define-c-struct mach_timebase_info
  (num UInt32)
  (den UInt32))

(define-foreign-function (mach_timebase_info "mach_timebase_info") 
    ((info (:ptr mach_timebase_info)))
  :module :system
  :result-type Sint32)

(define-foreign-function (mach_absolute_time "mach_absolute_time") ()
  :module :system
  :result-type Uint64)

(define-foreign-function (mach_wait_until "mach_wait_until") 
    ((deadline UInt64))
  :module :system
  :result-type Sint32)

(defvar *mach2nanosFactor* 1)

(defun init-nanofactor ()
  (setf *mach2nanosFactor*
        (fli:with-dynamic-foreign-objects ((timeBase mach::mach_timebase_info))
          (let ((result (mach::mach_timebase_info timeBase)))
            (if (zerop result)
                (/ (fli::foreign-slot-value timeBase 'mach::num) (fli::foreign-slot-value timeBase 'mach::den))
              (error (format nil "mach_timebase_info error ~d" result)))))))

;; internal
(defun mach2ms (mach) 
  ;(round (* *mach2nanosFactor* mach) 1000000)
  (round mach 1000000))
(defun ms2mach (ms) 
  ;(truncate (/ (* ms 1000000) *mach2nanosFactor*))
  (truncate (* ms 1000000)))

;; used in API/multiprocess
(defun mach-time ()
  (mach2ms (mach::mach_absolute_time)))

(defun mach-wait-delay (delay-ms) 
  (mach::mach_wait_until (+ (mach::mach_absolute_time) (ms2mach delay-ms))))

(defun mach-wait-until (absolute-time-ms) 
  (mach::mach_wait_until (max (ms2mach absolute-time-ms) 0)))


(defvar mach::*ommach-lib* nil)

(push :mach *features*)

(defun load-mach-lib ()
  (print "Loading System library")
  (setf mach::*ommach-lib*
        ;(om-fi::om-load-foreign-library 
        ; "LIBMACH"
        ; `((:macosx "/System/Library/Frameworks/System.framework/System") 
        ;   (t (:default "System"))))
        (fli::register-module 
         :system :connection-style :immediate
         :real-name "/System/Library/Frameworks/System.framework/System" )
        )
  (mach::init-nanofactor))

;;; CALL THIS BEFORE !
;;;(mach::load-mach-lib)
