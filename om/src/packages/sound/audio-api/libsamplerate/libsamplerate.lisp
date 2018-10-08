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
; File author: D. Bouche
;============================================================================

(in-package :cl-user)

(defpackage "LibSampleRate"
  (:nicknames "LSR")
  (:use common-lisp cffi))

(in-package :lsr)

(pushnew :libsamplerate *features*)

(defparameter SRC_SINC_BEST_QUALITY 0)
(defparameter SRC_SINC_MEDIUM_QUALITY 1)
(defparameter SRC_SINC_FASTEST 2)
(defparameter SRC_ZERO_ORDER_HOLD 3)
(defparameter SRC_LINEAR 4)

(defcstruct SRC_DATA 
  (data_in :pointer)
  (data_out :pointer)
  (input_frames :long)
  (output_frames :long)
  (input_frames_used :long)
  (output_frames_gen :long)
  (end_of_input :int)
  (src_ratio :double))

(defcstruct SRC_CB_DATA 
  (frames :long)
  (data_in :pointer))

(defcfun (src-simple "src_simple") :int 
         (src-data :pointer) 
         (converter-type :int) 
         (channels :int))

(defcfun (src-strerror "src_strerror") :string 
         (error :int))



(defun resample-audio-buffer (in-buffer in-size n-channels out-buffer out-size ratio method)
  (cffi:with-foreign-object (lsrdata '(:struct lsr::src_data))
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_in) in-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::input_frames) in-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::data_out) out-buffer)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames) out-size)
    (setf (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::src_ratio) ratio)
    (let ((res (lsr::src-simple lsrdata method n-channels)))
      (if (= res 0)
          (values T (cffi:foreign-slot-value lsrdata '(:struct lsr::src_data) 'lsr::output_frames_gen))
        (values NIL (lsr::src-strerror res))))))




