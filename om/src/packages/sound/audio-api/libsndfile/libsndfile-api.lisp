;============================================================================
; o7: visual programming language for computer-aided music composition
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

(in-package :sf)


(defconstant formatAiff 0)
(defconstant formatWave 1)
(defconstant formatAifc 2)
(defconstant formatWAVint 0)
(defconstant formatWAVfloat 1)
(defconstant formatAIFFint 2)
(defconstant formatAIFFfloat 3)

(defun decode-format (sndfile-format)
 (let* ((format_list (map 'list #'digit-char-p (prin1-to-string (write-to-string sndfile-format :base 16))))
        (ff (cond ((or (null (cadr format_list)) (null (cadddr (cddr format_list)))) nil)
                  ((and (= 1 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) 0)
                  ((and (= 1 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) 1)
                  ((and (= 2 (cadr format_list)) (< (cadddr (cddr format_list)) 6)) 2)
                  ((and (= 2 (cadr format_list)) (>= (cadddr (cddr format_list)) 6)) 3)
                  (t 0)))
        (ss (cond ((null (cadddr (cddr format_list))) nil)
                  ((= 1 (cadddr (cddr format_list))) 8)
                  ((= 2 (cadddr (cddr format_list))) 16)
                  ((= 3 (cadddr (cddr format_list))) 24)
                  ((= 4 (cadddr (cddr format_list))) 32)
                  ((= 5 (cadddr (cddr format_list))) 8)
                  ((= 6 (cadddr (cddr format_list))) 32)
                  (t 0)))
        (name (case ff
                (0 "Wav(int)")
                (1 "Wav(float)")
                (2 "AIFF(int)")
                (3 "AIFF(float)")
                (otherwise "Unknown")
                )))
   (values ff ss name)))
  
;(cadddr (cddr (map 'list #'digit-char-p (prin1-to-string (write-to-string SF_FORMAT_WAV :base 16)))))
;(write-to-string 255 :base 16)
;(logior (ash sf::sf_format_aiff 1) (ash b 8) c)

;;; READ
(defun sndfile-get-info (path)
  "Returns info about the soudn file (not the actual data)."
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (size (fli::dereference size-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (channels (fli::dereference channels-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (sr (fli::dereference sr-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format))
           (format (fli::dereference format-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable)))
      (multiple-value-bind (ff ss nn)
          (decode-format format)
        ;;;Detection format and Sample size : cf http://www.mega-nerd.com/libsndfile/api.html#open 
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
        (values nn channels sr ss size skip)))))


(defun sndfile-get-sound-buffer (path &optional (datatype :float))
  "Returns a sound data buffer + info. The soudn buffer must be freed."
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (size (fli::dereference size-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (channels (fli::dereference channels-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (sr (fli::dereference sr-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format))
           (format (fli::dereference format-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable))
           (buffer-size (* size channels))
           (buffer (fli:allocate-foreign-object :type datatype :nelems buffer-size :fill 0))
           (frames-read 
            (ignore-errors
              (case datatype
                (:double (sf::sf-readf-double sndfile-handle buffer buffer-size))
                (:float (sf::sf-readf-float sndfile-handle buffer buffer-size))
                (:int (sf::sf-readf-int sndfile-handle buffer buffer-size))
                (:short (sf::sf-readf-short sndfile-handle buffer buffer-size))
                (otherwise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype))))))))
      (multiple-value-bind (ff ss nn)
          (decode-format format)
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
        (values buffer nn channels sr ss size skip)))))

;; same function...
;; can we get a de-interleaved buffer with SNDfile ?
(defun sndfile-get-2D-sound-buffer (path &optional (datatype :float))
  "Returns a sound data buffer + info. The soudn buffer must be freed."
  (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
    (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) 0) ; Initialize the slots
    (let* ((sndfile-handle (sf::sf_open path sf::SFM_READ sfinfo))
           (size-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::frames))
           (size (fli::dereference size-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (channels-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels))
           (channels (fli::dereference channels-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (sr-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate))
           (sr (fli::dereference sr-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (format-ptr (cffi:foreign-slot-pointer sfinfo '(:struct |libsndfile|::sf_info) 'sf::format))
           (format (fli::dereference format-ptr :type :int :index #+powerpc 1 #-powerpc 0))
           (skip (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::seekable))
           (buffer-size (* size channels))
           (buffer (fli:allocate-foreign-object :type datatype :nelems buffer-size :fill 0))
           (frames-read 
            (ignore-errors
              (case datatype
                (:double (sf::sf-readf-double sndfile-handle buffer buffer-size))
                (:float (sf::sf-readf-float sndfile-handle buffer buffer-size))
                (:int (sf::sf-readf-int sndfile-handle buffer buffer-size))
                (:short (sf::sf-readf-short sndfile-handle buffer buffer-size))
                (otherwise (print (concatenate 'string "Warning: unsupported datatype for reading audio data: " (string datatype))))))))
      (multiple-value-bind (ff ss nn)
          (decode-format format)
        (sf::sf_close sndfile-handle) ; should return 0 on successful closure.
        (values buffer nn channels sr ss size skip)))))


;; WRITE 
(defun sndfile-save-sound-in-file (buffer filename size nch sr resolution format &optional (datatype :float))
  (let* ((res (case resolution
               (8 sf::sf_format_pcm_s8)
               (16 sf::sf_format_pcm_16)
               (24 sf::sf_format_pcm_24)
               (32 sf::sf_format_pcm_32)              
               (otherwise sf::sf_format_pcm_16)))
        (format (logior (case format 
                          (:aiff sf::sf_format_aiff)
                          (:wav sf::sf_format_wav)
                          (:ogg sf::sf_format_ogg)
                          (:flac sf::sf_format_flac)
                          (otherwise sf::sf_format_aiff))
                        res)))
        
    (cffi:with-foreign-object (sfinfo '(:struct |libsndfile|::sf_info))
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::samplerate) sr)
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::channels) nch)
      (setf (cffi:foreign-slot-value sfinfo '(:struct |libsndfile|::sf_info) 'sf::format) format)

      (let ((sndfile-handle-out (sf::sf_open filename sf::SFM_WRITE sfinfo)))
            ;(datatype (fli::pointer-element-type buffer))  ;; not reliable all the time :(
        (case datatype
          (:double (sf::sf-write-double sndfile-handle-out buffer (* nch size)))
          (:float (sf::sf-write-float sndfile-handle-out buffer (* nch size)))
          (:int (sf::sf-write-int sndfile-handle-out buffer (* nch size)))
          (:short (sf::sf-write-short sndfile-handle-out buffer (* nch size)))
          (otherwise (print (concatenate 'string "Warning: unsupported datatype for writing audio data: " (string datatype)))))
        
        (sf::sf_close sndfile-handle-out)
        )))
  (probe-file filename))


