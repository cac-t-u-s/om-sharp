;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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

;;==================================
;;; AUDIO FILE ACCESS TOOLS (R/W)
;;==================================

(in-package :cl-user)

;;; genertal package for low-level audio features
(defpackage :audio-io
  (:use cl-user common-lisp))

(in-package :audio-io)

(export '(
          om-get-sound-info
          om-get-audio-buffer
          om-free-audio-buffer
          om-save-buffer-in-file
          ) :audio-io)

;;==================================
;;; FILE I/O
;;==================================

;;;Convert path
(defun convert-filename-encoding (path) (namestring path))

;  doesn't work ? when is it used ?
;  #+cocoa (external-format::decode-external-string (external-format::encode-lisp-string (namestring path) :utf-8) :latin-1)
;  #-cocoa (namestring path))

; RETURNS: format n-channels sample-rate sample-size size
(defun om-get-sound-info (path)
  (juce::juce-get-sound-info (convert-filename-encoding path)))

; RETURNS: buffer format n-channels sample-rate sample-size size skip
(defun om-get-audio-buffer (path &optional (type :float) (interleaved nil))
  (juce::juce-get-sound-buffer (convert-filename-encoding path) type))

(defun om-free-audio-buffer (buffer nch)
  (when nch (dotimes (c nch) (fli::free-foreign-object (fli:dereference buffer :type :pointer :index c))))
  (fli::free-foreign-object buffer))

;;; audio buffer NOT interleaved
(defun om-save-buffer-in-file (buffer filename size nch sr resolution format)
  (juce::juce-save-sound-in-file buffer filename size nch sr resolution format))


#|
;;; USE LIBSNDFILE API

;;; interleave-util code
;(itl-buffer
; (if (> nch 1)
;     (fli:allocate-foreign-object :type :float :nelems (* nsmp nch))
;   (cffi::mem-aref (oa::om-pointer-ptr (buffer sound)) :pointer 0)))
;(when (> nch 1) 
;  (interleave-buffer (oa::om-pointer-ptr (buffer sound)) itl-buffer nsmp nch))
;(when (> nch 1) (om-free-memory itl-buffer))


;;; Acquire sound infos
(defun om-get-sound-info (path)
  ;; RETURNS format n-channels sample-rate sample-size size skip
  (sf::sndfile-get-sound-info (convert-filename-encoding path))
  )

;; RETURNS buffer format n-channels sample-rate sample-size size skip
(defun om-get-sound-buffer (path &optional (type :float) (interleaved nil))
  (when (probe-file path)
    (if interleaved 
    
        (sf::sndfile-get-sound-buffer (convert-filename-encoding path) type)

      (multiple-value-bind (buffer format n-channels sample-rate sample-size size skip)
        
          (sf::sndfile-get-sound-buffer (convert-filename-encoding path) type)
      
        (if (= 1 n-channels)
            
            (let ((buffer2 (fli::allocate-foreign-object :type :pointer)))
              (setf (fli:dereference buffer2) buffer)
              (values buffer2 format n-channels sample-rate sample-size size skip))
        
          (unwind-protect 
                
              (let ((buffer2 (fli::allocate-foreign-object 
                              :type :pointer :nelems n-channels
                              :initial-contents (loop for c from 0 to (1- n-channels) collect 
                                                      (fli::allocate-foreign-object 
                                                       :type type 
                                                       :nelems size)))))
                (dotimes (i size)
                  (dotimes (c n-channels)
                    ;;; apparently FLI:DEREFERENCE iS MUCH FASTER THAN CFFI:MEM-AREF
                    (setf (fli:dereference (fli:dereference buffer2 :type :pointer :index c) :type type :index i)
                          (fli:dereference buffer :type type :index (+ c (* n-channels i))))
                      ;(setf (cffi::mem-aref (cffi::mem-aref buffer2 :pointer c) type i)
                      ;      (cffi::mem-aref buffer type (+ c (* n-channels i))))
                    ))
                (values buffer2 format n-channels sample-rate sample-size size skip))
            (cffi::foreign-free buffer)))))))

(defun om-save-buffer-in-file (buffer filename size nch sr resolution format)
  (let ((interleaved (fli:allocate-foreign-object :type :float :nelems (* (n-samples self) (n-channels self)) :fill 0)))
    (dotimes (smp size)
      (dotimes (ch nch)
        (setf (cffi::mem-aref interleaved :float (+ (* smp channels) ch))
              (cffi::mem-aref (cffi::mem-aref buffer :pointer ch) :float smp))))
    (unwind-protect 
        (sf::sndfile-save-sound-in-file buffer filename size nch sr resolution format)
      (fli::free-foreign-object interleaved))
    ))



|#
