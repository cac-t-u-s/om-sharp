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
; File author: J. Bresson
;============================================================================


(in-package :om)

(defvar *default-audio-channels* 2)
(defvar *default-audio-sr* 44100)
(defvar *default-audio-type* :float)
(defvar *default-audio-format* :aiff)
(defvar *default-audio-resolution* 24)



(defmethod* adsr (amp1 amp2 a d s r &optional (decimals 3))
    :icon 110
    :indoc '("max amplitude" "sustained amplitude" "attack time" "decay time" "sustain time" "release time" "BPF precision")
    :initvals '(1 0.8 0.2 0.1 0.5 0.2 3)
    :doc "Generates an ADSR BPF (Attack-Decay-Sustain-Release)

If either <amp2> or <d> is NIL, generates a simple envelope with attack and release time (no decay).
"
    (if (and amp2 d)        
        (make-instance 'BPF 
                       :x-points (list 0 a (+ a d) (+ a d s) (+ a d s r))
                       :y-points (list 0 amp1 amp2 amp2 0)
                       :decimals decimals)
      (make-instance 'BPF 
                     :x-points(list 0 a (+ a s) (+ a s r))
                     :y-points (list 0 amp1 amp1 0)
                     :decimals decimals)))


;;;========================
;;; CONVERSIONS
;;;========================

;;; fromerly lintodb ;;;
(defun lin-to-dB (x)
  (let ((y (if (= 0.0 x) 0.00000001 x)))
    (* (log y 10) 20)))

(defun db-to-lin (x)
  (expt 10.0 (/ x 20.0)))

;;; DB / LIN
(defmethod* dB->lin ((x t))
  :icon 'conversion
  :indoc '("a value or list of values in dB")
  :initvals '(-20)
  :doc "Converts <x> from dB to linear value."
  (cond ((numberp x) (db-to-lin x))
        ((listp x) (mapcar #'(lambda (y) (dB->lin y)) x))
        (t (error "illegal arg ~a" x))))

(defmethod* lin->dB ((x t))
  :icon 'conversion
  :indoc '("a value or list of values")
  :initvals '(0.1)
  :doc "Converts <x> from linear to dB."
  (cond((numberp x) (lin-to-db x))
       ((listp x) (mapcar #'lin->dB x))
       (t (error "illegal arg ~a" x))))


;;; SAMPLES / SECONDS
(defmethod* sec->ms ((n number))
  :icon 'conversion
  :initvals '(0)
  :indoc '("seconds")
  :numouts 1
  :doc "Converts <n> (seconds / floats) to milliseconds (intergers)."
  (round (* n 1000)))

(defmethod* sec->ms ((n list)) (mapcar #'(lambda (s) (sec->ms s)) n))

(defmethod* ms->sec ((n number))
  :icon 'conversion
  :initvals '(0)
  :indoc '("milliseconds")
  :numouts 1
  :doc "Converts <n> (milliseconds / integers) to seconds (floats)."
  (* n 0.001))

(defmethod* ms->sec ((n list)) (mapcar #'(lambda (s) (ms->sec s)) n))

;;; SAMPLES / SECONDS
(defmethod* samples->sec ((samples number) samplerate)
  :icon 'conversion
  :initvals '(0 nil)
  :indoc '("number of samples" "sample rate (Hz)")
  :numouts 1
  :doc "Converts <samples> to a time (or duration) in seconds depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the time."
   (float (/ samples (or samplerate *default-audio-sr*))))

(defmethod* samples->sec ((samples list) samplerate)
  (mapcar #'(lambda (input) (samples->sec input samplerate)) samples))

(defmethod* sec->samples ((secs number) samplerate) 
  :icon 'conversion
  :initvals '(0 nil)
  :indoc '("duration (s)" "sample rate (Hz)")
  :numouts 1
  :doc "Converts <secs> to a number of samples depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the samples."
  (round (* secs (or samplerate *default-audio-sr*))))

(defmethod* sec->samples ((secs list) (samplerate number)) 
  (mapcar #'(lambda (input) (sec->samples input samplerate)) secs))


;;; SAMPLES / MILLISECONDS
(defmethod* samples->ms ((samples number) samplerate)
  :icon 'conversion
  :initvals '(0 nil)
  :indoc '("number of samples" "sample rate (Hz)")
  :numouts 1
  :doc "Converts <samples> to a time (or duration) in milliseconds depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the time."
  (* (/ samples (or samplerate *default-audio-sr*)) 1000.0))

(defmethod* samples->ms ((samples list) samplerate)
  (mapcar #'(lambda (input) (samples->ms input samplerate)) samples))

(defmethod* ms->samples ((ms number) samplerate) 
  :icon 'conversion
  :initvals '(0 nil)
  :indoc '("duration (ms)" "sample rate (Hz)")
  :numouts 1
  :doc "Converts <ms> to a number of samples depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the samples."
  (round (* ms (or samplerate *default-audio-sr*) 0.001)))

(defmethod* ms->samples ((ms list) (samplerate number)) 
  (mapcar #'(lambda (input) (ms->samples input samplerate)) ms))
           


;;;========================
;;; MISC. UTILS
;;;========================

(defun closest-pwr-of-2 (val)
  (let ((size 2))
    (loop while (> val size) do
          (setf size (* size 2)))
    size))

(defun clip (val &optional (min 0.0) (max 1.0))
" If val is below min, return min,
  if val is above max, return max,
  otherwise return val.
" 
  (let ((from min) (to max))
    (when (> min max) (setf from max) (setf to min))
    (cond
     ((> val to) to)
     ((< val from) from)
     (t val))))
