;============================================================================
; om#: visual programming language for computer-assisted music composition
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
; File authors: D. Bouche, J. Bresson
;============================================================================

(in-package :om)

;======================================================
;SND Process boxes
; THIS FILE USES LISPWORKS-SPECIFIC TOOLS FOR MEMORY ALLOCATION AND RELEASE
;======================================================
; D. Bouche 2013
;======================================================


(defmethod* save-sound ((self om-internal-sound) filename &key format resolution)
  :icon 'save-sound
  :initvals '(nil nil :aiff)
  :indoc '("a sound or om-internal-sound buffer" "output file pathname" "audio format" "audio resolution (16, 24, 32)")
  :menuins '((2 (("AIFF" :aiff) ("WAV" :wav) ("FLAC" :flac) ("OGG Vorbis" :ogg)))
             (3 ((16 16) (24 24) (32 32))))
  :doc "Saves a <self> (om-internal-sound buffer) as an audio file."
  (if (null (oa::om-pointer-ptr (buffer self)))
      (om-beep-msg "Error: null sound buffer")
    (let* ((format (or format (get-pref-value :audio :format)))
           (file (or filename (om-choose-new-file-dialog :directory (def-save-directory)
                                                         :prompt (om-str "Save as...")
                                                         :types (cond ((equal format :aiff) (list (format nil (om-str :file-format) "AIFF") "*.aiff;*.aif"))
                                                                      ((equal format :wav) (list (format nil (om-str :file-format) "WAV") "*.wav"))
                                                                      ((equal format :flac) (list (format nil (om-str :file-format) "FLAC") "*.flac"))
                                                                      ((equal format :ogg) (list (format nil (om-str :file-format) "OGG Vorbis") "*.ogg"))
                                                                      (t nil)))))
           )

      (when file
        (setf *last-saved-dir* (make-pathname :directory (pathname-directory file)))
        (audio-io::om-save-buffer-in-file (oa::om-pointer-ptr (buffer self))
                                          (namestring file)
                                          (n-samples self)
                                          (n-channels self)
                                          (sample-rate self)
                                          (or resolution (get-pref-value :audio :resolution))
                                          format))

      (probe-file (namestring file)))))



(defmethod* sound-resample ((s om-internal-sound) sample-rate &optional (resample-method 0))
  :icon 'sound-resample
  :initvals '(nil 44100 0)
  :menuins '((2 (("Best Quality" 0)
                 ("Medium Quality" 1)
                 ("Fastest" 2)
                 ("Zero-Order Hold" 3)
                 ("Linear" 4))))
  :indoc '("a sound or sound-data buffer" "new sample rate in Hz" "resampling method")
  :doc "Resamples a sound <s>."

  (cond ((null (oa::om-pointer-ptr (buffer s)))
         (om-beep-msg "Error: null sound buffer"))
        ((and (= (mod sample-rate 1) 0) (> (/ sample-rate (sample-rate s) 1.0) (/ 1.0 256)) (< (/ sample-rate (sample-rate s) 1.0) 256))
         (let* ((buffer (oa::om-pointer-ptr (buffer s)))
                (size (n-samples s))
                (nch (n-channels s))
                (sr (sample-rate s))
                (ratio (coerce (/ sample-rate sr) 'double-float))
                (out-size (round (* ratio size)))
                (interleaved-in (om-alloc-memory (* size nch) :type (smpl-type s) :clear t))
                (interleaved-out (om-alloc-memory (* out-size nch) :type (smpl-type s) :clear t))
                (final-buffer (make-audio-buffer nch out-size (smpl-type s)))
                s2)

           (interleave-buffer buffer interleaved-in size nch)
           ;;; USE LIBSAMPLERATE
           ;;; (resample-method values correspond to libsamplerate options)
           (multiple-value-bind (success newsize-or-error)
               (lsr::resample-audio-buffer interleaved-in size nch interleaved-out out-size ratio resample-method)

             (if success
                 (progn
                   (split-buffer interleaved-out final-buffer out-size nch)
                   (setq s2 (make-instance 'sound
                                           :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                                           :n-samples newsize-or-error
                                           :n-channels nch
                                           :sample-rate sample-rate
                                           :smpl-type (smpl-type s))))
               (progn
                 (om-beep-msg (format nil "Resample failed to resample and returned this error : ~A. Output is the original input." newsize-or-error ))
                 (setq s2 s))))
           (om-free-memory interleaved-in)
           (om-free-memory interleaved-out)
           s2))
        (t
         (om-beep-msg "The sample-rate you supplied is invalid. It must be an integer, and the output-sr/input-sr ratio must be inside [1/256, 256] range. Output is the original input.")
         s)))



(defmethod sound-rms ((s om-internal-sound))
  :icon 'sound-normalize
  :indoc '("a sound")
  :doc "Returns the linear Root-Mean-Square (RMS) value of <s>."

  (with-audio-buffer (input-buffer s)

    (if (null (oa::om-pointer-ptr input-buffer))
        (om-beep-msg "Error: null sound buffer")

      (let* ((ptr (oa::om-pointer-ptr input-buffer))
             (type (smpl-type s))
             (nch (n-channels s))
             (size (n-samples s)))

        (let ((summed-square-signal
               (loop for i from 0 to (1- size) sum
                     (loop for n from 0 to (1- nch)
                           sum (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type)
                           into sample-sum
                           finally return (let ((mean-value (/ sample-sum nch)))
                                            (* mean-value mean-value))))))

          (sqrt (/ summed-square-signal size)))
        ))))


(defmethod* sound-normalize ((s om-internal-sound) &key (level 0) (method :peak))
  :icon 'sound-normalize
  :initvals '(nil 0 :peak)
  :menuins '((2 (("Peak" :peak)
                 ("Peak RMS / Hard limiting" :peak-rms))))
  :indoc '("a sound" "a normalization level" "a normalization method")
  :doc "Normalizes a sound <s>.

<level> is the max level of normalized samples. It is used only in the default 'peak' normalization method.
<method> is a normalization method. Choose between Peak detection or Peak RMS detection."

  (with-audio-buffer (input-buffer s)

    (if (null (oa::om-pointer-ptr input-buffer))
        (om-beep-msg "Error: null sound buffer")

      (let* ((ptr (oa::om-pointer-ptr input-buffer))
             (type (smpl-type s))
             (nch (n-channels s))
             (size (n-samples s))
             (normalize-level (if (plusp level) level (db->lin level)))
             (final-buffer (make-audio-buffer nch size type)))

        (cond

         ((equal method :peak)
          (let ((gain 0.0)
                (peak 0.0)
                (x 0.0))

            ; get peak
            (dotimes (i size)
              (dotimes (n nch)
                (setq x (abs (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type)))
                (if (> x peak) (setf peak x))))

            (when (> peak 0)
              (setq gain (/ normalize-level peak))
              (dotimes (i size)
                (dotimes (n nch)
                  (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type)
                        (* gain (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type))))))))

         ((equal method :peak-rms)
          (let ((peak-rms 0.0)
                (tampon (list))
                (indx 0)
                (rms 0.0)
                (tampon-size 100)
                (x 0.0))

                 ; get peak-rms
            (loop while (< indx size) do
                  (dotimes (i tampon-size)
                    (dotimes (n nch)
                      (push (fli:dereference (fli:dereference ptr :index n :type :pointer) :index indx :type type) tampon));)
                    (incf indx))

                  (when tampon
                    (setq tampon (mapcar #'(lambda (x) (* x x)) tampon))
                    (setq rms (sqrt (/ (reduce #'+ tampon) tampon-size)))
                    (if (> rms peak-rms) (setf peak-rms rms))
                    (setf tampon nil)))

            (dotimes (i size)
              (dotimes (n nch)
                (setq x (/ (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type) peak-rms))
                (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type)
                      (cond ((< x -1) -1.0)
                            ((> x 1) 1.0)
                            (t x))))))))

        (make-instance 'sound
                       :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                       :n-samples size
                       :n-channels nch
                       :sample-rate (sample-rate s)
                       :smpl-type type)
        ))))



(defmethod* sound-silence ((dur float) &optional (channels 1) sample-rate)
  :icon 'sound-silence
  :initvals (list 1.0 1 nil)
  :indoc '("duration (float or integer)" "number of channels" "sample rate (Hz)")
  :doc "Generates a silence of duration = <dur>.
<dur> is considered to be in seconds if a float number is given (e.g. 20.0) or in milliseconds if integer (e.g. 20)\.

<channels> is the number of channel of generated audio sound.

<sample-rate> is the output sample rate in Hz. If NIL, the sample rate is used from the 'Audio' preferences."

  (let* ((sr (or sample-rate (get-pref-value :audio :samplerate)))
         (nsmpl (round (* dur sr)))
         (ch (if (< channels 1) 1 channels)))
    (make-instance 'om-internal-sound
                   :buffer (make-om-sound-buffer-GC :ptr (make-audio-buffer ch nsmpl :float) :nch ch)
                   :n-samples nsmpl
                   :n-channels ch
                   :sample-rate sr
                   :smpl-type :float)))

(defmethod* sound-silence ((dur integer) &optional (channels 1) sample-rate)
  (let* ((sr (or sample-rate (get-pref-value :audio :samplerate)))
         (nsmpl (round (* dur (/ sr 1000.0))))
         (ch (if (< channels 1) 1 channels)))
    (make-instance 'om-internal-sound
                   :buffer (make-om-sound-buffer-GC :ptr (make-audio-buffer ch nsmpl :float) :nch ch)
                   :n-samples nsmpl
                   :n-channels ch
                   :sample-rate sr
                   :smpl-type :float)))



(defmethod* sound-fade ((s om-internal-sound) (in float) (out float))
  :icon 'sound-fade
  :initvals '(nil 0.1 0.1)
  :indoc '("a om-internal-sound" "fade in duration" "fade out duration")
  :doc "Generates a fade-in and/or fade-out effect on <s>.

             <in> and <out> can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."

  (if (null (oa::om-pointer-ptr (buffer s)))
      (om-beep-msg "Error: null sound buffer")

    (let* ((nch (n-channels s))
           (sr (sample-rate s))
           (size (n-samples s))
           (size2 (* size nch))
           (fade-in-frames (round (* in sr nch)))
           (fade-in-factor (/ 1.0 fade-in-frames))
           (fade-out-frames (round (* out sr nch)))
           (fade-out-frames-start (- size2 (round (* out sr nch))))
           (fade-out-factor (- (/ 1.0 fade-out-frames)))
           (b1 (om-alloc-memory size2 :type (smpl-type s) :clear t))
           (b2 (om-alloc-memory size2 :type (smpl-type s) :clear t))
           (out-buffer (make-audio-buffer nch size (smpl-type s)))
           s2)
      (interleave-buffer (oa::om-pointer-ptr (buffer s)) b1 size nch)
      (dotimes (i size2)
        (setf (fli:dereference b2 :index i)
              (cond ((< i fade-in-frames)
                     (* fade-in-factor i (fli:dereference b1 :index i)))
                    ((> i fade-out-frames-start)
                     (* (1+ (* fade-out-factor (- i (- size2 fade-out-frames)))) (fli:dereference b1 :index i)))
                    (t (fli:dereference b1 :index i)))))

      (setq s2 (make-instance 'sound
                              :buffer (make-om-sound-buffer-GC :ptr (split-buffer b2 out-buffer size nch) :nch nch)
                              :n-samples size
                              :n-channels nch
                              :sample-rate sr
                              :smpl-type (smpl-type s)))
      (om-free-memory b2)
      (om-free-memory b1)
      s2)))


(defmethod* sound-fade ((s om-internal-sound) (in integer) (out integer))
  (sound-fade s (ms->sec in) (ms->sec out)))



(defmethod* sound-loop ((s om-internal-sound) n)
  :icon 'sound-loop
  :initvals '(nil 3)
  :indoc '("a sound" "a number")
  :doc "Generates a <n>-times repetition of <s>."

  (if (null (oa::om-pointer-ptr (buffer s)))
      (om-beep-msg "Error: null sound buffer")
    (let* ((nch (n-channels s))
           (size (n-samples s))
           (final-buffer (make-audio-buffer nch (* n size) (smpl-type s))))

      (dotimes (i (* n size))
        (dotimes (n nch)
          (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type (smpl-type s))
                (fli:dereference (fli:dereference (oa::om-pointer-ptr (buffer s)) :index n :type :pointer) :index (mod i size) :type :float))))

      (make-instance 'sound
                     :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                     :n-samples (* n size)
                     :n-channels nch
                     :sample-rate (sample-rate s)
                     :smpl-type (smpl-type s)))))



(defmethod* sound-cut ((s om-internal-sound) (beg float) (end float))
  :icon 'sound-cut
  :initvals '(nil 0.0 1.0)
  :indoc '("a sound" "begin time" "end time")
  :doc "Cuts and returns an extract between <beg> and <end> in <s>.

            <beg> and <end> can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."

  (if (null (oa::om-pointer-ptr (buffer s)))
      (om-beep-msg "Error: null sound buffer")

    (let* ((init-buffer (oa::om-pointer-ptr (buffer s)))
           (type (smpl-type s))
           (nch (n-channels s))
           (sr (sample-rate s))
           (size (round (* (- end beg) sr)))
           (start (round (* beg sr)))
           (final-buffer (make-audio-buffer nch size type)))

      (dotimes (i size)
        (dotimes (n nch)
          (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type)
                (fli:dereference (fli:dereference init-buffer :index n :type :pointer) :index (+ start i) :type type))))

      (make-instance 'sound
                     :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                     :n-samples size
                     :n-channels nch
                     :sample-rate sr
                     :smpl-type type))))


(defmethod* sound-cut ((s om-internal-sound) (beg integer) (end integer))
  (sound-cut s (ms->sec beg) (ms->sec end)))



(defmethod* sound-gain ((s om-internal-sound) gain &optional (in 1) (out 1))
  :icon 'sound-normalize
  :initvals '(nil 1.0 1 1)
  :indoc '("a sound" "a gain value" "fade in duration" "fade out duration")
  :doc "Adds gain effect (volume) on <s>.

<gain> is a multiplicative factor to the sound sample values.
<in> and <out> determine fade-in / fade-out periods for the gain effect.
They can be in seconds (floats, e.g. 0.3) or milliseconds (integer, e.g. 300)."

  (if (null (oa::om-pointer-ptr (buffer s)))
      (om-beep-msg "Error: null sound buffer")

    (let* ((ptr (oa::om-pointer-ptr (buffer s)))
           (nch (n-channels s))
           (sr (sample-rate s))
           (size (n-samples s))
           (type (smpl-type s))
           (in (if (integerp in) (* in 0.001) in))
           (out (if (integerp out) (* out 0.001) out))
           (fade-in-frames (round (* in sr)))
           (fade-in-factor (/ (1- gain) fade-in-frames))
           (fade-out-frames (round (* out sr)))
           (fade-out-factor (/ (- 1 gain) fade-out-frames))
           (fade-out-frame-start (- size fade-out-frames))
           (final-buffer (make-audio-buffer nch size type)))

      (dotimes (i size)
        (dotimes (n nch)
          (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type)
                (* (cond ((< i fade-in-frames) (1+ (* fade-in-factor i)))
                         ((>= i fade-out-frame-start) (+ gain (* fade-out-factor (- i (- size fade-out-frames)))))
                         (t gain))
                   (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type)))))

      (make-instance 'sound
                     :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                     :n-samples size
                     :n-channels nch
                     :sample-rate sr
                     :smpl-type type))))

; compatibility
(defmethod sound-vol ((s om-internal-sound) gain &optional (in 1) (out 1))
  (sound-gain s gain in out))



(defmethod* sound-mono-to-stereo ((s om-internal-sound) &optional (pan 0))
  :icon 'sound-mono-to-stereo
  :initvals '(nil 0)
  :indoc '("a sound" "a panoramic value between -100 and 100")
  :doc "Stereo-ize a mono sound with possible panoramic <s>.

<pan> is a panoramic value between -100 (Left channel) and 100 (Right channel)."

  (cond ((null (oa::om-pointer-ptr (buffer s)))
         (om-beep-msg "Error: null sound buffer"))

        ((= (n-channels s) 1)
         (let* ((ptr (oa::om-pointer-ptr (buffer s)))
                (type (smpl-type s))
                (size (n-samples s))
                (nch (n-channels s))
                (final-buffer (make-audio-buffer 2 size type))
                (pan (/ pan 100.0))
                (Lgain (if (<= pan 0) 1 (- 1 pan)))
                (Rgain (if (>= pan 0) 1 (+ 1 pan)))
                (x 0.0))

           (dotimes (i size)
             (setf x (fli:dereference (fli:dereference ptr :index 0 :type :pointer) :index i :type type))
             (setf (fli:dereference (fli:dereference final-buffer :index 0 :type :pointer) :type type :index i) (* Lgain x)
                   (fli:dereference (fli:dereference final-buffer :index 1 :type :pointer) :type type :index i) (* Rgain x)))

           (make-instance 'sound
                          :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                          :n-samples size
                          :n-channels 2
                          :sample-rate (sample-rate s)
                          :smpl-type type)))
        (t
         (om-beep-msg (format nil "Error : trying to stereo-ize a sound with ~A channels. Needs 1. Output is the original input." (n-channels s)))
         s)))



(defmethod* sound-to-mono ((s om-internal-sound))
  :icon 'sound-to-mono
  :initvals '(nil)
  :indoc '("a sound")
  :doc "Mono-ize a sound."

  (if (null (oa::om-pointer-ptr (buffer s)))
      (om-beep-msg "Error: null sound buffer")

    (let* ((ptr (oa::om-pointer-ptr (buffer s)))
           (type (smpl-type s))
           (final-buffer (make-audio-buffer 1 (n-samples s) type))
           (nch (n-channels s)))

      (dotimes (i (n-samples s))

        (setf (fli:dereference (fli:dereference final-buffer :index 0 :type :pointer) :index i :type type)

              (/ (loop for c from 0 to (1- nch)
                       sum (fli:dereference (fli:dereference ptr :index c :type :pointer) :index i :type type))
                 (float nch))
              ))

      (make-instance 'sound
                     :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch 1)
                     :n-samples (n-samples s)
                     :n-channels 1
                     :sample-rate (sample-rate s)
                     :smpl-type type))
    ))



(defmethod* sound-stereo-pan ((s om-internal-sound) left right)
  :icon 'sound-stereo-pan
  :initvals '(nil -100 100)
  :indoc '("a sound" "a left channel pan value" "a right channel pan value")
  :doc "Pan a stereo sound.

<left> is a panoramic value for the left channel between -100 (full left) and 100 (full right).
<right> is a panoramic value for the right channel between -100 (full left) and 100 (full right)."

  (cond ((null (oa::om-pointer-ptr (buffer s)))
         (om-beep-msg "Error: null sound buffer"))
        ((= (n-channels s) 2)
         (let* ((ptr (oa::om-pointer-ptr (buffer s)))
                (type (smpl-type s))
                (nch (n-channels s))
                (size (n-samples s))
                (left (cond ((< left -100) 100) ((> left 100) -100) (t (- left))))
                (right (cond ((< right -100) -100) ((> right 100) 100) (t right)))
                (leftRgain (- 0.5 (* (/ 1.0 200) left)))
                (leftLgain (+ 0.5 (* (/ 1.0 200) left)))
                (rightRgain (+ 0.5 (* (/ 1.0 200) right)))
                (rightLgain (- 0.5 (* (/ 1.0 200) right)))
                (xl 0.0) (xr 0.0)
                (final-buffer (make-audio-buffer nch size type)))

           (dotimes (i size)
             (setf xl (fli:dereference (fli:dereference ptr :index 0 :type :pointer) :index i :type type)
                   xr (fli:dereference (fli:dereference ptr :index 1 :type :pointer) :index i :type type))
             (setf (fli:dereference (fli:dereference final-buffer :index 0 :type :pointer) :index i :type type) (+ (* leftLgain xl) (* rightLgain xr))
                   (fli:dereference (fli:dereference final-buffer :index 1 :type :pointer) :index i :type type) (+ (* leftRgain xl) (* rightRgain xr))))

           (make-instance 'sound
                          :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                          :n-samples size
                          :n-channels nch
                          :sample-rate (sample-rate s)
                          :smpl-type type)))
        (t
         (om-beep-msg "Error : trying to pan a sound with ~A channels. Needs 2. Output is the original input." (n-channels s))
         s)
        ))



(defmethod* sound-mix ((s1 om-internal-sound) (s2 om-internal-sound) &key (s1-offset 0) (s2-offset 0) (method 0))
  :icon 'sound-mix
  :initvals '(nil nil 0)
  :menuins '((4 (("Sum" 0)
                 ("Sum / Average" 1)
                 ("Sum / Hard Limiting" 2))))
  :indoc '("an om-internal-sound" "an om-internal-sound" "a mixing method")
  :doc "Generates a mix of <s1> and <s2>.

<s1-offset> and <s2-offset> are temporal offsets in seconds (float) or milliseconds (int)."

  (cond ((or (null (oa::om-pointer-ptr (buffer s1))) (null (oa::om-pointer-ptr (buffer s2))))
         (om-beep-msg "Error : buffer(s) not initialized."))
        ((and (= (n-channels s1) (n-channels s2)) (= (sample-rate s1) (sample-rate s2)))
         (let* ((ptr1 (oa::om-pointer-ptr (buffer s1)))
                (type1 (smpl-type s1))
                (ptr2 (oa::om-pointer-ptr (buffer s2)))
                (type2 (smpl-type s2))
                (nch (n-channels s1))
                (size1 (n-samples s1))
                (size2 (n-samples s2))
                (offset1 (round (* (if (integerp s1-offset) (* s1-offset 0.001) s1-offset) (sample-rate s1))))
                (offset2 (round (* (if (integerp s2-offset) (* s2-offset 0.001) s2-offset) (sample-rate s2))))
                (final-size (max (+ size1 offset1) (+ size2 offset2)))
                (final-buffer (make-audio-buffer nch final-size type1))
                (res 0.0))

           (cond ((= method 0)
                  (dotimes (i final-size)
                    (dotimes (n nch)
                      (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type1)
                            (+ (if (< i offset1) 0.0
                                 (if (< i (+ offset1 size1))
                                     (fli:dereference (fli:dereference ptr1 :index n :type :pointer) :index (- i offset1) :type type1)
                                   0.0))
                               (if (< i offset2) 0.0
                                 (if (< i (+ offset2 size2))
                                     (fli:dereference (fli:dereference ptr2 :index n :type :pointer) :index (- i offset2) :type type2)
                                   0.0))
                               ))
                      )))
                 ((= method 1)
                  (dotimes (i final-size)
                    (dotimes (n nch)
                      (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type1)
                            (/
                             (+ (if (< i offset1) 0.0
                                  (if (< i (+ offset1 size1))
                                      (fli:dereference (fli:dereference ptr1 :index n :type :pointer) :index (- i offset1) :type type1)
                                    0.0))
                                (if (< i offset2) 0.0
                                  (if (< i (+ offset2 size2))
                                      (fli:dereference (fli:dereference ptr2 :index n :type :pointer) :index (- i offset2) :type type2)
                                    0.0))
                                )
                             2)))))
                 ((= method 2)
                  (dotimes (i final-size)
                    (dotimes (n nch)
                      (setf res
                            (+ (if (< i offset1) 0.0
                                 (if (< i (+ offset1 size1))
                                     (fli:dereference (fli:dereference ptr1 :index n :type :pointer) :index (- i offset1) :type type1)
                                   0.0))
                               (if (< i offset2) 0.0
                                 (if (< i (+ offset2 size2))
                                     (fli:dereference (fli:dereference ptr2 :index n :type :pointer) :index (- i offset2) :type type2)
                                   0.0))
                               ))
                      (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type1)
                            (cond ((< res -1) -1.0)
                                  ((> res 1) 1.0)
                                  (t res)))))))

           (make-instance 'sound
                          :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                          :n-samples final-size
                          :n-channels nch
                          :sample-rate (sample-rate s1)
                          :smpl-type type1)))
        (t
         (om-beep-msg "Error : trying to mix 2 sounds with different number of channels or different sample rate. Output is the input 1.")
         s1)))



(defmethod* sound-merge ((sound-list list))
  :icon 'sound-mix
  :initvals '(nil)
  :indoc '("a list of sounds")
  :doc "Merges several sounds into a single multi-channel sound."

  (let* ((sounds (mapcar 'get-sound sound-list))
         (type (smpl-type (car sounds)))
         (sr (sample-rate (car sounds)))
         ;;; actually we should check if all sounds have same type and sample-rate
         (n-samples-out (apply 'max (mapcar 'n-samples sounds)))
         (n-channels-out (apply '+ (mapcar 'n-channels sounds)))
         (final-buffer (make-audio-buffer n-channels-out n-samples-out type))
         (c 0))

    (loop for snd in sounds do
          (with-audio-buffer (b snd)
            (dotimes (srcchan (n-channels snd))
              (let ((bptr (oa::om-pointer-ptr b)))
                (dotimes (i (n-samples snd))
                  ;(print (list b c srcchan i))
                  (setf (fli:dereference (fli:dereference final-buffer :index c :type :pointer) :type type :index i)
                        (fli:dereference (fli:dereference bptr :index srcchan :type :pointer) :type type :index i)))
                (setf c (1+ c))))
            ))

    (make-instance 'sound
                   :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch n-channels-out)
                   :n-samples n-samples-out
                   :n-channels n-channels-out
                   :sample-rate sr
                   :smpl-type type)
    ))



(defmethod* sound-split ((s om-internal-sound))
  :icon 'sound-mix
  :initvals '(nil)
  :indoc '("a (multichannel) sounds")
  :doc "Outputs a list of mono sounds from the channels of <s>."

  (let ((type (smpl-type s)))
    (with-audio-buffer (b s)
      (let ((bptr (oa::om-pointer-ptr b)))
        (loop for c from 0 to (1- (n-channels s)) collect
              (let ((new-buffer (make-audio-buffer 1 (n-samples s) type)))
                (dotimes (i (n-samples snd))
                  (setf (fli:dereference (fli:dereference new-buffer :index 0 :type :pointer) :type type :index i)
                        (fli:dereference (fli:dereference bptr :index c :type :pointer) :type type :index i)))
                (make-instance 'sound
                               :buffer (make-om-sound-buffer-GC :ptr new-buffer :nch 1)
                               :n-samples (n-samples s)
                               :n-channels 1
                               :sample-rate (sample-rate s)
                               :smpl-type type)
                )))
      )))



(defmethod* sound-seq ((s1 om-internal-sound) (s2 om-internal-sound) &optional (crossfade 0))
  :icon 'sound-seq
  :initvals '(nil nil 0)
  :indoc '("a sound" "a sound" "cross-fading duration (ms)")
  :doc "Concatenates <s1> and <s2>.
<crossfade> (duration in seconds/flots or milliseconds/int) determines a fade-in/fade out overlapping between the sounds."

  (cond ((or (null (oa::om-pointer-ptr (buffer s1))) (null (oa::om-pointer-ptr (buffer s2))))
         (om-beep-msg "Error : buffer(s) not initialized."))
        ((and (= (n-channels s1) (n-channels s2)) (= (sample-rate s1) (sample-rate s2)))
         (let* ((ptr1 (oa::om-pointer-ptr (buffer s1)))
                (type1 (smpl-type s1))
                (ptr2 (oa::om-pointer-ptr (buffer s2)))
                (type2 (smpl-type s2))
                (nch (n-channels s1))
                (sr (sample-rate s1))
                (size1 (n-samples s1))
                (size2 (n-samples s2))
                (cf (if (integerp crossfade) (* crossfade 0.001) crossfade))
                (smp-cross (round (* cf sr)))
                (factor1 (- (/ 1.0 (max 1 smp-cross))))
                (factor2 (/ 1.0 (max 1 smp-cross)))
                (final-size (- (+ size1 size2) smp-cross))
                (final-buffer (make-audio-buffer nch final-size type1)))

           (dotimes (i final-size)
             (dotimes (n nch)
               (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type1)
                     (cond ((< i (- size1 smp-cross))
                            (fli:dereference (fli:dereference ptr1 :index n :type :pointer) :index i :type type1))
                           ((and (>= i (- size1 smp-cross)) (< i size1))
                            (+ (* (1+ (* factor1 (- i (- size1 smp-cross))))
                                  (fli:dereference (fli:dereference ptr1 :index n :type :pointer) :index i :type type1))
                               (* factor2 (- i (- size1 smp-cross))
                                  (fli:dereference (fli:dereference ptr2 :index n :type :pointer) :index (+ smp-cross (- i size1)) :type type2))))
                           ((>= i size1)
                            (fli:dereference (fli:dereference ptr2 :index n :type :pointer) :index (+ smp-cross (- i size1)) :type type2))))))

           (make-instance 'sound
                          :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                          :n-samples final-size
                          :n-channels nch
                          :sample-rate (sample-rate s1)
                          :smpl-type type1)))
        (t
         (om-beep-msg "Error: trying to sequence incompatible audio buffers: s1: ~Dch - sr=~DHz / s2: ~Dch - sr=~DHz. Output is input 1."
                      (n-channels s1) (sample-rate s1) (n-channels s2) (sample-rate s2))
         s1)))



(defmethod* sound-reverse ((s om-internal-sound))
  :icon 'sound-reverse
  :initvals '(nil -100 100)
  :indoc '("a sound")
  :doc "Reverse a sound."

  (cond ((null (oa::om-pointer-ptr (buffer s)))
         (om-beep-msg "Error: null sound buffer"))
        (t
         (let* ((ptr (oa::om-pointer-ptr (buffer s)))
                (type (smpl-type s))
                (nch (n-channels s))
                (size (n-samples s))
                (final-buffer (make-audio-buffer nch size type)))

           (dotimes (i size)
             (dotimes (n nch)
               (setf (fli:dereference (fli:dereference final-buffer :index n :type :pointer) :index i :type type)
                     (fli:dereference (fli:dereference ptr :index n :type :pointer) :index (- size i) :type type))))
           (make-instance 'sound
                          :buffer (make-om-sound-buffer-GC :ptr final-buffer :nch nch)
                          :n-samples size
                          :n-channels nch
                          :sample-rate (sample-rate s)
                          :smpl-type type)))))
