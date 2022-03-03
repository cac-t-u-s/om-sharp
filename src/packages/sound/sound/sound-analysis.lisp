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
; File author: J. Bresson
;============================================================================


(in-package :om)


(defmethod* sound-rms ((s om-internal-sound))
  :icon 'sound-normalize
  :indoc '("a sound")
  :doc "Returns the linear Root-Mean-Square (RMS) value of <s>."

  (when (check-valid-sound-buffer s)

    (with-audio-buffer (input-buffer s)
      (let* ((ptr (oa::om-pointer-ptr input-buffer))
             (type (smpl-type s))
             (nch (n-channels s))
             (size (n-samples s)))

        (let ((summed-square-signal
               (loop for i from 0 to (1- size) sum
                     (loop for n from 0 to (1- nch)
                           sum (fli:dereference (fli:dereference ptr :index n :type :pointer) :index i :type type)
                           into sample-sum
                           finally (return (let ((mean-value (/ sample-sum nch)))
                                             (* mean-value mean-value)))))))

          (sqrt (/ summed-square-signal size)))
        ))))


(defmethod* sound-transients ((s om-internal-sound)
                              &key (method :rms) (threshold 0.2)
                              (window-size 512) (step 256)
                              (time-threshold 0.0) (output :ms))
  :icon 'sound-transients
  :initvals '(nil :rms 0.2 512 256 0.01 :ms)
  :menuins '((1 (("RMS" :rms)
                 ("Mean" :mean)
                 ("Peak" :peak)))
             (6 (("seconds" :sec)
                 ("milliseconds" :ms))))
  :indoc '("a sound"
           "a detection method"
           "a threshold for transient detection"
           "detection window size (samples)"
           "step between two windows (samples)"
           "minimal time (sec) between transients"
           "output times format (seconds or milliseconds)")
  :doc "Finds and return transients/attacks detected in <s>.

Every <step> samples, a widow of <window-size> samples is analyzed by the selected <method>.
If the value is higher that <threshold> as compared to the previous window, the time is added to the returned list of transients.
<output> can be in seconds (float precision) or milliseconds (rounded).
"

  (let ((transient-times ()))

    (unless step (setf step window-size))

    (with-audio-buffer (input-buffer s)

      (if (null (oa::om-pointer-ptr input-buffer))
          (om-beep-msg "Error: null sound buffer")

        (let* ((ptr (oa::om-pointer-ptr input-buffer))
               (type (smpl-type s))
               (nch (n-channels s))
               (size (n-samples s))
               (sr (sample-rate s))
               (previous-window-value 0.0)
               (indx 0))

          (loop while (< indx size) do

                (let* ((real-size (min (- size indx) window-size))
                       (window-value
                        (cond
                         ((equal method :rms)
                          (let ((summed-square-window
                                 (loop for i from 0 to (1- real-size) sum
                                       (loop for n from 0 to (1- nch)
                                             sum (fli:dereference (fli:dereference ptr :index n :type :pointer) :index (+ indx i) :type type)
                                             into sample-sum
                                             finally (return (let ((mean-value (/ sample-sum nch)))
                                                               (* mean-value mean-value)))))))
                            (sqrt (/ summed-square-window real-size))))

                         ((equal method :mean)
                          (/
                           (loop for i from 0 to (1- real-size) sum
                                 (loop for n from 0 to (1- nch)
                                       sum (fli:dereference (fli:dereference ptr :index n :type :pointer) :index (+ indx i) :type type)
                                       into sample-sum
                                       finally (return (/ sample-sum nch))))
                           real-size))

                         ((equal method :peak)
                          (loop for i from 0 to (1- real-size) maximize
                                (loop for n from 0 to (1- nch)
                                      sum (fli:dereference (fli:dereference ptr :index n :type :pointer) :index (+ indx i) :type type)
                                      into sample-sum
                                      finally (return (/ sample-sum nch)))))

                         (t (om-beep-msg "Error unknown method: ~A" method)
                            0.0))
                        ))

                  (when (> (- window-value previous-window-value) threshold)
                    (let ((time (samples->sec indx sr)))
                      (when (or (null transient-times)
                                (> (- time (car transient-times)) time-threshold))
                        (push time transient-times))))

                  (setq previous-window-value window-value)
                  (incf indx real-size)))
          )))
    (reverse
     (if (equal output :ms)
         (sec->ms transient-times)
       transient-times))))
