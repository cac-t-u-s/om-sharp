(in-package :portaudio-tests)

(defconstant +frames-per-buffer+ 1024)
(defconstant +sample-rate+ 44100d0)
(defconstant +seconds+ 15)
(defconstant +sample-format+ :float)
(defconstant +num-channels+ 2)

(defun test-read-write-echo ()
  "Record input into an array; Playback recorded data."
  (with-audio
    (let ((input-parameters (make-stream-parameters))
          (output-parameters (make-stream-parameters)))
      (setf (stream-parameters-device input-parameters) (get-default-input-device)
            (stream-parameters-channel-count input-parameters) +num-channels+
            (stream-parameters-sample-format input-parameters) +sample-format+
            (stream-parameters-suggested-latency input-parameters) (device-info-default-low-input-latency (get-device-info
                                                                                                            (get-default-input-device))))
      (setf (stream-parameters-device output-parameters) (get-default-output-device)
            (stream-parameters-channel-count output-parameters) +num-channels+
            (stream-parameters-sample-format output-parameters) +sample-format+
            (stream-parameters-suggested-latency output-parameters) (device-info-default-low-output-latency (get-device-info
                                                                                                              (get-default-output-device))))
      (format t "~%=== Wire on. Will run ~D seconds . ===~%" +seconds+)
      (with-audio-stream (astream input-parameters output-parameters :sample-rate +sample-rate+ :frames-per-buffer +frames-per-buffer+ :stream-flags (:clip-off))
        (dotimes (i (round (/ (* +seconds+ +sample-rate+) +frames-per-buffer+)))
          (write-stream astream (read-stream astream)))))))
(export 'test-read-write-echo)

(defun test-read-write-converted-echo ()
  "Record input into an array; Separate array to channels; Merge channels into array; Play last array."
  (with-audio
    (format t "~%=== Wire on. Will run ~D seconds . ===~%" +seconds+)
    (with-default-audio-stream (astream +num-channels+ +num-channels+ :sample-format +sample-format+ :sample-rate +sample-rate+ :frames-per-buffer +frames-per-buffer+)
      (dotimes (i (round (/ (* +seconds+ +sample-rate+) +frames-per-buffer+)))
        (write-stream astream
                      (merge-channels-into-array astream
                                                 (separate-array-to-channels astream
                                                                             (read-stream astream))))))))
(export 'test-read-write-converted-echo)