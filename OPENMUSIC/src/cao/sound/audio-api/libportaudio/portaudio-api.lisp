

(in-package :pa)

(defun pa-play (buffer nch size)
  (with-audio
    (with-default-audio-stream (astream 0 nch)   
      (%write-stream astream buffer size))))
