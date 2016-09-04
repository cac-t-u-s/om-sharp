
(in-package :om)


(defmethod om-spat ((self sound) src-positions spk-positions)
  (unless src-positions (setf src-positions '((0 3 0))))
  (unless spk-positions (setf spk-positions '((1 1 0) (1 -1 0) (-1 -1 0) (-1 1 0))))
  (with-audio-buffer (b self)
    (when b
      (let* ((nch (length spk-positions))
             (outbuffer (spat::spat-spatialize-buffer (om-sound-buffer-ptr b)
                                                      (n-samples self) (n-channels self) nch 
                                                      nil nil)))
        (when outbuffer
          (let ((snd (make-instance 'om-internal-sound
                                    :n-samples (n-samples self) :sample-rate (sample-rate self)
                                    :n-channels nch)))
            (setf (buffer snd) (make-om-sound-buffer :ptr outbuffer :count 1 :nch nch))
            snd
            ))
        ))
    ))

(defmethod spat-osc ((self sound) nchannels (oscb osc-bundle) &optional (panning "angular"))
  (let* ((ob (make-o.bundle oscb)))
    (with-audio-buffer 
     (b self)
     (when b
       (let* ((nch nchannels)
              (outbuffer (spat::spat-spatialize-buffer (om-sound-buffer-ptr b)
                                                       (n-samples self) (n-channels self) nch 
                                                       (o.bundle-ptr ob) (o.bundle-size ob) 
                                                       panning)))
         (when outbuffer
           (let ((snd (make-instance 'om-internal-sound
                                     :n-samples (n-samples self) :sample-rate (sample-rate self)
                                     :n-channels nch)))
             (setf (buffer snd) (make-om-sound-buffer :ptr outbuffer :count 1 :nch nch))
             snd
             ))
         ))
     )))
