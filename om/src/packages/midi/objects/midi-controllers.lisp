

(in-package :om)

;;; FUNCTION SPECIFIED IN BPF
(defun midi-controller (bpf-point &optional (control-num 7) (channel 1) (port 0))
  (om-midi::midi-send-evt 
   (om-midi:make-midi-evt 
    :type :CtrlChange
    :chan channel :port port
    :fields (list control-num (round (cadr bpf-point))))))


;;; FUNCTION SPECIFIED IN BPC
(defun bpc-midi-controller (tpoint &optional (control-num-x 1) (control-num-y 2) (channel 1) (port 0))
  (om-midi::midi-send-evt 
   (om-midi:make-midi-evt 
    :type :CtrlChange
    :chan channel :port port
    :fields (list control-num-x (round (tpoint-x tpoint)))))
  (om-midi::midi-send-evt 
   (om-midi:make-midi-evt 
    :type :CtrlChange
    :chan channel :port port
    :fields (list control-num-y (round (tpoint-y tpoint)))))
  )

(defmethod arguments-for-action ((fun (eql 'midi-controller)))
  '((:int control-num 7)
    (:int channel 1)
    (:int port 0)))

(defmethod arguments-for-action ((fun (eql 'bpc-midi-controller)))
  '((:int control-num 1)
    (:int control-num 2)
    (:int channel 1)
    (:int port 0)))

;;; more specific
(defun midi-volume-controller (bpf-point &optional (channel 1) (port 0))
  (om-midi::midi-send-evt 
   (om-midi:make-midi-evt 
    :type :CtrlChange
    :chan channel :port port
    :fields (list 7 (round (cadr bpf-point))))))

(defmethod arguments-for-action ((fun (eql 'midi-volume-controller)))
  '((:int channel 1)
    (:int port 0)))



(defun midi-pitchbend-controller (bpf-point &optional (channel 1) (port 0))
  (om-midi::midi-send-evt 
   (om-midi:make-midi-evt 
    :type :PitchBend
    :chan channel :port port
    :fields (list 7 (round (cadr bpf-point))))))

(defmethod arguments-for-action ((fun (eql 'midi-pitchbend-controller)))
  '((:int channel 1)
    (:int port 0)))
