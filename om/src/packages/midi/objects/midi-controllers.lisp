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




;=======================================
; MIDI conversions
;=======================================

(defmethod! get-continuous-ctrl ((self midi-track) ctrl ref port channel)
  :initvals '(nil nil nil 0 1)
  :indoc '("a MIDI-TRACK" "type of controller (symbol or number)" "track" "MIDI port" "MIDI channel (1-16)")
  :doc "
Extracts control events of type <ctrl> from a MIDI file or sequence at channel <channel>, track <ref> and port <port> anf returns a BPF object.

<ctrl> can be a control-change number (see MIDI-CONTROL-CHANGE) or one of the special controls: :Tempo :KeyPress :ChanPress :PitchBend or :Private
"
  :icon :midi-filter
  
  (let* ((eventtype (if (numberp ctrl) :CtrlChange ctrl))
        
        (evtList (get-midievents self #'(lambda (e) 
                                           (and 
                                            (test-midi-port e port)
                                            (test-midi-track e ref)
                                            (test-midi-channel e channel) 
                                            (and (equal (ev-type e) eventtype)
                                                 (if (equal (ev-type e) :CtrlChange)
                                                     (or (= ctrl (first (ev-values e)))
                                                         (and (lsb-controller ctrl)
                                                              (= (- ctrl 32) (first (ev-values e))))
                                                         nil)
                                                   t)
                                                 )
                                            ))))
        values dates)
    
    (when  evtList
      (let ((last-date -1) 
            (curr-val 0))
        
        (loop for event in evtList do
              
              (cond 
               ((equal eventtype :Tempo)
                (setf curr-val (first (ev-values event))))
               
               ((equal eventtype :PitchBend) 
                (setf curr-val (msb-lsb2value (second (ev-values event)) (first (ev-values event)))))
              
               ((equal eventtype :KeyPress) 
                (setf curr-val (second (ev-values event))))
               
               ((equal eventtype :ChanPress)
                (setf curr-val  (first (ev-values event))))
               
               ((equal eventtype :Private)
                (setf curr-val  (first (ev-values event))))

               ((equal eventtype :CtrlChange)
                (if (lsb-controller ctrl)
                    (setf curr-val (if (= (first (ev-values event)) ctrl)
                                       (msb-lsb2value (msb curr-val) (second (ev-values event)))
                                     (msb-lsb2value (second (ev-values event)) (lsb curr-val))))
                  (setf curr-val (second (ev-values event))))
                ))

              (if (= last-date (onset event))
                  (setf (first values) curr-val)
                (progn
                  (push (onset event) dates)
                  (push curr-val values)))
              (setf last-date (onset event)))))
    
    (make-instance 'BPF :x-points (reverse dates) :y-points (reverse values) :decimals 0)
    ))
 



(defmethod bpf-to-midi ((self BPF) control-type channel &key track port)
  (let ((type (if (numberp control-type) :ctrlchange control-type)))
    (loop for p in (point-pairs self) collect
          (make-midievent :ev-type type
                          :ev-date (car p) 
                          :ev-values (if (numberp control-type) 
                                      (list control-type (cadr p))
                                      (list (cadr p)))
                          :ev-chan channel
                          :ev-track track
                          :ev-port port)
          )))
        



