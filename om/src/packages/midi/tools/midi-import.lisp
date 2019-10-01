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



; OM MIDI tempo
; 1000000 microseconds / beat
; i.e. tempo = 60
(defvar *midi-init-tempo* 1000000)   

;;; from OM6
(defun logical-time (abstract-time cur-tempo tempo-change-abst-time tempo-change-log-time unit/sec)
  (+ tempo-change-log-time
     (round (* (/ 1000.0 unit/sec) 
               (* (- abstract-time tempo-change-abst-time)
                  (/ cur-tempo *midi-init-tempo*))))))


(defun mstempo2bpm (mstempo)
   (round (* (/ *midi-init-tempo* mstempo) 60)))

(defun bpm2mstempo (bpm)
  (round (* (/ 60 bpm) *midi-init-tempo*)))


(defun midievents-to-milliseconds (evtseq units/sec)

  (let ((rep nil)
        (cur-tempo *midi-init-tempo*)
        (tempo-change-abst-time 0)
        (tempo-change-log-time 0) 
        (initdate (om-midi::midi-evt-date (car evtseq))))
    
    (loop for event in evtseq do
	  
          (if (equal :Tempo (om-midi::midi-evt-type event))
            
              (let ((date (- (om-midi::midi-evt-date event) initdate)))
                (setq tempo-change-log-time (logical-time date cur-tempo tempo-change-abst-time tempo-change-log-time units/sec))
                (setq cur-tempo (car (om-midi:midi-evt-fields event)))
                (setq tempo-change-abst-time date))
          
            (let ((date-ms (logical-time (om-midi::midi-evt-date event)  
                                         cur-tempo tempo-change-abst-time 
                                         tempo-change-log-time units/sec)))
            
              (push (om-midi::make-midi-evt :date date-ms
                                        :type (om-midi::midi-evt-type event) 
                                        :chan (om-midi::midi-evt-chan event)
                                        :ref (om-midi::midi-evt-ref event)
                                        :port (om-midi::midi-evt-port event)
                                        :fields (om-midi::midi-evt-fields event))
                    rep)
              )
            )
          )
    
    (reverse rep)))




;;; RETURNS A SORTED LIST OF CL-MIDI's MIDI-EVT structs
(defun import-midi-file (&optional file)
  (multiple-value-bind (evt-list ntracks unit format)
      (om-midi::midi-import file)
    (declare (ignore ntracks format))
    (when evt-list
      (midievents-to-milliseconds (sort evt-list '< :key 'om-midi::midi-evt-date) unit)
      )))




;=== KEY FUNCTION : to upgrade when voice accept tempo map... 
; Converts a sequence in tempo 60 into other tempo
; Returns a new seq
(defun insert-tempo-info (seq tempo) 
  (let ((tempoFactor (/ (bpm2mstempo tempo) *midi-init-tempo*)))
    (cons (om-midi::make-midi-evt :type :Tempo :date 0 :ref 0 :fields (list (bpm2mstempo tempo)))
          (loop for event in seq collect
                (let ((newevent (om-midi::copy-midi-evt event)))
                  (setf (om-midi::midi-evt-date newevent) (round (/ (om-midi::midi-evt-date event) tempoFactor)))
                  ;(when (equal (om-midi::midi-evt-type event) :Note) 
                  ;  (om-midi::midi-evt-dur newevent (round (/ (om-midi::midi-evt-dur event) tempoFactor))))
                  newevent))
          )))

;= Saves sequence with tempo 60
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60

(defun export-midi-file (list file &key tempo format) 
  (let ((seq (sort list 'om-midi::midi-evt-<)))
    (when seq
      
      (if tempo
          (setf seq (insert-tempo-info seq tempo))
        (push (om-midi::make-midi-evt :type :Tempo :date 0 :fields (list *midi-init-tempo*)) seq))
	  
      (om-midi::midi-export seq file (or format 1) 1000)
      )))
