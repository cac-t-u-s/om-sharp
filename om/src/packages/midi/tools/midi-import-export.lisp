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

; MIDI tempo
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


;;;==================================
;;; IMPORT
;;;==================================

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
;;; Deletes tempo events and converts all dates to milliseconds
(defun import-midi-events (&optional file)
  (multiple-value-bind (evt-list ntracks unit format)
      (om-midi::midi-import file)
    (declare (ignore ntracks format))
    (when evt-list
      (midievents-to-milliseconds (sort evt-list '< :key 'om-midi::midi-evt-date) unit)
      )))



;;; MAIN: GENERATES A MIDI-TRACK

(defmethod* import-midi (&optional file) 
  :initvals '(nil)
  :icon :midi-import
  :doc "Returns a MIDI-TRACK from imported data in <filename>"
  (if file 
      (when (probe-file (pathname file))
        (objFromObjs (pathname file) (make-instance 'midi-track)))
    (let ((path (om-choose-file-dialog :types '("MIDI Files" "*.mid;*.midi"))))
      (when path 
        (import-midi path)))
    ))
   

;;;==================================
;;; EXPORT
;;;==================================

; KEY FUNCTION : to upgrade when voice accept tempo map... 
; Converts a sequence in tempo 60 into other tempo
; Returns a new seq
(defun insert-tempo-info (seq tempo) 
  (let ((tempoFactor (/ (bpm2mstempo 60) *midi-init-tempo*)))
    (cons (om-midi::make-midi-evt :type :Tempo :date 0 :ref 0 :fields (list (bpm2mstempo tempo)))
          (loop for event in seq collect
                (let ((newevent (om-midi::copy-midi-evt event)))
                  (setf (om-midi::midi-evt-date newevent) (round (/ (om-midi::midi-evt-date event) tempoFactor)))
                  ;(when (equal (om-midi::midi-evt-type event) :Note) 
                  ;  (om-midi::midi-evt-dur newevent (round (/ (om-midi::midi-evt-dur event) tempoFactor))))
                  newevent))
          )))

;= Saves sequence with tempo 60 if no tempo is given
;= modif  --->  clicks = 1000 so that 1 click = 1ms at tempo 60
(defun export-midi-file (list file &key tempo format) 
  (let ((seq (sort list 'om-midi::midi-evt-<)))
    (when seq
      (if tempo
          ;;; add a tempo event and convert milliseconds
          (setf seq (insert-tempo-info seq tempo))
        ;;; insert a 60 bpm tempo event and times remain in milliseconds
        (push (om-midi::make-midi-evt :type :Tempo :date 0 :fields (list *midi-init-tempo*)) seq))
	  
      (om-midi::midi-export seq file (or format 1) 1000)
      )))


;;; MAIN 

(defmethod* save-as-midi ((object t) filename &key (format nil) retune-channels) 
  :initvals '(nil nil 2 nil nil)
  :icon :midi-export
  :doc "Saves <object> as a MIDI file.

- <filename> defines the target pathname. If not specified, will be asked through a file choose dialog.
- <approx> specifies the tone division (2, 4 or 8).
- <format> allows to choose the MIDIFile format (0 or 1)
- <retune-channels> (t or nil) send pitchbend message per channel to fit setting for approx

For POLY objects: If all voice have same tempo, this tempo is saved in MidiFile. Otherwise all voices are saved at tempo 60."
  
  (let ((evtlist (loop for ev in (get-midievents object) collect (export-midi ev))))
    
    (when retune-channels
      (setf evtlist (append (micro-bend-messages) evtlist)))
            
    (export-midi-file evtlist 
                      filename
                      :tempo 60 
                      :format format)
    ))


