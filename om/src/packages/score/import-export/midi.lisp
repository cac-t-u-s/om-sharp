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


;;;========================
;;; MIDI TO SCORE OBJECTS
;;;========================

(defmethod midinotes-to-chords ((notes list))
  
  (let ((chords nil))
    
    (loop for note in (sort notes #'< :key #'midinote-onset)
          do
          (if (and (car chords)
                   (= (midinote-onset note) (onset (car chords))))
              
              ;;; add a note to the last chord
              (setf (notes (car chords))
                    (append (notes (car chords))
                            (list (make-instance 'note :midic (* (midinote-pitch note) 100)
                                                 :dur (midinote-dur note)
                                                 :vel (midinote-vel note)
                                                 :chan (midinote-channel note)
                                                 :port (midinote-port note))
                                  )))
            ;;; new chord
            (push (make-instance 'chord 
                                 :onset (midinote-onset note)
                                   :lmidic (list (* (midinote-pitch note) 100))
                                   :ldur (list (midinote-dur note))
                                   :lvel (list (midinote-vel note))
                                   :lchan (list (midinote-channel note))
                                   :lport (list (midinote-port note)))
                  chords)))
    
    (reverse chords)
    ))
    

;;;================================
;;; DIRECT CONVERSIONS SELF=>SELF
;;;================================

(defmethod objfromobjs ((model midi-track) (target chord-seq))
    
  (set-chords 
   target 
   (midinotes-to-chords (flat (remove nil (get-midi-notes model)))))
  
  target)



(defmethod objfromobjs ((model midi-track) (target voice))
  (let ((cseq (objfromobjs model (make-instance 'chord-seq)))
        (tempo (or (tempo target) 60)))  ;;; get it from the MIDI-TRACK ??
    (make-instance 'voice
                   :tree (omquantify cseq tempo '(4 4) 8)
                   :lmidic (get-chords cseq) 
                   :tempo tempo)))



(defmethod objfromobjs ((model midi-track) (target multi-seq))
  (let ((voices 
         (loop for track in (remove nil (get-midi-notes model))
               collect 
               (let ((cseq (make-instance 'chord-seq)))
                 (set-chords cseq (midinotes-to-chords track))
                 cseq)
               )))
    (setf (obj-list target) voices)
    target))



(defmethod objfromobjs ((model midi-track) (target poly))
  (let ((voices 
         (loop for track in (remove nil (get-midi-notes model))
               collect 
               (let ((cseq (make-instance 'chord-seq))
                     (tempo 60)) ;;; get it from the MIDI-TRACK ??
                 (set-chords cseq (midinotes-to-chords track))
                 (make-instance 'voice
                                :tree (omquantify cseq tempo '(4 4) 8)
                                :lmidic (get-chords cseq) 
                                :tempo tempo))
               )))
    (setf (obj-list target) voices)
    target))



;;;========================
;;; SCORE OBJECTS TO MIDI
;;;========================

;;; get-midievents is the function called by SAVE-AS-MIDI

(defmethod get-midievents ((self note) &optional test)
  (get-midievents 
   (list (make-instance 'MidiEvent   
                        :onset (offset self)
                        :ev-type :KeyOn
                        :ev-port (port self)
                        :ev-chan (chan self)
                        :ev-values (list (round (/ (midic self) 100)) (vel self)))
        (make-instance 'MidiEvent   
                       :onset (+ (offset self) (dur self))
                       :ev-type :KeyOff
                       :ev-port (port self)
                       :ev-chan (chan self)
                       :ev-fields (list (round (/ (midic self) 100)) 0))
        )
   test))

(defmethod get-midievents ((self chord) &optional test)
  (loop for n in (notes self)
        append 
        (let ((evts (get-midievents n test)))
          (loop for evt in evts do
                (setf (onset evt) (+ (onset evt) (onset self))))
          evts)
        ))

(defmethod get-midievents ((self chord-seq) &optional test)
  (loop for c in (chords self) append 
        (let ((evts (get-midievents c)))
          ;;; do the test here: don't pass it inside to the chord/notes (because of the onset-test)
          (if test
              (remove-if-not test evts)
            evts))
        ))



(defmethod get-midievents ((self multi-seq) &optional test)
  
  (let ((evtlist 
         (loop for voice in (obj-list self) 
               for i from 1 append
               (let ((voice-evts (get-midievents voice)))
                 (loop for evt in voice-evts do
                       (setf (ev-track evt) i))
                 voice-evts))))

    ;;; do the test here: don't pass it inside to the voice (because of the track-test)
    (get-midievents (sort evtlist #'< :key #'onset) test)
    ))



(defmethod tempo-a-la-noire ((self number)) self)
(defmethod tempo-a-la-noire ((tempo list)) (* (second tempo) (/ (first tempo) 1/4)))

(defmethod get-midievents ((self voice) &optional test)
  (let ((evtlist 
         (sort 
          (cons 
           
           (make-midievent    
            :ev-type :Tempo
            :ev-date 0
            :ev-track 0 
            :ev-port 0
            :ev-chan 1
            :ev-values (list (tempo-a-la-noire (tempo self))))
           
           (append 
            
            (loop for c in (chords self) append (get-midievents c))
            
            (loop for m in (inside self) collect
                  (make-midievent   
                   :ev-type :TimeSign 
                   :ev-date (beat-to-time (symbolic-date m) (tempo self))
                   :ev-track 0 
                   :ev-port 0
                   :ev-chan 1
                   :ev-values (list (first (first (tree m)))
                                    (round (log (second (first (tree m))) 2))
                                    24 
                                    8 
                                    )))
            ))
          #'< :key #'onset)))
    
    (if test
        (remove-if-not test evtlist)
      evtlist)
    ))



;;; not yet supported
(defmethod has-tempo-change ((self voice)) nil)


(defmethod get-midievents ((self poly) &optional test)
  (let* ((tempo (tempo-a-la-noire (tempo (car (obj-list self)))))
         (evtlist 
          (loop for voice in (obj-list self) 
                for i from 1 
                do (if (and tempo 
                            (or (has-tempo-change voice)
                                (not (= (tempo-a-la-noire (tempo voice)) tempo))))
                       (setf tempo nil))
                append
                (let ((voice-evts (cdr (get-midievents voice)))) ;;; do not take the voice tempo event
                  (loop for evt in voice-evts do
                        (setf (ev-track evt) i))
                  voice-evts)))) 
    
    ;;; 1 global tempo event
    (setf evtlist 
          (cons (make-midievent    
                 :ev-type :Tempo
                 :ev-date 0
                 :ev-track 0 
                 :ev-port 0
                 :ev-chan 1
                 :ev-values (list (or tempo 60)))
                evtlist))
    
    ;;; do the test here: don't pass it inside to the voice (because of the track-test)
    (get-midievents (sort evtlist #'< :key #'onset) test)
    ))


;;;================================
;;; DIRECT CONVERSION SELF=>SELF
;;;================================

(defmethod objfromobjs ((model score-element) (target midi-track)) 
  (data-stream-set-frames target (midievents-to-midinotes (get-midievents model)))
  target)
 


