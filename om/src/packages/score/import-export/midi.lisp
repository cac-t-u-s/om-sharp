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


(defmethod objfromobjs ((model midi-track) (target chord-seq))
  (let ((chords nil))
    
    (loop for note in (sort (flat (get-midi-notes model)) #'< :key #'midinote-onset)
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
    
    (data-stream-set-frames target (reverse chords))
    
    target))


(defmethod objfromobjs ((model midi-track) (target voice))
  (let ((cseq (objfromobjs model (make-instance 'chord-seq)))
        (tempo (or (tempo target) 60)))  ;;; get it from the MIDI-TRACK ??
    (make-instance 'voice
                   :tree (omquantify cseq tempo '(4 4) 8)
                   :lmidic (get-chords cseq) 
                   :tempo tempo)))


