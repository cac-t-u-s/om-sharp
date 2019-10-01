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
    

(defmethod objfromobjs ((model midi-track) (target chord-seq))
    
  (data-stream-set-frames 
   target 
   (midinotes-to-chords (flat (get-midi-notes model))))
  
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
                 (data-stream-set-frames cseq track)
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
                 (data-stream-set-frames cseq track)
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

