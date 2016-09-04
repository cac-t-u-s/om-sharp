(in-package :om)

(om-midi::portmidi-connect-ports (om-midi::portmidi-setup nil nil))

(defun Memory-Solo ()
  (let (( Improvizer 
          (ImprovizerExBeat->MidiHarmBeat (concatenate-improvizer-list
                                           (list 
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-solo1*)
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-solo2*)
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-solo3*)
                                            ;(load-realtimeImprovizer-fromSavedImprovizer *db-path-soloblues1*)
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-soloblues2*)
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-soloblues3*)
                                            ;(load-realtimeImprovizer-fromSavedImprovizer *db-path-JovinoSantosNeto1*)
                                            ;(load-realtimeImprovizer-fromSavedImprovizer *db-path-JovinoSantosNeto2*)
                                            ;(load-realtimeImprovizer-fromSavedImprovizer *db-path-Brassens*)
                                            ;(load-realtimeImprovizer-fromSavedImprovizer *db-path-Dicidenbas_Lubat*)
                                            )))))
    (setf 
     ;(AuthorizedTranspos Improvizer) '(1 2 3 4 5 -1 -2 -3 -4 -5 0)
     (AuthorizedTranspos Improvizer) '(0)
     (max-continuity Improvizer) 1000
     (bestTranspoMode Improvizer) t
     (FirstWithoutTranspoMode Improvizer) nil
     (randomPrefixOccurrenceMode Improvizer) t
     (LengthFactorsFromScen Improvizer) '(1 1000)   
     (modeRT Improvizer) nil
     (newtabousperf Improvizer) nil
     (nexteventifnosolution Improvizer) nil)
    
    Improvizer))

(defun Memory-Accomp () 
  (let ((Improvizer 
         ;(ImprovizerExBeat->MidiHarmBeat (load-realtimeImprovizer-fromSavedImprovizer *db-path-accomp1*))
         (ImprovizerExBeat->MidiHarmBeat (concatenate-improvizer-list
                                           (list 
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-accomp1*)
                                            (load-realtimeImprovizer-fromSavedImprovizer *db-path-accomp2*))))
         ))

    (setf 
     (AuthorizedTranspos Improvizer) '(0)
     (max-continuity Improvizer) 1000
     (bestTranspoMode Improvizer) nil
     (FirstWithoutTranspoMode Improvizer) nil
     (randomPrefixOccurrenceMode Improvizer) t
     (LengthFactorsFromScen Improvizer) '(1 1000)   
     (modeRT Improvizer) nil
     (newtabousperf Improvizer) nil
     (nexteventifnosolution Improvizer) nil)
    Improvizer))


(defun makescenario ()
  *scenario-degeu*)

;(test-wait-for-relay)
;(length (flat *testout*))
;(length (slice-list *testimp*))
;(setq *testout* (reverse *testout*))
;(midiset (data (cadr *testout*)))

(defun format-midi-note (note)
  (let ((onset (nth 1 note))
        (pitch (abs (nth 0 note)))
        (vel (nth 3 note))
        (dur (nth 2 note))
        (chan (nth 4 note)))
    (list onset pitch vel dur chan)))

(defun format-midi-list (notelist)
  (loop for note in notelist collect
        (format-midi-note note)))

#|
;To use coupled with add-feature (Improvizer.lisp)
(add-feature improvizer :pitch '(min max))
(add-feature improvizer :density '(min max))
|#




(defmethod eligible-feature? ((self midiharmbeat) (o improvizer))
  (let ((pitch_int (find-value-in-arg-list (feature o) :pitch))
        (density_int (find-value-in-arg-list (feature o) :density))
        (nbdistonsets_int (find-value-in-arg-list (feature o) :nbdistonsets))
        (meandur_int (find-value-in-arg-list (feature o) :meandur))
   
        (pitch_ok t)
        (density_ok t)
        (nbdistonsets_ok t)
        (meandur_ok t)
        
        )

    (if pitch_int
        (loop for note in (Midiset self) do
              (if (and (< (MEChannel note) 14) (> (MEPitch note) 0) (> (MEVel note) 0))
                  (setf pitch_ok
                        (and pitch_ok
                             (<= (MEPitch note) (nth 1 pitch_int))
                             (>= (MEPitch note) (nth 0 pitch_int)))))))
    
    ;(if density_int
    ;    (setf density_ok
    ;           (and density_ok
    ;                (< (list-length (Midiset self)) (nth 1 density_int))
    ;                (> (list-length (Midiset self)) (nth 0 density_int)))))

    (if nbdistonsets_int
        
        (let ((listonsets '()))
        
          (loop for note in (Midiset self) do          
                (if (and (< (MEChannel note) 14) (> (MEVel note) 0))
                    (if (not (member (MEOnset note) listonsets))
                        (nconc listonsets (list (MEOnset note))) 
                        )))
          
          (setf nbdistonsets_ok
                (and nbdistonsets_ok
                     (< (list-length listonsets) (nth 1 nbdistonsets_int))
                     (> (list-length listonsets) (nth 0 nbdistonsets_int))))
          
          ))  

    (if density_int
        
        (let ((listonsets '()))
        
          (loop for note in (Midiset self) do          
                (if (and (< (MEChannel note) 14) (> (MEVel note) 0))
                    (if t ;(not (member (MEOnset note) listonsets))
                        (nconc listonsets (list (MEOnset note))) 
                        )))
          
          (setf density_ok
                (and density_ok
                     (< (list-length listonsets) (nth 1 density_int))
                     (> (list-length listonsets) (nth 0 density_int))))
          
          ))

    (if meandur_int
        
        (let ((listonsets '()))
        
          (loop for note in (Midiset self) do          
                (if (and (< (MEChannel note) 14) (> (MEVel note) 0))
                    (if t ;(not (member (MEOnset note) listonsets))
                        (nconc listonsets (list (MEDur note))) 
                        )))
          (if (car listonsets) 
              (setf meandur_ok
                (and meandur_ok
                     (< (om-mean listonsets) (nth 1 meandur_int))
                     (> (om-mean listonsets) (nth 0 meandur_int)))))
          
          ))

    (and density_ok pitch_ok nbdistonsets_ok meandur_ok)
    ))


;----------------------
;Pimp the patch

(defmethod List->HarmScenario ((l list))
  (makelabelsfromlist (expand_grid l) 'harmlabel))