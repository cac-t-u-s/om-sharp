(in-package :om)

;Fats.lisp
;Marc Chemillier
;August 14th 2012, July-August 2013


(defclass* fatstune (tune)
()
)
    

(defmethod substitution ((self fatstune)) 
  (setf (alternategrid self) (substitute-from-Fats-alternategrids)))

(setf fatsMDvoicings '(
((f maj7)   ((72 77 81 84)))
((f m7)     ((72 77 80 84)))
((f 7)      ((72 75 81 84)))
((c# dim)   ((73 79 82 85))) 

((c 7)      ((72 79 82 84)))
((f# dim)   ((72 78 81 84)))
((c maj7)   ((72 76 79 84)))
((c m7)     ((72 75 79 84)))

))


(defmethod change-class-to-fatstune ((self tune))
  (change-class self 'fatstune)            ;;;destructively modifies and returns the instance
  (setf (voicings self) fatsMDvoicings))

(change-class-to-fatstune Handfulofkeys_tune)     

(voicings *current-tune*)
(class-of *current-tune*)
;-----> after that, *current-tune* must be re-instanciated

(defmethod generate-improbeatlist-from-oraclelist ((self fatstune) (oraclelist list) refchannel)          
  (loop for i from refchannel 
        for oracle in oraclelist 
        ;Jerome 29/04/2013
        for saved-impro = (gethash i (improtable self))
        ;Marc 8/5/13
        with gridforimprovizing = nil     ;;;;;;Marc 9/8/13
        with gridforvoicings = (substitution self)
        with substgrid = nil
        
        for impro = (progn (format *om-stream* "-----------------------~%ORACLE ~a~%" i)
                      ;Jerome 29/04/2013
                      (if (and saved-impro (not (member i '(6 7))))
                          (progn (format *om-stream* "Unchanged oracle -> previous impro loaded~%")
                            saved-impro)
                        ;Marc 8/5/2013 conditions added for playing voicings on channels 6 and 7

                        (cond ((> (maxetat oracle) 1)    ;non empty oracle
                               (when (/= (beatduration self) (RefTempo oracle)) 
                                 (SetReftempo oracle (beatduration self)))     ;'oracle N' is adapted to the beat duration of the tune
                             ;(ImprovizeOnHarmGrid oracle (length (simplegrid self)) (simplegrid self))))))
                             ;Jerome 29/04/2013
                             ; TODO : IL FAUT DEJA LE SAUVEGARDER DANS LA TABLE AVEC LE BON CANAL !!!! OU BIEN REVOIR L'ARCHITECTURE DE CETTE FONCTION !
                             ; PB : ON A POUR L'INSTANT UN PATE DANS LA TABLE POUR LE CANAL 8... POURQUOI ?????
                               (setf gridforimprovizing (if (= i 4) (expandgrid gridforvoicings) (simplegrid self))
                                     (gethash i (improtable self)) (ImprovizeOnHarmGrid oracle (length gridforimprovizing) gridforimprovizing)))

                              ;Marc 8/5/2013  -> if maxetat <= 1 => empty oracle, compute voicings on channel 6-7
                              
                              ((= i 14)                 
                               (format *om-stream*  "Voicings~%")
                               (setf (gethash i (improtable self)) (PlayVoicings (voicings self) gridforvoicings (beatduration self))))
                               (t (format *om-stream*  "Empty oracle~%") nil)
                              
                              )))

        with mix = nil
        
        do (setf impro (replace-channel impro i)
                 mix (add-voice-to-beatsmix impro mix i (beatduration self)))
        finally return (progn (format *om-stream* "-----------------------~%") 
                         (thread-Beats mix (beatduration self)))))


;22/7/13 from Garner.lisp
(defun remove-channels (beatlist list-of-channels)
  (loop for beat in beatlist for newbeat = (clone beat)
        do (setf (MidiSet newbeat) 
                 (remove list-of-channels (MidiSet newbeat) 
                         :test #'(lambda (x y) (member y x)) :key 'fifth))
        collect newbeat))

;-> voir ou mettre cette fonction


#|
(loop for x being the hash-key of (hashlabeltranspo StaccPumpleftoracle) using (hash-value q) collect (list x q))
(gethash '(c 7) (hashlabeltranspo StaccPumpleftoracle))



(substitution *current-tune*)

(PlayVoicings (voicings *current-tune*) (alternategrid *current-tune*) (beatduration *current-tune*))


(setf (gethash 3 (oracletable *current-tune*)) (load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-legpump.or")) 
(setf (gethash 4 (oracletable *current-tune*)) (load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-staccpump.or"))

(setf beatsmix (generate-improbeatlist-from-oraclelist *current-tune* (loop for k from 3 to 7 collect (gethash k (oracletable *current-tune*))) 3))


A simple left hand example with various chord changes:

(setf labels '((f maj7) (f maj7) (f maj7) (f maj7) (f maj7) (f maj7) (f maj7) (f maj7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (f maj7) (f maj7) (f maj7) (f maj7) (c 7) (c 7) (c 7) (c 7)))

(progn 
(setf (max-continuity Fatsleftoracle) 1000 (feature Fatsleftoracle) '(36))
(setf leftbeatlist (thread-Beats (ImprovizeOnHarmGrid Fatsleftoracle (length labels) labels)))
(setf impro (beats->chseq leftbeatlist Fats_beatdur 0))
(play impro)
)



(setf labels '((a 7) (a 7) (a 7) (a 7) (a 7) (a 7) (a 7) (a 7) (d 7) (d 7) (d 7) (d 7) (d 7) (d 7) (d 7) (d 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (g 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7) (c 7)))

(Stop-Player *general-player*)

|#



;Two slots added 14/8/2012
;   - 'feature' added to the class 'improvizer'
;   - 'feature' added to the class 'beat'
;--> (eligible-feature? index self) eliminates indexes of the improvizer 'self' that do not have the desired feature

;(remove-channels beatlist list-of-channels)  --> from Garner.lisp



#|
;LOADING FATS DATA FROM TRANSCRIPTIONS AND SPECIAL MIDIFILES:
;------------------------------------------------------------
;Full examples for calculating Antescofo data
(progn 
(print "Loading MIDi files")

(setf Handfulsimple (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/Handful of Keys+CHIFsimple-ARTICUL.mid")
      Handfuldetailed (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/Handful of Keys+CHIFdetailed-ARTICUL.mid")
      ShiftedPump (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/ShiftedPump+CHIF-ARTICUL.mid")
      ContBass (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/ContBass+CHIF-ARTICUL.mid")
      4dec (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/4dec_5+CHIF.mid")
      T+C (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/T+C_5+CHIF.mid")
      Altern (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/Altern+CHIF.mid")
)


;BUILDING ORACLES:

;Original transcription
;----------------------
(print "Computing oracles")
(setf Handful_beatfromfile (car Handfulsimple) Handful_beatdur (cadr Handfulsimple) Handful_beatlist (make-beat-list Handful_beatfromfile Handful_beatdur)
      Handful2_beatfromfile (car Handfuldetailed) Handful2_beatdur (cadr Handfuldetailed) Handful2_beatlist (make-beat-list Handful2_beatfromfile Handful2_beatdur)
      Handful_beatdur 250)   ;;;beatdur=250, BPM=240

;Specific beatlists for particular elements of the language of Fats Waller (breaks, etc.) selected manually:
;-----------------------------------------------------------------------------------------------------------
(setf ShiftedPump_beatfromfile (car ShiftedPump) ShiftedPump_beatdur (cadr ShiftedPump) ShiftedPump_beatlist (make-beat-list ShiftedPump_beatfromfile ShiftedPump_beatdur)
      ContBass_beatfromfile (car ContBass) ContBass_beatdur (cadr ContBass) ContBass_beatlist (make-beat-list ContBass_beatfromfile ContBass_beatdur)
      4dec_beatfromfile (car 4dec) 4dec_beatdur (cadr 4dec) 4dec_beatlist (make-beat-list 4dec_beatfromfile 4dec_beatdur)
      T+C_beatfromfile (car T+C) T+C_beatdur (cadr T+C) T+C_beatlist (make-beat-list T+C_beatfromfile T+C_beatdur)
      Altern_beatfromfile (car Altern) Altern_beatdur (cadr Altern) Altern_beatlist (make-beat-list Altern_beatfromfile Altern_beatdur)
)

(setf 4dec_beatlist (avoid-nothing-for-handful 4dec_beatlist))
(setf T+C_beatlist (avoid-nothing-for-handful T+C_beatlist))

;Oracle computation:
;-------------------
;right hand
(setf beatlist-for-righthand-theme (select-right-hand-with-feature Handful_beatlist 60))
(setf Themerightoracle (NewImprovizer (append (subseq beatlist-for-righthand-theme 0 160)   ;intro+theme1 8+32=40 mes. -> beats 0-159 (= 0+4*40-1)
                                              (subseq beatlist-for-righthand-theme 308)     ;coda 7 mes. suite theme1 dans beatlist apres selection feature -> beats 308-fin
                                              (subseq beatlist-for-righthand-theme 160 308)) ;trans+theme2 5+32=37 mes. suite theme1 dans beatlist apres selection feature
                                      Handful_beatdur))                                      ;-> beats 160-307 (= 160+4*37-1)
(remove-additionaltransitions-and-suppl Themerightoracle)

;(setf beatlist-for-righthand-4+3 (avoid-nothing-for-handful (select-right-hand-with-feature Handful_beatlist 86)))
;(setf 4+3rightoracle (NewImprovizer beatlist-for-righthand-4+3 Handful_beatdur))  
;(remove-additionaltransitions-and-suppl 4+3rightoracle)

;guirlande 4+3: ---> saved into MIDi file and adjusted by hand for grouping both hands in the 2 breaks in Pont
;(save-improvizer-as-midi 4+3rightoracle)
(setf 4+3 (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/4+3-OR.mid")
      4+3_beatfromfile (car 4+3) 4+3_beatdur (cadr 4+3) 4+3_beatlist (make-beat-list 4+3_beatfromfile 4+3_beatdur)
      4+3rightoracle (NewImprovizer 4+3_beatlist Handful_beatdur))
(remove-additionaltransitions-and-suppl 4+3rightoracle)
(save-improvizer 4+3rightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-right-4+3.or")


(setf 4decrightoracle (NewImprovizer (select-right-hand-with-feature 4dec_beatlist) Handful_beatdur))
(setf T+Crightoracle (NewImprovizer (select-right-hand-with-feature T+C_beatlist) Handful_beatdur))
(setf Alternrightoracle (NewImprovizer Altern_beatlist Handful_beatdur))

;guirlande 4dec:
;---> empruntee a Smashing ---> transposer octave 4 au lieu de 5

;guirlande 4+3:
;---> deux A de Theme 1-4 (presque un 3eme) + ajouter le Pont (avec breaks arpeges tenus) 

;guirlande T+C: ---> voir Valentine
;---> conclusion Pont de Theme1-1 dans Handful
;---> element principal Pont de Theme1-2 (meme formule pour conclusion)

;motif Altern:
;---> Handful Theme2 F7 / F7  + coda Fmaj7 F7 / Bb ---> note MD = 53 octave 2
;---> Smashing Theme3-3 Cmaj7 C7 / F ---> note MD = 60 octave 3
;---> Hallelujah Theme1-4 Dmaj7 / A7 ---> note MD = 50 octave 2

;guirlande Tdesc: ---> voir Carolina


;MANQUE:
;guirlande brod:??? 
;---> Valentine1 Theme3-1: Cm C7 / Fm G7 / Cm / Fm G7 / Cm C#dim / G7  
;---> California Theme1-2: C F#dim / C7 / F C7 / F D7 / G7 D#dim / G7 
;---> Nero a 1'34:
;---> It's A Sin
;---> I Got Rhythm


;left hand
(setf beatlist-for-lefthand-theme (select-left-hand-with-feature Handful_beatlist 36))
(setf Themeleftoracle (NewImprovizer (append (subseq beatlist-for-lefthand-theme 0 160)   ;intro+theme1
                                             (subseq beatlist-for-lefthand-theme 308)     ;coda
                                             (subseq beatlist-for-lefthand-theme 160 308)) ;trans+theme2
                                     Handful_beatdur))
(remove-additionaltransitions-and-suppl Themeleftoracle)

(setf ShiftedPumpleftoracle (NewImprovizer (select-left-hand-with-feature ShiftedPump_beatlist) Handful_beatdur))
(setf ContBassleftoracle (NewImprovizer (select-left-hand-with-feature ContBass_beatlist) Handful_beatdur))

(setf StaccPump_beatlist (avoid-nothing-for-handful (select-left-hand-with-feature Handful2_beatlist 37)))
(setf StaccPumpleftoracle (NewImprovizer StaccPump_beatlist Handful_beatdur))

;staccato pump: ---> saved into MIDi file and adjusted by hand for deleting 8ves (just single bass notes)
;(save-improvizer-as-midi StaccPumpleftoracle)
(setf StaccPump (midi-to-beats "/Users/marc/Documents/RECHERCHE/Transcriptions Jazz/FatsWaller/FatsTranscriptionsChiffrees/StaccPump-OR.mid")
      StaccPump_beatfromfile (car StaccPump) StaccPump_beatdur (cadr StaccPump) StaccPump_beatlist (make-beat-list StaccPump_beatfromfile StaccPump_beatdur)
      StaccPumpleftoracle (NewImprovizer StaccPump_beatlist Handful_beatdur))
(remove-additionaltransitions-and-suppl StaccPumpleftoracle)
(save-improvizer StaccPumpleftoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-staccpump.or")


(print "Oracles computation OK")


(save-improvizer Themerightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-rigth-theme.or")
(save-improvizer Themeleftoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-theme-legpump.or")
(save-improvizer 4+3rightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-right-4+3.or")
(save-improvizer 4decrightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-rigth-4dec_5.or")
(save-improvizer T+Crightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-right-T+C_5.or")
(save-improvizer Alternrightoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-right-altern.or")
(save-improvizer StaccPumpleftoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-staccpump.or")
(save-improvizer ContBassleftoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-contbass.or")
(save-improvizer ShiftedPumpleftoracle "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/Handful-left-shiftedpump.or")

(print "Saved oracles OK")

) ;end of progn


;Verifications diverses:
(mapcar 'harmlabel (subseq beatlist-for-righthand-theme 308))   ;coda -> OK
(mapcar 'harmlabel (subseq beatlist-for-righthand-theme 0 160))  ;intro+theme1 -> OK
(mapcar 'harmlabel (subseq beatlist-for-righthand-theme 160 308))  ;trans+theme2 -> OK




(loop for i from 1 to (1- (maxetat Themerightoracle)) do (format *om-stream* " ~a " (gethash i (hashtransition Themerightoracle))))
(loop for i from 1 to (1- (maxetat Themerightoracle)) do (format *om-stream* " ~a " (feature (otext Themerightoracle i)) ))
(loop for i from 1 to (1- (maxetat Themerightoracle)) do (format *om-stream* " ~a " (harmlabel (otext Themerightoracle i)) ))
(loop for x in (select-feature (annotate-left Handful_beatlist) 36) do (format *om-stream* " ~a " (feature x)))
(loop for i from 1 to (1- (maxetat (gethash 8 (oracletable *current-tune*)))) do (format *om-stream* " ~a " (harmlabel (otext (gethash 8 (oracletable *current-tune*)) i)) ))


(ImprovizeOnHarmGrid Themerightoracle (length (simplegrid Handfulofkeys_tune)) (simplegrid Handfulofkeys_tune))


;=====================================================================================================================================================
;FEATURE TABLE                                                                                                                                       |
;=====================================================================================================================================================
;Note  | Code| Type        | Chan. |  MIDIfile                                | Description                  | Subst.| Oracle                        |
;=====================================================================================================================================================
;      |     |             | 14    |                                          | substituted MD voicings      |   X   |                               |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;      |     | Tdesc_5     | 13    | Tdesc_5+CHIF.mid                         | motive                       |       | Handful-rigth-Tdesc_5.or      |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;      |     | Altern      | 12    | Altern+CHIF.mid                          | motive                       |       | Handful-rigth-altern.or       |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;      |     | T+C_5       | 11    | T+C_5+CHIF.mid                           | motive                       |       | Handful-rigth-T+C_5.or        |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;(Mib6 | 99) | 4dec_5      | 10    | 4dec_5+CHIF.mid                          | motive                       |       | Handful-rigth-4dec_5.or       |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;Re5   | 86  | 4+3         | 9     | 4+3+CHIF.mid                             | motive (= 3e theme)          |       | Handful-rigth-4+3.or          |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;(Do3  | 60) | Theme       | 8     | Theme+CHIF-ARTICUL.mid                   | theme                        |       | Handful-rigth-theme.or        |
;=====================================================================================================================================================
;(Mi1  | 40) | ShiftedPump | 6     | ShiftedPump+CHIF-ARTICUL.mid             | shifted pump 'back beat'     |       | Handful-left-shiftedpump.or   |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;(Mib1 | 39) | ContBass    | 5     | ContBass+CHIF-ARTICUL.mid                | continuous bass              |       | Handful-left-contbass.or      | 
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;Do#1  | 37  | StaccPump   | 4     | Handful of Keys+CHIFdetailed-ARTICUL.mid | staccato pump (single bass)  |   X   | Handful-left-staccpump.or     |
;-----------------------------------------------------------------------------------------------------------------------------------------------------
;Do1   | 36  | Theme       | 3     | Handful of Keys+CHIFsimple-ARTICUL.mid   | theme + legato pump (10ths)  |       | Handful-left-theme-legpump.or |
;=====================================================================================================================================================


;one oracle
(setf labels0 (substitute-from-Fats-alternategrids))
(play (beats->chseq (select-feature (annotate-left Handful_beatlist) 37) Handful_beatdur 0))
(progn 
  (setf beatlist (ImprovizeOnHarmGrid StaccPumpleftoracle (length labels0) labels0))
  (setf impro (beats->chseq beatlist Handful_beatdur 0))
  (play impro)
)

;si la grille a des accords identiques deux par deux, il n'y a aucune raison que l'oracle saute entre deux accords identiques???????????????????
;---> si on supprime les liens suffixiels, le pb des sauts sur des accords en position paire disparaît
;---> SUPPRIMER LES LIENS SUFFIXCIELS DANS LES ORACLES DE POMPE


(setf (max-continuity StaccPumpleftoracle) 1000)
(setf (randomPrefixOccurrenceMode LegPumpleftoracle) t)

(loop for i from 1 to (maxetat StaccPumpleftoracle) do (format *om-stream* "etat=~a, label=~a~% " i (harmlabel (otext StaccPumpleftoracle i))))

(Stop-Player *general-player*)


(my-save-as-midi impro Handful_beatdur)

(setf (max-continuity StaccPumpleftoracle) 1000)

;both hands
(progn     ;(set-start-point rightoracle 1)    --> Handful 144 = 2nd chorus, 417 = 3dn chorus, 449 = 2nd A 3nd chorus
;(setf (feature Themerightoracle) '(60))
                                                                                    

    (format *om-stream* "-----------------------~%RIGHT HAND~%")
    (setf rightbeatlist (thread-Beats ;(PlayVoicings basicvoicings labels0 Handful_beatdur)) 
                                      (ImprovizeOnHarmGrid 4decrightoracle (length labels0) labels0))
          improright (beats->chseq rightbeatlist Handful_beatdur 0))
    (format *om-stream* "-----------------------~%LEFT HAND~%")
    (setf leftbeatlist (thread-Beats (ImprovizeOnHarmGrid StaccPumpleftoracle (length labels0) labels0))
          improleft (beats->chseq leftbeatlist Handful_beatdur 0))
    (setf impro (merger improright improleft))
    (play impro)
    )
(Stop-Player *general-player*)


(setf (max-continuity Themerightoracle) 1000)
(setf (randomPrefixOccurrenceMode LegPumpleftoracle) t)

(ImprovizeOnHarmGrid (gethash 3 (oracletable *current-tune*)) (length (simplegrid *current-tune*)) (simplegrid *current-tune*))

(setf *current-tune* Handfulofkeys_tune)
(grid *current-tune*)
(setf labels (loop for x in (grid *current-tune*) append (make-list (third x) :initial-element (nthcar 2 x))))
(loop for i from 1 to (1- (maxetat (gethash 3 (oracletable *current-tune*)))) do (format *om-stream* " ~a " (harmlabel (otext (gethash 3 (oracletable *current-tune*)) i)) ))
(loop for i from 1 to (1- (maxetat (gethash 8 (oracletable *current-tune*)))) do (format *om-stream* " ~a " (harmlabel (otext (gethash 8 (oracletable *current-tune*)) i)) ))
(loop for i from 1 to (1- (maxetat Themeleftoracle)) do (format *om-stream* " ~a " (harmlabel (otext Themeleftoracle i)) ))



(loop for x being the hash-key of (hashsuppl-> ContBassleftoracle) using (hash-value q) collect (list x q))
(setf (gethash 2 (hashsuppl-> ContBassleftoracle)) nil)


|#
;;;;;;;;;;;;;;;;;;;;;;;;;;
;Marc 13/8/2013
;Pour evtier d'attendre tres longtemps quand on calcule un mix de plusieurs oracles
;sur une grille tres longue et pour laquelle certaines parties ne donnent rien

;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;          AJOUTER 2 BEATS VIDES A LA FIN DE L'ORACLE AVEC LES 2 LABELS MANQUANT:
;          (bb maj7), (c m7)
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


(defun avoid-nothing-for-handful (beatlist)
  (cons (make-instance 'beat :harmlabel '(bb maj7))
        (cons (make-instance 'beat :harmlabel '(c m7)) beatlist)))


(defmethod save-improvizer-as-midi ((self improvizer))
  (let* ((beatlist (thread-Beats (oracle->beatlist self) (RefTempo self))))
    (setf beatlist (loop for x in beatlist for newbeat = (clone x) 
                         do (setf (MidiSet newbeat) (append (make-grid-event-for-beat newbeat (RefTempo self)) (MidiSet newbeat)))
                         collect newbeat))
    (my-save-as-midi (beats->chseq beatlist (RefTempo self) 0) (RefTempo self))))
    


(defmethod remove-additionaltransitions-and-suppl  ((self improvizer))
  (loop for i from 1 to (1- (maxetat self)) 
        do (setf (gethash i (hashtransition self)) (list (1+ i))
                 (gethash i (hashsuppl self)) 0)
        (remhash i (hashsuppl-> self))))

(defmethod remove-additionaltransitions ((self improvizer))
  (loop for i from 1 to (1- (maxetat self)) do (setf (gethash i (hashtransition self)) (list (1+ i)))))

(defmethod remove-suppl ((self improvizer))
  (loop for i from 1 to (1- (maxetat self)) do (setf (gethash i (hashsuppl self)) 0)))

(defmethod remove-suppl-> ((self improvizer))
  (loop for i from 1 to (1- (maxetat self)) do (remhash i (hashsuppl-> self))))



(defun substitute-from-Fats-alternategrids ()
  (let ((alternate-Theme1-A1     ;---> = first A and last A
         '(
           ;((f maj7 2) (c 7 2) (f maj7 4)            (c 7 4)            (c 7 2) (g# dim 2) (c 7 4)            (c 7 4)          (f maj7 4)            (g m7 2) (c 7 2))
           ;((f maj7 2) (c 7 2) (f maj7 4)            (c 7 4)            (c 7 2) (g# dim 2) (c 7 4)            (g m7 2) (c 7 2) (f maj7 4)            (f maj7 2) (c 7 2))

          ; ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2)   (c 7 2) (g# dim 2) (c 7 2) (g m7 2)   (c 7 4)          (f maj7 2) (g# dim 2)            (c 7 4))
          ; ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2)   (c 7 2) (f# dim 2) (c 7 2) (g m7 2)   (c 7 4)          (f maj7 4)            (f maj7 2) (c 7 2))

           ;((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 4)            (c 7 2) (g# dim 2) (c 7 2) (g m7 2)   (c 7 4)          (f maj7 2) (f# dim 2) (c 7 4))
           ;((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2)   (c 7 2) (f# dim 2) (c 7 4)            (c 7 4)          (f maj7 4)            (c 7 4))

           ;((f maj7 2) (c 7 2) (f maj7 2) (f# dim 2) (c 7 2) (f maj7 2) (c 7 2) (f# dim 2) (c 7 2) (g# dim 2) (c 7 4)          (f maj7 2) (d 7 2)    (g m7 2) (c 7 2))

;essais de substitutions:
           ;((f maj7 2) (f 7 2) (bb 7 2) (g# dim 2)      (c 7 4)            (c 7 2) (g# dim 2) (c 7 4)            (c 7 4)          (f maj7 4)            (g m7 2) (c 7 2))
           ; allant vers Bb7 au début ------> OK marche tres bien

           ;((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2)  (g dim 2) (f# dim 2) (f dim 2) (e dim 2) (c 7 4)            (c 7 4)          (f maj7 4)            (g m7 2) (c 7 2))
           ;------> pas mal, pb des 10e sur (f maj7 3) (g# dim 1) qui s'enchainent sur basse stacc.

           ;((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2)  (g dim 2) (f# dim 2) (f dim 2) (e dim 2) (g dim 2) (f# dim 2) (f dim 2) (e dim 2)  (f maj7 4)      (g m7 2) (c 7 2))
           ;------> OK

           ((f maj7 2) (f 7 2) (bb 7 2) (g# dim 2) (c 7 2)  (g m7 2)     (c 7 2) (f# dim 2) (c 7 2) (g m7 2)   (c 7 4)          (f maj7 4)            (f maj7 2) (c 7 2))

           ))
        (alternate-Theme1-A2     ;---> second A going to B
         '(
           ;((f maj7 2) (c 7 2) (f maj7 4)            (c 7 4)            (c 7 4)            (c 7 2) (f# dim 2) (g m7 2) (c 7 2) (f maj7 2) (c 7 2)    (f maj7 2) (g# dim 2))
           ;((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 4)            (c 7 2) (g# dim 2) (c 7 2) (f# dim 2) (c 7 4)          (f maj7 2) (c 7 2)    (f maj7 2) (e 7 2))
           ;((f maj7 4)         (f maj7 2) (g# dim 2) (c 7 2) (g# dim 2) (c 7 2) (f# dim 2) (c 7 2) (g m7 2)   (c 7 4)          (f maj7 2) (c 7 2)    (f maj7 2) (e 7 2))))
           ((f maj7 2) (bb 7 2) (f maj7 2) (g# dim 2)  (g dim 2) (f# dim 2) (f dim 2) (e dim 2) (g dim 2) (f# dim 2) (f dim 2) (e dim 2) (f maj7 2) (c 7 2)    (f maj7 2) (e 7 2))
           ))
        (alternate-Theme1-B 
         '(;((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 2) (g# dim 2) (c 7 4))
           ((a 7 2) (e 7 2) (a 7 4) (d 7 2) (a 7 2) (d 7 4) (g 7 2) (c# dim 2) (g 7 4) (c 7 2) (c dim 2) (c 7 4))
           ;((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 2) (c# dim 2) (g 7 4) (c 7 4) (c 7 4))
           ;((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4))
           ))
        (grid))

    (setf grid (append Handfulofkeys_intro 
                       (append (nth-random alternate-Theme1-A1) (nth-random alternate-Theme1-A2) (nth-random alternate-Theme1-B) (nth-random alternate-Theme1-A1))
                       Handfulofkeys_trans Handfulofkeys_theme2 Handfulofkeys_coda))
    ;(loop for x in grid append (make-list (third x) :initial-element (nthcar 2 x)))
))
    

;(length (expandgrid (grid *current-tune*)))
;(length (expandgrid (alternategrid *current-tune*)))

; pb de la transition vers Pont    
; pb de la fin de grille qui bifurque vers la coda
; plus de liberte harmo dans dernier chorus en guirlance 4+3 ???????????


;Modelisation des substitutions chez Fats:
;-----------------------------------------
;- 3 anticipations de C7 par Gm7, ou F#dim, ou G#dim: X / C7 ---> Gm7 / C7 ou F#dim / C7 ou G#dim / C7

;- alternance de Fmaj7 par C7: Fmaj7 / Fmaj7 ---> Fmaj7 C7 / Fmaj7
;- turn around de la fin Fmaj7 / C7: voir anatoles substitues "Hallelujah" (cf. Hallelujah-comparatif.ods): Dmaj7 F7 / Bb7 A7
;  ---> cf.  tunrn around dernier chorus Fmaj7 D7 / Gm7 C7



(setf grid '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) ;;;;;;;;;;;;;; intro
             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)
             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)
             (a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4) 
             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)))


;ChordRiff-2.mid -> one needs a specific grid for ChordRiff impros:
(setf Theme1-A '((f maj7 2) (c 7 2) (f maj7 4) (c 7 4) (g m7 2) (c 7 2) (c 7 4) (g m7 2) (c 7 2) (f maj7 2) (c 7 2) (f maj7 2) (c 7 2)))
(setf grid1 (append Theme1-A Theme1-A
                    '((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4)) 
                    Theme1-A))

(setf labels0 (loop for x in grid append (make-list (third x) :initial-element (nthcar 2 x))))
(setf labels1 (loop for x in grid1 append (make-list (third x) :initial-element (nthcar 2 x))))


(defun beats-to-beatfromfile (beatlist)
  (loop for x in beatlist collect (list (harmlabel x) (MidiSet x)) into tocollect
        finally return (list (duration (first beatlist)) tocollect)))
        
(defun select-left-hand-with-feature (beatlist &optional feature) 
  (loop for beat in (remove-channels beatlist '(1 11 15 3 4)) for newbeat = (clone beat)
        do (setf codelist (mapcar 'abs (mapcar 'first (remove 12 (MidiSet newbeat) :test '/= :key 'fifth)))
                 (feature newbeat) (if (and feature (member feature codelist)) 
                                       feature 
                                     (first codelist))     ;only 1 feature kept, possibly nil
                 (MidiSet newbeat) (remove 12 (MidiSet newbeat) :key 'fifth))
        when (or (null feature) 
                 (and (feature newbeat) (= feature (feature newbeat)))) collect newbeat))

(defun select-right-hand-with-feature (beatlist &optional feature)
  (loop for beat in (remove-channels beatlist '(2 12 15 3 4)) for newbeat = (clone beat)
        do (setf codelist (mapcar 'abs (mapcar 'first (remove 11 (MidiSet newbeat) :test '/= :key 'fifth)))
                 (feature newbeat) (if (and feature (member feature codelist)) 
                                       feature 
                                     (first codelist))     ;only 1 feature kept, possibly nil
                 (MidiSet newbeat) (remove 11 (MidiSet newbeat) :key 'fifth))
        when (or (null feature) 
                 (and (feature newbeat) (= feature (feature newbeat)))) collect newbeat))




;Handful's grid with various chord changes:

;               ((f maj7 2) (c 7 2) (f maj7 4) (c 7 4) (c 7 2) (g# dim 2) (c 7 4) (c 7 4) (f maj7 4) (g m7 2) (c 7 2)))
;               ((f maj7 2) (c 7 2) (f maj7 4) (c 7 4) (c 7 4) (c 7 2) (f# dim 2) (g m7 2) (c 7 2) (f maj7 2) (c 7 2) (f maj7 2) (sib dim 2))
;               ((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 2) (g# dim 2) (c 7 4))
;               ((f maj7 2) (c 7 2) (f maj7 4) (c 7 4) (c 7 2) (g# dim 2) (c 7 4) (g m7 2) (c 7 2) (f maj7 4) (f maj7 2) (c 7 2)))

;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 4) (f maj7 4) (c 7 4)))
;               ((f maj7 2) (c 7 2) (f maj7 4) (c 7 4) (c 7 2) (g# dim 2) (c 7 2) (f# dim 2) (c 7 4) (f maj7 2) (c 7 2) (f maj7 2) (e 7 2))
;               ((a 7 2) (e 7 2) (a 7 4) (d 7 2) (a 7 2) (d 7 4) (g 7 2) (c# dim 2) (g 7 4) (c 7 2) (c dim 2) (c 7 4))
;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 2) (f# dim 2) (c 7 2) (g m7 2) (c 7 4) (f maj7 4) (f maj7 2) (c 7 2)))

;               ((bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 2) (f 7 2) (bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 4))
;               ((d m7 2) (d 7 2) (d 7 4) (g m7 2) (eb m7 2) (g 7 4) (c 7 4) (c 7 4) (f 7 4) (f 7 4))
;               ((bb maj7 4) (bb maj7 2) (c# dim 2) (f 7 4) (bb 7 4) (bb 7 4) (eb maj7 4) (eb maj7 2) (bb 7 2) (eb maj7 2) (bb 7 2))
;               ((eb maj7 2) (eb m7 2) (bb maj7 4) (g 7 4) (c 7 4) (f 7 4) (bb maj7 2) (f 7 2) (bb maj7 2) (c 7 2))

;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 4) (c 7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 4) (f maj7 2) (f# dim 2) (c 7 4)))
;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 4) (c 7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 4) (f maj7 2) (c 7 2) (f maj7 2) (e 7 2))
;               ((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 2) (c# dim 2) (g 7 4) (c 7 4) (c 7 4))
;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g m7 2) (c 7 2) (f# dim 2) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)))

;               ((f maj7 2) (c 7 2) (f maj7 2) (f# dim 2) (c 7 2) (f maj7 2) (c 7 2) (f# dim 2) (c 7 2) (g# dim 2) (c 7 4) (f maj7 2) (d 7 2) (g m7 2) (c 7 2)))
;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (c 7 2) (g# dim 2) (c 7 2) (f# dim 2) (c 7 2) (g m7 2) (c 7 4) (f maj7 2) (c 7 2) (f maj7 2) (e 7 2))
;               ((a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4))
;               ((f maj7 2) (c 7 2) (f maj7 2) (g# dim 2) (g 7 2) (d 7 2) (g 7 4) (g 7 4) (g 7 2) (c 7 2) (f conclusion



#|
;Grids for specific element: the chord (z maj 4) is used to avoid searching empty data
;Thus the search process is skiped for unknown chords, and the result is an impro with empty bars
(setf gridbreak '((z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z maj7 4) (z 7 4)
                  (z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z maj7 4) (z 7 4)
                  (z 7 4) (z 7 4) (d 7 4) (d 7 4) (z 7 4) (z 7 4) (c 7 4) (c 7 4) 
                  (z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z maj7 4) (z 7 4)))


(setf gridconclu '((z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (f maj7 4) (c 7 4)
                   (z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (f maj7 4) (c 7 4)
                   (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) 
                   (z maj7 4) (z maj7 4) (z 7 4) (z 7 4) (z 7 4) (z 7 4) (f maj7 4) (c 7 4)))


;Function used for handling (z maj7 4) chords in Backup version (April 2013):
(defmethod undefined-label? ((label list)) 
  (not (member (first label) (append '(c c# d eb e f f# g g# a bb b) '(db d# gb ab a#)))))

(defmethod find-beat-label-match ((self improvizer) label)  
  (if (undefined-label? label)   ;;;; Marc 22/8/2012 for grid with silence excepted specific elements (breaks, etc.)
      (progn (format *om-stream* " undefined ") nil)
    (let* ((start (max 1 (min (first (start-region self)) (1- (maxetat self))))) ;-> take the lower bound of 'start-region' 
           (transpotable (make-hash-table :test '=))
           (deltalist (om- '(-3 -2 -1 0 1 2 3) (CurrentTranspo self))))
      (loop for i from start 
            to (min (second (start-region self)) (1- (maxetat self)))     
            append (loop for delta in deltalist
                         when (and (eligible-beat? (otext self i) (TransposeCloneLabel label delta))  ;;;; M.C. 5/11/12 to get a beat which is the first one of a bar:
                                   (or (= i 1)                             ;=> the previous beat must NOT be eligible according to the harmonic label   
                                       (not (eligible-beat? (otext self (1- i)) (TransposeCloneLabel label delta))))) 
                         do (push delta (gethash i transpotable))
                         and collect i)
            into res
            finally (return (progn (setf res (reduce-eligible-beats self res))
                              (let* ((chosen-index (nth-random res))
                                     (transpochoices (gethash chosen-index transpotable))
                                     (chosen-transpo (nth-random transpochoices))) 

                                (when chosen-index (incf (CurrentTranspo self) chosen-transpo)
                                  (setf (gethash chosen-index (tabou self)) t) 
                                  )
                        
                                chosen-index)))))))

|#
