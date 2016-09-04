(in-package :om)

; HarmonizationNew.lisp
;------------------------------------------------------------------------------------------------------------
; Learn a corpus for harmonization/arrangement 
; Generate accomponiment for a given input melody
;
; Jérôme Nika - May 17th 2013
;------------------------------------------------------------------------------------------------------------


; JEROME REVIEW 15/05/2013 : A REVOIR ! NOTE LES HARMO TROUVEE PAR MODULE D'HARMONISATION (?)
; /!\ Writes on channel 15 the labels in "Harmlabel => physical modification of MidiHarmBeat-list
;(midi onset dur vel can)
(defun annote_chords_generated_harmo  (MidiHarmBeat-list)
  
  (loop for cur-MidiHarmBeat in MidiHarmBeat-list
       when (HarmLabel cur-MidiHarmBeat)

       do

       ;
       (format *om-stream* "HarmLabel du MidiHarmBeat : ~a~%" (HarmLabel cur-MidiHarmBeat)) 
       ;
       (setf (MidiSet cur-MidiHarmBeat)
              (append 
               (list (list (MidiRoot (first (HarmLabel cur-MidiHarmBeat))) 50 (- (duration cur-MidiHarmBeat) 100) (case (second (HarmLabel cur-MidiHarmBeat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 15))
               (MidiSet cur-MidiHarmBeat)
               )  
              )
        )
  MidiHarmBeat-list
)



#|
(progn (pgmout 4 1) (pgmout 4 2)
(setf impro (merger (midiharmbeats->chseq (thread-Beats roundbeatlist) roundbeatdur 0)     ;melody
                    (first (harmonize-hermeto roundbeatlist roundbeatdur))))       ;harmonisation
(play impro)
)

(save-as-midi-with-tempo impro roundbeatdur "/Users/jnika/Desktop/testHermeto.mid")


|#

 
#|
;-------------------------------------------------------------------------------------
; Harmonization & arrangement : step by step tutorial
;-------------------------------------------------------------------------------------

;SET THE HARMONIZATION IMPROVIZER (on melobeats)
;(complete the first line of the chosen paragraph and evaluate)
;==============================================================
;1) Loading a saved melobeats improvizer
(setf path-harmo-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Corpus/Oracles_melo_harmo/Allthethingsyouare_solo.or")
(setf melobeats-harmo-improvizer (load-improvizer path-harmo-improvizer))

;2) or from a beatlist and the associated beat duration
;[optional argument (list) to precise the wanted channels to keep]
(setf beatlist-harmo nil beatdur-harmo nil)
(setf melobeats-harmo-improvizer (learn-melobeats-improvizer beatlist-harmo beatdur-harmo))

;3) or from "list-of-beats" data loaded with the library and the associated beat duration
;[optional argument (list) to precise the wanted channels to keep]
(setf list-of-beats-harmo Dicidenbas_beatsfromfile beatdur-harmo Dicidenbas_beatdur)
(setf beatlist-harmo (thread-beats (make-beat-list list-of-beats-harmo beatdur-harmo)))
(setf melobeats-harmo-improvizer (learn-melobeats-improvizer beatlist-harmo beatdur-harmo)) 

;4) or from a midi file
;[optional argument (list) to precise the wanted channels to keep]
;(setf path-corpus-harmo-midifile "/Users/jnika/Desktop/ArchivesUzeste/ArchivesUzesteMarc130313/Harmo17Mars2012/Goodbyeporkpiehat.3.17.2012-14-43soloB.mid" channels '(1))
(setf path-corpus-harmo-midifile "/Users/jnika/Desktop/Jovino/Balaio.Chords-Chan1.mid" channels '(1))
(setf melobeats-harmo-improvizer (learn-melobeats-improvizer-from-midifile path-corpus-harmo-midifile channels))



;((Save this improvizer, just give the tune title))
(save-harmo-melobeats-improvizer melobeats-harmo-improvizer "toto")

;SET THE ARRANGEMENT IMPROVIZER (on beats)
;(complete the first line of the chosen paragraph and evaluate)
;==============================================================
;1) Loading a saved beats improvizer
(setf path-arrang-improvizer "/Users/jnika/Zeugm/Zeugm_Max/Dev/CurrentVersion/_Corpus/Oracles_voicings/Allthethingsyouare_accomp.or")
(setf beats-arrang-improvizer (load-improvizer path-arrang-improvizer))

;2) or from a beatlist and the associated beat duration
;[optional argument (list) to precise the wanted channels to keep]
(setf beatlist-arrang nil beatdur-arrang nil)
(setf beats-arrang-improvizer (learn-beats-improvizer beatlist-arrang beatdur-arrang))

;3) or from "list-of-beats" data loaded with the library and the associated beat duration
;[optional argument (list) to precise the wanted channels to keep]
(setf list-of-beats-arrang Dicidenbas_beatsfromfile beatdur-arrang Dicidenbas_beatdur)
(setf beatlist-arrang (thread-beats (make-beat-list list-of-beats-arrang beatdur-arrang)))
(setf beats-arrang-improvizer (learn-beats-improvizer beatlist-arrang beatdur-arrang)) 

;4) or from a midi file
;[optional argument (list) to precise the wanted channels to keep]
(setf path-corpus-arrang-midifile "/Users/jnika/Desktop/ArchivesUzeste/ArchivesUzesteMarc130313/Harmo17Mars2012/Goodbyeporkpiehat.3.17.2012-14-43soloB.mid" channels '(3))
(setf beats-arrang-improvizer (learn-beats-improvizer-from-midifile path-corpus-arrang-midifile channels))


;((Save this improvizer, just give the tune title))
(save-arrang-beats-improvizer beats-arrang-improvizer "toto")

;((IMPROVIZERS SETTINGS))
;=====================
(setf (max-continuity melobeats-harmo-improvizer) 1000
      (Lengthfactorsfromscen melobeats-harmo-improvizer) '(1 1000))
; default value for the others : 
; (bestTranspoMode melobeats-harmo-improvizer) : t 
; (FirstWithoutTranspoMode melobeats-harmo-improvizer) : nil 
; (randomPrefixOccurrenceMode melobeats-harmo-improvizer) : t 

(setf (max-continuity beats-arrang-improvizer) 1000
      (Lengthfactorsfromscen beats-arrang-improvizer) '(1 1000))
; default value for the others : 
; (bestTranspoMode beats-arrang-improvizer) : t 
; (FirstWithoutTranspoMode beats-arrang-improvizer) : nil 
; (randomPrefixOccurrenceMode beats-arrang-improvizer) : t 


;CHOSE AN INPUT MELODY
;======================
;From a midifile
;(setf midifromfile-input-melo (midi-to-beatlist "/Users/jnika/Desktop/ArchivesUzeste/ArchivesUzesteMarc130313/Harmo17Mars2012/Goodbyeporkpiehat.3.17.2012-14-43soloB.mid"))
;(setf beatlist-input-melo (extract-channels-from-beatlist (first midifromfile-input-melo) '(1))
;      beatdur-input-melo (second midifromfile-input-melo))

;(setf beatlist-input-melo roundbeatlist
;      beatdur-input-melo roundbeatdur)

;(setf beatlist-input-melo (thread-beats (make-beat-list Jaimesolo-juil2011_beatsfromfile Jaimesolo-juil2011_beatdur))
;      beatdur-input-melo Dicidenbas_beatdur)

(setf beatlist-input-melo (thread-beats (make-beat-list Dicidenbas_beatsfromfile Dicidenbas_beatdur))
      beatdur-input-melo Dicidenbas_beatdur)



;GENERATE THE ACCOMPANIMENT TRACK
;=================================
(setf accomp (harmonize&arrange beatlist-input-melo beatdur-input-melo melobeats-harmo-improvizer beats-arrang-improvizer))

;((PLAY))
;======
(pgmout 4 1) 
(pgmout 4 2)
(setf mix (merger (midiharmbeats->chseq beatlist-input-melo beatdur-input-melo 0) (midiharmbeats->chseq accomp beatdur-input-melo 0)))
(Stop-Player *general-player*)
(play mix)

;((SAVE MIX IMPRO-ACCOMP AS A MIDIFILE))
;Precise the channel for accompaniment track
;============================================
(save-mix-melo-accomp-midifile beatlist-input-melo accomp beatdur-input-melo 2 "/Users/jnika/Desktop/testMix.mid")

|#


;LEARN THE CORPUS
;============================================================================================================================================
(defun learn-beats-improvizer (beatlist beatdur)
(NewImprovizer beatlist beatdur))

(defun learn-melobeats-improvizer (beatlist beatdur)
  (let ((melobeatlist (beats->melobeats (thread-Beats beatlist))))
    (NewImprovizer melobeatlist beatdur)))

;Creates a copy of a beatlist keeping the channels listed in "listchannels" (+ chan 16 & 14)
(defun extract-channels-from-beatlist (beatlist listchannels) 
  (loop for beat in beatlist          
        collect (let* ((newBeat beat)) 
                  (setf (MidiSet newBeat) 
                        (loop for midiEvent in (MidiSet beat) if (member (MEChannel midiEvent) (append '(14 16) listchannels)) collect midiEvent))
                  newBeat)))

(defun learn-beats-improvizer-on-channels (beatlist beatdur listchannels)
  (learn-beats-improvizer (extract-channels-from-beatlist beatlist listchannels) beatdur)) 

(defun learn-melobeats-improvizer-on-channels (beatlist beatdur listchannels)
  (learn-melobeats-improvizer (extract-channels-from-beatlist beatlist listchannels) beatdur))

(defun learn-melobeats-improvizer-from-midifile (absolute-path-midifile &optional listchannels)
  (let* ((import (midi-to-beatlist absolute-path-midifile))
         (beatlist (first import))
         (beatdur (second import)))
    (when (null listchannels) (setf listchannels (loop for i from 1 to 16 collect i)))
    (learn-melobeats-improvizer-on-channels beatlist beatdur listchannels)))

(defun learn-beats-improvizer-from-midifile (absolute-path-midifile &optional listchannels)
  (let* ((import (midi-to-beatlist absolute-path-midifile))
         (beatlist (first import))
         (beatdur (second import)))
    (when (null listchannels) (setf listchannels (loop for i from 1 to 16 collect i)))
    (learn-beats-improvizer-on-channels beatlist beatdur listchannels)))



;SAVE THE CORPUS
;============================================================================================================================================

(defun save-harmo-melobeats-improvizer (melobeatsImprovizer namecorpus)
  (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
    (save-improvizer 
     melobeatsImprovizer 
     (format nil "~a/~a" 
             path_dir_oracle_melo_harmo 
             (format nil "~a-HarmOracle-~a.~a.~a-~ah~a.or" namecorpus (first l) (second l) (third l) (fourth l) (fifth l))))))

(defun save-voicings-beats-improvizer (beatsImprovizer namecorpus)
  (let ((l (cdddr (reverse (multiple-value-list (get-decoded-time))))))
    (save-improvizer 
     beatsImprovizer 
     (format nil "~a/~a" 
             path_dir_oracle_voicings 
             (format nil "~a-VoicingsOracle-~a.~a.~a-~ah~a.or" namecorpus (first l) (second l) (third l) (fourth l) (fifth l))))))

(defun learn&save-harmo-improvizer-from-midifile (absolute-path-midifile namecorpus &optional listchannels)
  (save-harmo-melobeats-improvizer (learn-melobeats-improvizer-from-midifile absolute-path-midifile listchannels) namecorpus))

(defun learn&save-voicings-improvizer-from-midifile (absolute-path-midifile namecorpus &optional listchannels)
  (save-voicings-beats-improvizer (learn-beats-improvizer-from-midifile absolute-path-midifile listchannels) namecorpus))

(defun learn&save-harmo&voicings-improvizers-from-midifile (absolute-path-midifile namecorpus &optional listchannels-harmo listchannels-voicings)
  (if (> (nth 0 listchannels-harmo) 0) 
      (learn&save-harmo-improvizer-from-midifile absolute-path-midifile namecorpus listchannels-harmo))
  (if (> (nth 0 listchannels-voicings) 0)
      (learn&save-voicings-improvizer-from-midifile absolute-path-midifile namecorpus listchannels-voicings)))


;HARMONIZE AND ARRANGE
;============================================================================================================================================

; Symbolic harmonization step
;-------------------------------------------------------------------------------------
;"beatlist" is the input to harmonize
;The (symbolic) harmonization corpus is given as an improvizer learnt on MELOBEATS.
;-> The output is a list of harmonic labels
;-------------------------------------------------------------------------------------
;==========================================================================================
;== ATTENTION : modifié le 05/06/14 !!! cf ligne commentée et utilisation nouvelle fonction
;==========================================================================================
(defun harmonize (beatlist beatdur melobeatsImprovizer)
  (let* ((melolist (mapcar 'MeloSignature (beats->melobeats (thread-Beats beatlist beatdur))))
         (melobeatres (ImprovizeOnHarmGrid melobeatsImprovizer (length melolist) melolist)))
    ;(loop for x in (mapcar 'HarmLabel melobeatres) collect (if (numberp (first x)) nil x))))
    (listofslots-from-listofOjects melobeatres 'HarmLabel)))
;;;;;---------------- A METTRE ENSUITE DANS BEATLIST !!! (ou pas ???)... ne marche que pour les harmLabels NON ?
;;;;;;;;--------!!!!!!!-------- A GENERIQUER--------- !!!!!!!!!
;(defmethod listofslots-from-listofObjects ((listofobjects t) (symbolslotname symbol)) 
;; ---------> Rentrer le nom du slot en tant que string !!!
(defun listofslots-from-listofObjects (listofobjects symbolslotname)
  (loop for x in (mapcar #'(lambda (obj) (slot-value obj (intern (string-upcase symbolslotname)))) listofobjects) collect (if (numberp (first x)) nil x))) ;;;;; <<<----- A QUOI SERT CE TEST ????? VIEUX ?? A VIRER ??? SI ! 
                                                                                                ;;;;;... en fait non... c'est pour le cas ou la grid n'est pas "expanded" 
                                                                                                ;;;;;et qu'il y a des (d m7 4) ??? je sais pas....
  ;(loop for x in (mapcar (slot-value listofobjects symbolslotname)) collect (if (numberp (first x)) nil x)))
  ;(loop for obj in listofobjects collect (slot-value obj symbolslotname)))


; Whole process to get the accompaniment track
;-------------------------------------------------------------------------------------
;"beatlist" is the input to harmonize
;The (symbolic) harmonization corpus is given as an improvizer learnt on MELOBEATS
;The arrangement corpus is given as an improvizer learnt on BEATS
;-> The output is the arrangement track (beatlist)
;-------------------------------------------------------------------------------------
(defun harmonize&arrange (beatlist beatdur melobeatsImprovizer beatsImprovizer)
  (let* ((symbolic-harmonization (harmonize beatlist beatdur melobeatsImprovizer)))
    (when (/= beatdur (RefTempo beatsImprovizer)) 
                                    (SetReftempo beatsImprovizer beatdur))
    (thread-beats (ImprovizeOnHarmGrid beatsImprovizer (length symbolic-harmonization) symbolic-harmonization))))



;-------------------------------------------------------------------------------------
;"beatlist" is the input to harmonize
;The (symbolic) harmonization corpus and the arrangement corpus are given as BEATLISTS
;-> The output is the arrangement track (beatlist)
;-------------------------------------------------------------------------------------
(defun learn-corpus-from-beatlists-harmonize&arrange (beatlist-input-melo beatdur-input-melo beatlist-corpus-harmo beatdur-corpus-harmo beatlist-corpus-arrang beatdur-corpus-arrang)
  (let* ((improvizer-harmo (learn-melobeats-improvizer beatlist-corpus-harmo beatdur-corpus-harmo))
         (improvizer-arrang (learn-beats-improvizer beatlist-corpus-arrang beatdur-corpus-arrang)))
    (harmonize&arrange beatlist-input-melo beatdur-input-melo improvizer-harmo improvizer-arrang)))
#|
;EXEMPLE WITH CORPUS GIVEN AS BEATLISTS
;---------------------------------
(setf beatlist-input-melo (make-beat-list Dicidenbas_beatsfromfile Dicidenbas_beatdur)
      beatdur-input-melo Dicidenbas_beatdur)

(setf beatlist-corpus-harmo beatlist-input-melo
      beatdur-corpus-harmo beatdur-input-melo)

(setf beatlist-corpus-arrang (make-beat-list ?? ??)
      beatdur-corpus-arrang ??)

(setf accomp (learn-corpus-from-beatlists-harmonize&arrange beatlist-input-melo beatdur-input-melo beatlist-corpus-harmo beatdur-corpus-harmo beatlist-corpus-arrang beatdur-corpus-arrang))

(setf mix (merger (midiharmbeats->chseq beatlist-input-melo Dicidenbas_beatdur 0) (midiharmbeats->chseq accomp Dicidenbas_beatdur 0)))
(Stop-Player *general-player*)
(play mix)
|#

; USE THE MODULE OFFLINE
; MIDI TOOLS
;============================================================================================================================================
(defun beatlist-mix-melo-accomp (beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp)
  (let ((mix beatlist-melo))
    (loop for beat in mix do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-melo))))) 
    (add-voice-to-beatsmix beatlist-accomp mix chan-accomp beatdur)
    mix))

(defun chseq-mix-melo-accomp (beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp)
  (let ((melo beatlist-melo) (accomp beatlist-accomp))
    (loop for beat in melo do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-melo))))) 
    (loop for beat in accomp do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-accomp)))))
    (merger (midiharmbeats->chseq melo beatdur 0) (midiharmbeats->chseq accomp beatdur 0))))

(defun save-as-midi-mix-melo-accomp-midifile (beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp absolute-path-filename)
  ;(let ((mix (mix-melo-accomp beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp)))
  ;  (save-as-midi-with-tempo (midiharmbeats->chseq mix beatdur 0) beatdur absolute-path-filename)))
  (save-as-midi-with-tempo (chseq-mix-melo-accomp beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp) beatdur absolute-path-filename))

(defun harmonize&arrange&saveMixAsMidi-midifile (path-input-melo chan-input-melo melobeats-harmo-improvizer beats-arrang-improvizer chan-save-melo chan-save-accomp save-absolute-path-filename)
  (let* ((midifromfile-input-melo (midi-to-beatlist path-input-melo))
         (beatlist-input-melo (thread-beats (extract-channels-from-beatlist (first midifromfile-input-melo) (list chan-input-melo))))
         (beatdur-input-melo (second midifromfile-input-melo))
         (beatlist-accomp (harmonize&arrange beatlist-input-melo beatdur-input-melo melobeats-harmo-improvizer beats-arrang-improvizer)))
  (save-as-midi-mix-melo-accomp-midifile beatlist-input-melo beatlist-accomp beatdur-input-melo chan-save-melo chan-save-accomp save-absolute-path-filename)))

#|
;SET THE HARMONIZATION IMPROVIZER (on melobeats)
;(complete the first line of the chosen paragraph and evaluate)
;==============================================================
(setf path-corpus-harmo-midifile "/Users/jnika/Desktop/Jovino/Balaio.Solo-Chan1.mid" channels '(1))
(setf melobeats-harmo-improvizer (learn-melobeats-improvizer-from-midifile path-corpus-harmo-midifile channels))


;SET THE ARRANGEMENT IMPROVIZER (on beats)
;(complete the first line of the chosen paragraph and evaluate)
;==============================================================
(setf path-corpus-arrang-midifile "/Users/jnika/Desktop/Jovino/Balaio.Chords-Chan1.mid" channels '(1))
(setf beats-arrang-improvizer (learn-beats-improvizer-from-midifile path-corpus-arrang-midifile channels))

;((IMPROVIZERS SETTINGS))
;=====================
(setf (max-continuity melobeats-harmo-improvizer) 1000
      (Lengthfactorsfromscen melobeats-harmo-improvizer) '(1 1000))
; default value for the others : 
; (bestTranspoMode melobeats-harmo-improvizer) : t 
(setf (FirstWithoutTranspoMode melobeats-harmo-improvizer) t) 
; (randomPrefixOccurrenceMode melobeats-harmo-improvizer) : nil 

(setf (max-continuity beats-arrang-improvizer) 1000
      (Lengthfactorsfromscen beats-arrang-improvizer) '(1 1000))
; default value for the others : 
; (bestTranspoMode beats-arrang-improvizer) : t 
(setf (FirstWithoutTranspoMode beats-arrang-improvizer) t)
; (randomPrefixOccurrenceMode beats-arrang-improvizer) : nil 

;(om-inspect melobeats-harmo-improvizer)
;(om-inspect beats-arrang-improvizer)


;CHOSE AN INPUT MELODY
;======================
(setf midifromfile-input-melo (midi-to-beatlist "/Users/jnika/Desktop/Jovino/Balaio.Solo-Chan1.mid"))
(setf beatlist-input-melo  (thread-beats (extract-channels-from-beatlist (first midifromfile-input-melo) '(1)))
      beatdur-input-melo (second midifromfile-input-melo))

;GENERATE THE ACCOMPANIMENT TRACK
;=================================
(setf accomp (harmonize&arrange beatlist-input-melo beatdur-input-melo melobeats-harmo-improvizer beats-arrang-improvizer))

;((PLAY))
;======
(setf mix (merger (midiharmbeats->chseq beatlist-input-melo beatdur-input-melo 0) (midiharmbeats->chseq accomp beatdur-input-melo 0)))
(Stop-Player *general-player*)
(play mix)

;((SAVE MIX IMPRO-ACCOMP AS A MIDIFILE))
;Precise the channel for accompaniment track
;============================================
(save-as-midi-mix-melo-accomp-midifile beatlist-input-melo accomp beatdur-input-melo 1 2 "/Users/jnika/Desktop/testSave.mid")

;((Save an improvizer, just give the tune title))
(save-arrang-beats-improvizer beats-arrang-improvizer "toto")
|#



; USE THE MODULE ONLINE
; ANTESCOFO TOOLS AND METHODS FOR "TUNE" CLASS
;============================================================================================================================================

(defun harmonize&arrange&saveAntescofo2 (beatlist-melo beatdur-melo melobeatsImprovizer beatsImprovizer chan-accomp absolute-path-save-antescofo &optional absolute-path-save-midi chan-melo)
(format *om-stream* "ARGUMENTS : ~a ~a ~%" absolute-path-save-midi chan-melo)
  (let ((accomp (harmonize&arrange beatlist-melo beatdur-melo melobeatsImprovizer beatsImprovizer)))
    (loop for beat in accomp do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-accomp)))))
    (save-for-antescofo2 accomp beatdur-melo absolute-path-save-antescofo)
    (if (not (null absolute-path-save-midi))
        (progn 
          (ensure-directories-exist absolute-path-save-midi)
          (save-as-midi-mix-melo-accomp-midifile beatlist-melo accomp beatdur-melo chan-melo chan-accomp absolute-path-save-midi)
          (format *om-stream* "Saved as midi in current tune directory (MIDIharmomix) ~%")))))

(defmethod harmonize&arrange&saveAntescofo2-improchan ((self tune) improchannel melobeatsImprovizer beatsImprovizer chan-accomp &optional save-midi)
  (let* ((dir-antescofo2 (append (pathname-directory (tunedir self)) (list (tunename self) "accomp")))
         (dir-midi (if (and (not (null save-midi)) (= save-midi 1)) (append (pathname-directory (tunedir self)) (list (tunename self) "MIDIharmomix")) nil ))
         (l (mapcar #'(lambda (x) (format nil "~2,'0d" x)) (cdddr (reverse (multiple-value-list (get-decoded-time))))))
         (filename-antescofo2 (format nil "~a-harmo&arrang-ImproChan~D-~a.~a.~a-~ah~a.txt" (tunename self) improchannel (first l) (second l) (third l) (fourth l) (fifth l)))
         (filename-midi (if (and (not (null save-midi)) (= save-midi 1)) (format nil "~a-harmo&arrang-ImproChan~D-~a.~a.~a-~ah~a" (tunename self) improchannel (first l) (second l) (third l) (fourth l) (fifth l)) nil ))
         (path-antescofo2 (make-pathname :directory dir-antescofo2 :name filename-antescofo2))
         (path-midi (if (and (not (null save-midi)) (= save-midi 1)) (make-pathname :directory dir-midi :name filename-midi) nil ))
         (impro (gethash improchannel (improtable self))))
    (if impro (harmonize&arrange&saveAntescofo2 impro (beatduration self) melobeatsImprovizer beatsImprovizer chan-accomp path-antescofo2 path-midi improchannel)    
      (print "No saved impro to harmonize in (improtable ~D)" improchannel))))     



