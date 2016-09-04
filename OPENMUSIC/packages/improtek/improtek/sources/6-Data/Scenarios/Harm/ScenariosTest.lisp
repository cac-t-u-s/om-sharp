(in-package :om)


;FabriceLille240115
;--------------------------------------------------------------------------------------------------------------------
(setf FabriceLille240115_grid '(

                                (c maj7 3) (c m7 1) (c 7 2)   
                                (eb maj7 3) (eb m7 1) (eb 7 2)
                                (g maj7 3) (g m7 1) (g 7 2)    
                                (b maj7 3) (b m7 1) (b 7 2)

                            )
                                                        
      FabriceLille240115_beatdur (bpmtobeatdur 160) )  
(setf FabriceLille240115_tune (make-instance 'tune :grid FabriceLille240115_grid :chapters '(1) :beatduration FabriceLille240115_beatdur :tunename "FabriceLille240115" :NbBeatsPerMeasure 3))
(setf (gethash '"FabriceLille240115" *available-grids*) FabriceLille240115_tune)

;Test
;--------------------------------------------------------------------------------------------------------------------
(setf Test_grid '(
                        (c maj7 384)     ;384 = 3 x 128 -> 3 grilles de 32 mesures avec chapitres
)
                                                        
      Test_beatdur 400)   ;BPM= + que 150, beatdur= - que 400 ms
(setf Test_tune (make-instance 'tune :grid Test_grid :beatduration Test_beatdur :tunename "Test" :chapters '(1 33 65)))
(setf (gethash '"Test" *available-grids*) Test_tune)


;Valse (Test Jérôme)
;--------------------------------------------------------------------------------------------------------------------
(setf Valse_grid '(
                        (eb maj7 3) (db m7 1) (db 7 2)   
                        (eb maj7 3) (db m7 1) (db 7 2)
                        (eb maj7 3) (db m7 1) (db 7 2)    
                        (eb maj7 3) (db m7 1) (db 7 2)
)
                                                        
      Valse_beatdur 380)   ;BPM= + que 150, beatdur= - que 400 ms
(setf Valse_tune (make-instance 'tune :grid Valse_grid :chapters '(1) :beatduration Valse_beatdur :tunename "Valse" :NbBeatsPerMeasure 3))
(setf (gethash '"Valse" *available-grids*) Valse_tune)

;Helvella (Test Jérôme)
;--------------------------------------------------------------------------------------------------------------------
(setf Helvella_grid '(
                      (e m7 4) (b m7 4) (c maj7 4) (g maj7 4)                    
                      (e m7 2) (b m7 2) (c maj7 2) (g maj7 2) 
                      (e m7 2) (b m7 5) (e m7 1)
                            )
                                                        
      Helvella_beatdur 750)   ;BPM=80, beatdur=750 ms
(setf Helvella_tune (make-instance 'tune :grid Helvella_grid :chapters '(1) :beatduration Helvella_beatdur :tunename "Helvella" :NbBeatsPerMeasure 4))
(setf (gethash '"Helvella" *available-grids*) Helvella_tune)


#|
; Fausse grille pour sauvegarder les exemples du séminaire avec Jovino
;--------------------------------------------------------------------------------------------------------------------
(setf SeminaireEHESSJovino_grid '(
                             (d maj7 4) (a m7 4) (d maj7 4) (a m7 2) (g maj7 2)
                             (d maj7 4) (b m7 4) (bb m7 4) (f m7 4)
                             (a m7 4) (d m7 2) (g 7 2) (c maj7 4) (g m7 4)
                             (c maj7 4) (g m7 4) (f m7 4) (eb m7 2) (ab 7 2) (db maj7 4)
                             (d m7 1) (eb m7 1) (ab 7 1) (g 7 1) (c maj7 4) (d 7 2) (g 7 2) 
                             (e m7 4) (bb 7 4) (c 7 4) (d 7 4) (e 7 2) (a 7 2)
                             (bb 7 4) (c 7 4) (d 7 2) (g 7 2) (c maj7 4) (e m7 2) (a 7 2)
)
                                                        
      SeminaireEHESSJovino_beatdur 400)   ;BPM=150, beatdur=400 ms
(setf SeminaireEHESSJovino_tune (make-instance 'tune :grid SeminaireEHESSJovino_grid :chapters '(1 33) :beatduration SeminaireEHESSJovino_beatdur :tunename "SeminaireEHESSJovino"))
(setf (gethash '"SeminaireEHESSJovino" *available-grids*) SeminaireEHESSJovino_tune)
|#




#|
;St-Just Blues
;--------------------------------------------------------------------------------------------------------------------
;"Scatrap Jazzcogne" BPM=56, beatdur=1071 ms 
(setf St-Justblues_grid '((f 7 4) (db 7 4) (f 7 4) (c m7 2) (f 7 2)



      St-Justblues_beatdur 600)
(setf St-Justblues_tune (make-instance 'tune :grid St-Justblues_grid :beatduration St-Justblues_beatdur :tunename "St-Justblues"))
|#



;=====================
;DATA FOR SOLO ORACLES
;=====================
; MIDI Files:
;      - "Allthethingsyouare-solo+voicings.mid"  ---> Uzeste, November 19th 2011
;      - "Dicidenbas.soloBernard18nov2011.mid"   ---> Uzeste, November 18th 2011   BPM=168, beatdur=357
;      - "Dicidenbas.soloBernard19nov2011.mid"   ---> Uzeste, November 19th 2011
;      - "Jaime-solo2.mid" ("ticotico")          ---> Uzeste, November 17th 2011

; Jaime-nov2007_beatsfromfile ---> Uzeste, November 3rd, 2007 (BPM=88, beatdur=682, FROM ImprotekInprogress.lisp, folder "Bernard BAL")         
; Jaimesolo-juil2011_beatsfromfile ---> Uzeste, July 13th, 2011 (BPM=102, beatdur=589)
; Allthethingssolo-juil2011_beatsfromfile ---> Uzeste, July 13th, 2011 (BPM=180, beatdur=333, mean 326)
; Goodbyesolo-juil2011_beatsfromfile  ---> Uzeste, July 13th, 2011 (BPM=70, beatdur=857)
; Dicidenbassolo-juil2011_beatsfromfile  ---> Uzeste, July 13th, 2011 (BPM=204, beatdur=294)

; + from ImprotekData.lisp
; Dicidenbas_beatsfromfile  ---> Ircam, May 8th 2004 (BPM=188, beatdur=319)   ---> percu table "188.128dicirhytmic.aif"
; Goodbye_beatsfromfile   ---> Uzeste, October 1rst 2004 (BPM=77, beatdur=779) 
; Zistesolo_beatsfromfile   ---> Uzeste, April 4th 2003 (BPM=112, beatdur=536) 
; Billevans_beatsfromfile   ---> "Israel", Bill Evans, February 2nd 1961 (BPM=182, beatdur=328)

; + from Garner.lisp
; Garner-TeaForTwo_beatsfromfile
; Garner-CloseToYou_beatsfromfile
; ---> 2 oracles: garnerrightoracle, garnerleftoracle  (BPM=254, beatdur=236, half tempo BPM=127)


#|
;--------------------------------------------------------------
;Exploration of the oracles for preparing phrases to Antescofo:
;--------------------------------------------------------------

;;; For oracles on other tunes see below
(setf beatduroracle Dicidenbassolo-juil2011_beatdur           ;BPM=204 beatdur=294             
      beatsfromfile Dicidenbassolo-juil2011_beatsfromfile
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

(setf beatduroracle Garner-CloseToYou_beatdur           ;BPM=204 beatdur=294             
      beatsfromfile Garner-CloseToYou_beatsfromfile
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

;;; ... or open a MIDI file
(setf tmp (midi-to-beats) beatduroracle (second tmp) beatsfromfile (first tmp)
      oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle))

;for cumulating oracles: (setf oracle (add-improvizer oracle (NewImprovizer (make-beat-list beatsfromfile beatduroracle) beatduroracle)))


(setf toto (make-instance 'tune))
(gethash 3 (oracletable toto))
(gethash 6 (oracletable toto))
(max-continuity (gethash 6 (oracletable toto)))
(max-continuity (gethash 3 (oracletable toto)))
(max-continuity (gethash 7 (oracletable )))

(max-continuity (gethash 3 (oracletable *current-tune*)))


(setf *current-tune* Dicidenbas_tune)                         ;BPM=188 beatdur=319
(setf (gethash 6 (oracletable *current-tune*)) oracle)                ;channel 6: solo oracle
(setf (gethash 3 (oracletable *current-tune*)) garnerleftoracle)      ;channel 3: comping oracle, max-continuity = 1000


;;;;;;;;;
(ImprovizeOnHarmGrid (gethash 6 (oracletable *current-tune*)) (length (simplegrid *current-tune*)) (simplegrid *current-tune*))
(mix-impro-multi-oracle *current-tune* 
                        (list (gethash 6 (oracletable *current-tune*)) (gethash 7 (oracletable *current-tune*)) (gethash 8 (oracletable *current-tune*))) 6)

(remove nil '(142 270 398 526 654 782) :test #'(lambda (x y) (remove-if-tabou-or-region y (gethash 6 (oracletable *current-tune*)))))
(remove nil '(1 2 3) :test (lambda (x) (= x 2)))
(loop for x in '(142 270 398 526 654 782) collect (remove-if-tabou-or-region x (gethash 6 (oracletable *current-tune*))))
(remove-if-tabou-or-region 142 (gethash 6 (oracletable *current-tune*)))
(start-region (gethash 6 (oracletable *current-tune*)))
(and (tabou-mode (gethash 6 (oracletable *current-tune*))) (gethash 142 (tabou (gethash 6 (oracletable *current-tune*)))))
(or (< 142 (first (start-region (gethash 6 (oracletable *current-tune*))))) (> 142 (second (start-region (gethash 6 (oracletable *current-tune*))))))))
(setf (tabou-mode (gethash 6 (oracletable *current-tune*))) t)

(max-continuity (gethash 6 (oracletable *current-tune*)))

(setf (max-continuity (gethash 6 (oracletable *current-tune*))) 1000)
(start-region (gethash 6 (oracletable *current-tune*)))
(tabou-mode (gethash 6 (oracletable *current-tune*)))
(setf (tabou-mode (gethash 6 (oracletable *current-tune*))) t)
(loop for x being the hash-key of (tabou (gethash 6 (oracletable *current-tune*))) using (hash-value q) do (print (list x q)))
;;;;;;;;



(progn (pgmout 4 6) (Stop-Player *general-player*)
(setf impro (mix-impro-multi-oracle *current-tune* (loop for i in '(6 7 8) collect (gethash i (oracletable *current-tune*))) 6))
(setf impro1 (merger (beats->chseq impro (beatduration *current-tune*) 0)  
                     (beats->chseq (make-clocks (length (simplegrid *current-tune*)) (beatduration *current-tune*) 2) 
                                   (beatduration *current-tune*) 0)))
(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play impro1))


(save-for-antescofo impro (beatduration *current-tune*))

(save-for-antescofo impro beatduroracle)  
;/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt

(Stop-Player *general-player*)




;;; PLAY THE ORIGINAL ORACLE WITH ITS OWN BEAT DURATION

(progn (pgmout 4 2) (Stop-Player *general-player*)
(setf impro1 (merger (beats->chseq (thread-Beats (oracle->beatlist oracle) (RefTempo oracle)) (RefTempo oracle) 0)
                     (beats->chseq (make-clocks (maxetat oracle) (RefTempo oracle) 2) (RefTempo oracle) 0)))
(play impro1))

(Stop-Player *general-player*)
(my-save-as-midi impro1 beatduroracle)

;;; IMPROVIZE ON THE ORACLE ACCORDING TO ORIGINAL ORACLE BEAT DURATION                                                     

(progn  (Stop-Player *general-player*)
(setf impro (beats->chseq (mix-poly-impro *current-tune* garnerleftoracle) (beatduration *current-tune*) 0))
;(setf impro (merger impro
;                    (beats->chseq (mix-poly-impro *current-tune* oracle) (beatduration *current-tune*) 0)))
(setf impro1 (merger impro  (beats->chseq (make-clocks (length (simplegrid *current-tune*)) (beatduration *current-tune*) 2) 
                                          (beatduration *current-tune*) 0)))
(pgmout 4 3) (pgmout 4 4) (pgmout 4 5)
(play impro1))

(my-save-as-midi impro1 (beatduration *current-tune*))

(save-for-antescofo impro (beatduration *current-tune*))

(save-for-antescofo impro beatduroracle)  
;/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/buffer-for-antescofo.txt


;;; CHECK PARAMETERS:

(setf (bestSuffixMode oracle) nil)
(beatduration *current-tune*)

(set-start-point oracle 1)
(set-start-point oracle 200)
(setf (max-continuity oracle) 20)
(setf (max-continuity oracle) 50)
(setf (max-continuity oracle) 100)
(setf (max-continuity garnerleftoracle) 1000)
(maxetat oracle)


(setf (max-continuity (gethash 3 (oracletable *current-tune*))) 1000)
(setf (max-continuity (gethash 6 (oracletable *current-tune*))) 2)
(set-start-point (gethash 3 (oracletable *current-tune*)) 50)
(set-start-point (gethash 6 (oracletable *current-tune*)) 1000)
(maxetat (gethash 3 (oracletable *current-tune*)))
(maxetat (gethash 6 (oracletable *current-tune*)))

(tunename *current-tune*)
(beatduration *current-tune*)                         ;;;; beat duration of current live performance (given by Max)

(pathname-directory (tunedir *current-tune*))
(setf resfromfile (midi-to-beatlist) refbeatdur (second resfromfile) (beatduration *current-tune*) refbeatdur
      oracle (NewImprovizer (first resfromfile) refbeatdur))
(RefTempo oracle)
           
;;;;;;;;;;;!!!!!!!!!!!!!!! not in use anymore - !!!!!!!!!!!!!!!!!!!!!!
(setf (max-continuity liveoracle) 1000)
(maxetat (liveoracle *current-tune*))  
(setf (max-continuity (liveoracle *current-tune*)) 1000)
(reset-liveoracle *current-tune*)
(om-inspect (liveoracle *current-tune*))
(setf (maxpolyphony *current-tune*) 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(generate-offline-harmos *current-tune* (beatduration *current-tune*))
(load-realtime-data-and-generate-impros *current-tune* 1)


;;; OTHER TUNES

(setf beatduroracle Jaime-nov2007_beatdur   
      beatsfromfile Jaime-nov2007_beatsfromfile)

(setf beatduroracle Allthethingssolo-juil2011_beatdur  ;;;; beat duration of the original oracle             
      beatsfromfile Allthethingssolo-juil2011_beatsfromfile)

(setf beatduroracle Dicidenbas_beatdur           ;Ircam, May 8th 2004, BPM=188 beatdur=319          
      beatsfromfile Dicidenbas_beatsfromfile) 


|#


#|
;Coco
;--------------------------------------------------------------------------------------------------------------------
;Cycle 8 pulsations =  2 x 4
;Forme la plus courante: solo tambour grave bombo ou zabumba (grosse caisse) trois types de frappes (m.d. = baguette peau ou bord, m.g. = tige)
;+ accompagnement fixe pandeiro (tambourin), tarol (caisse claire), ganza (hochet)
(setf Coco_grid '((c maj7 4) (c maj7 4))
      Coco_beatdur 666)   ;BPM=90
(setf Coco_tune (make-instance 'tune :grid Coco_grid :beatduration Coco_beatdur :tunename "Coco"))

(setf (gethash '"Coco" *available-grids*) Coco_tune)
|#


#|
;Autumn Leaves
;--------------------------------------------------------------------------------------------------------------------
(setf Autumnleaves-en-mi_grid '((a m7 4) (d 7 4) (g maj7 4) (c maj7 4) (f# m7 4) (b 7 4) (e m7 4) (e m7 4)
                          (a m7 4) (d 7 4) (g maj7 4) (c maj7 4) (f# m7 4) (b 7 4) (e m7 4) (e m7 4)
                          (f# m7 4) (b 7 4) (e m7 4) (e m7 4) (a m7 4) (d 7 4) (g maj7 4) (g maj7 4)
                          (f# m7 4) (b 7 4) (e 7 2) (a 7 2) (d 7 2) (g 7 2) (c 7 4) (b 7 4) (e m7 4) (e m7 4) ) 
      Autumnleaves-en-mi_beatdur 330)
(setf Autumnleaves-en-mi_tune (make-instance 'tune :grid Autumnleaves-en-mi_grid :beatduration Autumnleaves-en-mi_beatdur :tunename "Autumnleaves-en-mi"))

(setf (gethash '"Autumnleaves-en-mi" *available-grids*) Autumnleaves-en-mi_tune)
|#

#|
;Baleine
;--------------------------------------------------------------------------------------------------------------------
;BPM=67, beatdur=896 ms
(setf Baleine_grid '((d m7 4) (e m7 4))
      Baleine_beatdur 896)
(setf Baleine_tune (make-instance 'tune :grid Baleine_grid :beatduration Baleine_beatdur :tunename "Baleine"))

(setf (gethash '"Baleine" *available-grids*) Baleine_tune)
|#

#|
;Test
;--------------------------------------------------------------------------------------------------------------------
(setf Test_grid 
'((d m7 4) (g 7 2) (e m7 2)  
  (d m7 4) (a m7 4)
  (d m7 4) (g 7 2) (e m7 2)  
  (d m7 4) (a m7 4)
  (d m7 2) (g 7 2) (d m7 2) (g 7 2)
  (d m7 2) (eb maj7 2) (db maj7 4)
  (d m7 4) (a m7 4)
  (d m7 4) (g 7 4)
  (d m7 4) (g 7 4)
  (d m7 4) (g 7 4)
(c maj7 4) (c maj7 4)
)
Test_beatdur 500)
(setf Test_tune (make-instance 'tune :grid Test_grid :beatduration Test_beatdur :tunename "Test"))

(setf (gethash '"Test" *available-grids*) Test_tune)
|#

#|
;Test2
;--------------------------------------------------------------------------------------------------------------------

(setf Test2_grid 
'( 
  (c maj7 4) (c maj7 4) (c maj7 4) (c maj7 4)
  (a m7 4) (a m7 4) 
  (f maj7 4) (g 7 4) (c maj7 4) (f maj7 4) 
  (c maj7 2) (g 7 2)
  (c maj7 4) (c maj7 4)
)
Test2_beatdur 500)
(setf Test2_tune (make-instance 'tune :grid Test2_grid :beatduration Test2_beatdur :tunename "Test2"))

(setf (gethash '"Test2" *available-grids*) Test2_tune)
|#

#|
;====
;TODO
;====
;23 de Junho de 1997 (Hermeto's calendario)
;--------------------------------------------------------------------------------------------------------------------
(setf 23deJunhode1997_grid '(
                           (bb m7 1) (gb 7 1)
                                                  ; REVOIR
                           (eb m7 2) (db maj7 2) (eb 7 1) (f# maj7 1)
                           (g 7 1) (ab m7 1) (bb m7) (f# maj7 1)
                           (bb m7 1) (f# maj7 1) (bb m7 1) (c 7 1)
                           (db maj7 1) (e 7 1) (ab maj7 1) (db 7 1)
                           (a 7 1) (bb m7 1) (g m7 1) (f# maj7 1) (bb m7 1)

                           (c m7 1) (db maj7 1) (d 7 1)
                           (bb m7 1) (c 7 1) (db maj7 1) (d m7 1) (e m7 1)
                           (g m7 1) (bb m7 1) (a 7 1) (bb m7) (a 7 1) (c maj7 1)
)
                                                        
      23deJunhode1997_beatdur 896)   ;BPM=67, beatdur=896 ms
(setf 23deJunhode1997_tune (make-instance 'tune :grid 23deJunhode1997_grid :chapters '(1) :beatduration 23deJunhode1997_beatdur :tunename "23deJunhode1997"))
(setf (gethash '"23deJunhode1997" *available-grids*) 23deJunhode1997_tune)


;23 de Junho de 1997 (Hermeto's calendario)
;--------------------------------------------------------------------------------------------------------------------
(setf 13deJunhode1997_grid '(
                            (e maj7 1) (d maj7 2) (e maj7 2) (d maj 7 1) (c maj7 1)
                            (a m7 2) (b m7 1) (g maj7 1) (e maj7 1) (eb 7 1) (d maj7 1) (db 7 1) (c maj7 2)
                            (a maj7 1) (ab m7 1) (g 7 1) (f# maj7 1) (e maj7 1) (c# m7 1) (d# m7 1) (e maj7 1) 
                            (a maj7 1) (ab m7 1) (g maj7 1) (f# maj7 1) (f maj7 1) (d maj7 1) (e maj7 1) (c# m7 1) (c# maj7 2)
)
                                                        
      13deJunhode1997_beatdur 896)   ;BPM=67, beatdur=896 ms
(setf 13deJunhode1997_tune (make-instance 'tune :grid 13deJunhode1997_grid :chapters '(1) :beatduration 13deJunhode1997_beatdur :tunename "23deJunhode1997"))
(setf (gethash '"23deJunhode1997" *available-grids*) 13deJunhode1997_tune)
|
|#

