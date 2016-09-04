; J. Nika, Nov. 2011
;
; Some harmonic scenarios: chord charts.

(in-package :om)

;--------------------------------------------------------------------------------------------------------------------
(defparameter *available-grids* (make-hash-table :test #'string= ))

(defun bpmtobeatdur (bpm) (round (/ 60000 bpm)))

(defmethod improtest-current-tune () 
            (beats->chseq (mix-poly-impro *current-tune* (oracle *current-tune*)) (beatduration *current-tune*) 0)) ;for OM patch 'improtest-current-tune'

; (d 7 4) => (d 7)(d 7)(d 7)(d 7)
(defun expand_grid (grid)
  (setf expanded_grid 
        (loop for i from 0 to (1- (list-length grid)) 
              append 
              (if (> (list-length (nth i grid)) 2)
                  (loop for j from 0 to (1- (nth 2 (nth i grid))) 
                        collect
                        (list (nth 0 (nth i grid)) (nth 1 (nth i grid)))
                        ) (list (nth i grid))))))

(defun make-oracle-from-beatlist (beatlist) (NewImprovizer beatlist))
;--------------------------------------------------------------------------------------------------------------------




;Take Five, BPM=170  (Brubeck mib mineur, Al Jarreau re mineur, mib, mi, fa, puis retour re)
;--------------------------------------------------------------------------------------------------------------------
(setf TakeFive_grid '(
                        (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2)
                        (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2)

                        (b maj7 3) (f m7b5 2) (bb m7 3) (eb m7 2) (ab m7 3) (c# 7 2) (f# maj7 5) 
                        (b maj7 3) (f m7b5 2) (bb m7 3) (eb m7 2) (ab m7 3) (c# 7 2) (f m7 3) (bb 7 2) 

                        (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2)
                        (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2) (eb m7 3) (bb m7 2)

)
                                                        
      TakeFive_beatdur 352)   ;BPM=170, beatdur= 352 ms
(setf TakeFive_tune (make-instance 'tune :grid TakeFive_grid :beatduration TakeFive_beatdur :tunename "TakeFive" :NbBeatsPerMeasure 5))
(setf (gethash '"TakeFive" *available-grids*) TakeFive_tune)


;So What, BPM=135 "Kinf Of Blue" (soiree Faravohitra BPM=180)
;--------------------------------------------------------------------------------------------------------------------
(setf SoWhat_grid '(
                        (d m7 32) (d m7 32) (eb m7 32) (d m7 32)

)
                                                        
      SoWhat_beatdur 444)   ;BPM=135, beatdur= 444 ms
(setf SoWhat_tune (make-instance 'tune :grid SoWhat_grid :beatduration SoWhat_beatdur :tunename "SoWhat"))
(setf (gethash '"SoWhat" *available-grids*) SoWhat_tune)


;Caravan, BPM=220
;--------------------------------------------------------------------------------------------------------------------
(setf Caravan_grid '(
                        (c 7 48) (f m7 16) 
                        (c 7 48) (f m7 16) 
                        (f 7 16) (bb 7 16) (eb 7 16) (ab 7 8) (g 7 8)
                        (c 7 48) (f m7 16) 
)
                                                        
      Caravan_beatdur 273)   ;BPM=220, beatdur= 273 ms
(setf Caravan_tune (make-instance 'tune :grid Caravan_grid :beatduration Caravan_beatdur :chapters '(1 33) :tunename "Caravan"))
(setf (gethash '"Caravan" *available-grids*) Caravan_tune)


;All Of Me, BPM=190
;--------------------------------------------------------------------------------------------------------------------
(setf AllOfMe_grid '(
                        (c maj7 8) (e 7 8) (a 7 8) (d m7 8) (e 7 8) (a m7 8) (d 7 8) (d m7 4) (g 7 4)
                        (c maj7 8) (e 7 8) (a 7 8) (d m7 8) (d m7 4) (eb dim 4) (c maj7 4) (a 7 4) (d m7 4) (g 7 4) (c maj7 8)
)
                                                        
      AllOfMe_beatdur 315)   ;BPM=190, beatdur= 315 ms
(setf AllOfMe_tune (make-instance 'tune :grid AllOfMe_grid :beatduration AllOfMe_beatdur :tunename "AllOfMe"))
(setf (gethash '"AllOfMe" *available-grids*) AllOfMe_tune)


;Spain, BPM=190
;--------------------------------------------------------------------------------------------------------------------
(setf Spain_grid '(
                        (g maj7 8) (f# 7 8) (e m7 4) (a 7 4) (d maj7 4) (g maj7 4) (c# 7 4) (f# 7 4) (b m7 4) (b 7 4)
)
                                                        
      Spain_beatdur 500)   ;BPM=120, beatdur= 500 ms
(setf Spain_tune (make-instance 'tune :grid Spain_grid :beatduration Spain_beatdur :tunename "Spain"))
(setf (gethash '"Spain" *available-grids*) Spain_tune)


#| Structure Al Jarreau (si mineur)
unisson: re-fa#-re-mi-re-si-re-do#-la-do#...
tempo: Gmaj7 Gmaj7 F#7 F#7 Em7 A7 Dmaj7 Gmaj7 F#7 Bm7 Bm7
unisson: si-mi-sol-fa#-re-si-mi-la-re-do#...
impro: Gmaj7 Gmaj7 F#7 F#7 Em7 A7 Dmaj7 Gmaj7 F#7 Bm7 B7
(Stevie en sib mineur)
|#


;Summertime
;--------------------------------------------------------------------------------------------------------------------
(setf Summertime_grid '(
                        (a m7 4) (a m7 4) (a m7 4) (a m7 4) 
                        (d m7 4) (d m7 4) (e 7 4) (e 7 4) 
                        (a m7 4) (a m7 4) (a m7 4) (a m7 4) 
                        (c maj7 4) (f maj7 4) (b m7b5 4) (e 7 4) 
)
                                                        
      Summertime_beatdur 1000)   ;BPM=60, beatdur= 1000 ms
(setf Summertime_tune (make-instance 'tune :grid Summertime_grid :beatduration Summertime_beatdur :tunename "Summertime"))
(setf (gethash '"Summertime" *available-grids*) Summertime_tune)





;Balaio (Hermeto)
;--------------------------------------------------------------------------------------------------------------------
(setf Balaio_grid '(
                             (d maj7 4) (a m7 4) (d maj7 4) (a m7 2) (g maj7 2)
                             (d maj7 4) (b m7 4) (bb m7 4) (f m7 4)
                             (a m7 4) (d m7 2) (g 7 2) (c maj7 4) (g m7 4)
                             (c maj7 4) (g m7 4) (f m7 4) (eb m7 2) (ab 7 2) (db maj7 4)
                             (d m7 1) (eb m7 1) (ab 7 1) (g 7 1) (c maj7 4) (d 7 2) (g 7 2) 
                             (e m7 4) (bb 7 4) (c 7 4) (d 7 4) (e 7 2) (a 7 2)
                             (bb 7 4) (c 7 4) (d 7 2) (g 7 2) (c maj7 4) (e m7 2) (a 7 2)
)
                                                        
      Balaio_beatdur 400)   ;BPM=150, beatdur=400 ms
(setf Balaio_tune (make-instance 'tune :grid Balaio_grid :chapters '(1 33) :beatduration Balaio_beatdur :tunename "Balaio"))
(setf (gethash '"Balaio" *available-grids*) Balaio_tune)




;Campinas (Hermeto's calendario p.21)
; CODA ?
;--------------------------------------------------------------------------------------------------------------------
(setf Campinas_grid '(
                      (c 7 4)
                      ;---A
                      (f maj7 4) (c 7 4) (f maj7 4)                 ;(A/F)
                      (d m7 2) (c 7 2) (a m7 2) (bb maj7 2) (c 7 4) (a maj7 2) (bb maj7 2)
                      (a m7 2) (d 7 2)
                      
                      ;---B              ;(A/F)
                      (bb m7 4) (eb 7 4) (a maj7 4)
                      ;(A/G)           ;(Dm/C)  ;(Dm/B)
                      (a 7 4) (d m7 4) (d m7 4) (d m7 4)
                      ;e13 puis e7#5
                      (e 7 4) (a maj7 4) (d maj7 4) (gb maj7 4)
                      (eb m7 4) (b maj7 2) (e maj7 2) (d maj7 2) (f# m7 2) (g maj7 4)
                      (c maj7 4)

                      ;---C
                      (f maj7 4) (bb maj7 2) (g m7 2) (a 7 4)
                      (f# 7 2) (b m7 2) (c# m7 4) (g maj7 2) (f maj7 2) (e m7 2) (f# m7 2)
                      (g# m7 2) (a# m7 2) (a maj7 4) (g maj7 4)

                      ;---D
                      (e maj7 4) (eb m7 4)
                      (c maj7 4) (e m7 4) (g maj7 4) (c maj7 4)
                                                        ;"last time only..."
                      (gb maj7 4) (f maj7 4) (b maj7 4) (ab m7 4)
                           
)
                                                        
      Campinas_beatdur 857)   ;BPM=70, beatdur=857 ms
(setf Campinas_tune (make-instance 'tune :grid Campinas_grid :chapters '(1 33) :beatduration Campinas_beatdur :tunename "Campinas"))
(setf (gethash '"Campinas" *available-grids*) Campinas_tune)


;Hermeto (Hermeto's calendario p.34)
;--------------------------------------------------------------------------------------------------------------------
(setf Hermeto_grid '(
                                           
                     (a maj7 4) (g maj7 2) (f# 7 2)
                     (b 7 2) (c 7 2) (b 7 2) (e 7 2) 

                     ;---A
                                                    ;B13
                     (a maj7 4) (g maj7 1) (f# 7 1) (b 7 2)
                     ;B13
                     (b 7 2) (b m7 2) (e 7 2)
                     (g# 7 2) (a maj7 2) (d# m7 2)
                     (g# 7 2) (c# m7 2) (f# 7 2)
                     (f maj7 2) (d m7 2) (b 7 2) (a# 7 2)

                     ;---B
                     (a m7 4) (g# 7 4)
                     (g m7 4) (gb 7 4)
                     (bb maj7 1) (a m7 1) (g m7 2) (a m7 2)
                     (ab maj7 2) (g m7 4)

                     ;---C
                     (b m7 2) (bb maj7 2) (a m7 4)
                     (a# m7 2) (a maj7 2)
                               ;C#13
                     (g# m7 2) (c# 7 2) (g# m7 2) (e 7 2)

                     ;---Coda
                               ;C#13
                     (g# m7 2) (c# 7 2)

                            )
                                                        
      Hermeto_beatdur 750)   ;BPM=80, beatdur=750 ms
(setf Hermeto_tune (make-instance 'tune :grid Hermeto_grid :chapters '(1 33) :beatduration Hermeto_beatdur :tunename "Hermeto" :NbBeatsPerMeasure 2))
(setf (gethash '"Hermeto" *available-grids*) Hermeto_tune)


;Samba do Belaqua (Hermeto's calendario p.50)
;--------------------------------------------------------------------------------------------------------------------
(setf SambaDoBelaqua_grid '(
                            (c m7 2) (f m7 2) (bb 7 2) (eb maj7 2)
                            (c m7 2) (f m7 2) (db maj7 1) (c m7 1) (g m7 1) (ab maj7 1) (g m7 1) (ab maj7 1)

                                                                        
                            (db maj7 1) (c m7 1) (bb m7 1) (ab maj7 1) (db maj7 1) (f maj7 1) (f maj7 2) (g maj7 2) 
                                                                         ;Bb13       ;A13       ;C13
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (bb 7 1) (a 7 1) (c 7 1)
                                                                                 ;Dm7b5
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (a 7 1) (d m7 1) (g 7 1)

                                                                         ;Bb13       ;A13       ;C13
                            (e m7 1) (a 7 1) (c maj7 1) (b 7 1) (e m7 1) (bb 7 1) (a 7 1) (c 7 1)
                            (a 7 1) (c maj7 3)

                            )
                                                        
      SambaDoBelaqua_beatdur 600)   ;BPM=100, beatdur=600 ms
(setf SambaDoBelaqua_tune (make-instance 'tune :grid SambaDoBelaqua_grid :chapters '(1) :beatduration SambaDoBelaqua_beatdur :tunename "SambaDoBelaqua" :NbBeatsPerMeasure 2))
(setf (gethash '"SambaDoBelaqua" *available-grids*) SambaDoBelaqua_tune)


;Santo Antonio (Hermeto's calendario p.51)
;--------------------------------------------------------------------------------------------------------------------
(setf SantoAntonio_grid '(
                          ;---A
                          (f m7 2) (c 7 2) (f m7 2) (c 7 2)
                          ;Db13       ;Bb13       ;Db13
                          (db 7 4) (bb 7 2) (db 7 1) (c 7 1)
                      
                          ;---B
                          ;F13
                          (f 7 8)
                          (e 7 8)
                                   ;D13
                          (a m7 6) (d 7 2)

                          ;---C
                          (f maj7 8)

                          ;---D
                          (f maj7 20)
)
                                                        
      SantoAntonio_beatdur 600)   ;BPM=100, beatdur=600 ms
(setf SantoAntonio_tune (make-instance 'tune :grid SantoAntonio_grid :chapters '(1 33) :beatduration SantoAntonio_beatdur :tunename "SantoAntonio" :NbBeatsPerMeasure 2))
(setf (gethash '"SantoAntonio" *available-grids*) SantoAntonio_tune)


;Moment's notice   BPM=240, beatdur=250 ms (corrections 11/1/2015)
;--------------------------------------------------------------------------------------------------------------------
(setf MomentsNotice_grid '(
                             (e m7 2) (a 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (ab m7 2) (db 7 2)
                             (d m7 2) (g 7 2) (eb m7 2) (ab 7 2) (db maj7 4) (d m7 2) (g 7 2)
                             (c m7 2) (b 7 2) (bb m7 2) (eb 7 2) (ab maj7 4) (ab m7 2) (db 7 2)
                             (g m7 2) (c 7 2) (ab m7 2) (db 7 2) (gb maj7 4) (f m7 2) (bb 7 2)
                             
                             (e m7 2) (a 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (ab m7 2) (db 7 2)
                             (d m7 2) (g 7 2) (eb m7 2) (ab 7 2) (db maj7 4) (d m7 2) (g 7 2)
                             (c m7 2) (b 7 2) (bb m7 2) (eb 7 2) (ab maj7 4) (ab m7 2) (db 7 2)
                             (g m7 2) (c 7 2) (f m7 2) (bb 7 2) (eb maj7 4) (f m7 4) 
                             (g m7 4) (f m7 4) (eb maj7 2) (f m7 2) (g m7 2) (f m7 2) (eb maj7 4) (f m7 2) (bb 7 2)
                             )
      MomentsNotice_beatdur 250)
(setf MomentsNotice_tune (make-instance 'tune :grid MomentsNotice_grid :beatduration MomentsNotice_beatdur :tunename "MomentsNotice"))

(setf (gethash '"MomentsNotice" *available-grids*) MomentsNotice_tune)


;Song for my father      ;BPM=120, beatdur=500 ms
;--------------------------------------------------------------------------------------------------------------------
(setf SongForMyFather_grid '(;A
                             (f m7 4) (f m7 4) (eb 7 4) (eb 7 4) (db 7 4) (c 7 4) (f m7 4) (f m7 4)
                             ;A
                             (f m7 4) (f m7 4) (eb 7 4) (eb 7 4) (db 7 4) (c 7 4) (f m7 4) (f m7 4)
                             ;B
                             (eb 7 4) (eb 7 4) (f m7 4) (f m7 4) (eb 7 2) (db 7 2) (c 7 4) (f m7 4) (f m7 4)
                             )
      SongForMyFather_beatdur 500)   ;BPM=120, beatdur=500 ms
(setf SongForMyFather_tune (make-instance 'tune :grid SongForMyFather_grid :beatduration SongForMyFather_beatdur :tunename "SongForMyFather"))

(setf (gethash '"SongForMyFather" *available-grids*) SongForMyFather_tune)


;Cantelope Island   ;BPM=100, beatdur=600 ms
;--------------------------------------------------------------------------------------------------------------------
(setf CantelopeIsland_grid '((f m7 4) (f m7 4) (f m7 4) (f m7 4)
                             (f m7 4) (f m7 4) (f m7 4) (f m7 4)
                             (db 7 4) (db 7 4) (db 7 4) (db 7 4)
                             (d m7 4) (d m7 4) (d m7 4) (d m7 4))
      CantelopeIsland_beatdur 600)
(setf CantelopeIsland_tune (make-instance 'tune :grid CantelopeIsland_grid :beatduration CantelopeIsland_beatdur :tunename "CantelopeIsland"))

(setf (gethash '"CantelopeIsland" *available-grids*) CantelopeIsland_tune)



;Cecile ma fille   ;BPM=160 (beatdur=375)
;--------------------------------------------------------------------------------------------------------------------
(setf Cecilemafille_grid '((c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           (f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3) (g 7 3)
                           (c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           (f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3)
                           (f m7 3) (g 7 3) (c m7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3)  (eb maj7 3)
                           (f m7 3) (g 7 3) (c m7 3) (d 7 3) (g m7 3) (d 7 3) (g 7 3) (g 7 3)
                           ;(c m7 3) (f m7 3) (g 7 3) (c m7 3) (f m7 3) (bb 7 3) (eb maj7 3) (eb maj7 3) 
                           ;(f m7 3) (g 7 3) (c m7 3) (f 7 3) (bb m7 2) (eb 7 1) (ab maj7 3) (d m7 2) (g 7 1) (c m7 3)
                           )
      Cecilemafille_beatdur 375)
(setf Cecilemafille_tune (make-instance 'tune :grid Cecilemafille_grid :chapters '(1 33) :beatduration Cecilemafille_beatdur :tunename "Cecilemafille" :NbBeatsPerMeasure 3))

(setf (gethash '"Cecilemafille" *available-grids*) Cecilemafille_tune)


;Handful of keys, Theme1   ;BPM=240 (beatdur=250)
;--------------------------------------------------------------------------------------------------------------------
(setf HandfulofkeysTheme1_intro '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4))        ;8 mes. 
      HandfulofkeysTheme1_theme1 '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)     ;32 mes. 
                                   (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)
                                   (a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4) 
                                   (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4))

   
      HandfulofkeysTheme1_grid (append HandfulofkeysTheme1_intro HandfulofkeysTheme1_theme1)

      HandfulofkeysTheme1_beatdur 250)
(setf HandfulofkeysTheme1_tune (make-instance 'tune :grid HandfulofkeysTheme1_grid :beatduration HandfulofkeysTheme1_beatdur :tunename "HandfulofkeysTheme1" :chapters '(1 9 41)))

(setf (gethash '"HandfulofkeysTheme1" *available-grids*) HandfulofkeysTheme1_tune)



;Handful of keys   ;BPM=240 (beatdur=250)
;--------------------------------------------------------------------------------------------------------------------
(setf Handfulofkeys_intro '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4))        ;8 mes. 
      Handfulofkeys_theme1 '((f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)     ;32 mes. 
                             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4)
                             (a 7 4) (a 7 4) (d 7 4) (d 7 4) (g 7 4) (g 7 4) (c 7 4) (c 7 4) 
                             (f maj7 4) (f maj7 4) (c 7 4) (c 7 4) (c 7 4) (c 7 4) (f maj7 4) (c 7 4))
      Handfulofkeys_trans '((f maj7 4) (f maj7 2) (c 7 2) (c 7 4) (f 7 4) (f 7 4))                        ;5 mes. (-> bifurque sur derniere mes. Theme1 = C7)
      Handfulofkeys_theme2                                                                                ;32 mes.
                          '((bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) (bb maj7 4) 
                            (d 7 4) (d 7 4) (g m7 4) (g 7 4) (c 7 4) (c 7 4) (f 7 4) (f 7 4)
                            (bb maj7 4) (bb maj7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4)      ;;;;; error (bb maj7 4) (bb maj7 4) 
                                                                    (eb maj7 4) (eb maj7 4) (eb maj7 4) (eb maj7 4)
                            (bb maj7 4) (bb maj7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 2) (c 7 2))
                            

                           ; (bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 2) (f 7 2) (bb maj7 2) (bb 7 2) (eb maj7 2) (eb m7 2) (bb maj7 2) (eb m7 2) (bb maj7 4)
                           ;  (d m7 2) (d 7 2) (d 7 4) (g m7 2) (eb m7 2) (g 7 4) (c 7 4) (c 7 4) (f 7 4) (f 7 4)
                           ;  (bb maj7 4) (bb maj7 2) (c# dim 2) (f 7 4) (bb 7 4) (bb 7 4) (eb maj7 4) (eb maj7 2) (bb 7 2) (eb maj7 2) (bb 7 2)
                           ;  (eb maj7 2) (eb m7 2) (bb maj7 4) (g 7 4) (c 7 4) (f 7 4) (bb maj7 2) (f 7 2) (bb maj7 2) (c 7 2))

      Handfulofkeys_coda   '((f maj7 4) (f maj7 4) (f maj7 4) (c 7 2) (f 7 2) (f 7 4) (f 7 4) (f 7 4) (f 7 4) (f 7 4))     ;7 mes. (-> bifurque sur 2 dernieres mes. Theme1 = Fmaj7 C7) 
                                                                                                                      ;6/1/2014 ajout 1 mes. coda = 9 mes. 
                                                                                          ;29/4/2014 num-beat = 4 * (num-mes - 1) + 1
                                                                                          ;INTRO:
                                                                                          ;          m. 1 -> beat1 = 1er beat mes. 1 = 4 * (1 - 1) + 1 = 0+1 = 1    
                                                                                          ;          m. 2 -> beat5 = 1er beat mes. 2 = 4 * (2 - 1) + 1 = 4+1 = 5
                                                                                          ;          m. 3 -> beat9 = 1er beat mes. 3 = 4 * (3 - 1) + 1 = 8+1 = 9

                                                                                          ;THEME1
                                                                                          ;          m. 9 -> beat33 = 4*8+1 = 33           (f maj7 4)
                                                                                          ;    
                                                                                          ;          m. 38 = 9+8+8+8+5 -> beat149 = 4*37+1 = 149         (c 7 4)  -> bifurque vers THEME2
                                                                                          ;          m. 39 = 9+8+8+8+6 -> beat153 = 4*38+1 = 153         (f maj7 4) -> bifurque vers CODA 
                                                                                          ;          m. 40 = 9+(32-1)  -> beat157          = 157         (c 7 4)  
                                        
                                                                                          ;CODA  
                                                                                          ;          m. 41             -> beat161 = 4*40+1 = 161         (f maj7 4)
                                                                                          ;          m. 42             -> beat165 = 4*41+1 = 165         (f maj7 4) 
                                                                                          ;          m. 43             -> beat169 = 4*42+1 = 169         (f maj7 4)
                                                                                          ;          m. 44             -> beat173 = 4*43+1 = 173         (c 7 2)
                                                                                          ;                            -> beat175          = 175         (f 7 2)
                                                                                          ;          m. 45             -> beat177 = 4*44+1 = 177         (f 7 4)

                                                                                          ;THEME2
                                                                                          ;          m. 50 transition
                                                                                          ;          m. 55 Theme2      -> beat217 = 4*54+1 = 217         (bb maj7 4)

                           ;'((f maj7 2) (f 7 2) (bb m7 4) (b dim 4) (c 7 2) (f 7 2) (f 7 4) (f 7 4) (f 7 4))
   
      Handfulofkeys_grid (append Handfulofkeys_intro Handfulofkeys_theme1 Handfulofkeys_coda 
                                 Handfulofkeys_trans Handfulofkeys_theme2)   ;;;permutation of Theme2: pb of 'gridconnect' limited to 64 bars
                       
;      Handfulofkeys_grid (append Handfulofkeys_intro Handfulofkeys_theme1 Handfulofkeys_trans Handfulofkeys_theme2 Handfulofkeys_coda)

      Handfulofkeys_beatdur 250)
(setf Handfulofkeys_tune (make-instance 'tune :grid Handfulofkeys_grid :beatduration Handfulofkeys_beatdur :tunename "Handfulofkeys" :chapters '(1 9 41 50)))

(setf (gethash '"Handfulofkeys" *available-grids*) Handfulofkeys_tune)



;Maracatu
;--------------------------------------------------------------------------------------------------------------------
;Solo tambour grave (un seul type de frappe) + accompagnement hochet, BPM=90
(setf Maracatu_grid '((c maj7 1) (c# maj7 1) (d maj7 1) (eb maj7 1)  ;;; necessite de distinguer les 8 temps (accents localises)
                      (e maj7 1) (f maj7 1) (f# maj7 1) (g maj7 1))
      Maracatu_beatdur 666)   ;BPM=90
(setf Maracatu_tune (make-instance 'tune :grid Maracatu_grid :beatduration Maracatu_beatdur :tunename "Maracatu"))

(setf (gethash '"Maracatu" *available-grids*) Maracatu_tune)





;Blue in green      ;BPM=75 (beatdur=790 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf BlueInGreen_grid '((bb maj7 4) (a 7 4) (d m7 2) (db 7 2) (c m7 2) (f 7 2)
                         (bb maj7 4) (a 7 4) (d m7 4) (e 7 4) (a m7 4) (d m7 4)) 
      BlueInGreen_beatdur 790)
(setf BlueInGreen_tune (make-instance 'tune :grid BlueInGreen_grid :beatduration BlueInGreen_beatdur :tunename "BlueInGreen"))

(setf (gethash '"BlueInGreen" *available-grids*) BlueInGreen_tune)



;Straight no chaser     ;BPM=181 (beatdur=330 ms) 
;--------------------------------------------------------------------------------------------------------------------
(setf StraightNoChaser_grid '((bb 7 4) (eb 7 4) (bb 7 4) (bb 7 4) (eb 7 4) (eb 7 4)
                              (bb 7 4) (bb 7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4)) 
      StraightNoChaser_beatdur 330)
(setf StraightNoChaser_tune (make-instance 'tune :grid StraightNoChaser_grid :beatduration StraightNoChaser_beatdur :tunename "StraightNoChaser"))

(setf (gethash '"StraightNoChaser" *available-grids*) StraightNoChaser_tune)


;Autumn Leaves DoMin    ;BPM=181 (beatdur=330 ms)     ---> en fait SolMin, Cm7 est le premier accord
;--------------------------------------------------------------------------------------------------------------------
(setf AutumnleavesDoMin_grid '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)

                             (c m7 4) (f 7 4)    (bb maj7 4) (eb maj7 4)  (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                                              ;;;;SUBSTITUTION Bernard:
                             ;(c m7 4) (f 7 4)   (b m7 2) (e 7 2) (bb m7 2) (eb 7 2)   (a m7 4) (d 7 4) (g m7 4) (g m7 4)

                             (a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4)     (bb maj7 4) (bb maj7 4)
                                                                                ;;;SUBSTITUTION Bernard:
                             ;(a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4)    (b m7 4) (e 7 4)

                              (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4)          (g m7 4) (g m7 4) ) 
                                                                        ;;;SUBSTITUTION Bernard:
                                                                      ;;;(bb 7 2) (eb maj7 4) (a m7 2) (d 7 2) 
                                              ;;;SUBSTITUTION Marc -> uniquement accords 7
                                              ;;;(g 7 2) (gb 7 2) (f 7 2) (e 7 2) (eb 7 4) (d 7 4)


      AutumnleavesDoMin_beatdur 330)
(setf AutumnleavesDoMin_tune (make-instance 'tune :grid AutumnleavesDoMin_grid :beatduration AutumnleavesDoMin_beatdur :tunename "AutumnleavesDoMin"))

(setf (gethash '"AutumnleavesDoMin" *available-grids*) AutumnleavesDoMin_tune)


;Trompettes    ;BPM=260 (beatdur=230 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Trompettes_grid '((f# m7 4) (eb m7 4) (ab 7 4) (db m7 4) (f# m7 4) (b m7 4) (e 7 4) (a maj7 4)
                        (d 7 4) (g maj7 4) (c# 7 4) (f# m7 4) (b m7 4) (f# m7 4) (g# 7 4) (c# 7 4)

                        (f# m7 4) (eb m7 4) (ab 7 4) (db m7 4) (f# m7 4) (b m7 4) (e 7 4) (a maj7 4)
                        (d 7 4) (g maj7 4) (c# 7 4) (f# m7 4) (eb m7 4) (g# 7 4) (c# 7 4) (f# m7 4)

                        (a maj7 4) (a maj7 4) (c# 7 4) (c# 7 4) (f# m7 4) (c# 7 4) (f# m7 4) (f# m7 4)
                        )


      Trompettes_beatdur 230)
(setf Trompettes_tune (make-instance 'tune :grid Trompettes_grid :beatduration Trompettes_beatdur :tunename "Trompettes" :chapters '(1 33)))

(setf (gethash '"Trompettes" *available-grids*) Trompettes_tune)


;Garnerloop             ;BPM=127 (beatdur=472 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Garnerloop_grid '((d m7 4) (g 7 4) (c maj7 4) (a 7 4)
) 
      Garnerloop_beatdur 472)
(setf Garnerloop_tune (make-instance 'tune :grid Garnerloop_grid :beatduration Garnerloop_beatdur :tunename "Garnerloop"))

(setf (gethash '"Garnerloop" *available-grids*) Garnerloop_tune)



;Reveeveille            ;BPM=126 (beatdur=476 ms)
;--------------------------------------------------------------------------------------------------------------------
;"126.Rev 3.wav", BPM=126, basse re-mi-sol-la, riff la#-si,  impro sur E7
;Dijon mars 2008: BPM=150 (beatdur=400 ms), basse sol-re-fa-do, grille Am7 Bm7 Gm7 Am7

(setf Reveeveille_grid '((e 7 16)) 
      Reveeveille_alternativegrid '((a m7 4) (b m7 4) (g m7 4) (a m7 4)) 
      Reveeveille_beatdur 476)
(setf Reveeveille_tune (make-instance 'tune :grid (append Reveeveille_grid Reveeveille_grid Reveeveille_grid Reveeveille_grid)
                                      :alternativegrid (append Reveeveille_alternativegrid Reveeveille_alternativegrid 
                                                               Reveeveille_alternativegrid Reveeveille_alternativegrid)
                                      :beatduration Reveeveille_beatdur :tunename "Reveeveille"))

(setf (gethash '"Reveeveille" *available-grids*) Reveeveille_tune)


;BagsGroove (ou Au privave)     ;BPM=144 (beatdur=414 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Bagsgroove_grid '((f 7 4) (f 7 4) (f 7 4) (f 7 4) (bb 7 4) (bb 7 4) (f 7 4) (f 7 4) (g m7 4) (c 7 4) (f 7 4) (c 7 4)) 
      Bagsgroove_beatdur 414)
(setf Bagsgroove_tune (make-instance 'tune :grid Bagsgroove_grid :beatduration Bagsgroove_beatdur :tunename "Bagsgroove"))

(setf (gethash '"Bagsgroove" *available-grids*) Bagsgroove_tune)



;Night in Tunisia
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=240, beatduration=250 ms  
; A / chant: AABA+C AABA+C /  scat: AAB+AABA+C / chant: AABA+C
(setf Nightintunisia_grid '((eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A
                            (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A

                            (a m7 4) (d 7 4) (g m7 4) (g m7 4) (g m7 4) (c 7 4) (f maj7 4) (e m7 2) (a 7 2)  ; B

                            (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (eb 7 4) (d m7 4) (e m7 2) (a 7 2) (d m7 4)  ; A

                            (e m7 4) (e m7 4) (eb 7 4) (eb 7 4) (d m7 4) (d m7 4) (g 7 4) (g 7 4)            ; C
                            (g m7 4) (g m7 4) (f# 7 4) (f# 7 4) (f maj7 4) (f maj7 4) (e m7 4) (a 7 4))
      Nightintunisia_beatdur 250)
(setf Nightintunisia_tune (make-instance 'tune :grid Nightintunisia_grid :beatduration Nightintunisia_beatdur :tunename "Nightintunisia"))

(setf (gethash '"Nightintunisia" *available-grids*) Nightintunisia_tune)



;D'ici d'en bas             ;BPM=188 (batdur=319 ms)
;--------------------------------------------------------------------------------------------------------------------
;"188.128dicirhytmic.aif"  BPM=188, beatdur=319 ms
;"Chansons enjazzees" BPM=220, beatdur=272 ms  (WARNING: quarter notes, not half notes !!!)
; chant: AABB AABB AABB / scat: AABB / chant: AABB AABB / basse: AABB AABB AABB "choeur": AABB AABB
(setf Dicidenbas_grid '((f m7 4) (g 7 4) (c m7 4) (c m7 4) (f m7 4) (g 7 4) (c m7 4) (c m7 4)  ; A
                        (f m7 4) (g 7 4) (c m7 4) (c m7 4) (f m7 4) (g 7 4) (c m7 4) (c 7 4)  ; A
                        (f m7 4) (bb 7 4) (eb maj7 4) (ab maj7 4) (d 7 4) (g 7 4) (c m7 4) (c 7 4) ; B
                        (f m7 4) (bb 7 4) (eb maj7 4) (ab maj7 4) (d 7 4) (g 7 4) (c m7 4) (c m7 4)) ; B
      Dicidenbas_beatdur 319)
(setf Dicidenbas_tune (make-instance 'tune :grid Dicidenbas_grid :beatduration Dicidenbas_beatdur :tunename "Dicidenbas"))

(setf (gethash '"Dicidenbas" *available-grids*) Dicidenbas_tune)


;J'aime pour la vie        BPM=100, beatdur=600 ms
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=100, beatdur=600 ms         -> structure AAB (faire tourner le A si besoin)
; chant: ABA / scat: AAB / chant: ABA
(setf Jaime_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4) (d 7 4)
                   (d 7 4) (d 7 4) (d 7 4) (d 7 4) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2)
                   (g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2))
 
      Jaime_beatdur 600)
(setf Jaime_tune (make-instance 'tune :grid Jaime_grid :beatduration Jaime_beatdur :tunename "Jaime"))

(setf (gethash '"Jaime" *available-grids*) Jaime_tune)


;Free        BPM=200, beatdur=300 ms
;--------------------------------------------------------------------------------------------------------------------
;---------4 mesures
(setf Free4_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A 
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2)
) 
      Free4_beatdur 300)
(setf Free4_tune (make-instance 'tune :grid Free4_grid :beatduration Free4_beatdur :tunename "Free4"))

(setf (gethash '"Free4" *available-grids*) Free4_tune) 

;---------8 mesures
(setf Free8_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A 
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2)
) 
      Free8_beatdur 300)
(setf Free8_tune (make-instance 'tune :grid Free8_grid :beatduration Free8_beatdur :tunename "Free8"))

(setf (gethash '"Free8" *available-grids*) Free8_tune) 

;---------16 mesures
(setf Free16_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A 
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  ;(d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2)
) 
      Free16_beatdur 300)
(setf Free16_tune (make-instance 'tune :grid Free16_grid :beatduration Free16_beatdur :tunename "Free16"))

(setf (gethash '"Free16" *available-grids*) Free16_tune) 

;---------24 mesures
(setf Free24_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A 
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2) ; B
                  ;(g 7 2) (ab 7 2) (g 7 2) (f 7 2) (g 7 2) (ab 7 2) (g 7 2) (f 7 2)
) 
      Free24_beatdur 300)
(setf Free24_tune (make-instance 'tune :grid Free24_grid :beatduration Free24_beatdur :tunename "Free24"))

(setf (gethash '"Free24" *available-grids*) Free24_tune) 

;---------32 mesures a 4 temps
(setf Free32_grid '((d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A 
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)

                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)    ; A
                  (d 7 4) (d 7 4) (d 7 4) (d 7 4)
) 
      Free32_beatdur 300)
(setf Free32_tune (make-instance 'tune :grid Free32_grid :beatduration Free32_beatdur :tunename "Free32" :NbBeatsPerMeasure 4))

(setf (gethash '"Free32" *available-grids*) Free32_tune) 


;Goodbye Porkpie Hat (Mes nuits blanches)   BPM=70, beatdur=857 ms
;--------------------------------------------------------------------------------------------------------------------
;"Chansons enjazzees" BPM=70, beatdur=857 ms 
(setf Goodbyeporkpiehat_grid '((f 7 2) (db 7 2) (gb maj7 2) (b 7 2) (eb 7 2) (db 7 2) (eb 7 2) (f 7 2)
                     (bb m7 2) (ab 7 2) (g m7 2) (c 7 2) (d 7 2) (g 7 2) (db 7 2) (gb maj7 2)
                     (bb 7 2) (db 7 2) (c 7 2) (eb 7 2) (f 7 2) (db 7 2) (gb maj7 2) (b 7 2))
      Goodbyeporkpiehat_beatdur 857)
(setf Goodbyeporkpiehat_tune (make-instance 'tune :grid Goodbyeporkpiehat_grid :beatduration Goodbyeporkpiehat_beatdur :tunename "Goodbyeporkpiehat"))

(setf (gethash '"Goodbyeporkpiehat" *available-grids*) Goodbyeporkpiehat_tune)



;All the things you are       BPM=180 (beatdur=333 ms)
;--------------------------------------------------------------------------------------------------------------------
(setf Allthethingsyouare_grid '((f m7 4) (bb m7 4) (eb 7 4) (ab maj7 4)
                                (db maj7 4) (d m7 2) (g 7 2) (c maj7 4) (c maj7 4)

                                (c m7 4) (f m7 4) (bb 7 4) (eb maj7 4)
                                (ab maj7 4) (a m7 2) (d 7 2) (g maj7 4) (g maj7 4)

                                (a m7 4) (d 7 4) (g maj7 4) (g maj7 4)
                                (f# m7 4) (b 7 4) (e maj7 4) (g m7 2) (c 7 2)
                                ;(f# m7b5 4) (b 7alt 4) (e maj7 4) (g m7b5 2) (c 7alt 2)

                                (f m7 4) (bb m7 4) (eb 7 4) (ab maj7 4)
                                (db maj7 4) (db m7 4) (c m7 4) (b m7 4)
                                ;(db maj7 4) (db m7 4) (c m7 4) (b dim7 4)
                                (bb m7 4) (eb 7 4) (ab maj7 4) (g m7 2) (c 7 2)
                                ;(bb m7 4) (eb 7 4) (ab maj7 4) (g m7b5 2) (c 7alt 2)


)
      Allthethingsyouare_beatdur 333)
(setf Allthethingsyouare_tune (make-instance 'tune :grid Allthethingsyouare_grid :chapters '(1 33) :beatduration Allthethingsyouare_beatdur :tunename "Allthethingsyouare"))

(setf (gethash '"Allthethingsyouare" *available-grids*) Allthethingsyouare_tune)



;Alice in wonderland
;--------------------------------------------------------------------------------------------------------------------

(setf AliceInWonderland_grid 
      '( 
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3) (e 7 3) 
  ;                                         m7b5
        (a m7 3) (eb 7 3) (d m7 3) (g 7 3) (e m7 3) (a m7 3)
        (d m7 3) (g 7 3) (e m7 2) (a 7 1) (d m7 2) (g 7 1)
        
        
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3) (e 7 3) 
  ;                                         m7b5
        (a m7 3) (eb 7 3) (d m7 3) (g 7 3) (e m7 3) (a m7 3)
        (d m7 3) (g 7 3) (c maj7 3) (a m7 3) 
        
        
        (d m7 3) (g 7 3) (e m7 3) (a m7 3) (d m7 3)
        (g 7 3) (c maj7 3) (f maj7 3) (f# m7 3) (b 7 3)
  ;                                          b9
        (e m7 3) (a 7 3) (d m7 2) (a 7 1) (d m7 2) (a 7 1) (d m7 2) (ab 7 1) (g 7 3)
        (d m7 3) (g 7 3) (c maj7 3) (f maj7 3) (b m7 3)
  ;                                       m7b5
        (e 7 3) (a m7 3) (eb 7 3) (d m7 3) (g 7 3)
        (e m7 3) (a m7 3) (d m7 3) (g 7 3) (c maj7 3) (c maj7 3)
        
        )
      AliceInWonderland_beatdur 400)
(setf AliceInWonderland_tune (make-instance 'tune :grid AliceInWonderland_grid :beatduration AliceInWonderland_beatdur :NbBeatsPerMeasure 3 :tunename "AliceInWonderland"))

(setf (gethash '"AliceInWonderland" *available-grids*) AliceInWonderland_tune)



;AuPrivave
;--------------------------------------------------------------------------------------------------------------------

(setf AuPrivave_grid 
      '( 
        (f maj7 4) (g m7 2) (c 7 2) (f maj7 2) (g m7 2)
        (c m7 2) (f 7 2) (bb 7 4) (bb m7 2) (eb 7 2)
        ;           +7       7b9
        (f maj7 2) (g m7 2) (a m7 2) (d 7 2) (g m7 4)
        (g m7 2) (c 7 2) (f maj7 2) (d 7 2) (g m7 2) (c 7 2)
        ;                              7b9
        
        )
      
      AuPrivave_beatdur 460)
(setf AuPrivave_tune (make-instance 'tune :grid AuPrivave_grid :beatduration AuPrivave_beatdur :tunename "AuPrivave"))

(setf (gethash '"AuPrivave" *available-grids*) AuPrivave_tune)





;BluesForAlice
;--------------------------------------------------------------------------------------------------------------------

(setf BluesForAlice_grid 
      '( 
        (f maj7 4) (e m7 2) (a 7 2) (d m7 2) (g 7 2)
        ;             m7b5     7b9
        (c m7 2) (f 7 2) (bb 7 4) (bb m7 2) (eb 7 2)
        (a m7 2) (d 7 2) (ab m7 2) (db 7 2) (g m7 4)
        (c 7 4) (f maj7 2) (d m7 2) (g m7 2) (c 7 2)
        
        )
      
      BluesForAlice_beatdur 400)
(setf BluesForAlice_tune (make-instance 'tune :grid BluesForAlice_grid :beatduration BluesForAlice_beatdur :tunename "BluesForAlice"))

(setf (gethash '"BluesForAlice" *available-grids*) BluesForAlice_tune)













