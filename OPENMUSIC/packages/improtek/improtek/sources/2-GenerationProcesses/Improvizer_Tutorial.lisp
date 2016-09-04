 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;Tutorial pour Improvizer.lisp
;--------------------------------------------------------------------------------


;==============================================================
;IMPROVIZING WITH MORRIS & PRATT
; Jerome 25/1/2013
;==============================================================

;Example : D'ici d'en bas
;------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 1000)


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-LengthFactorsFromScen oracle_solo '(10 40))


(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(play impro)


;Example : D'ici d'en bas : PLAY STEP BY STEP
;-------------------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))
(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 4)
;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(4 10))
; EVALUATE SEVERAL TIMES THESE TWO LINES
;-----------------------------------------
(Improvize-play-next-state oracle_solo grid beatduration)
(pop grid)
;-----------------------------------------




;Example : "J'aime pour la vie counterpoint"
;-------------------------------------------
(setf tune Jaime_tune)
(setf beatduration Jaimesolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp2 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp3 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration))

(setf (max-continuity oracle_solo) 1
      (max-continuity oracle_accomp) 10
      (max-continuity oracle_accomp2) 50
      (max-continuity oracle_accomp3) 100)

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(setf (Lengthfactorsfromscen oracle_solo) '(1 4)
      (Lengthfactorsfromscen oracle_accomp) '(10 20)
      (Lengthfactorsfromscen oracle_accomp2) '(10 40)
      (Lengthfactorsfromscen oracle_accomp3) '(10 50))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid)
      impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid) grid)
      impro_accomp2 (ImprovizeOnHarmGrid oracle_accomp2 (length grid) grid)
      impro_accomp3 (ImprovizeOnHarmGrid oracle_accomp3 (length grid) grid))

(setf impro (merger 
             (beats->chseq impro_solo beatduration 0) 
             (merger 
              (beats->chseq impro_accomp beatduration 0) 
              (merger 
               (beats->chseq impro_accomp2 beatduration 0) 
               (beats->chseq impro_accomp3 beatduration 0)))))

(pgmout 3 3)
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




;Example : "J'aime pour la vie" + "Cantelope Island"
;-------------------------------------------
(setf tune3 Jaime_tune)
(setf grid3 (expand_grid (grid tune3)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) (beatduration tune3)))
(setf oracle_accomp (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(4 10))
(set-Lengthfactorsfromscen oracle_accomp '(1 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid3) grid3))
(setf impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid3) grid3))

(setf impro (merger (beats->chseq impro_solo (beatduration tune3) 0) (beats->chseq impro_accomp (beatduration tune3) 0)))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)



;Example : Cantelope
;-------------------
(setf tune CantelopeIsland_tune)
(setf beatduration (beatduration tune))
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))
(setf (max-continuity oracle_solo) 1000)
(setf (start-region oracle_solo) '(33 100))


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(10 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




===========Save===========
(my-save-as-midi impro beatduration) 




#|
;==============================================================
;FIND-PREFIX-LABEL-MATCH WITH NEW CONTROLS IN CLASS "IMPROVIZER"
;==============================================================

;Data
;----
(setf beat-list3 '(
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((a) ((60 0 500 80 1)))
((a) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((62 0 500 80 1)))
((a) ((64 0 500 80 1)))
((d) ((60 0 500 80 1)))
((gb) ((60 0 500 80 1)))
((c#) ((60 0 500 80 1)))
((d#) ((62 0 500 80 1)))
((e#) ((64 0 500 80 1)))
))
(setf oracle (NewImprovizer (make-beat-list beat-list3)))
(setf grid '((gb) (c#) (a) (d) (e) (c)))


; (Lengthfactorsfromscen [Improvizer]) filters the length of the returned prefixes in SELECT-matching-prefixes
;-------------------------------------------------------------------------------------------------------------

;Indexes filtered
(setf (Lengthfactorsfromscen oracle) '(1 2))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)

;No index filtered
(setf (Lengthfactorsfromscen oracle) '(1 1000))
(setf resultSelect (select-matching-prefixes oracle grid))
(print-select-matching-prefixes resultSelect)


; in FIND-prefix-labels-match
; (bestTranspoMode [Improvizer]) = nil : first random transposition giving a prefix with length >=1 
; (bestTranspoMode [Improvizer]) = t : looking for the longest prefix with all the transpositions authorized in (AuthorizedTranspos [self]) (default)
;-----------------------------------------------------------------------------------------------------------------------------------------------------
(setf (bestTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

(setf (bestTranspoMode oracle) nil)
(find-prefix-labels-match oracle grid)


; (firstWithoutTranspoMode [Improvizer]) = t : first searches with no transposition (default = nil)
;----------------------------------------------------------------------------------------------------
(setf (firstWithoutTranspoMode oracle) t)
(find-prefix-labels-match oracle grid)

;(randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;(randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;------------------------------------------------------------------------------------------------------------------------------------------------------------
(setf (randomPrefixOccurrenceMode oracle) t)
(find-prefix-labels-match oracle grid)

; In find-prefix-label-match, the length is randomly chosen among those returned by select-matching-prefixes
; ==> TO SELECT A PRECISE LENGTH N : (setf (Lengthfactorsfromscen [Improvizer] '(N N)))
;----------------------------------------------------------------------------------------------------------
(setf (Lengthfactorsfromscen oracle) '(4 4))
(setf #|
;==============================================================
;IMPROVIZING WITH MORRIS & PRATT
; Jerome 25/1/2013
;==============================================================

;Example : D'ici d'en bas
;------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 1000)


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(10 40))


(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(play impro)


;Example : D'ici d'en bas : PLAY STEP BY STEP
;-------------------------------------
(setf tune Dicidenbas_tune)
(setf beatduration Dicidenbassolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))
(setf oracle_solo (NewImprovizer (make-beat-list Dicidenbassolo-juil2011_beatsfromfile) beatduration))
(setf (max-continuity oracle_solo) 4)
;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(4 10))
; EVALUATE SEVERAL TIMES THESE TWO LINES
;-----------------------------------------
(Improvize-play-next-state oracle_solo grid beatduration)
(pop grid)
;-----------------------------------------




;Example : "J'aime pour la vie counterpoint"
;-------------------------------------------
(setf tune Jaime_tune)
(setf beatduration Jaimesolo-juil2011_beatdur)
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp2 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration)
      oracle_accomp3 (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) beatduration))

(setf (max-continuity oracle_solo) 1
      (max-continuity oracle_accomp) 10
      (max-continuity oracle_accomp2) 50
      (max-continuity oracle_accomp3) 100)

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(setf (Lengthfactorsfromscen oracle_solo) '(1 4)
      (Lengthfactorsfromscen oracle_accomp) '(10 20)
      (Lengthfactorsfromscen oracle_accomp2) '(10 40)
      (Lengthfactorsfromscen oracle_accomp3) '(10 50))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid)
      impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid) grid)
      impro_accomp2 (ImprovizeOnHarmGrid oracle_accomp2 (length grid) grid)
      impro_accomp3 (ImprovizeOnHarmGrid oracle_accomp3 (length grid) grid))

(setf impro (merger 
             (beats->chseq impro_solo beatduration 0) 
             (merger 
              (beats->chseq impro_accomp beatduration 0) 
              (merger 
               (beats->chseq impro_accomp2 beatduration 0) 
               (beats->chseq impro_accomp3 beatduration 0)))))

(pgmout 3 3)
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




;Example : "J'aime pour la vie" + "Cantelope Island"
;-------------------------------------------
(setf tune3 Jaime_tune)
(setf grid3 (expand_grid (grid tune3)))

(setf oracle_solo (NewImprovizer (make-beat-list Jaimesolo-juil2011_beatsfromfile) (beatduration tune3)))
(setf oracle_accomp (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))

;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(4 10))
(set-Lengthfactorsfromscen oracle_accomp '(1 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid3) grid3))
(setf impro_accomp (ImprovizeOnHarmGrid oracle_accomp (length grid3) grid3))

(setf impro (merger (beats->chseq impro_solo (beatduration tune3) 0) (beats->chseq impro_accomp (beatduration tune3) 0)))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)



;Example : Cantelope
;-------------------
(setf tune CantelopeIsland_tune)
(setf beatduration (beatduration tune))
(setf grid (expand_grid (grid tune)))

(setf oracle_solo (load-improvizer "/Users/Nika/Developpement/ImproteK/Dev/ImproteK_Max_010213/_Oracles/Cantelope-29janv13-solo1.or"))
(setf (max-continuity oracle_solo) 1000)
(setf (start-region oracle_solo) '(33 100))


;++++++++++++++++++++++++++++++++++++++++
; New parameter !
;++++++++++++++++++++++++++++++++++++++++
; default value for the others : 
; (bestTranspoMode oracle) : t 
; (FirstWithoutTranspoMode oracle) : nil 
; (randomPrefixOccurrenceMode oracle) : t 
;++++++++++++++++++++++++++++++++++++++++
(set-Lengthfactorsfromscen oracle_solo '(10 1000))

(setf impro_solo (ImprovizeOnHarmGrid oracle_solo (length grid) grid))
(setf impro (beats->chseq impro_solo beatduration 0))
(pgmout 4 1)
(pgmout 5 2)
(Stop-Player *general-player*)
(play impro)




===========Save===========
(my-save-as-midi impro beatduration) 
|#resultSelect (select-matching-prefixes oracle grid))
(find-prefix-labels-match oracle grid)
|#





#|
;Marc 26/1/2013 for preparing oracles in the directory "_Oracles":

(save-improvizer (gethash 8 (oracletable *current-tune*)) "caca.or")
(load-improvizer "/Users/marc/Documents/RECHERCHE/TUTORIAL_MAX/Antescofo~_Max_UB/_Oracles/BluesForAlice_solo.or")
(load-improvizer-from)

(setf path_oracle (format nil "~a/~a" path_dir_live_oracles "Cantalope_Uzeste29janv12.or" ))
(save-improvizer (gethash 8 (oracletable *current-tune*)) path_oracle)

;'choose-new-file-dialog' does not seem to work anymore ---> ask Jean???

|#

#|
;DOES NOT SEEM TO WORK IN LispWork !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
(defmethod save-improvizer-as ((self improvizer))
  (catch-cancel
     (let* ((name (choose-new-file-dialog :prompt "Save Improvizer As...")))
       (when name
         (save-improvizer self name)))))

(defmethod load-improvizer-from ()
  (catch-cancel
     (let ((name (choose-file-dialog)))
       (when name
         (load-improvizer name)))))
|#



#|

(setf beat-list '(((b 7) ((35 0 237 97 1) (68 0 479 69 0) (63 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (92 44 244 100 2) (69 68 67 40 2) (64 79 81 26 2) (58 79 56 21 2) (47 242 237 97 1) (93 288 102 70 2) (92 365 109 48 2) (90 435 49 56 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (90 0 56 56 2) (87 56 220 61 2) (41 242 237 97 1) (85 315 126 62 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (63 21 67 21 2) (57 28 60 21 2) (68 32 56 14 2) (45 242 237 97 1))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (80 24 265 64 2) (81 303 91 36 2) (80 372 112 25 2) (78 424 60 25 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (78 0 50 25 2) (80 0 15 25 2) (75 39 231 30 2) (32 242 116 97 1) (73 315 108 26 2) (44 363 116 97 1))) ((e m7) ((40 0 237 97 1) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (40 242 242 97 1) (75 321 125 47 2) (56 345 104 18 2) (62 351 64 22 2) (67 356 70 13 2))) ((e m7) ((40 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (40 242 237 97 1) (70 307 110 28 2))) ((a 7) ((33 0 237 97 1) (66 0 479 69 0) (61 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (61 3 129 21 2) (66 3 105 15 2) (55 13 115 20 2) (40 242 237 97 1) (68 468 16 24 2))) ((a 7) ((45 0 237 97 1) (59 0 479 69 0) (61 0 479 69 0) (45 0 237 97 1) (68 0 169 24 2) (67 121 245 69 0) (71 363 116 69 0) (68 469 15 27 2))) ((g# 7) ((32 0 237 97 1) (65 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (68 0 310 27 2) (44 242 237 97 1) (70 310 125 24 2))) ((g# 7) ((32 0 237 97 1) (58 0 479 69 0) (60 0 479 69 0) (62 0 479 69 0) (65 0 479 69 0) (32 0 237 97 1) (44 242 237 97 1) (56 316 115 21 2) (51 326 115 18 2) (61 326 95 16 2))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (63 332 150 25 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (29 242 116 97 1) (63 298 175 29 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 484 69 0) (64 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (65 67 49 36 2) (66 162 80 35 2) (46 242 237 97 1) (67 267 45 34 2) (68 357 60 32 2) (69 447 37 60 2))) ((bb 7) ((34 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (64 0 479 69 0) (67 0 479 69 0) (34 0 237 97 1) (69 0 27 60 2) (70 50 103 33 2) (71 163 60 59 2) (46 242 237 97 1) (72 258 80 37 2) (73 338 35 33 2) (74 398 86 46 2))) ((c m7) ((36 0 479 97 1) (62 0 479 69 0) (58 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (74 0 7 46 2) (75 29 105 32 2) (76 144 45 41 2) (77 214 105 27 2) (75 309 100 35 2) (74 419 65 50 2))) ((b) ((35 0 237 97 1) (75 0 479 69 0) (70 0 479 69 0) (66 0 479 69 0) (63 0 479 69 0) (59 0 479 69 0) (74 0 10 50 2) (75 20 95 25 2) (76 135 60 43 2) (77 215 85 51 2) (47 242 237 97 1) (78 300 45 39 2) (79 380 95 43 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (80 14 92 31 2) (81 106 55 45 2) (82 165 101 41 2) (46 242 237 97 1) (81 291 35 14 2) (80 301 85 35 2) (79 381 70 33 2) (80 466 18 23 2))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (80 0 87 23 2) (81 117 63 23 2) (82 192 100 31 2) (45 242 237 97 1) (83 307 45 53 2) (84 387 35 41 2) (85 447 37 20 2))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (85 0 18 20 2) (86 43 60 58 2) (87 158 73 22 2) (88 228 35 51 2) (89 288 80 44 2) (87 388 95 40 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (86 0 44 35 2) (87 69 105 27 2) (88 199 35 35 2) (32 242 116 97 1) (89 279 60 32 2) (44 363 116 97 1) (90 385 23 20 2) (91 419 65 51 2))) ((c#) ((37 0 237 97 1) (68 0 479 69 0) (65 0 479 69 0) (63 0 479 69 0) (60 0 479 69 0) (91 0 15 51 2) (92 50 105 26 2) (49 242 237 97 1))) ((g) ((31 0 237 97 1) (71 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (59 0 479 69 0) (55 0 479 69 0) (43 242 237 97 1))) ((g 7) ((31 0 237 97 1) (64 0 479 69 0) (59 0 484 69 0) (57 0 484 69 0) (53 0 484 69 0) (68 12 440 18 2) (63 36 418 16 2) (73 57 427 11 2) (38 242 237 97 1))) ((g 7) ((43 0 237 97 1) (57 0 479 69 0) (59 0 479 69 0) (43 0 237 97 1) (73 0 108 11 2) (65 121 245 69 0) (69 363 116 69 0))) ((c 7) ((36 0 237 97 1) (69 0 479 69 0) (62 0 484 69 0) (58 0 484 69 0) (57 0 484 69 0) (52 0 484 69 0) (75 20 464 11 2) (65 29 325 18 2) (70 29 363 16 2) (36 242 242 97 1))) ((c 7) ((36 0 237 97 1) (52 0 479 69 0) (57 0 479 69 0) (58 0 479 69 0) (62 0 479 69 0) (75 0 10 11 2) (64 121 228 69 0) (36 242 237 97 1) (67 363 116 69 0))) ((f 7) ((29 0 237 97 1) (62 0 484 69 0) (59 0 484 69 0) (57 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (68 16 128 18 2) (73 26 129 20 2) (78 26 105 16 2) (41 242 237 97 1))) ((f 7) ((29 0 237 97 1) (55 0 479 69 0) (57 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (29 0 237 97 1) (97 7 230 84 2) (41 242 237 97 1) (96 310 137 98 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (41 242 237 97 1) (92 298 105 48 2) (73 473 11 23 2))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (63 0 59 24 2) (68 0 73 23 2) (73 0 108 23 2) (94 3 140 72 2) (68 121 245 69 0) (72 363 116 69 0))) ((b m7) ((35 0 479 97 1) (66 0 484 69 0) (62 0 484 69 0) (61 0 484 69 0) (57 0 484 69 0) (70 0 170 17 2) (90 0 158 48 2) (65 15 165 16 2) (75 19 136 9 2))) ((b m7) ((47 0 237 97 1) (61 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (47 0 237 97 1) (87 0 206 35 2) (35 242 116 97 1) (47 363 116 97 1))) ((e 7) ((40 0 479 97 1) (70 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (56 0 479 69 0) (85 2 265 24 2) (82 302 154 25 2) (56 302 115 20 2) (66 316 116 16 2) (61 327 70 19 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1) (80 293 140 24 2))) ((eb 7) ((39 0 479 97 1) (69 0 479 69 0) (65 0 479 69 0) (61 0 479 69 0) (55 0 479 69 0) (78 9 164 22 2))) ((g# m7) ((32 0 237 97 1) (63 0 479 69 0) (59 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (75 0 210 21 2) (44 242 237 97 1) (78 465 19 18 2))) ((c# 7) ((37 0 237 97 1) (70 0 479 69 0) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (53 0 484 69 0) (78 0 284 18 2) (37 242 242 97 1) (75 284 102 18 2))) ((c# 7) ((37 0 237 97 1) (53 0 479 69 0) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (65 121 228 69 0) (37 242 237 97 1) (80 252 150 25 2) (68 363 116 69 0))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (42 242 242 97 1) (75 243 140 21 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (78 294 140 20 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (63 229 46 22 2) (73 229 111 20 2) (68 240 59 18 2) (75 250 188 19 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (80 206 85 35 2) (75 221 70 18 2) (30 242 116 97 1) (70 244 57 15 2) (81 291 139 33 2) (42 363 116 97 1))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (80 2 210 30 2) (78 397 87 22 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (78 0 133 22 2) (68 240 103 15 2) (29 242 116 97 1) (63 248 70 18 2) (75 252 231 24 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (73 239 167 26 2) (41 242 237 97 1))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (90 50 434 70 2) (68 121 245 69 0) (72 363 116 69 0))) ((b 7) ((35 0 237 97 1) (68 0 479 69 0) (63 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (90 0 356 70 2) (47 242 237 97 1) (87 321 115 55 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (41 242 237 97 1) (85 347 80 32 2))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (46 242 237 97 1) (82 338 80 27 2))) ((eb m7) ((39 0 479 97 1) (65 0 479 69 0) (61 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (80 109 235 14 2))) ((g# 7) ((32 0 237 97 1) (65 0 484 69 0) (62 0 484 69 0) (60 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (80 45 185 21 2) (44 242 237 97 1))) ((g# 7) ((32 0 237 97 1) (58 0 479 69 0) (60 0 479 69 0) (62 0 479 69 0) (65 0 479 69 0) (32 0 237 97 1) (78 21 284 16 2) (44 242 237 97 1))) ((c# 7) ((37 0 237 97 1) (70 0 479 69 0) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (53 0 484 69 0) (37 242 242 97 1))) ((c# 7) ((37 0 237 97 1) (53 0 479 69 0) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (73 68 416 22 2) (65 121 228 69 0) (37 242 237 97 1) (68 363 116 69 0))) ((eb m7) ((39 0 237 97 1) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (66 0 49 20 2) (61 0 69 26 2) (73 0 314 22 2) (56 14 45 16 2) (39 242 242 97 1) (75 304 93 33 2))) ((eb m7) ((39 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (63 35 90 20 2) (68 49 81 18 2) (39 242 237 97 1))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (87 16 130 28 2) (42 242 242 97 1) (65 281 130 29 2) (75 281 105 20 2) (70 296 140 19 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (82 297 45 27 2))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (85 278 80 30 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (63 4 80 22 2) (68 14 60 22 2) (73 29 55 15 2) (85 39 245 34 2) (29 242 116 97 1) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (84 50 120 33 2) (41 242 237 97 1))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (80 36 448 27 2) (68 121 245 69 0) (70 301 115 16 2) (75 301 80 12 2) (65 301 105 18 2) (72 363 116 69 0))) ((c 7) ((36 0 479 97 1) (66 0 479 69 0) (62 0 479 69 0) (58 0 479 69 0) (52 0 479 69 0) (80 0 282 27 2) (82 292 105 45 2))) ((f 7) ((29 0 237 97 1) (62 0 479 69 0) (57 0 479 69 0) (55 0 479 69 0) (51 0 479 69 0) (41 242 237 97 1) (63 287 66 28 2) (73 298 59 16 2) (68 308 49 17 2))) ((bb m7) ((34 0 237 97 1) (65 0 479 69 0) (61 0 479 69 0) (60 0 479 69 0) (56 0 479 69 0) (87 9 140 47 2) (46 242 237 97 1))) ((a) ((33 0 237 97 1) (73 0 479 69 0) (68 0 479 69 0) (64 0 479 69 0) (61 0 479 69 0) (57 0 479 69 0) (65 15 160 20 2) (70 25 165 18 2) (75 25 130 17 2) (45 242 237 97 1) (82 315 140 35 2))) ((g# m7) ((32 0 479 97 1) (63 0 484 69 0) (59 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (85 286 115 58 2))) ((g# m7) ((44 0 237 97 1) (58 0 479 69 0) (59 0 479 69 0) (63 0 479 69 0) (44 0 237 97 1) (85 32 185 34 2) (32 242 116 97 1) (73 287 85 13 2) (68 287 60 18 2) (63 297 50 13 2) (44 363 116 97 1) (84 462 22 45 2))) ((g) ((31 0 237 97 1) (71 0 484 69 0) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (84 0 118 45 2) (43 242 242 97 1))) ((g) ((43 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (71 0 479 69 0) (80 0 260 27 2) (38 242 237 97 1) (78 469 15 27 2))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (78 0 160 27 2) (42 242 242 97 1) (70 310 140 17 2) (75 310 130 14 2) (65 320 105 18 2))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (37 242 237 97 1) (75 276 203 25 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (78 262 150 50 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (77 0 277 25 2) (30 242 116 97 1) (66 248 140 24 2) (56 253 115 23 2) (73 263 154 29 2) (61 263 105 25 2) (42 363 116 97 1))) ((f m7) ((29 0 479 97 1) (60 0 484 69 0) (56 0 484 69 0) (55 0 484 69 0) (51 0 484 69 0) (75 239 215 31 2) (68 244 153 18 2) (58 254 178 20 2) (63 258 136 21 2))) ((f m7) ((41 0 237 97 1) (55 0 479 69 0) (56 0 479 69 0) (60 0 479 69 0) (41 0 237 97 1) (29 242 116 97 1) (70 250 170 28 2) (41 363 116 97 1))) ((bb 7) ((34 0 237 97 1) (67 0 479 69 0) (62 0 484 69 0) (60 0 484 69 0) (56 0 484 69 0) (41 242 237 97 1) (73 251 140 61 2))) ((bb 7) ((46 0 237 97 1) (60 0 479 69 0) (62 0 479 69 0) (46 0 237 97 1) (72 1 241 21 2) (68 121 245 69 0) (68 257 220 28 2) (72 363 116 69 0))) ((b m7) ((35 0 237 97 1) (66 0 479 69 0) (62 0 484 69 0) (61 0 484 69 0) (57 0 479 69 0) (51 237 186 18 2) (47 242 237 97 1) (70 243 241 26 2) (56 248 150 20 2) (61 258 119 18 2))) ((e 7) ((56 0 479 69 0) (62 0 479 69 0) (34 0 237 97 1) (73 0 479 69 0) (56 0 479 69 0) (70 0 218 26 2) (63 241 173 28 2) (46 242 237 97 1))) ((e 7) ((40 0 479 97 1) (70 0 479 69 0) (66 0 479 69 0) (62 0 479 69 0) (56 0 479 69 0) (63 198 77 27 2) (66 430 54 42 2))) ((eb 7) ((39 0 479 97 1) (69 0 479 69 0) (65 0 479 69 0) (61 0 479 69 0) (55 0 479 69 0) (66 0 151 42 2) (66 421 63 22 2))) ((eb 7) ((66 0 147 22 2) (66 237 95 27 2) (51 237 60 30 2) (61 252 55 20 2) (67 367 117 23 2))) ((g# m7) ((32 0 237 97 1) (63 0 479 69 0) (59 0 479 69 0) (58 0 479 69 0) (54 0 479 69 0) (67 0 103 23 2) (68 218 235 43 2) (44 242 237 97 1))) ((g) ((31 0 237 97 1) (71 0 484 69 0) (66 0 484 69 0) (62 0 484 69 0) (59 0 484 69 0) (55 0 484 69 0) (68 119 135 34 2) (43 242 242 97 1) (68 409 75 47 2))) ((g) ((43 0 237 97 1) (55 0 479 69 0) (59 0 479 69 0) (62 0 479 69 0) (66 0 479 69 0) (71 0 479 69 0) (68 0 40 47 2) (69 50 70 48 2) (38 242 237 97 1) (68 263 221 43 2))) ((f#) ((30 0 237 97 1) (70 0 484 69 0) (65 0 484 69 0) (61 0 484 69 0) (58 0 484 69 0) (54 0 484 69 0) (68 0 484 43 2) (42 242 242 97 1))) ((f#) ((42 0 237 97 1) (54 0 479 69 0) (58 0 479 69 0) (61 0 479 69 0) (65 0 479 69 0) (70 0 479 69 0) (68 0 484 43 2) (37 242 237 97 1) (63 427 57 22 2))) ((f# m7) ((30 0 479 97 1) (61 0 484 69 0) (57 0 484 69 0) (56 0 484 69 0) (52 0 484 69 0) (63 0 63 22 2) (68 0 443 43 2) (66 433 51 28 2))) ((f# m7) ((42 0 237 97 1) (56 0 479 69 0) (57 0 479 69 0) (61 0 479 69 0) (42 0 237 97 1) (66 0 274 28 2) (30 242 116 97 1) (63 274 95 21 2) (42 363 116 97 1))) ((f# 7) ((30 0 237 97 1) (63 0 479 69 0) (58 0 479 69 0) (56 0 479 69 0) (52 0 479 69 0) (42 242 237 97 1) (51 245 80 22 2) (56 255 60 19 2) (61 255 84 16 2))))

)

(setf labels (mapcar 'car beat-list))
(setf beats (make-beat-list beat-list))
(setf charlie (NewImprovizer))
(loop for i from 0 to (1- (length beats)) do (learn-event charlie (nth i beats)))
(setf impro (ImprovizeOnHarmGrid charlie 50 labels))


(SetMaxCont charlie 2)
(play (beats->chseq impro 484 0))

(beats-check-sustain impro 484 5)

(inspect impro)
(inspect charlie)


setf impro (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)))
(setf impro (ImprovizeOnHarmGrid charlie 150 labels))
(inspect impro)
(beats-check-sustain impro 484 5)


(play (beats->chseq impro 484 0))
(save-improvizer-as charlie)
(setf charlie (load-improvizer-from))


(play ( beats->chseq  (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)) 484 0))
(play ( beats->chseq  (ImprovizeOnHarmGrid charlie 100 labels) 484 0))

(flink charlie 2)

(setf impro (ImprovizeByContinuation charlie 50 (improvize charlie 5 :start-etat 3)))
(setf impro (ImprovizeOnHarmGrid charlie 10 labels))
(inspect impro)
(beats-check-sustain impro 484 5)


(play (beats->chseq impro 484 0))
(save-improvizer-as charlie)
(setf charlie (load-improvizer-from))

(mapcar 'length (beats->5list  impro 484 0))

(setf l '(1 2 3 4))
(loop for i in l for j in l collect (list i j))

|#