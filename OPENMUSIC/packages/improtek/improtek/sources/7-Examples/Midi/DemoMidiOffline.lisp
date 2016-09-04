;========================================================================
;                   "ImproteK" Demo, European Lisp Simposium
;========================================================================
(in-package :om)



; SCENARIO : CHORD PROGRESSION OF "AUTUMN LEAVES"
;------------------------------------------------
(setf tune AutumnleavesDoMin_tune)

(setf Autumnleaves_part1 '((c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4)
                           (c m7 4) (f 7 4) (bb maj7 4) (eb maj7 4) (a m7 4) (d 7 4) (g m7 4) (g m7 4))
      Autumnleaves_part2 '((a m7 4) (d 7 4) (g m7 4) (g m7 4) (c m7 4) (f 7 4) (bb maj7 4) (bb maj7 4)
                           (a m7 4) (d 7 4) (g m7 2) (gb 7 2) (f m7 2) (e 7 2) (eb maj7 4) (d 7 4) (g m7 4) (g m7 4)) 
      Autumnleaves2_tune (make-instance 'tune :grid Autumnleaves_part2 :beatduration 330 :tunename "Autumnleaves2"))


(setf scenario_part1 (MakeLabelsFromList (expand_grid Autumnleaves_part1) 'harmlabel))
(setf scenario_part2 (MakeLabelsFromList (expand_grid Autumnleaves_part2) 'harmlabel))
;(om-inspect scenario_part1)
;(om-inspect scenario_part2)



; MEMORY 1 : "AUTUMN LEAVES", played by Bernard Lubat
;----------------------------------------------------
(setf path_dir_saved_improvizers (append (pathname-directory *load-pathname*) (list "DataDemoMidiOffline"))
      name_saved_improvizer_solo "AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
      name_saved_improvizer_accomp "AutumnleavesDoMin-walkingChan3-2014.2.27-22h54.or")

(setf memory_solo 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_solo))
      memory_accomp 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_accomp)))
;(om-inspect memory_solo)


; GENERATE WHOLE IMPRO WITH IMPROVIZER
;----------------------------------------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario_part1) scenario_part1)
      impro_accomp 
      (ImprovizeOnHarmGrid memory_accomp (length scenario_part1) scenario_part1))
;(om-inspect impro_solo)
 (setf impro_mix (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
(play impro_mix)

#|
; Just one query beginning at idx-beginning-next-phase with real-time improvizer
(let* (
        (idx-beginning-next-phase 5)
        (current-scenario-suffix (nthcdr idx-beginning-next-phase scenario_part1))             
        (impro-fragment nil)
        (memory_solo (load-realtimeImprovizer-fromSavedOldImprovizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_solo)))

        )
      (setf impro-fragment 
            (Improvize_OnePhase 
             memory_solo (list-length current-scenario-suffix) current-scenario-suffix idx-beginning-next-phase))
      (setf impro_mix (chseq-mix-melo-accomp impro-fragment impro-fragment (beatduration tune) 4 4))

      (play impro_mix))
|#



; MEMORY 2 : "BALAIO", Hermeto Pascoal, played by Jovino Santos Neto
;-------------------------------------------------------------------
(setf name_saved_improvizer_solo "JSantosNeto-Balaio.or"
      memory_solo 
      (load-old-improvizer (make-pathname :directory path_dir_saved_improvizers :name name_saved_improvizer_solo)))

; GENERATE
;----------
(setf impro_solo 
      (ImprovizeOnHarmGrid memory_solo (length scenario_part2) scenario_part2)
      impro_accomp 
      (PlayVoicings (voicings AutumnLeaves2_tune) 
                    (substitution AutumnLeaves2_tune) 
                    (beatduration tune))
      impro_mix
      (chseq-mix-melo-accomp impro_solo impro_accomp (beatduration tune) 4 4))

; PLAY
;----------
;lire
(play impro_mix)

