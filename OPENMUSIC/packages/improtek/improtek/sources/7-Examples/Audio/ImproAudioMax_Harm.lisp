(in-package :om)

#|
MyRentPartyLong_tune
MyRentPartyLong-MEM_tune
MyOleo_tune
|#


(progn
  (setf 
;**************************************************************************
;**************************************************************************
   scenario MyOleo_tune
        memory (list
                MyOleo_tune
                ))
;**************************************************************************
;**************************************************************************
  (setf *current-tune* (clone scenario))
  (setf *Current-Memory* (clone (car memory)))
  (setf memory_tune (Clone-Object *Current-Memory*)))




(let (
;**************************************************************************
;**************************************************************************
      (VOICES '(1 2 3)) ;liste des voix qui seront calculées
;------------------------------------------------------------
; Pour les param. suivant: liste (param pour voix 1, 2 3)
;------------------------------------------------------------      
      (NUMGRILLES '(1 1 1))
      (modeRT '(nil nil nil))
      (newtabousperf '(nil nil nil))
      (nexteventifnosolution '(nil nil nil))

      (transpos (list 
                 '(1 2 3 0 -1 -2) 
                 '(1 2 3 0 -1 -2)  
                 '(1 2 3 0 -1 -2) ))

      (besttranspomode '(t t t))
           
      (maxcont '(1000 4 4))

      ;(taboustoadd '(nil nil nil))
      (taboustoadd 
       (list
        '(33 1000)
        '(0 32)
        '(0 32)
        ))
;**************************************************************************
;**************************************************************************

      )

(loop for NUMVOICE in VOICES do

(progn

;3-Instantiate an improviser
;--> Define Numvoice (player in Max patch, "Voices") and evaluate
(progn
  (setf 
        beatdur (beatduration *current-tune*)
        Imp (NewSymbolicRealtimeImprovizer_AudioHarmbeats memory_tune beatdur (nth (1- NUMVOICE) NUMGRILLES) ))
  (setf scenario (MakeLabelsFromList (expand_grid (grid *current-tune*)) 'harmlabel)
        beatduration (clone beatdur)
        Improvizer (clone Imp))
  (osc-send-list-as-antescofo-map scenario 0 "127.0.0.1" 7657 "/scenario" 0 t)
  (sleep 1))
;(FormatLabellist (scenariofromimprovizer improvizer))



;3-Parametrize the improvize
;--> Define parameters and evaluate
;*************************************
;"yes": t, "no": nil


(setf 
 (AuthorizedTranspos Improvizer) (nth (1- NUMVOICE) transpos)
 ;(AuthorizedTranspos Improvizer)  '(1 2 3 4 5 -1 -2 -3 -4 -5 0) ;Semitones
 (max-continuity Improvizer) (nth (1- NUMVOICE) maxcont)
 (bestTranspoMode Improvizer) (nth (1- NUMVOICE) besttranspomode)
 (FirstWithoutTranspoMode Improvizer) nil
 (randomPrefixOccurrenceMode Improvizer) t
 (LengthFactorsFromScen Improvizer) (list 1 (nth (1- NUMVOICE) maxcont))
 
 (modeRT Improvizer) (nth (1- NUMVOICE) modeRT) ;;;;

 
 (newtabousperf Improvizer) (nth (1- NUMVOICE) newtabousperf) ;;;;;;;;; /!\ if this option is 't' some slice may be empty!
 (nexteventifnosolution Improvizer) (nth (1- NUMVOICE) nexteventifnosolution) ; /!\ if this option is 't' some slice will not match the scenario !
 )

(add-tabous Improvizer (nth (1- NUMVOICE) taboustoadd))





;*************************************

;4-Generate the improvisation
;--> Evaluate
(progn 
  (format *om-stream* "~%=================Generating voice number ~a =================~%" NUMVOICE)
  (setf current-scenario-suffix scenario
        idx-beginning-next-phase 0
        impro-fragment nil)
  
  (loop while (and current-scenario-suffix (< idx-beginning-next-phase (- (list-length scenario) 1))) do 
        ;Generate one fragment
        (format *om-stream* "------~%Idx ~a / ~a~%" idx-beginning-next-phase (list-length scenario))
        (setf (start-region Improvizer) (list 1 (maxetat Improvizer)))
        (setf impro-fragment 
              (Improvize_OnePhase 
               Improvizer (list-length current-scenario-suffix) current-scenario-suffix idx-beginning-next-phase))
        ;Format and send this fragment to the Max patch
        (if impro-fragment
            (progn
              (format *om-stream* "[~a,~a] generated and sent to Max !~%" 
                      idx-beginning-next-phase (- (+ idx-beginning-next-phase (list-length impro-fragment)) 1))
              (osc-send-sequence-fragment impro-fragment idx-beginning-next-phase "127.0.0.1" 7657 "/modify" NUMVOICE)
              (setf idx-beginning-next-phase (+ idx-beginning-next-phase (list-length impro-fragment))))
          (progn 
            (incf idx-beginning-next-phase)
            (format *om-stream* "No impro fragment generated~%")))
        ;Prepare next generation phase
        (setf current-scenario-suffix (nthcdr idx-beginning-next-phase scenario)))
  (format *om-stream* "~%=================Voice number ~a generated !=================~%" NUMVOICE))

)
))

      