(in-package :om)


;------------------------
;Run the program: 
;1- Steps 1 to 4.1 in the Max patch
;2- Evaluate the whole file : Cmd+Y
;3- Step 4.2 in the Max patch when the generation is ended


;(Evaluate step by step: select lines and Cmd+E)
;-----------------------


#|
;CHANGE THE PROPERTIES OF THE ALPHABET (evaluate the chosen version)
;===================================================================

;Original
(defmethod equalLabel ((d1 AudioDescr) (d2 AudioDescr))
  (and 
   (equal (/ (IdxClusterDesc1 d1) 2) (/ (IdxClusterDesc1 d2) 2))
   (equal (IdxClusterDesc2 d1) (IdxClusterDesc2 d2))
   ))

;OnlyDim1
(defmethod equalLabel ((d1 AudioDescr) (d2 AudioDescr))
  (and 
   (equal (/ (IdxClusterDesc1 d1) 2) (/ (IdxClusterDesc1 d2) 2))
   ;(equal (IdxClusterDesc2 d1) (IdxClusterDesc2 d2))
   ))

;OnlyDim2
(defmethod equalLabel ((d1 AudioDescr) (d2 AudioDescr))
  (and 
   ;(equal (IdxClusterDesc1 d1) (IdxClusterDesc1 d2))
   (equal (IdxClusterDesc2 d1) (IdxClusterDesc2 d2))
   )
)

|#

#|
Some scenarios defined in 6-Data/Scenarios/AudioDescr: 
------------------------------------------------------------------
Gurtu_ED10-SC4
MyRentPartyLong_tune

|#



;1-Define scenario and memory (both must be defined in the file 6-Data)
;--> Replace in the following lines and evaluate
(progn
  ;*************************************
  (setf scenario Gurtu_ED10-SC4_tune    
        memory Gurtu_ED10-SC4_tune)
  ;*************************************
  (setf *current-tune* (clone scenario))
  (setf *Current-Memory* (clone memory))
  (setf memory_tune (Clone-Object *Current-Memory*)))


;2-Define scenario and memory in the Max patch
;--> Max patch : online or offline mode ("---1---")


;3-Instantiate an improviser
;--> Define Numvoice (player in Max patch, "Voices") and evaluate
;(save-list impro-tot "/Users/jnika/Google\ Drive/Dev/ImproteK/GURTUDEOUF.lisp") 
(progn
  ;*************************************
  (setf NUMVOICE 1
  ;*************************************
        beatdur (beatduration *current-tune*)
        Imp (NewSymbolicRealtimeImprovizer_AudioDescrBeats memory_tune beatdur 1))
  (setf scenario (MakeLabelsFromList (expand_grid (grid *current-tune*)) 'AudioDescr)
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
 (AuthorizedTranspos Improvizer)  '(0)
 (max-continuity Improvizer) 100
 (bestTranspoMode Improvizer) nil
 (FirstWithoutTranspoMode Improvizer) nil
 (randomPrefixOccurrenceMode Improvizer) t
 (LengthFactorsFromScen Improvizer) '(1 100)
 ;
 (tabou-mode Improvizer) nil ; /!\ if this option is 't' some slice may be empty!
 (nexteventifnosolution Improvizer) nil ; /!\ if this option is 't' some slice will not match the scenario !
 
)
;*************************************

;4-Generate the improvisation
;--> Evaluate
(progn 
  (format *om-stream* "~%=================Generating voice number ~a =================~%" NUMVOICE)
  (setf current-scenario-suffix scenario
        idx-beginning-next-phase 0
        impro-fragment nil
        impro-tot nil)
  
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
              (setf idx-beginning-next-phase (+ idx-beginning-next-phase (list-length impro-fragment)))
              (setf impro-tot (append impro-tot impro-fragment))
              )
          (progn 
            (incf idx-beginning-next-phase)
            (format *om-stream* "No impro fragment generated~%")))
        ;Prepare next generation phase
        (setf current-scenario-suffix (nthcdr idx-beginning-next-phase scenario)))
  (format *om-stream* "~%=================Voice number ~a generated !=================~%" NUMVOICE))



      
#|
Debug

(om-inspect (otext Improvizer 10)))
(TransposeClonedEvent (otext Improvizer 10) 2)

(om-inspect (otext Improvizer 34))
(setf clone (clone-object (otext Improvizer 34)))
(setf (IdxInBuffer (data clone)) 1000)

(TransposeLabel (label clone) 2)
(TransposeData (data clone) 2)
(om-inspect clone)
(om-inspect (otext Improvizer 14))

(om-inspect (data (otext Improvizer 56)))
(setf clone (clone-object (data (otext Improvizer 56))))
(om-inspect clone)

(om-inspect (otext Improvizer 15))
(om-inspect (transposeclonedevent (otext Improvizer 15) 2))

(IdxInBuffer (data (otext Improvizer 15)))
(IdxInBuffer (data (transposeclonedevent (otext Improvizer 15) 2)))

|#
      