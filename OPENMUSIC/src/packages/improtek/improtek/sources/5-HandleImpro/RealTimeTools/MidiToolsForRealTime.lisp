;MidiToolsForRealTime.lisp
;J.Nika

(in-package :om)

; Adapté de midi-to-beatlist (beatlist.lisp)
(defun midievtsList-to-beatlist ( l )
  (let ((beats (midievtsList-to-beats l)))
    ; JEROME 03/09/12 : DELETE OU NON LES FIRST BEATS VIDES ????
    ;(when beats (list (delete_first_empty_beats (make-beat-list (first beats) (second beats))) 
    (when beats (list (make-beat-list (first beats) (second beats))
                      (second beats)))))   ;Marc 5/3/2012 'make-beat-list' WITH beatdur

; Adapté de midi-to-beats (beatlist.lisp)
(defun midievtsList-to-beats (l)
  (let ((midifromevtsList (evts-from-midievtsList l)))
    (cond ((null midifromevtsList) (format *om-stream* "Empty MIDI events list~%"))  
                                                                     
          ((and (not (member 16 midifromevtsList :key 'fifth)) 
                (not (member 14 midifromevtsList :key 'fifth)))     ;22/4/2012   TEST CHANNEL 16 (and 14 for older MIDI files)
           (format *om-stream* "No chord data on channel 16~%"))
          ; 05/08/2013 !!! 
          ; POUR CORRIGER BUG DE LA DIVISION PAR 0 DANS OM-MEAN
          (t (let ((time-intervals (x->dx (mapcar 'first (car (check-clocks midifromevtsList))))))
               (if time-intervals 
                   (let ((defaultbeatdur (round (om-mean time-intervals)))
                         (beats_w_labels  (clocked-evts->beats midifromevtsList)))
                     (list beats_w_labels defaultbeatdur))
                 (format *om-stream* "Impossible to compute \"x->dx\" for time-intervals~%")))))))
#|
;Version originale issue de midi-to-beats (beatlist.lisp) posant problème dans le cas de listes avec uniquements info canal 16.
;-------------------------------------------------------------------------------------------------------------------------------
(t (let ((defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromevtsList)))))))
                   (beats_w_labels  (clocked-evts->beats midifromevtsList)))
               (list beats_w_labels defaultbeatdur))))))
|#


; Adapté de evts-from-midi-file (beatlist.lisp)
(defun evts-from-midievtsList (l)
  (sort l '< :key 'second))
;(sort (apply 'append l) '< :key 'second))




;*********APPRENTISSAGE TR***********
; Calls functions in MidiTR.lisp (midievtsList-to-eventlist)
; Or in AudioEvent.lisp
;************************************
 
;-------------------------------------------------------------
; !!!!!!!!! VERSION MIDI !!!!!!!!!
;-------------------------------------------------------------
(defmethod learn-realtime ((self RealtimeImprovizer) (midievts-list list))
  (let* ((res (midievtsList-to-eventlist midievts-list))
         (eventlist (first res))
         (defaulteventdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaulteventdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
    ;(format *om-stream* "~%~%ENTER LEARNING PHASE !!!~%")
    (if eventlist 
        (progn
          ;;(format *om-stream* "... indeed I learn !!!~%")
          (setf (RefTempo self) defaulteventdur)
                  ;------> TEST (cf defmethod learn-event-list)
          (learn-event-list self eventlist)
          (format *om-stream* "~%~%++++++ Learn eventlist of length : ~D -----> New maxetat : ~D~%" (list-length eventlist) (maxetat self))))
    ;(format *om-stream* "LEAVE LEARNING PHASE !!!~%~%~%")
    ))

#|
;-------------------------------------------------------------
; !!!!!!!!! EQUIVALENT AUDIO DEFINI DANS AUDIOEVENT.LISP !!!!!!!!!
;-------------------------------------------------------------
(defmethod learn-realtime ((self RealtimeImprovizer) (evtsfrombuffer-list list))
  (let* ((res (evtsfrombuffer-list-to-audioeventlist evtsfrombuffer-list))
         (audioeventlist (first res))
         (defaulteventdur (second res)))
      ; /!\/!\/!\/!\ TODO : Pour l'instant OK car toujours le même defaulteventdur, mais ensuite ???
      ; quand differents tempi donnés par Antescofo ????? 
      ; LE PROBLEME SE POSAIT DEJA AVANT NON ?????????
      ;-----------------------------------------------
    ;(format *om-stream* "~%~%ENTER LEARNING PHASE !!!~%")
    (if audioeventlist 
        (progn
          ;;(format *om-stream* "... indeed I learn !!!~%")
          (setf (RefTempo self) defaulteventdur)
                  ;------> TEST (cf defmethod learn-event-list)
          (learn-event-list self audioeventlist)
          (format *om-stream* "~%~%++++++ Learn audioeventlist of length : ~D -----> New maxetat : ~D~%" (list-length audioeventlist) (maxetat self))))
    ;(format *om-stream* "LEAVE LEARNING PHASE !!!~%~%~%")
    ))
|#






#|
(setf evtsList '( (12 0 10 100 16) (1 50 279 100 16) (12 379 10 100 16) (1 429 279 100 16) (12 758 10 100 16) (1 808 279 100 16) (12 1137 10 100 16) (75 1311 137 6 1) (3 1187 280 100 16) (12 1516 10 100 16) (72 1553 90 5 1) (3 1566 279 100 16) (76 1487 372 26 1) (12 1895 10 100 16) (63 1746 246 7 1) (70 1998 152 47 1) (3 1945 279 100 16) (12 2274 10 100 16) (75 2271 306 43 1) (3 2324 279 100 16) (70 2304 340 45 1) (12 2653 10 100 16) (67 2345 327 28 1) (63 2591 347 52 1) (3 2703 279 100 16) (70 2905 134 58 1) (12 3032 10 100 16) (67 2930 114 47 1) (75 2880 187 51 1) (67 3231 53 30 1) (3 3082 279 100 16) (12 3411 10 100 16) (75 3183 271 46 1) (70 3223 331 48 1) (1 3461 279 100 16) (12 3790 10 100 16) (61 3562 322 47 1) (65 3892 65 35 1) (73 3866 97 6 1) (68 3875 133 52 1) (1 3840 279 100 16) (12 4169 10 100 16) (73 4153 328 27 1) (65 4210 287 42 1) (1 4219 279 100 16) (68 4183 341 50 1) (12 4548 10 100 16) (61 4466 375 57 1) (1 4598 279 100 16) (12 4927 10 100 16) (73 4803 151 45 1) (68 4847 139 43 1) (65 4855 167 53 1) (1 4977 279 100 16) (12 5306 10 100 16) (65 5154 265 35 1) (68 5162 326 56 1) (73 5131 369 38 1) (1 5356 279 100 16) (12 5685 10 100 16) (63 5431 382 47 1) (70 5795 114 55 1) (67 5804 119 52 1) (75 5772 171 31 1) (3 5735 279 100 16) (12 6064 10 100 16) (3 6114 279 100 16) (75 6085 343 35 1) (12 6443 10 100 16) (67 6129 333 51 1) (70 6107 364 61 1) (3 6493 279 100 16) (12 6822 10 100 16) (70 6735 119 55 1) (75 6697 179 51 1) (67 6743 173 57 1) (63 6420 585 62 1) (67 7015 37 14 1) (3 6872 279 100 16) (12 7201 10 100 16) (75 6980 250 33 1) (70 7023 330 53 1) (3 7251 279 100 16) (12 7580 10 100 16) (61 7340 316 58 1) (73 7633 120 24 1) (68 7664 130 42 1) (65 7690 123 22 1) (3 7630 279 100 16) (12 7959 10 100 16) (73 7951 303 23 1) (68 7997 288 33 1) (1 8009 279 100 16) (65 8013 278 23 1) (12 8338 10 100 16) (1 8388 279 100 16) (12 8717 10 100 16) (73 8603 127 47 1) (61 8269 613 27 1) (65 8696 210 8 1) (1 8767 279 100 16) (12 9096 10 100 16) (68 8682 468 14 1) (73 8875 447 31 1) (1 9146 279 100 16) (12 9475 10 100 16) (63 9201 347 38 1) (67 9587 53 23 1) (75 9529 117 24 1) (70 9540 122 43 1) (1 9525 279 100 16) (12 9854 10 100 16) (75 9804 308 27 1) (67 9934 187 6 1) (70 9826 336 55 1) (1 9904 279 100 16) (12 10233 10 100 16) (87 9209 1298 43 1) (3 10283 279 100 16) (12 10612 10 100 16) (63 10127 639 51 1) (67 10444 338 43 1) (85 10435 396 52 1) (75 10393 527 40 1) (3 10662 279 100 16) (12 10991 10 100 16) (70 10731 359 55 1) (87 10739 392 40 1) (3 11041 279 100 16) (12 11370 10 100 16) (61 11096 330 53 1) (73 11410 78 13 1) (65 11418 106 28 1) (68 11436 97 41 1) (3 11420 279 100 16) (12 11749 10 100 16) (89 11104 742 47 1) (73 11682 287 25 1) (65 11726 327 48 1) (68 11716 354 42 1) (3 11799 279 100 16) (12 12128 10 100 16) (3 12178 279 100 16) (73 12320 141 27 1) (12 12507 10 100 16) (61 12045 590 48 1) (65 12380 276 30 1) (87 11707 1089 51 1) (1 12557 279 100 16) (12 12886 10 100 16) (73 12642 331 35 1) (68 12365 621 35 1) (85 12696 367 53 1) (1 12936 279 100 16) (12 13265 10 100 16) (63 13004 316 50 1) (75 13296 134 23 1) (70 13312 126 48 1) (1 13315 279 100 16) (12 13644 10 100 16) (75 13603 237 23 1) (70 13629 303 46 1) (67 13667 287 35 1) (1 13694 279 100 16) (82 13016 1017 30 1) (12 14023 10 100 16) (70 14240 48 35 1) (1 14073 279 100 16) (75 14200 162 56 1) (12 14402 10 100 16) (63 13908 600 37 1) (67 14250 280 36 1) (82 14233 362 40 1) (75 14437 249 43 1) (1 14452 279 100 16) (12 14781 10 100 16) (70 14522 323 47 1) (87 14545 349 55 1) (3 14831 279 100 16) (12 15160 10 100 16) (61 14836 338 53 1) (65 15143 126 46 1) (73 15152 123 24 1) (68 15161 143 55 1) (3 15210 279 100 16) (12 15539 10 100 16) (85 14851 779 62 1) (73 15450 411 46 1) (3 15589 279 100 16) (65 15498 404 52 1) (68 15471 452 55 1) (12 15918 10 100 16) (61 15837 310 64 1) (3 15968 279 100 16) (12 16297 10 100 16) (3 16347 279 100 16) (12 16676 10 100 16) (3 16726 279 100 16) (12 17055 10 100 16) (1 17105 279 100 16) (12 17434 10 100 16) (1 17484 279 100 16) (12 17813 10 100 16) ) )


(setf evtsList '( (1 12557 279 100 16) (12 12886 10 100 16)
                  (73 12642 331 35 1) (68 12365 621 35 1) (85 12696 367 53 1) 
                  (1 12936 279 100 16) (12 13265 10 100 16) 
                  (63 13004 316 50 1) (75 13296 134 23 1) (70 13312 126 48 1) 
                  (1 13315 279 100 16) (12 13644 10 100 16) 
                  (75 13603 237 23 1) (70 13629 303 46 1) (67 13667 287 35 1) 
                  (1 13694 279 100 16) 
                  (82 13016 1017 30 1) 
                  (12 14023 10 100 16) 
                  (70 14240 48 35 1) 
                  (1 14073 279 100 16) 
                  (75 14200 162 56 1) 
                  (12 14402 10 100 16) 
                  (63 13908 600 37 1) (67 14250 280 36 1) (82 14233 362 40 1) (75 14437 249 43 1) 
                  (1 14452 279 100 16) (12 14781 10 100 16) 
                  (70 14522 323 47 1) (87 14545 349 55 1)
                  (3 14831 279 100 16) (12 15160 10 100 16) 
                  (61 14836 338 53 1) (65 15143 126 46 1) (73 15152 123 24 1) (68 15161 143 55 1) 
                  (3 15210 279 100 16) (12 15539 10 100 16) 
                  (85 14851 779 62 1) (73 15450 411 46 1) 
                  (3 15589 279 100 16) 
                  (65 15498 404 52 1) (68 15471 452 55 1) 
                  (12 15918 10 100 16) 
                  (61 15837 310 64 1) 
                  (3 15968 279 100 16) (12 16297 10 100 16) ) )

(setf sorted-evtsList (evts-from-midievtsList evtsList))
(om-inspect  sorted-evtsList)

(setf beatlist-from-evtsList (midievtsList-to-beatlist sorted-evtsList))
(om-inspect beatlist-from-evtsList)


(setf impro (beats->chseq (nth 0 beatlist-from-evtsList) (nth 1 beatlist-from-evtsList) 0))
(pgmout 4 1)
(pgmout 5 2)
(play impro)
|#


#|
;Test pour liste ne contenant que des infos sur le canal 16 (il ne devrait pas y avoir de bug !!!)
;-------------------------------------------------------------------------------------------------
(setf l '( (12 0 10 100 16) (1 50 279 100 16) (12 379 10 100 16) (1 429 279 100 16) (12 758 10 100 16) (1 808 279 100 16) (12 1137 10 100 16)))
;Plantage avec interface :
;-------------------------
(setf l'((12 0 108 100 16))) ; OK
;Plantage 2 :
;--------------
(setf l '((12 0 108 100 16) (0 93 101 101 16)))

(launch-realtime-on-oraclechan *current-tune* 8 host_server 3008)

(setf *last-received-messageforlearning* l)

(kill-realtime-on-oraclechan *current-tune* 8)
|#