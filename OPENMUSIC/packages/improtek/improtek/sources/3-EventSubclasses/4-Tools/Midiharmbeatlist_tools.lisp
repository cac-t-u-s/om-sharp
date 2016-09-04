;Beatlist.lisp
;by Marc Chemillier (2004, revised version 2012)
; LA PLUPART A METTRE DANS UN FICHIER MIDITOOLS ????

;GENERICITE : A REPRENDRE DEPUIS LA DEFINITION DE LA CLASSE HARMLABEL !!

(in-package :om)


(defmacro nthcar (n l) `(loop for x in ,l for i from ,n downto 1 collect x))

;Open a MIDI file and gives a pair: (MidiHarmBeats_with_labels defaultbeatdur) 
(defun midi-to-MidiHarmBeats (&optional path_file)
  (let ((midifromfile (evts-from-midifile path_file)))
    (cond ((null midifromfile) (format *om-stream* "Empty MIDI file~%"))  ;nil when midi buffer is empty                                                            
          ((and (not (member 16 midifromfile :key 'fifth)) 
                (not (member 14 midifromfile :key 'fifth)))     ;Test channel 16 (and 14 for older midi files)
           (format *om-stream* "No chord data on channel 16~%"))
          (t (let ((defaultbeatdur (round (om-mean (x->dx (mapcar 'first (car (check-clocks midifromfile)))))))
                   (MidiHarmBeats_w_labels  (clocked-evts->MidiHarmBeats midifromfile))) ; enharmonies
               (list MidiHarmBeats_w_labels defaultbeatdur))))))

;Deletes the first MidiHarmBeats in a list if they have an empty content
(defun delete_first_empty_MidiHarmBeats (MidiHarmBeatlist)
  (loop while (and ; if the whole list has an empty content
               (car MidiHarmBeatlist) 
               (not (MidiSet (car MidiHarmBeatlist)))) 
        do (setf MidiHarmBeatlist (cdr MidiHarmBeatlist))) 
  MidiHarmBeatlist
)

(defun midi-to-MidiHarmBeatlist (&optional path_file)
  (let ((MidiHarmBeats (midi-to-MidiHarmBeats path_file)))
    (when MidiHarmBeats (list (delete_first_empty_MidiHarmBeats (make-MidiHarmBeat-list (first MidiHarmBeats) (second MidiHarmBeats))) 
                      (second MidiHarmBeats))))) 



(defun add-MidiHarmBeat-list (MidiHarmBeatlist refbeatdur MidiHarmBeatlist1 beatdur1)
  (let ((adjustedMidiHarmBeatlist1 (loop for MidiHarmBeat in MidiHarmBeatlist1 for newMidiHarmBeat = (clone MidiHarmBeat)
                                 do (setf (MidiSet newMidiHarmBeat) (timestretch (MidiSet newMidiHarmBeat) (/ refbeatdur MidiHarmBeatdur1))
                                          (duration newMidiHarmBeat) refbeatdur)
                                 collect newMidiHarmBeat)))
    (append MidiHarmBeatlist adjustedMidiHarmBeatlist1)))

(defun add-list-of-MidiHarmBeat-list (list-of-MidiHarmBeatlist)    ; Marc 23/11/2012
  (loop for x in (cdr list-of-MidiHarmBeatlist) with res = (car list-of-MidiHarmBeatlist)
        do (setf (car res) (add-MidiHarmBeat-list (car res) (cadr res) (car x) (cadr x)))
        finally return res))






#|
Chord labels and clocks are coded in MIDI files as notes on channel 16:
   - clocks = note 12 (duration 10 ms, vel 100)
   - root chord = note between 0 and 11
   - quality chord = velocity 100=maj7, 101=m7, 102=7, 103=m7b5, dim=104
|#
(defun make-label-from-midi (midi5up) 
  (list (nth (mod (MEPitch midi5up) 12) '(c c# d eb e f f# g g# a bb b))
        (nth (cond ((= (MEChannel midi5up) 16) (mod (MEVel midi5up) 100))
                   ((= (MEChannel midi5up) 14) (1- (MEVel midi5up))))
             '(maj7 m7 7 m7b5 dim))))

(defun make-clock-from-midi (midi5up) (list (MEOnset midi5up) 248))
(defun normalize-tempoevent-from-midi (midi5up) (list 12 (MEOnset midi5up) 10 100 16))     ;clock dur=10 vel=100

(defun grid-event-from-midi? (midi5up) (or (= (MEChannel midi5up) 16) (= (MEChannel midi5up) 14)))
(defun clock-event-from-midi? (midi5up) (and (or (= (MEChannel midi5up) 16) (= (MEChannel midi5up) 14)) (= (MEPitch midi5up) 12)))
(defun label-event-from-midi? (midi5up) (and (or (= (MEChannel midi5up) 16) (= (MEChannel midi5up) 14)) (<= (MEPitch midi5up) 11)))

;GENERICITE : ICI OU DANS MidiHarmBeat.lisp
;Midi channel 16 encodes the chord chart (do not use it for musical inputs).
(defmethod make-grid-event-for-MidiHarmBeat ((MidiHarmBeat MidiHarmBeat) beatduration)
  (list (list 12 0 10 100 16)  ; clock dur=10 vel=100
        (list (MidiRoot (first (HarmLabel MidiHarmBeat))) 50 (- beatduration 100) (case (chordtype (label MidiHarmBeat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 16)))

(defun clocked-evts->MidiHarmBeats (midi5up)      
  (let (tmp MidiHarmBeats)
    (when (listp midi5up)
      (setf tmp (check-clocks midi5up) clocks (first tmp) 5uples (second tmp))
      (setf MidiHarmBeats (quintuples->MidiHarmBeats clocks 5uples))
      (setf MidiHarmBeats (cut-MidiHarmBeat-events clocks MidiHarmBeats))
      (setf MidiHarmBeats (set-relative-time-MidiHarmBeat clocks MidiHarmBeats))
      (setf MidiHarmBeats (label-chord-MidiHarmBeat MidiHarmBeats)))))
 
(defun evts-from-midifile (&optional absolute-path-filename)      
  (catch-cancel
    (let ((name (or absolute-path-filename (om-choose-file-dialog))))
      (when name ; mf-info gives a list of tracks, each track is a list of notes as 5uplets.  
                 ; WARNING: in live data, the last tempo clock (midi code=12) should be repeated at least twice
                 ; because there may be some NoteOn before which do not correspond to NoteOff
        (sort (apply 'append (mf-info (load-midi-file name))) '< :key 'second)))))


(defun check-midifile-evts (evts)
   (setf evts (loop with res = nil            ; remove multiple occurrences of evts
                    for x in evts
                    when (not (member x res :test 'equal)) do (setf res (cons x res))
                    finally return (reverse res)))
   (setf evts (loop with res = nil            ; remove too close clocks, or too long clocks
                    for x in evts
                    if (clock-event-from-midi? x)  ; tempo clock 
                    do (when (not (member x res :test #'(lambda (x y) (and (clock-event-from-midi? y)  ; midi=12 tempo clock 
                                                                           (< (abs (- (second x) 
                                                                                      (second y))) 100))))) ; too close onsets
                         (setf res (cons (normalize-tempoevent-from-midi x) res)))   ; clock duration which must be 10 ms, vel=1
                    else do (setf res (cons x res))
                    finally return (reverse res)))
   evts)

(defun check-clocks (evts)
   (loop for event in evts
         with cur-clock = nil
         with collectedclocks = nil
         with collectedevents = nil
         do (if (clock-event-from-midi? event)
                (when (or (null cur-clock)   
                          (> (MEOnset event) (+ (MEOnset cur-clock) 100)))   ; remove clocks when too close from each other !!!!!!!
                  (push (make-clock-from-midi event) collectedclocks) (setf cur-clock event))
                (push event collectedevents))
         finally return (list (reverse collectedclocks) (reverse collectedevents))))

(defun quintuples->MidiHarmBeats (clocks quintuples)
  (loop for startMidiHarmBeat in clocks
        for endMidiHarmBeat in (rest clocks)   ; only takes events before a endclock, may lose some at the end of the sequence
        append (list (loop while quintuples
                           while (< (second (first quintuples)) 
                                    (- (first endMidiHarmBeat) 100)) ; unquantified note falling on next MidiHarmBeat (will get a negative relative time)
                           collect (pop quintuples)))))

(defun cut-MidiHarmBeat-events (clocks MidiHarmBeat-list)       ;warning: physical modifications
  (loop for startMidiHarmBeat in clocks
        for endMidiHarmBeat in (rest clocks)
        for cur-MidiHarmBeat in MidiHarmBeat-list
        for next-MidiHarmBeat in (rest MidiHarmBeat-list)
        for next-parent on (rest MidiHarmBeat-list)
        for new-events = (loop for event in cur-MidiHarmBeat
                               for end-event  = (+ (MEOnset event) (MEDur event))
                               if (and (< (- (first endMidiHarmBeat) (MEOnset event)) 
                                          (* 0.25 (- (first endMidiHarmBeat) (first startMidiHarmBeat))))
                                       (> end-event (* 1.25 (first endMidiHarmBeat))))  
                                  ; syncopation belonging to the next MidiHarmBeat
                               collect event       ; the event is put into the next MidiHarmBeat (and will get a negative relative time)
                               if (> end-event (first endMidiHarmBeat))
                               do (setf (MEDur event)  
                                        (- (first endMidiHarmBeat) (MEOnset event)))   ; cut event
                               and collect (list (if (label-event-from-midi? event) 
                                                     (MEPitch event) ;pitch as chord label
                                                     (- (abs (MEPitch event))))  ; minus => becomes a prolongation in the next MidiHarmBeat
                                                                                 ; be careful that - applied twice becomes + ...
                                                 (first endMidiHarmBeat) (- end-event (first endMidiHarmBeat)) 
                                                 (MEvel event) (MeChannel event)))  
        if new-events do (setf (car next-parent) (append new-events (car next-parent)))  ;add remaining part to next MidiHarmBeat
      )
  MidiHarmBeat-list)
        ;finally return MidiHarmBeat-list))

(defun set-relative-time-MidiHarmBeat (clocks MidiHarmBeat-list)          ; physical modification of MidiHarmBeat-list
  (loop for startMidiHarmBeat in clocks
        for cur-MidiHarmBeat in MidiHarmBeat-list
        do (loop for event in cur-MidiHarmBeat
                 do (setf (MEOnset event) (- (MEOnset event) (first startMidiHarmBeat)))))    ; possibly negative onset
  MidiHarmBeat-list)

(defun label-chord-MidiHarmBeat (MidiHarmBeat-list)
   (loop for cur-MidiHarmBeat in MidiHarmBeat-list
         ;for lpitch = nil
         ;for lvel = nil
         for label = nil                
         for newevents = (loop for event in cur-MidiHarmBeat
                               if (label-event-from-midi? event)    
                               do (when (> (MEDur event) 20) (push (make-label-from-midi event) label))        
                                                             ;(push (MEPitch event) lpitch) (push (MEVel event) lvel))
                               else collect event)
                          ;for label = (pitch-vel->label (reverse lpitch) (reverse lvel))
                          ;if label collect (list  label newevents)))
          if label collect (list (last-elem label) newevents)))     ; if more than one label in a MidiHarmBeat, take only the first


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
               (list (list (MidiRoot (first (HarmLabel cur-MidiHarmBeat))) 50 (- (duration cur-MidiHarmBeat) 100) (case (chordtype (label cur-MidiHarmBeat)) (maj7 100) (m7 101) (7 102) (m7b5 103) (dim 104)) 15))
               (MidiSet cur-MidiHarmBeat)
               )  
              )
        )
  MidiHarmBeat-list
)
  

;----------------------------------------
;formerly in file "Improvizer.lisp" 10/2/21012

;GENERICITE : A REPRENDRE
;by M.C. (12/2/2012 bug fixed "mapcar '(lambda" instead of "mapcar #'(lambda" finally replaced by "loop")
(defun make-MidiHarmBeat-list (list-of-MidiHarmBeats &optional beatdur)
  (let ((MidiHarmBeats (mapcar 'make-MidiHarmBeat list-of-MidiHarmBeats)))
    (if beatdur 
        (loop for MidiHarmBeat in MidiHarmBeats 
              do (setf (duration MidiHarmBeat) 
                       (if (and (listp (harmlabel MidiHarmBeat)) (third (harmlabel MidiHarmBeat)))   ;for older versions with "multiple MidiHarmBeats": 
                           (* beatdur (third (harmlabel MidiHarmBeat)))            ;-> 3rd element of harmlable MUST be a number of beats
                         beatdur))
              collect MidiHarmBeat)
      MidiHarmBeats)))

;GENERICITE : A REPRENDRE
(defun make-MidiHarmBeat (MidiHarmBeat-list)
  (NewMidiharmbeat (nth 0 (first MidiHarmBeat-list)) (nth 1 (first MidiHarmBeat-list)) (second MidiHarmBeat-list)))

(defmethod oracle->MidiHarmBeatlist ((self oracle))
  (loop for i from 1 to (maxetat self)
        collect (otext self i)))

;by M.C.
;WARNING: for older versions with "multiple MidiHarmBeats" -> if label has a 3rd element, it MUST be the duration in number of MidiHarmBeats
(defun MidiHarmBeats->chseq (MidiHarmBeatlist refMidiHarmBeatvalue deltachords)
   (let (chords MidiHarmBeatvalue
         lonset 
         ;last-note 
         (MidiHarmBeats (loop for MidiHarmBeat in MidiHarmBeatlist
                      for eventlist = (MidiSet MidiHarmBeat)
                      for label = (label MidiHarmBeat)
                      for onset = 0 then (+ onset MidiHarmBeatvalue)
                      do (setf MidiHarmBeatvalue              ; MidiHarmBeatvalue of previous MidiHarmBeat for shifting onset 
                               (if (and (listp label) (third label)) (* refMidiHarmBeatvalue (third label)) 
                                   refMidiHarmBeatvalue))    ;if label does not contain informations on the number of MidiHarmBeats,
                                                     ;the chords has the same duration as refMidiHarmBeatvalue
                      ;;;;;;do (format *om-stream* "~a ~a~%" (HarmLabel MidiHarmBeat) MidiHarmBeatvalue)
                      append (loop for event in eventlist
                                   collect (list (first event) (+ onset (second event)) (third event) (fourth event) 
                                                 ;(1+ 
                                                  (fifth event)      ;)
                                                 )))))
     (setf chords (make-quanti-chords MidiHarmBeats deltachords)
         lonset (mapcar 'offset chords))
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset )))


(defun MidiHarmBeats->5list (MidiHarmBeatlist MidiHarmBeatvalue deltachords)
   (let* ((chseq (MidiHarmBeats->chseq MidiHarmBeatlist MidiHarmBeatvalue deltachords)))
     (list (om/ (Lmidic  chseq) 100)
           (butlast (LOnset chseq))
           (Ldur chseq)
           (Lvel chseq)
           (om- (LChan chseq) 1))))
        
(defun events->5list (eventlist  deltachords)
   (let* ((chseq (CrossEvents->ChordSeq eventlist)))
     (list (om/ (Lmidic  chseq) 100)
           (butlast (LOnset chseq))
           (Ldur chseq)
           (Lvel chseq)
           (om- (LChan chseq) 1))))
        

;Function prolongating notes that have a negative midi code (after a similar function 'thread-crossEvents' for non MidiHarmBeat events)
(defmethod thread-MidiHarmBeats ((MidiHarmBeatlist list) &optional beatdur)   ; prolongation of notes marked with negative midi codes
   (let ((clonedMidiHarmBeatlist (mapcar #'clone-object MidiHarmBeatlist)))
     (loop for event in clonedMidiHarmBeatlist
           for delay = (if beatdur beatdur (duration event))
           with new-waiting-list
           for waiting-list = nil then new-waiting-list
           initially do (setf new-waiting-list nil)
           do  (loop for ME in (midiset event)
                     with new-midiset = nil

                     do (cond ((plusp (MEPitch ME))
                               (when (>= (+ (MEOnset ME) (MEDur ME)) (- delay 30))       
                                 (push ME new-waiting-list)   ; add condition: the event duration must reach the end of MidiHarmBeat
                                                              ;... but in human tempo, the "reaching condition" MUST be up to an approximation value
                                                              ;(there is no "exact" beatduration in human tempo)
                                 )
                              (push ME new-midiset))
                             ((minusp (MEPitch ME))      ; negative midi code = prolongation
                              (let ((waiting (find ME waiting-list 
                                                   :test #'(lambda (m1 m2) 
                                                             (and (= (abs (MEPitch m1)) (abs (MEPitch m2))) 
                                                                  (= (MEChannel m1) (MEChannel m2)))))))
                                (cond 
                                 (waiting
                                  (setf (MEDur waiting) (+ (MEDur waiting) (MEDur ME)))
                                  (push waiting new-waiting-list))
                                 (t ;forget prolongation note when there is no beginning note (setf (MEPitch ME) (abs (MEPitch ME)))
                                  ;(push ME new-waiting-list)
                                  ;(push ME new-midiset)
                                  )))))
                    finally (setf (Midiset event) new-midiSet)))
     clonedMidiHarmBeatlist))

(defun MidiHarmBeatlist->chordseq (MidiHarmBeatlist MidiHarmBeatvalue channel)
  (let ((MidiHarmBeats (loop for MidiHarmBeat in MidiHarmBeatlist
                     for onset = 0 then (+ onset MidiHarmBeatvalue)
                     for label = (first MidiHarmBeat)
                     for eventlist = (second MidiHarmBeat)
                     append (loop for event in eventlist
                                  if (or (eq channel t) (= channel (fifth event)))
                                  collect (list (first event) (+ onset (second event)) (third event) (fourth event) (1+ (fifth event)))))))
        (mf-info->chord-seq  MidiHarmBeats)))


(defun filter-MidiHarmBeatlist (MidiHarmBeatlist channels)
  (loop for MidiHarmBeat in MidiHarmBeatlist
        for label = (first MidiHarmBeat)
        for eventlist = (second MidiHarmBeat)
        collect (list label
                      (loop for event in eventlist
                            if (member (fifth event) channels)
                            collect event))))

(defun enhance-channel (MidiHarmBeatlist channel)
  (loop for MidiHarmBeat in MidiHarmBeatlist
        for label = (first MidiHarmBeat)
        for eventlist = (second MidiHarmBeat)
        collect (list label
                      (loop for event in eventlist
                            for velocity = (fourth event)
                            if (= (fifth event) channel) do (setf velocity (min 127 (* 3 velocity))) end
                            collect (list (first event) (second event) (third event) velocity (fifth event))))))

 

;;; PASSAGE OM6.4->OM6.9 : PLUS BESOIN !! DEJA DEFINI !!      

;(defmethod mf-info->chord-seq ((self list) );&optional deltachord)
;  (let* ((chords (make-quanti-chords self *global-deltachords*))
;         (lonset (mapcar 'offset chords))
;         (last-note (first (inside (first (last chords))))))
;    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
;    (make-instance 'chord-seq
;      :lmidic chords
;      :lonset lonset 
;      ;:legato 200
;)))

(if (<= *om-version* 6.04)
    (defmethod mf-info->chord-seq ((self list) );&optional deltachord)
  (let* ((chords (make-quanti-chords self *global-deltachords*))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      ;:legato 200
)))

(defmethod mf-info->chord-seq ((self list) &optional deltachord)
  (let* ((chords (make-quanti-chords self *global-deltachords*))
         (lonset (mapcar 'offset chords))
         (last-note (first (inside (first (last chords))))))
    (setf lonset (append lonset (list (+ (extent->ms last-note) (first (last lonset)))))) 
    (make-instance 'chord-seq
      :lmidic chords
      :lonset lonset 
      ;:legato 200
))))



(defun chseq-mix-melo-accomp (beatlist-melo beatlist-accomp beatdur chan-melo chan-accomp)
  (let ((melo beatlist-melo) (accomp beatlist-accomp))
    (loop for beat in melo do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-melo))))) 
    (loop for beat in accomp do 
                       (setf (Midiset beat) 
                             (loop for z in (MidiSet beat) collect (append (nthcar 4 z) (list chan-accomp)))))
    (merger (midiharmbeats->chseq melo beatdur 0) (midiharmbeats->chseq accomp beatdur 0))))


