 (in-package :om)

;ISSU D'UN DECOUPAGE DE L'ANCIEN "Improvizer.lisp".

;J.Nika (after M. Chemillier and G. Assayag)
;--------------------------------------------------------------------------------

(defclass* improvizer (pythie)
   (
    (name :initform "improvizer" :initarg :name :accessor name)
    (context :initform nil :initarg :context :accessor context)
    (continuity :initform 0 :initarg :continuity :accessor continuity)
    ;+++++++++++++
    ;Current info navigation
    (NavigationMode :initform 'continuity :initarg :NavigationMode :accessor NavigationMode)
    (CurrentStateIdx :initform 0 :initarg :CurrentStateIdx :accessor CurrentStateIdx)
    (PrevStateIdx :initform -1 :initarg :PrevStateIdx :accessor PrevStateIdx)
    ;+++++++++++++
    (max-continuity :initform 8 :initarg :max-continuity :accessor max-continuity)
    (start-region :initform '(0 100) :initarg :start-region :accessor start-region)
    (fwsuffix :initform T :initarg :fwsuffix :accessor fwsuffix)
    (bwsuffix :initform T :initarg :bwsuffix :accessor bwsuffix)
    (bestSuffixMode :initform nil :initarg :bestSuffixMode :accessor bestSuffixMode)  

    (RefHarmScen :initform nil :initarg :RefHarmScen :accessor RefHarmScen)
    (HarmScenLength :initform 48 :initarg :HarmScenLength :accessor HarmScenLength)
    (Beats/Measure :initform 4 :initarg :Beats/Measure :accessor Beats/Measure)
    (RefTempo :initform 536 :initarg :RefTempo :accessor RefTempo) ; beat duration in ms
    (CurrentTranspo :initform 0 :initarg :CurrentTranspo :accessor CurrentTranspo)   
    ;+++++++++++++
    ;New controls -----> ADD TO MAX INTERFACE !!!
    (nexteventifnosolution :initform T :initarg :nexteventifnosolution :accessor nexteventifnosolution)
    (LengthFactorsFromScen :initform '(1 100) :initarg :LengthFactorsFromScen :accessor LengthFactorsFromScen)
    (bestTranspoMode :initform T :initarg :bestTranspoMode :accessor bestTranspoMode)
    (firstWithoutTranspoMode :initform nil :initarg :firstWithoutTranspoMode :accessor firstWithoutTranspoMode)
    (AuthorizedTranspos :initform '(-4 -3 -2 -1 1 2 3 4) :initarg :AuthorizedTranspos :accessor AuthorizedTranspos)
    (randomPrefixOccurrenceMode :initform nil :initarg :randomPrefixOccurrenceMode :accessor randomPrefixOccurrenceMode)
    ;+++++++++++++
    (newtabousperf :initform nil :initarg :newtabousperf :accessor newtabousperf)
    (tabou-mode :initform t :initarg :tabou-mode :accessor tabou-mode)     ;;;;;;; added by M.C. 11/5/2012: PB after the first impro, 
    (tabou :initform (make-hash-table :test '=) :initarg :tabou :accessor tabou)  ;;;     --> it becomes difficult to find 'matches' 
    (feature  :initform '() :initarg :feature :accessor feature)               ;;;;;;; added by M.C. 15/8/0212, list of 'features' as MIDI codes
    (hashlabeltranspo :initform (make-hash-table :test 'equal) :initarg :hashlabeltranspo :accessor hashlabeltranspo)     ;;;;;;;Marc 13/8/2013 essai pour eviter attente "nothing"   
    ))

(defmethod add-feature ((self improvizer) key val)
  (set-value-in-arg-list (feature self) key val))

(defmethod set-LengthFactorsFromScen ((self improvizer) (interval list))
  (setf (LengthFactorsFromScen self) interval))

(defmethod set-start-region ((self improvizer) (region list))
  (setf (start-region self) region))

; Ex set-start-region-oraclechan
(defmethod set-start-region-scaling ((self Improvizer) begin end begin_slider end_slider)
  (set-start-region self (list (floor (* begin (/ (max begin_slider (1- (maxetat self))) end_slider))) 
                                 (floor (* end (/ (max begin_slider (1- (maxetat self))) end_slider))))))

(defmethod set-start-point ((self improvizer) int)
  (setf (start-region self) (list int (maxetat self))))

(defmethod set-suffix-law ((self improvizer) (suffixes list))
  (setf (bwsuffix self) (first suffixes)
        (fwsuffix self) (second suffixes)))
        
(defmethod set-best-suffix-mode ((self improvizer) (value symbol))
  (setf (bestSuffixMode self) value))

(defmethod NbEvent? ((self improvizer))
  (maxetat self))

(defmethod SetmaxCont ((self improvizer) (cont integer))
  (setf (continuity self) 0)
  (setf (max-continuity self) cont))

(defmethod add-tabous ((self improvizer) (tab list))
  (loop for idx from (nth 0 tab) to (nth 1 tab)  do
        (setf (gethash idx (tabou self)) t)))

(defmethod ResetTabou ((self improvizer)) (setf (tabou self) (make-hash-table :test '=)))

;Change (RefTempo improvizer) to adapt to live performance
(defmethod SetReftempo ((self improvizer) newbeatdur)
  (let ((coef (/ newbeatdur (RefTempo self))))
    (loop for i from 1 to (maxetat self) for event = (otext self i)
          do
          (TimeStretchEvent event coef))
    (setf (RefTempo self) newbeatdur)                                 ; beat duration in ms
    self))

(defmethod null-event ((self improvizer))
  (when (otext self 1) (make-instance (class-of (otext self 1)))))
        

(defmethod NewImprovizer (&optional sequence beatdur &key max-continuity)
  (let* ((improvizer (make-instance 'Improvizer
                                   :comparateur 'CompareEvents
                                   :lrsMode t)))
    (when sequence (loop for i from 0 to (1- (length sequence)) do (learn-event improvizer (nth i sequence))))
    (when beatdur (setf (RefTempo improvizer) beatdur))
    (when max-continuity (SetmaxCont improvizer max-continuity))
    (set-start-point improvizer 0)
    improvizer))



;--------------------------------------------------------------------------------
;;; LEARNING

(defmethod learn-event ((self improvizer) (event event))
   (ajouter-objet self event))

;For genericity, when event is a non 'event' event
(defmethod learn-event ((self improvizer) (event t))   
   (ajouter-objet self event))

(defmethod learn-event-list ((self improvizer) (events list))
   (loop for event in events 
         do (ajouter-objet self event) ) self)

;Comparison with learnt object is done by the function 'transition' (see Pythie class in Oracle.lisp)
(defmethod ajouter-objet ((self improvizer) (objet t))
  (let ((m (maxetat self)) (Pi1))
    (when (>= (1+ m) (length (vectext self)))
      (setf (vectext self) (adjust-array (vectext self) (+ 500 (length (vectext self)))))
      (when (lrsmode self) (setf (veclrs self) (adjust-array (veclrs self) (+ 500 (length (veclrs self)))))))
    
    (creer-etat self (1+ m))
    ;Jerome, 19/02/13
    (set-start-point self 0)
    (setf (otext self (1+ m)) objet)
    (setf (transition self m objet) (1+ m))
    (loop for etat = (suppleance self m) then (suppleance self etat)
          with Pi1 = m
          while (and (> etat -1)
                     (null (transition self etat objet)))   ; no arrow
          do (setf (transition self etat objet) (1+ m)      ; => add arrow + follow link
                   Pi1 etat)
          finally
          (let ((sp (if (= etat -1)  0 (transition self etat objet)))) ; suffix link -> 0 or target
            ;(format *om-stream* " label=~a SUPPL: ~a => ~a " (harmlabel objet) (1+ m) sp)
            (when sp 
              (setf (suppleance self (1+ m)) sp)
              (when (lrsMode self)
                (setf (lrs self (1+ m)) (LenghrepeatedSuffix self Pi1 sp))))))
    self))

;--------------------------------------------------------------------------------
;;; GENERATION

(defmethod eligible-feature? ((self event) (o improvizer))
  (if (null (feature o)) t 
    ;(if (integerp (feature self))
    ;    (member (abs (feature self)) (feature o))  ;'features' are MIDI codes, thus 'abs' is needed for prolongation
    ;    nil)
    ))     ;'feature' = nil when the midiharmbeat has no feature, thus it should be rejected if the oracle looks for features
;(defmethod eligible-feature? ((self t) (o improvizer)) t)      ;;;;;;;;;;for genericity
(defmethod eligible-feature? ((self t) (o improvizer))
  (if (null (feature o)) nil 
    )) 


(defmethod ScenarioFromImprovizer ((self Improvizer))
  (loop for i from 1 to (maxetat self) collect (label (otext self i))))

  

;Overloaded method finding prefixes for the navigation
;-----------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;-----------------------------------------------------
(defmethod PrefixIndexing ((memory Improvizer) (scen list) &optional CompareFunction)
  (let* ((text (ScenarioFromImprovizer memory))
         ;(text (append (list '(nil)) text))
         (text (append (list (make-instance 'label)) text))
         ;/!\ append nil because the first state (idx=0) of an oracle is empty !
         (word scen)
         (Compare 'equalLabel))
    (when CompareFunction (setf Compare CompareFunction))
    (PrefixIndexing text word Compare)))

#|
;Original Morris&Pratt algorithm
;Overloads method defined in PrefixIndexing.lisp
;---------------------------------------------------
;/!\ Arguments : 1)Improvizer 2)grid (list)
;Returns the list of idxs where the whole grid is found
;---------------------------------------------------
(defmethod MorrisPratt ((memory Improvizer) (scen list) &optional CompareFunction)
  (let* ((text (ScenarioFromImprovizer memory))
         (text (append (list '(nil)) text))
         ;/!\ append nil because the first state (idx=0) of an oracle is empty !
         (word scen)
         (Compare 'equalLabel))
    (when CompareFunction (setf Compare CompareFunction))
    (MorrisPratt text word Compare)))
|#    


;;; Navigation methods

                     
(defmethod reset-navigation-params ((self improvizer))
  (setf (CurrentTranspo self) 0
          (continuity self) 0 
          (NavigationMode self) 'continuity 
          (CurrentStateIdx self) 0
          (PrevStateIdx self) -1))



; GENERATION OF A WHOLE IMPROVISATION (reset navigation parameters at the beginning)
;------------------------------------------------------------
(defmethod Improvize ((self improvizer) (length integer) &optional (scenario nil))
  (let ((impro nil) (current-scenario-suffix nil) (next-scenario-suffix nil))  
    (format *om-stream* "-----------------------~%")
    (when (null (veclrs self)) (setf (bestSuffixMode self) nil) (format *om-stream* "Old Oracle, setting bestSuffix mode to nil~%"))  
    (when scenario (setf next-scenario-suffix scenario))
  ;Reset navigation info in (self Improvizer)
  (reset-navigation-params self)
  ;Navigation following the whole scenario
  (loop for i from 1 to length
        for label =  (pop scenario) 
        do
        (setf current-scenario-suffix next-scenario-suffix)
        ;Update for next navigation step
        (setf next-scenario-suffix scenario)
        ;One navigation step : /!\ (CurrentStateIdx self) is modified
        collect (Improvize-next-state self current-scenario-suffix)
        )))
(defmethod ImprovizeOnHarmGrid ((self improvizer) (length integer) (harmGrid list))
  (Improvize self length harmGrid))
(defmethod ImprovizeOnScenario ((self improvizer) (length integer) (scenario list))
  (ImprovizeOnScenario self length scenario))



; One step in navigation, modification of current/previous states slots
; and PLAYS the corresponding event
;---------------------------------------------------------------------------------
(defmethod Improvize-play-next-state ((self improvizer)  &optional (scenario nil) (beatduration integer))  
  ; -> Navigation info displayed at a lower level : Improvize-next-idx
  (let ((beat (Improvize-next-state self scenario)))
    (if beat
        (progn
          (Stop-Player *general-player*)
          (play (beats->chseq (list beat) beatduration 0)))
      (format *om-stream* "End of impro !~%"))))


;One step in navigation : modification of current/previous states slots
;and RETURNS the corresponding event
;---------------------------------------------------------------------------------
(defmethod Improvize-next-state ((self improvizer)  &optional (scenario nil))
  (Improvize-navigate-one-step self scenario)
  (if (> (CurrentStateIdx self) 0) 
      (TransposeClonedEvent (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
    (null-event self))
  )


;One step in navigation : modification of current/previous states slots
;---------------------------------------------------------------------------------
(defmethod Improvize-navigate-one-step ((self improvizer)  &optional (scenario nil))
  (let* (;(time-call (get-internal-real-time))
         (index (CurrentStateIdx self))
         (nextindex (Improvize-next-idx self scenario)))
    (setf (PrevStateIdx self) index)
    (setf (CurrentStateIdx self) nextindex)
    ;(if *print-navig-basics* 
    ;    (format *om-stream* "Time computation : ~5,2F~%~%" (/ (- (get-internal-real-time) time-call) internal-time-units-per-second)))
    ))

;One step in navigation : compute the next index in the improvizer 
; (no modification of current/previous states slots)
;---------------------------------------------------------------------------------
(defmethod Improvize-next-idx ((self improvizer)  &optional (scenario nil))
  (let* ((index (CurrentStateIdx self)) (previndex (PrevStateIdx self)) (nextindex nil)
         (scenario-current-transp (TransposeCloneLabelList scenario (CurrentTranspo self)))
         (label-current-transp (car scenario-current-transp))
         (links nil))

    ;*print-navig-basics* defini dans LoadImproteK   (setf *print_info_navig* t)
    (when *print-navig-basics* (display-infolabel self scenario))
    
    (cond
     
     ;Starting point
     ((and (zerop index) (setf nextindex (find-prefix-labels-match self scenario-current-transp)))
      (format *om-stream* "Starting point in memory : ~a ~%" nextindex))
     
     ;Navigation
     (t
      ;Update navigation mode
      (if (check-continuity self) 
          (setf (NavigationMode self) 'continuity)  
        (if (available-suppleance self index) 
            (setf (NavigationMode self) 'suppleance)
          (setf (NavigationMode self) 'continuity)))
      
      ; MODE CONTINUITY
      (when (eq (NavigationMode self) 'continuity)
        (setf links (flink self index)
              nextindex (choose-factor-link self links label-current-transp))
        (if nextindex
              (if *print-navig-basics* (format *om-stream* "c : ~a (~a / ~a) ~%" nextindex (continuity self) (max-continuity self)));JNMR 
          (if (available-suppleance self index) 
              (setf (NavigationMode self) 'suppleance)
            (setf (NavigationMode self) 'nothing))))
      ;MODE SUPPLEANCE
      (when (eq (NavigationMode self) 'suppleance)
        (setf nextindex (continuations-by-suppleance self index label-current-transp)) 
        (if (and nextindex (/= nextindex previndex))
            (progn 
              (if *print-navig-basics* (format *om-stream* "--->s : ~a ~%" nextindex))
              (setf (continuity self) 0))
          (progn
            (setf (NavigationMode self) 'nothing)
            (if *print-navig-basics* (format *om-stream*  "~a, " (NavigationMode self))))))
      ;MODE NOTHING
      (when (eq (NavigationMode self) 'nothing)
          (setf nextindex (find-prefix-labels-match self scenario-current-transp))
        (if nextindex 
            (progn 
              (if *print-navig-basics* (format *om-stream* "new : ~a " nextindex))
              (if (or (null scenario) (zerop (CurrentTranspo self))) 
                  (if *print-navig-basics* (format *om-stream* " ~%")) (if *print-navig-basics* (format *om-stream* "new mem. transpo=~a ~%" (CurrentTranspo self))))
              (setf (continuity self) 0))
          (progn               
            (if *print-navig-basics* (format *om-stream* "~a " 'empty))
            
            (if (nexteventifnosolution self) 
                (progn
                  (if *print-navig-basics* (format *om-stream* "-> nextevent ~a~%" (+ index 1)))
                  (setf nextindex (+ index 1))
                  )
              (progn
              (setf nextindex 0)
              (if *print-navig-basics* (format *om-stream* "~%"))
              ))
            
            )))))
    nextindex))

; *print_info_navig* defini dans LoadImproteK   (setf *print_info_navig* 1)
(defmethod display-infolabel ((self improvizer) scen)
    
    (if *print-navig-basics* 
        (progn 
          (format *om-stream* "label scen=~a (mem current transpo = ~a)" (FormatLabel (car scen)) (CurrentTranspo self))
          (if (= (CurrentTranspo self) 0) (format *om-stream* ", ") (format *om-stream* ", mem orig. label=~a, " 
                                                                            ;(FormatLabel (TransposeCloneLabel (car scen) (- (CurrentTranspo self))))
                                                                            ;(FormatLabel (car scen))
                                                                            (FormatLabel (TransposeCloneLabel (car scen) (+ (CurrentTranspo self))))
                                                                            ))))

    (if (= *print_info_navig* 1) (format *om-stream* "current transp. scen. suffix = ~a~%" (FormatLabelList (TransposeCloneLabelList scen (CurrentTranspo self))))))


(defmethod available-suppleance ((self improvizer) (index integer))
  (or (and (bwsuffix self)
           (not (zerop (suppleance self index))))
      (and (fwsuffix self)
           (suppleance-> self index))))


(defmethod ImprovizeByContinuation ((self Improvizer) (nbevents integer) (context list))
   (let ((start-etat
          (loop for suffixe on context
                for etat = (factor-p self context)
                while (null etat)
                finally return (if suffixe etat 0))))
     (Improvize self nbevents :start-etat start-etat)))
     
;--------------------------------------------------------------------------------


(defmethod check-continuity ((self improvizer))
  (if (< (continuity self) (max-continuity self))
      (true (incf (continuity self)))
    (false (setf (continuity self) 0))))

(defmethod eligible-index? (index (self improvizer))
  (and (or (not (tabou-mode self)) (not (gethash index (tabou self))))
       (>= index (first (start-region self))) (<= index (second (start-region self)))))

;(defmethod eligible-feature? ((self event) (o improvizer))
;  (if (null (feature o)) t 
;    (if (integerp (feature self))
;        (member (abs (feature self)) (feature o))  ;'features' can be MIDI codes, thus 'abs' is needed for prolongation
;        nil)))     ;'feature' = nil when the event has no feature, thus it should be rejected if the oracle looks for features

(defmethod reduce-eligible-events ((self improvizer) list-of-choices)
  (remove nil list-of-choices :test #'(lambda (x y) (not (and (eligible-index? y self) 
                                                              (eligible-feature? (otext self y) self))))))


;JEROME avril 2015
(defmethod available-continuations-by-suppleance ((self improvizer) index label)
  (let (back-cont forw-cont (best-suffix-state -1) (max-suffix 0) (supps nil))
     (when (bwSuffix self)
       (setf back-cont
             (loop for supp = (suppleance self index) then (suppleance self supp)
                   with previous = index
                   with suffix-length = 0
                   while (and supp (> supp 0))
                   do 
                   ;(print (lrs self previous)) 
                   (when (bestSuffixMode self) (setf suffix-length (lrs self previous)  previous supp))
                   append (loop for cont in (flink self  supp)
                                if (eligible-event? (otext self cont) label)
                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))
     (when (fwSuffix self)
       (setf forw-cont 
             (loop for supp = (suppleance-> self index) then (suppleance-> self supp)
                   with suffix-length = 0
                   while supp
                   do
                   (when (bestSuffixMode self) (setf suffix-length (lrs self supp)))
                   ;(print (list '-> (lrs self supp)))
                   ;(when (> suffix-length max-suffix) (setf best-suffix-state supp max-suffix suffix-length))
                   append (loop for cont in (flink self supp)
                                if (eligible-event? (otext self cont) label)

                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))
     (if (bestSuffixMode self)   
         (progn
           (setf supps
                 (loop for cont in (append back-cont forw-cont)
                       if (= (second cont) max-suffix) collect (first cont)))
           (if supps supps (append back-cont forw-cont)))
       (append back-cont forw-cont)
     )))




(defmethod continuations-by-suppleance ((self improvizer) index label)
   (let (back-cont forw-cont (best-suffix-state -1) (max-suffix 0))
     (when (bwSuffix self)
       (setf back-cont
             (loop for supp = (suppleance self index) then (suppleance self supp)
                   with previous = index
                   with suffix-length = 0
                   while (and supp (> supp 0))
                   do 
                   ;(print (lrs self previous)) 
                   (when (bestSuffixMode self) (setf suffix-length (lrs self previous)  previous supp))
                   append (loop for cont in (flink self  supp)
                                if (eligible-event? (otext self cont) label)
                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))
     (when (fwSuffix self)
       (setf forw-cont 
             (loop for supp = (suppleance-> self index) then (suppleance-> self supp)
                   with suffix-length = 0
                   while supp
                   do
                   (when (bestSuffixMode self) (setf suffix-length (lrs self supp)))
                   ;(print (list '-> (lrs self supp)))
                   ;(when (> suffix-length max-suffix) (setf best-suffix-state supp max-suffix suffix-length))
                   append (loop for cont in (flink self supp)
                                if (eligible-event? (otext self cont) label)

                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))   

     (if (bestSuffixMode self)    ;MARC 24/4/2012 bestSuffixMode = nil
                                  ;since bestSuffixMode = t => TOO RESTRICTED: always the same path in the oracle
       
       ;(nth-random (loop for cont in (append back-cont forw-cont)
       ;                  if (= (second cont) max-suffix) collect (first cont)))

         ;;;
(let ((chosen-index (nth-random (reduce-eligible-events self                          ;;;;MARC 11/5/12      ???????????????????? JEROME : POURQUOI PAS reduce.... dans le cas best ?
                                                              (mapcar 'first (append back-cont forw-cont))    ))))
         (if (newtabousperf self) (when chosen-index (setf (gethash chosen-index (tabou self)) t)))
         chosen-index)
         ;;;

       (let ((chosen-index (nth-random (reduce-eligible-events self                          ;;;;MARC 11/5/12      ???????????????????? JEROME : POURQUOI PAS reduce.... dans le cas best ?
                                                              (mapcar 'first (append back-cont forw-cont))    ))))
         (if (newtabousperf self) (when chosen-index (setf (gethash chosen-index (tabou self)) t)))
         chosen-index)
       )))




#|
(defmethod continuations-by-suppleance ((self improvizer) index label)
   (let (back-cont forw-cont (best-suffix-state -1) (max-suffix 0))
     (when (bwSuffix self)
       (setf back-cont
             (loop for supp = (suppleance self index) then (suppleance self supp)
                   with previous = index
                   with suffix-length = 0
                   while (and supp (> supp 0))
                   do 
                   ;(print (lrs self previous)) 
                   (when (bestSuffixMode self) (setf suffix-length (lrs self previous)  previous supp))
                   append (loop for cont in (flink self  supp)
                                if (eligible-event? (otext self cont) label)
                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))
     (when (fwSuffix self)
       (setf forw-cont 
             (loop for supp = (suppleance-> self index) then (suppleance-> self supp)
                   with suffix-length = 0
                   while supp
                   do
                   (when (bestSuffixMode self) (setf suffix-length (lrs self supp)))
                   ;(print (list '-> (lrs self supp)))
                   ;(when (> suffix-length max-suffix) (setf best-suffix-state supp max-suffix suffix-length))
                   append (loop for cont in (flink self supp)
                                if (eligible-event? (otext self cont) label)

                                do (when (> suffix-length max-suffix) (setf max-suffix suffix-length))
                                and collect (list cont suffix-length)))))   

     (if (bestSuffixMode self)    ;MARC 24/4/2012 bestSuffixMode = nil
                                  ;since bestSuffixMode = t => TOO RESTRICTED: always the same path in the oracle
       
       (nth-random (loop for cont in (append back-cont forw-cont)
                         if (= (second cont) max-suffix) collect (first cont)))
       (let ((chosen-index (nth-random (reduce-eligible-events self                          ;;;;MARC 11/5/12
                                                              (mapcar 'first (append back-cont forw-cont))    ))))
         (when chosen-index (setf (gethash chosen-index (tabou self)) t))
         chosen-index)
       )))
|#
;================================================================================== FIND-PREFIX-LABEL-MATCH ================================================================================
;SEARCH FOR A BEAT WITHOUT CONTINUITY

; Jerome 20/02/13
;Collect the prefixes of a scen (list) found in an improvizer
;------------------------------------------------------------------------------------------------------------------------------------
;Prefixes filtered according to :
;   - the length (in (LengthFactorsFromScen [Improvizer]))
;   - "tabous"
;Returns a 2 elements list :
; (nth 0 ) -> List of (prefix_length list_of_idxs_beginning)
; (nth 1 ) -> length of the longest prefix (used in find-prefix-label-match to compare the results of thee different transpositions)
;------------------------------------------------------------------------------------------------------------------------------------
(defmethod select-matching-prefixes ((self improvizer) (scen list))
  (let* (
         (MP (PrefixIndexing self scen)) 
         (found-scen-prefixes (nth 0 MP)) 
         (length_longest (nth 1 MP)) 
         (length_longest (nth 1 MP)) 
         (selected-prefixes nil))
    (if (= *print_info_find* 1) (format *om-stream* "(((Selecting prefixes ((according to constraints)) of ~a)))~%" (FormatLabellist scen))) 
    (list  
     (setf selected-prefixes 
           (loop for len being the hash-key of found-scen-prefixes using (hash-value idxs)
                 if (and (reduce-eligible-events self idxs) 
                         (and (>= len (nth 0 (LengthFactorsFromScen self))) 
                              (<= len (min (max-continuity self) (nth 1 (LengthFactorsFromScen self)))))) ;;;;;;;; TEST JNMR
                 collect (list len (reduce-eligible-events self idxs))
                 if (= *print_info_find* 1) do (format *om-stream* "---->Found prefix(es) : Length = ~D, Idxs =~a ------> FILTERED : idxs =~a~%" len idxs (reduce-eligible-events self idxs) )
                 ))
     (if selected-prefixes length_longest 0))))


; Jerome 20/02/13
;Display select-matching-prefixes results
;-----------------------------------------
(defun print-select-matching-prefixes (list)
(format *om-stream* "~%Length longest prefix before filtering : ~D~%" (nth 1 list))
(format *om-stream* "Selected prefixes after filtering (LengthFactorsFromScen and tabous):~%")
(loop for l in (nth 0 list) do
      (format *om-stream* "Length = ~D -> ~D occurence [idxs = ~a]~%" (nth 0 l) (list-length (nth 1 l)) (nth 1 l))))

; Jerome 20/02/13
;Get an index in the Improvizer where a prefix of the scen begins, according to the navigation modes in the Improvizer class
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
; Replaces "find-beat-label-match"
; -- Transpositions are managed by (bestTranspoMode [Improvizer]) and (firstWithoutTranspoMode [Improvizer])
; -- For all the transpositions, the prefixes are selected and filtered in select-matching-prefixes (above).
; -- The length is randomly chosen among those returned by select-matching-prefixes
;   ==> TO SELECT A PRECISE LENGTH N : (setf (LengthFactorsFromScen [Improvizer] '(N N)))
; -- (randomPrefixOccurrenceMode [Improvizer]) = nil : with a given length, choose the leftmost occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;    (randomPrefixOccurrenceMode [Improvizer]) = t : with a given length, choose a random occurrence of a prefix in the corpus (in FIND-prefix-label-match)
;--------------------------------------------------------------------------------------------------------------------------- ------------------------------
(defmethod find-prefix-labels-match ((self improvizer) (scen list)) 
  (if (= *print_info_find* 1) (format *om-stream* "~%~%=========================================FIND-PREFIX-LABELS-MATCH  of  ~a, length in ~a=========================================~%" (FormatLabellist scen) (LengthFactorsFromScen self))) 
  (if (= (maxetat self) 1) 1  

    (let* ((Transpos (AuthorizedTranspos self))
           (intervalTranspo 
            (if (FirstWithoutTranspoMode self) ;(firstWithoutTranspoMode self) = t : first search without transposition
                (append '(0) (rotate Transpos (random (list-length Transpos)))) 
              (rotate (append '(0) Transpos) (random (1+ (list-length Transpos))))))) 
      
          
      ;(bestTranspoMode self) = t : all the transpositions in (AuthorizedTranspos self) are compared to chose the transposition giving the longest prefix
      (if (bestTranspoMode self)
          (let* ((best_transp_len 0) (best_transp_idxs nil) (best_delta 0)
                 (cur_transp_selected_prefixes_info nil) (cur_transp_max_length nil) (cur_transp_selected_prefixes nil) (cur_transp_chosen_prefix nil) (cur_transp_idxs nil) )
            (loop for delta in (om- intervalTranspo (CurrentTranspo self)) 
                  do 
                  (if (= *print_info_find* 1) 
                      (format *om-stream* "[Searching for BEST transpo of ~a]~%[with transpo = ~D -> transp-scen = ~a]~%" (FormatLabellist scen) delta (FormatLabellist (TransposeCloneLabelList scen delta))))
                  (setf cur_transp_selected_prefixes_info (select-matching-prefixes self (TransposeCloneLabelList scen delta)))
                  (setf cur_transp_max_length (nth 1 cur_transp_selected_prefixes_info)) (setf cur_transp_selected_prefixes (nth 0 cur_transp_selected_prefixes_info))
                  (setf cur_transp_chosen_prefix (nth-random cur_transp_selected_prefixes)) 
                  (setf cur_transp_len (nth 0 cur_transp_chosen_prefix)) (setf cur_transp_idxs (nth 1 cur_transp_chosen_prefix))
                  (if (> cur_transp_max_length best_transp_len) (setf best_transp_len cur_transp_max_length best_transp_idxs cur_transp_idxs best_delta delta)))
            (if (> best_transp_len 0)
                (let* ((transp_states best_transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                  (if (= *print_info_find* 1) 
                      (format *om-stream* ">>>>Chosen prefix with BEST TRANSP = ~D, TRANSP_SCEN = ~a :  ~%----->length = ~D, idxs = ~a ~%~%~%" 
                              best_delta (FormatLabellist (TransposeCloneLabelList scen best_delta)) best_transp_len best_transp_idxs))
                  (if (newtabousperf self) (setf (gethash chosen_transp_state (tabou self)) t)) 
                  (incf (CurrentTranspo self) best_delta) chosen_transp_state) 
              nil))
        
        
        ;(bestTranspoMode self) = nil : uses the first transposition returning a prefix with length >= 1
        (loop for delta in (om- intervalTranspo (CurrentTranspo self))
              do 
              (if (= *print_info_find* 1) 
                      (format *om-stream* "~%~%~%[Searching for FIRST RANDOM transpo of ~a]~%[with transpo = ~D -> transp-scen = ~a]~%" (FormatLabellist scen) delta (FormatLabellist (TransposeCloneLabelList scen delta))))
              (let* ((transp_selected_prefixes_info (select-matching-prefixes self (TransposeCloneLabelList scen delta)))
                        (transp_max_length (nth 1 transp_selected_prefixes_info)) (transp_selected_prefixes (nth 0 transp_selected_prefixes_info))
                        (transp_chosen_prefix (nth-random transp_selected_prefixes)) 
                        (transp_len (nth 0 transp_chosen_prefix)) (transp_idxs (nth 1 transp_chosen_prefix)))
                   
                   (if (> transp_max_length 0)
                       (let* ((transp_states transp_idxs) (chosen_transp_state (if (randomPrefixOccurrenceMode self) (nth-random transp_states) (apply 'min transp_states))))
                         (if (= *print_info_find* 1) 
                             (format *om-stream* ">>>>Chosen prefix with FIRST RANDOM TRANSP = ~D, TRANSP_SCEN = ~a :  ~%----->length = ~D, idxs = ~a ~%" 
                                     delta (FormatLabellist (TransposeCloneLabelList scen delta)) transp_len transp_idxs))
                         (if (newtabousperf self) (setf (gethash chosen_transp_state (tabou self)) t)) 
                         (incf (CurrentTranspo self) delta) 
                         (return chosen_transp_state)))
                   )
              finally return nil))
      )))


;MODE CONTINUITY
(defmethod choose-factor-link ((self improvizer) indexes label)
  (if (null label)
      (nth-random indexes)
    (let* ((succeed (loop for index in indexes 
                          if (eligible-event? (otext self index) label) 
                          collect index))
           (reducedindexes (reduce-eligible-events self succeed))
           (chosen-index (nth-random reducedindexes)))
      (if (newtabousperf self) (when chosen-index (setf (gethash chosen-index (tabou self)) t)))
      chosen-index)))

          
(defmethod factor-p ((self improvizer) (factor list))
       (loop for etat = 0 then (transition self etat (pop factor))
             while (and factor (not (null etat)))
             finally
             (if (null etat) (return nil)  (return etat))))



;================================================================================== Save and load ================================================================================


(defmethod save-improvizer ((self improvizer) (name t))
  (WITH-OPEN-FILE (out name :direction :output  :if-does-not-exist :create :if-exists :supersede)
                       ;:external-format :|TEXT|)
    (prin1 '(in-package :om) out)
    (prin1 (omng-save  self) out))
  t)


(defmethod load-improvizer ((name t))
  (WITH-OPEN-FILE (in name :direction :input  );:if-does-not-exist) ;:nil)
    (eval (read in)) (eval (read in))))


(defun eval-my-list (list)
  (loop for item in list collect (eval item)))


;(defmethod omNG-save ((self string) &optional (values? nil))
;  self)


;(defmethod omNG-save ((self array) &optional (values? nil))
;   `(make-array ,(length self) :initial-contents (eval-my-list '(,.(map 'list #'(lambda (elt) (omNG-save elt values?)) self)))))



;All the events are stretched to fit the tempo of the first Impr
(defmethod add-improvizer ((refImpr improvizer) (Impr improvizer))
  (when (/= (RefTempo Impr) (RefTempo refImpr)) 
    (SetReftempo Impr (RefTempo refImpr)))
  (loop for i from 1 to (maxetat Impr) do 
        (learn-event refImpr (otext Impr i)))
  refImpr)


;All the events are stretched to fit the tempo of the first Impr
(defun concatenate-improvizer-list (Imprlist)
  (let ((result (car Imprlist)))
    (loop for imp in (cdr Imprlist) do
          (add-improvizer result imp)
          )
    result))

#|
(setf pathdossier "/Users/jnika/Google\ Drive/Dev/Max-Interface/_Oracles/"
      nameImprov1 "AutumnleavesDoMin-solo1-Chan9-2014.3.1-13h40.or"
      nameImprov2 "AutumnleavesDoMin-JOVINOmixPlanant.or")

(setf Improv (concatenate-Impr-list (list
  (load-Impr (concatenate 'string pathdossier nameImprov1))
  (load-Impr (concatenate 'string pathdossier nameImprov2))
  )))
(save-Impr Improv (concatenate 'string *pathdossier* "AutumnLeaves_Florilege" ".or"))
|#

