;RealTimeImprovizer.lisp
;J. Nika

(in-package :om)

;=========
;= CLASS =
;=========
(defclass* RealtimeImprovizer (improvizer)
   (
    (CurrentImproIdx :initform -1 :initarg :CurrentImproIdx :accessor CurrentImproIdx)
    (StartingPointNextPhaseFound? :initform nil :initarg :StartingPointNextPhaseFound? :accessor StartingPointNextPhaseFound?) 
    ;"true" only if last step in navigation succesfully called "find-prefix-label-match"
    (improTrace :initform (make-hash-table :test '=) :initarg :improTrace :accessor improTrace)
    (modeRT :initform nil :initarg :modeRT :accessor modeRT)
    (output :initform nil :initarg :output :accessor output)
    (gen-callback :initform nil :initarg :gen-callback :accessor gen-callback)))
   

(defmethod (setf CurrentImproIdx) (index (self realtimeimprovizer))
  (setf (slot-value self 'CurrentImproIdx) index)
  ;(print index)
  (if (functionp (gen-callback self))
      (funcall (gen-callback self) index)))

;==========================
;= INITIALIZE & GET METHODS =
;==========================

(defmethod NewRealtimeImprovizer (&optional memory-list eventdur &key max-continuity)
  (let* ((RealtimeImprovizer (make-instance 'RealtimeImprovizer
                                   :comparateur 'CompareEvents
                                   :lrsMode t)))
    (when memory-list (loop for i from 0 to (1- (length memory-list)) do (learn-event RealtimeImprovizer (nth i memory-list))))
    (when eventdur (setf (RefTempo RealtimeImprovizer) eventdur))
    (when max-continuity (SetmaxCont RealtimeImprovizer max-continuity))
    (set-start-point RealtimeImprovizer 0)
    (setf (gethash 0 (improtrace RealtimeImprovizer)) '(0 0))
    RealtimeImprovizer))

(defmethod reset-navigation-params ((self RealtimeImprovizer))
  (call-next-method)
  (setf (CurrentImproIdx self) 0
        (improTrace self) (make-hash-table :test #'=)
        (StartingPointNextPhaseFound? self) nil)
  (setf (gethash 0 (improtrace self)) '(0 0)))

(defmethod StateIdx-at-ImproIdx-in-traceimpro ((self RealtimeImprovizer) (ImproIdx t))
  (nth 0 (gethash ImproIdx (improtrace self))))

(defmethod Continuity-at-ImproIdx-in-traceimpro ((self RealtimeImprovizer) (ImproIdx t))  
  (nth 1 (gethash ImproIdx (improtrace self))))

(defmethod Improvizer->RealtimeImprovizer ((self improvizer))
  (let ((newRealtimeImprovizer (NewRealtimeImprovizer)))
    (loop for sl in 
          (mapcar #'slot-definition-name (class-slots (class-of self))) do
          (setf (slot-value newRealtimeImprovizer sl) 
                (slot-value self sl)))      
    newRealtimeImprovizer))

(defmethod load-realtimeImprovizer-fromSavedImprovizer ((name t))
  (let ((loadedimprovizer (load-improvizer name)))
    (Improvizer->RealtimeImprovizer loadedimprovizer)))

(defmethod load-realtimeImprovizer-fromSavedOldImprovizer ((name t))
  (let ((loadedimprovizer (ImprovizerExBeat->MidiHarmBeat (load-improvizer name))))
    (Improvizer->RealtimeImprovizer loadedimprovizer)))




 
;===============================================
;=  NAVIGATION METHODS OVER A WHOLE "PHASE"
;= (with "StartingPointNextPhaseFound?")  
;===============================================

;Generates one phase of the improvisation : a sequence matching a prefix of the scenario
(defmethod Improvize_OnePhase ((self RealtimeImprovizer) (length t) (scenario list) eventIdxInImpro)
  (let ((impro nil) (current-scenario-suffix nil) (next-scenario-suffix nil) (next-state nil))  
    (when scenario (setf next-scenario-suffix scenario))
    ;(format *om-stream* "~%~%****** Launching new navigation beginning at idx ~D in impro~%" eventIdxInImpro)
    
    (go-backwards-with-improtrace? self eventIdxInImpro)

    (setf (output self) nil)
    
    (setf (StartingPointNextPhaseFound? self) nil)

    (if (modeRT self) (setf (start-region self) (list 0 (- eventIdxInImpro 1))))

    (loop for i from 1 to (min length (maxetat self))
          for label = (pop scenario) 
          do
          (setf current-scenario-suffix next-scenario-suffix)
          (setf next-scenario-suffix scenario)
          (setf next-state (Improvize-next-state self current-scenario-suffix))
          (setf (output self) (append (output self) (list next-state)))
          collect next-state
          while (not (StartingPointNextPhaseFound? self)))))
          ;while t)))

;JNMR
(defmethod ImprovizeFormat_OnePhase ((self RealtimeImprovizer) &optional (scenario nil) (eventduration t) (eventIdxInImpro t))
  (let ((GeneratedImpro (ImprovizeImprovize_OnePhase self (list-length scenario) scenario eventIdxInImpro)))
        (FormatOutputSequence GeneratedImpro eventduration)))

;ex Improvize&send-groupsAnte-loop-next-factor
(defmethod ImprovizeFormatSend_OnePhase ((self RealtimeImprovizer) &optional (scenario nil) (eventduration t) (eventIdxInImpro t) (hostsend t) (portsend t) (adresssend simple-base-string) (numVoice t))
  (let ((ProcessedGeneratedImpro (ImprovizeFormat_OnePhase self scenario eventduration eventIdxInImpro)))
    (osc-send-sequence-fragment ProcessedGeneratedImpro eventIdxInImpro hostsend portsend adresssend numVoice)))

; Quand generation query, teste si sur partie déjà calculée (donc réécriture) 
; ou si nouvelle partie, et mise à jour des états... du RealTimeImprovizer
; si nécessaire
; --------------------------------------------------------------------------
(defmethod go-backwards-with-improtrace? ((self RealtimeImprovizer) (eventIdxInImpro t))
  (if (< eventIdxInImpro 2) 
      (reset-navigation-params self)
    (if (not (= eventIdxInImpro (1+ (CurrentImproIdx self))))
        (progn
          (setf (CurrentImproIdx self) (- eventIdxInImpro 1))
          (if (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))
            ; when a previous phase already generated something for this index
              (progn 
                (if *print-navig-basics* (format *om-stream* "---> GOING BACK TO STATE ~D IN ORACLE~%" (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))))
                (setf (CurrentStateIdx self) (StateIdx-at-ImproIdx-in-traceimpro self (CurrentImproIdx self)))
                ;MAJCONT 15/10/13
                (setf (continuity self) (Continuity-at-ImproIdx-in-traceimpro self (CurrentImproIdx self))))))))
                  ; --> sinon cas où au début on attaque directement pour la 1ere fois sur un ImproIdx > 1 
                  ; (par exemple parce qu'on attendait que la mémoire soit déjà un peu remplie)
  )

; GENERIQUE OK ... -> VOIR ImprovizeFormatSend_OnePhase
(defmethod navigate-realtime ((self RealtimeImprovizer) (NavigationOrder list) (eventduration t) (hostsend t) (portsend t) (adresssend simple-base-string) &optional numVoice)
  (let* ((SuffixScenario (nth 1 NavigationOrder))
         (eventIdxInImpro (nth 0 NavigationOrder)))
    (if (otext self 10)
        (let ((VoiceNum 1))
          (when numVoice (setf VoiceNum numVoice))
          (ImprovizeFormatSend_OnePhase self suffixgrid eventduration eventIdxInImpro hostsend portsend adresssend numVoice)))))

;===================================
;=  OVERLOADED IMPROVIZER METHODS 
;= (one step, keeping the improtrace)
;===================================
(defmethod Improvize-next-state ((self RealtimeImprovizer)  &optional (scenario nil))
  (Improvize-navigate-one-step self scenario)
  ;(if (> (CurrentStateIdx self) 0) 
  (if (and  (otext self (CurrentStateIdx self)) (> (CurrentStateIdx self) 0))
      (TransposeClonedEvent (otext self (CurrentStateIdx self)) (- (CurrentTranspo self)))
    (null-event self)))

(defmethod Improvize-navigate-one-step ((self RealtimeImprovizer)  &optional (scenario nil))
  (call-next-method)  
  ;26/10/13 NON !!!! PARALLELISME !!!! ICI DES QUE CALCULE INCREMENTE !!!
  (setf (CurrentImproIdx self) (1+ (CurrentImproIdx self)))
  (if *print-navig-basics* (format *om-stream* "ImproIdx ~D : " (CurrentImproIdx self)))
  (setf (gethash (CurrentImproIdx self) (improTrace self)) (list (CurrentStateIdx self) (continuity self)))
  ;;;;;
  (if (modeRT self) (setf (start-region self) (list (nth 0 (start-region self)) (min (- (maxetat self) 1) (+ 1 (nth 1 (start-region self))) )))) 
)


(defmethod Improvize-next-idx ((self RealtimeImprovizer)  &optional (scenario nil))
  (let* ((index (CurrentStateIdx self)) (previndex (PrevStateIdx self)) (nextindex nil)
         (scenario-current-transp (TransposeCloneLabelList scenario (CurrentTranspo self)))
         (label-current-transp (car scenario-current-transp))
         (links nil))
    (when *print-navig-basics* (display-infolabel self scenario))
    
    (cond
     
     ;Starting point
     ((and (zerop index) (setf nextindex (find-prefix-labels-match self scenario-current-transp)))
      (if *print-navig-basics* (format *om-stream* "Starting point in memory: ~a ~%" nextindex)))
     
     ;Navigation
     (t
      ;Update navigation mode
      (if (check-continuity self) 
          (setf (NavigationMode self) 'continuity)  
        (if t ;(available-suppleance self index) ;JNMR DEBILE
            (setf (NavigationMode self) 'suppleance)
          (setf (NavigationMode self) 'continuity)))
      
      ; MODE CONTINUITY
      (when (eq (NavigationMode self) 'continuity)
        (setf links (flink self index)
              nextindex (choose-factor-link self links label-current-transp))
        (if nextindex
            (progn
              (if *print-navig-basics* (format *om-stream* "c : ~a (~a / ~a) ~%" nextindex (continuity self) (max-continuity self)))    
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) nil))
          (if (available-suppleance self index) 
              (setf (NavigationMode self) 'suppleance)
            (setf (NavigationMode self) 'nothing))))
      ;MODE SUPPLEANCE
      (when (eq (NavigationMode self) 'suppleance)
        (setf nextindex (continuations-by-suppleance self index label-current-transp)) 
        (if (and nextindex (/= nextindex previndex))
            (progn 
              (if *print-navig-basics* (format *om-stream* "--->s : ~a ~%" nextindex))
              (setf (continuity self) 0)
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) nil))
          (progn
            (setf (NavigationMode self) 'nothing)
            (if *print-navig-basics* (format *om-stream*  "~a, " (NavigationMode self))))))
      ;MODE NOTHING
      (when (eq (NavigationMode self) 'nothing)
          (setf nextindex (find-prefix-labels-match self scenario-current-transp))
        (if nextindex 
            (progn 
              (if *print-navig-basics* (format *om-stream* "new : ~a " nextindex))
              (if (or (null scenario)) ;(zerop (CurrentTranspo self))) 
                  (if *print-navig-basics* (format *om-stream* " ~%")) (if *print-navig-basics* (format *om-stream* "new mem. transpo=~a ~%" (CurrentTranspo self))))
              (setf (continuity self) 0)
              ;For "real-time" navigation :"true" only if last step in navigation succesfully called "find-prefix-label-match"
              (setf (StartingPointNextPhaseFound? self) T))
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
              )

))))))
    nextindex))
 


        


