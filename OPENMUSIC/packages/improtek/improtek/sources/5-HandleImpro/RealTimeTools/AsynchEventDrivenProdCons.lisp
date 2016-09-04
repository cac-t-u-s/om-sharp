;(require-library 'bordeaux-threads)
(in-package :om)

;-------------------------------------------------------------------------
;http://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;-------------------------------------------------------------------------

;AsyncEvtDrivenProdConsSYSTEM
(defclass* AEDPCS ()
           (
            (running :initform t :initarg :running :accessor running)
            ;SharedData
            (sharedData :initform nil :initarg :sharedData :accessor sharedData)
            (LearnMethodOnSharedData :initform 'learnInAEDPCS :initarg :LearnMethodOnSharedData :accessor LearnMethodOnSharedData)
            (GenerateMethodOnSharedData :initform 'generateWithAEDPCS :initarg :GenerateMethodOnSharedData :accessor GenerateMethodOnSharedData)
            ;Locks and CV on sharedData
            (lock-on-SD :initform (bordeaux-threads:make-lock "lock-on-SD") :initarg :lock-on-SD :accessor lock-on-SD)
            (CV-on-SD :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-SD :accessor CV-on-SD)
            ;Events to learn channel
            (L-EventsToLearn :initform () :initarg :L-EventsToLearn :accessor L-EventsToLearn)
            (lock-on-L :initform (bordeaux-threads:make-lock "lock-on-L") :initarg :lock-on-L :accessor lock-on-L)
            (CV-on-L :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-L :accessor CV-on-L)
            ;Orders for navigation channel
            (G-NavigationOrders :initform () :initarg :G-NavigationOrders :accessor G-NavigationOrders)
            (lock-on-G :initform (bordeaux-threads:make-lock "lock-on-G") :initarg :lock-on-L :accessor lock-on-G)
            (CV-on-G :initform (bordeaux-threads:make-condition-variable) :initarg :CV-on-L :accessor CV-on-G)))


(defmethod NewAEDPCS ((sharedObject t) (learningMethod function) (generationMethod function))
  (let* ((newAedpcs (make-instance 'AEDPCS 
                                :sharedData sharedObject;(let ((improv (NewRealTimeImprovizer)))
                                    ; (setf (max-continuity improv) 1000)
                                     ;improv)
                                :LearnMethodOnSharedData learningMethod  ;#'learn-realtime 
                                :GenerateMethodOnSharedData generationMethod;#'(lambda (realTimeImpro NavigationOrder) 
                                                   ;    (navigate-realtime 
                                                    ;    realTimeImpro NavigationOrder 
                                                    ;    beatduration host_send port_send_ante numAntescofo))
                                )))newAedpcs))

 
  
(defmethod! L-fillEventsToLearnChannelWithEvent ((self AEDPCS) (evt t))
            ;;(format *om-stream* "(L-fillEventsToLearnChannel asks L... )~%")
            (bordeaux-threads:acquire-lock (lock-on-L self) t)
            ;;(format *om-stream* "(L-fillEventsToLearnChannel LOCK L )~%")
            ;Produire et remplir le canal
            ;(push evt (self L-EventsToLearn))
            ;;(format *om-stream* "L-GONNAFILLWITH ~a : ~%" evt)
            (setf (L-EventsToLearn self) (append (L-EventsToLearn self) (list evt)))
            ;;(format *om-stream* "Now (L-EventsToLearn self) = ~a : ~%" (L-EventsToLearn self))
            (bordeaux-threads:condition-notify (CV-on-L self))
            ;(format *om-stream* "(L-fillEventsToLearnChannel NOTIFY CVL, after setting L-EventsToLearn = ~a)~%" (L-EventsToLearn self))
            (bordeaux-threads:release-lock (lock-on-L self))
            ;;(format *om-stream* "(L-fillEventsToLearnChannel UNLOCK L)~%")
)

(defmethod! G-fillNavigationOrdersChannelWithEvent ((self AEDPCS) (evt t))
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel asks G...)~%")
          (bordeaux-threads:acquire-lock (lock-on-G self) t)
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel LOCK G)~%")
          ;Produire et remplir le canal
          ;(push i G-NavigationOrders)
                      ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ; BRICOLAGE !
            ; ICI ON OUBLIE TOUT POUR NE GARDER QUE LE DERNIER INDICE (POUR NE PAS LANCER TOUS LES CALCULS CONTIGUS PRECEDENTS)
            ; -> DONC TRES PARTICULIER A CETTE UTILISATION, PAS GENERIQUE
            ; ET PUIS TOUT CA DEVRAIT ETRE GERE DANS UNE METHODE PROPRE A CHAQUE CLASSE, COMME POUR GENERATE-EVENT...
          ;(setf (G-NavigationOrders self) (append (G-NavigationOrders self) (list evt)))
          (setf (G-NavigationOrders self) (list evt))
          
          (bordeaux-threads:condition-notify (CV-on-G self))
          ;(format *om-stream* "(G-fillNavigationOrdersChannel NOTIFY CVG, after setting G-NavigationOrders = ~a )~%" (G-NavigationOrders self))
          (bordeaux-threads:release-lock (lock-on-G self))
          ;;(format *om-stream* "(G-fillNavigationOrdersChannel UNLOCK G)~%")
          )

(defmethod! L-processLearning ((self AEDPCS))
  (let ((OwnsLockSD nil) (OwnsLockL nil))
    ;(loop for i from 1 to 50 do
    (loop while (running self) do
        (progn
          (if (not OwnsLockL)
              (progn
                ;;(format *om-stream* "(L-processLearning asks L...)~%")
                (setf OwnsLockL (bordeaux-threads:acquire-lock (lock-on-L self) t))
                ;;(format *om-stream* "(L-processLearning LOCK L)~%")
                ))
          
          ;Traiter les évènements
          ;Vider le canal
          (if OwnsLockL
              (progn
                (if (not OwnsLockSD)
                    (progn
                      ;;(format *om-stream* "(L-processLearning asks SD...)~%")
                      (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self) t))
                      ;;(format *om-stream* "(L-processLearning LOCK SD)~%")
                      ))
                (loop while (car (L-EventsToLearn self)) do
                        (learn-event self (pop (L-EventsToLearn self))))
                (bordeaux-threads:condition-notify (CV-on-SD self)) 
                ;(format *om-stream* "(L-processLearning NOTIFY CVSD, after setting sharedData = ~a)~%" (sharedData self))
                (bordeaux-threads:release-lock (lock-on-SD self))
                (setf OwnsLockSD nil)
                ;;(format *om-stream* "(L-processLearning UNLOCK SD)~%")
                ))

          ;Si le canal est vide -> CV-wait
          (loop while (not (L-EventsToLearn self)) do
                ;"the caller must always test on waking that there is threading to be done, instead of assuming that it can go ahead."
                (progn 
                  ;;(format *om-stream* "(L-processLearning sleeps waiting L CVL...)~%")
                  ;VU LA FONCTION, SUREMENT PAS BESOIN, c'est plutôt le suivant qui doit provoquer le deadlock
                  ;**********************************************************************************
                  (if OwnsLockSD 
                      (progn
                        (bordeaux-threads:release-lock (lock-on-SD self))
                        (setf OwnsLockSD nil)
                        ;;(format *om-stream* "(L-processLearning and so releases SD that it held...)~%")
                        ))
                  ;**********************************************************************************
                  (bordeaux-threads:condition-wait (CV-on-L self) (lock-on-L self))))
          (setf OwnsLockL t)
          ;;(format *om-stream* "(L-processLearning WOKE UP => LOCK L)~%")  
          ;**********************************************************************************
          ;;(format *om-stream* "(L-processLearning WOKE UP => ask SD...)~%") 
          (bordeaux-threads:acquire-lock (lock-on-SD self))
          ;;(format *om-stream* "(L-processLearning LOCK SD)~%") 
          (setf OwnsLockSD t)
          ;**********************************************************************************  
          ;(sleep 0.2)
          ;(sleep 0.05)
          ))
    ;(bordeaux-threads:release-lock (lock-on-L self))
    ;(bordeaux-threads:release-lock (lock-on-SD self))
    ))
 

(defmethod! G-processNavigation ((self AEDPCS))
  (let ((OwnsLockSD nil) (OwnsLockG nil))
    ;(loop for i from 1 to 40 do
    (loop while (running self) do
          (progn
            (if (not OwnsLockG)
                (progn
                  ;;(format *om-stream* "(G-processNavigation asks G...)~%")
                  (setf OwnsLockG (bordeaux-threads:acquire-lock (lock-on-G self) t))
                  ;;(format *om-stream* "(G-processNavigation LOCK G)~%")
                  ))
            ;Traiter les évènements
            ;Vider le canal
            (if (not OwnsLockSD)
                (progn   
                  ;;(format *om-stream* "(G-processNavigation asks SD...)~%")
                  (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self) t))
                  ;;(format *om-stream* "(G-processNavigation LOCK SD)~%")
                  ))
              
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ;/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
            ; BRICOLAGE !
            ; ICI ON OUBLIE TOUT POUR NE GARDER QUE LE DERNIER INDICE (POUR NE PAS LANCER TOUS LES CALCULS CONTIGUS PRECEDENTS)
            ; -> DONC TRES PARTICULIER A CETTE UTILISATION, PAS GENERIQUE
            ; ET PUIS TOUT CA DEVRAIT ETRE GERE DANS "GENERATE EVENT" POUR QUE CELA PUISSE ETRE DEFINI A CHAQUE FOIS
            ;=====>> REFAIRE GENERATE-EVENT PRENANT POUR ARGUMENT LA liste ET PAS JUSTE SA TETE
            (loop while (car (G-NavigationOrders self)) do
                  (generate-event self (pop (G-NavigationOrders self))))
            
            ;;(format *om-stream* "(G-processNavigation sleeps waiting SD CVSD...)~%")
            ;**********************************************************************************
            (if OwnsLockG 
                (progn
                  (bordeaux-threads:release-lock (lock-on-G self))
                  (setf OwnsLockG nil)
                  ;;(format *om-stream* "(G-processNavigation  and so releases G that it held...)~%")
                  ))
            ;**********************************************************************************
            (bordeaux-threads:condition-wait (CV-on-SD self) (lock-on-SD self))
            (setf OwnsLockSD t)
            ;;(format *om-stream* "(G-processNavigation WOKE UP => LOCK SD )~%")
            ;**********************************************************************************
            (if (not OwnsLockG)
                (progn
                  ;;(format *om-stream* "(G-processNavigation WOKE UP => ask G...)~%") 
                  (setf OwnsLockG (bordeaux-threads:acquire-lock (lock-on-G self)))
                  ;;(format *om-stream* "G-processNavigation LOCK G)~%")
                  )) 
            ;**********************************************************************************  
            
            ;Si le canal est vide -> CV-wait
            (loop while (not (car (G-NavigationOrders self))) do
                (progn
                  ;(format *om-stream* "(G-processNavigation sleeps waiting G CVG)~%")
                  ;**********************************************************************************
                  (if OwnsLockSD 
                      (progn
                        (bordeaux-threads:release-lock (lock-on-SD self))
                        (setf OwnsLockSD nil)
                        ;;(format *om-stream* "(G-processNavigation  and so releases SD that it held...)~%")
                        ))
                  ;**********************************************************************************
                  (bordeaux-threads:condition-wait (CV-on-G self) (lock-on-G self))))
            (setf OwnsLockG t)
            ;;(format *om-stream* "(G-processNavigation WOKE UP => LOCK G)~%")
            ;**********************************************************************************
            (if (not OwnsLockSD)
                (progn
                  ;;(format *om-stream* "(G-processNavigation WOKE UP => ask SD...)~%") 
                  (setf OwnsLockSD (bordeaux-threads:acquire-lock (lock-on-SD self)))
                  ;;(format *om-stream* "G-processNavigation LOCK SD)~%")
                  ))
            
            ;**********************************************************************************  
            ;(sleep 0.2)
            ))
    ;(bordeaux-threads:release-lock (lock-on-G self))
    ;(bordeaux-threads:release-lock (lock-on-SD self))
    ))


(defmethod learn-event ((self AEDPCS) (event-to-learn t))
            (apply (LearnMethodOnSharedData self) (list (sharedData self) event-to-learn )))
(defmethod generate-event ((self AEDPCS) (GenerationOrder t))
   (apply (GenerateMethodOnSharedData self) (list (sharedData self) GenerationOrder)))

 

;========================================================================================================================================================================================================
;========================================================================================================================================================================================================
;========================================================================================================================================================================================================

; AsyncEvtDrivenProdConsENVIRONMENT
; ((Plus juste le système en lui-même, contient en plus les THREADS qui travailleront dessus, et les infos de l'OSC dont on recevra élements à apprendre et requètes.))
; UPDATE :: MAINTENANT PLUS HERITAGE, MAIS DANS LA CLASSE CI-DESSOUS UN _CHAMP_ DE TYPE AEDPCS
(defclass* OSCenvforAEDPCS ()
  ((RealTimeSystem :initform nil :initarg :RealTimeSystem :accessor RealTimeSystem) 
   (ThreadRcvLearning :initform nil :initarg :ThreadRcvLearning :accessor ThreadRcvLearning) 
   (ThreadRcvNavigOrder :initform nil :initarg :ThreadRcvNavigOrder :accessor ThreadRcvNavigOrder)
   (ThreadProcessLearning :initform nil :initarg :ThreadProcessLearning :accessor ThreadProcessLearning)
   (ThreadProcessNavig :initform nil :initarg :ThreadProcessNavig  :accessor ThreadProcessNavig )
   (OSChost :initform "127.0.0.1" :initarg :OSChost :accessor OSChost)
   (OSCSendPort :initform 3008 :initarg :OSCSendPort :accessor OSCSendPort)
   (OSCSendAdress :initform "/send" :initarg :OSCSendAdress :accessor OSCSendAdress)
   (OSCRcvPort_Learn :initform 7415 :initarg :OSCRcvPort_Learn :accessor OSCRcvPort_Learn)
   (OSCRcvAdress_Learn :initform "/rcvLearn" :initarg :OSCRcvAdress_Learn :accessor OSCRcvAdress_Learn)
   (OSCRcvPort_GenQuery :initform 7416 :initarg :OSCRcvPort_GenQuery :accessor OSCRcvPort_GenQuery)
   (OSCRcvAdress_GenQuery :initform "/rcvGenQuery" :initarg :OSCRcvAdress_GenQuery :accessor OSCRcvAdress_GenQuery)
   (ServerRcvLearning :initform nil :initarg :ServerRcvLearning :accessor ServerRcvLearning) 
   (ServerRcvGenQuery :initform nil :initarg :ServerRcvGenQuery :accessor ServerRcvGenQuery)))
  

;(defun NewAEDPCS ((sharedObject t) (learningMethod function) (generationMethod function))


(defmethod NewOSCenvforAEDPCS ((RealTimeSystem AEDPCS) (OSChost t) (OSCSendPort fixnum) (OSCSendAdress simple-base-string) (OSCRcvPort_Learn fixnum) (OSCRcvAdress_Learn simple-base-string) (OSCRcvPort_GenQuery fixnum) (OSCRcvAdress_GenQuery simple-base-string))
  (let* ((OscEnv (make-instance 'OSCenvforAEDPCS 
                                :RealTimeSystem RealTimeSystem
                                :OSChost OSChost
                                :OSCSendPort OSCSendPort
                                :OSCSendAdress OSCSendAdress
                                :OSCRcvPort_Learn OSCRcvPort_Learn
                                :OSCRcvAdress_Learn OSCRcvAdress_Learn
                                :OSCRcvPort_GenQuery OSCRcvPort_GenQuery
                                :OSCRcvAdress_GenQuery OSCRcvAdress_GenQuery)))
    
    (setf (ThreadRcvLearning OscEnv) (bordeaux-threads:make-thread #'(lambda () 
                                                                       (setf (ServerRcvLearning OscEnv)
                                                                             (om-start-osc-server (OSCRcvPort_Learn OscEnv) (OSChost OscEnv) 
                                                                                                  #'(lambda (message host) 
                                                                                                      ;(handle-evts-to-learn-from-osc (RealTimeSystem OscEnv) (om-decode-msg-or-bundle message)) nil)))) :name 'L-filler))
                                                                                                      (handle-evts-to-learn-from-osc OscEnv (om-decode-msg-or-bundle message)) nil)))) :name 'L-filler))
    (setf (ThreadRcvNavigOrder OscEnv) (bordeaux-threads:make-thread #'(lambda () 
                                                                         (setf (ServerRcvGenQuery OscEnv)
                                                                               (om-start-osc-server (OSCRcvPort_GenQuery OscEnv) (OSChost OscEnv)
                                                                                                    #'(lambda (message host) 
                                                                                                        ;(handle-navig-order-from-osc (RealTimeSystem OscEnv) (om-decode-msg-or-bundle message)) nil)))) :name 'G-filler))
                                                                                                        (handle-navig-order-from-osc OscEnv (om-decode-msg-or-bundle message)) nil)))) :name 'G-filler))
    (setf (ThreadProcessLearning OscEnv) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem OscEnv))) :name 'L-processer))
    (setf (ThreadProcessNavig OscEnv) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem OscEnv))) :name 'G-processer))
    OscEnv))

 

(defmethod NewAEDPCS-in-New-OSC-env ((sharedObject t) (learningMethod function) (generationMethod function) 
                              (OSChost t) (OSCSendPort fixnum) (OSCSendAdress simple-base-string) (OSCRcvPort_Learn fixnum) (OSCRcvAdress_Learn simple-base-string) (OSCRcvPort_GenQuery fixnum) (OSCRcvAdress_GenQuery simple-base-string))
  (let ((aedcps (NewAEDPCS sharedObject learningMethod generationMethod)))
  (NewOSCenvforAEDPCS
   aedcps
   OSChost OSCSendPort OSCSendAdress OSCRcvPort_Learn OSCRcvAdress_Learn OSCRcvPort_GenQuery OSCRcvAdress_GenQuery)))

 

(defmethod! NewRealtimeImprovizer-in-OSC-env ((beatduration fixnum) (OSChost t) (OSCSendPort fixnum) (OSCSendAdress simple-base-string) (OSCRcvPort_Learn fixnum) (OSCRcvAdress_Learn simple-base-string) (OSCRcvPort_GenQuery fixnum) (OSCRcvAdress_GenQuery simple-base-string))
  (NewAEDPCS-in-New-OSC-env
   (let ((improv (NewRealTimeImprovizer))) (setf (max-continuity improv) 1000) improv)
   #'learn-realtime
   #'(lambda (realTimeImpro NavigationOrder) 
       (navigate-realtime 
        realTimeImpro NavigationOrder beatduration OSChost OSCSendPort OSCSendAdress))
   OSChost
   OSCSendPort
   OSCSendAdress
   OSCRcvPort_Learn
   OSCRcvAdress_Learn
   OSCRcvPort_GenQuery
   OSCRcvAdress_GenQuery))

(defmethod end-realtime ((self OSCenvforAEDPCS))
  (if self
      (progn
 ; TODO : KILL THREADS !!!!!!
        (if (ServerRcvLearning self)
            (progn
              (om-stop-osc-server (ServerRcvLearning self))
              (setf (ServerRcvLearning self) nil)))
        (if (ServerRcvGenQuery self)
            (progn
              (om-stop-osc-server (ServerRcvGenQuery self))
              (setf (ServerRcvGenQuery self) nil))))))
 

;===================================================================================================
; METHODE DE LA CLASSE TUNE !!!!!!!
; A DEPLACER ENSUITE CAR APPLICATION PARTICULIERE, ET NON PAS METHODE D'UNE DES CLASSES DEFINIES ICI 
;===================================================================================================
(defmethod! LaunchRealtimeImpro ((self tune) (OSChost t) (OSCSendPort fixnum) (OSCSendAdress simple-base-string) (OSCRcvPort_Learn fixnum) (OSCRcvAdress_Learn simple-base-string) (OSCRcvPort_GenQuery fixnum) (OSCRcvAdress_GenQuery simple-base-string))
(if (RealtimeImprovizer-in-OSC-env self) (end-realtime (RealtimeImprovizer-in-OSC-env self)))
(setf (RealtimeImprovizer-in-OSC-env self)  
      (NewRealtimeImprovizer-in-OSC-env 
       (beatduration self) OSChost OSCSendPort OSCSendAdress OSCRcvPort_Learn OSCRcvAdress_Learn OSCRcvPort_GenQuery OSCRcvAdress_GenQuery)))
  
   
  
  #|
(defmethod launch-realtime ((self AEDPCE) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
  (end-realtime self)
  (setf (RealTimeSystem self) 
        (make-instance 'AEDPCS 
                       :sharedData (let ((improv (NewRealTimeImprovizer)))
                                     (setf (max-continuity improv) 1000)
                                     improv)
                       :LearnMethodOnSharedData #'learn-realtime 
                       :GenerateMethodOnSharedData #'(lambda (realTimeImpro NavigationOrder) 
                                                       (navigate-realtime ;;;;;;;;;;;;;;;;;;;;;;;;;;-------------REMARQUE DU 19 mai 2014 : FAIRE PLUS MODULAIRE !!! DISSOCIER FONCTIONS DE NAVIGATION PAS PAR PAS ET ENVOIE ANTESCOFO !!!!
                                                        realTimeImpro NavigationOrder 
                                                        beatduration host_send port_send_ante numAntescofo))))

  (setf (ThreadRcvLearning self) (bordeaux-threads:make-thread #'(lambda () 
                                                                   (setf (ServerRcvLearning self)
                                                                         (om-start-osc-server port_rcv_learn host_send 
                                                                                              #'(lambda (message host) 
                                                                                                  (handle-evts-to-learn-from-osc (RealTimeSystem self) (om-decode-msg-or-bundle message) "/learnforTR") nil)))) :name 'L-filler)
        (ThreadRcvNavigOrder self) (bordeaux-threads:make-thread #'(lambda () 
                                                                     (setf (ServerRcvGenQuery self)
                                                                           (om-start-osc-server port_rcv_navig_order host_send 
                                                                                                #'(lambda (message host) 
                                                                                                    (handle-navig-order-from-osc (RealTimeSystem self) (om-decode-msg-or-bundle message) "/antescofo/improvize_next_steps") nil)))) :name 'G-filler)
        (ThreadProcessLearning self) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer)
        (ThreadProcessNavig self) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))
|#







;-------------------------------------------------------------
; MODIF 12/05/14
; Formerly " handle-evts-to-learn-from-max "
; 'RealTimeImprovizer.lisp --> AsynchEventDrivenProdCons.lisp'
;-------------------------------------------------------------
(defmethod! handle-evts-to-learn-from-osc ((self OSCenvforAEDPCS) (message list)); (osc-adress simple-base-string))
  ;(format *om-stream* "~% *********************** handle-evts-to-learn-from-max RECEIVED_FROM_MAX : ~a~%" message)
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  
  ;(if (= *print_received_from_Max* 1) 
  ;    (progn
  ;      (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
  ;      (format *om-stream* "Type : ~a~%" (type-of message))
  ;      (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
  ;      (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
  ;      (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))

  ;(if (string= (car message) "/learnforTR")
  ;(if (string= (car message) osc-adress )
  (if (string= (car message) (OSCRcvAdress_Learn self))
          (L-fillEventsToLearnChannelWithEvent (RealTimeSystem self) (eval (read-from-string (nth 1 message))))))

 
;-------------------------------------------------------------
; MODIF 12/05/14
; Formerly " handle-navig-order-from-max "
; 'RealTimeImprovizer.lisp --> AsynchEventDrivenProdCons.lisp'
;-------------------------------------------------------------
(defmethod! handle-navig-order-from-osc ((self OSCenvforAEDPCS) (message list)); (osc-adress simple-base-string))
  ;(format *om-stream* "~% *********************** handle-navig-order-from-max RECEIVED_FROM_MAX : ~a~%" message)
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  #|
  (if (= *print_received_from_Max* 1) 
      (progn
        (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
        (format *om-stream* "Type : ~a~%" (type-of message))
        (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
        (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
        (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))
  |#

  ; /!\/!\ATTENTION ! PRESUPPOSE SUR FORMAT DU MESSAGE (IDX, REQUETE EN ELLE MEME)/!\/!\
  (let ((query_idx (nth 1 message)) 
        (query (eval (read-from-string (nth 2 message)))))
    ;(if (string= (car message) "/antescofo/improvize_next_steps")
    ;(if (string= (car message) osc-adress)
    (if (string= (car message) (OSCRcvAdress_GenQuery self))
        (G-fillNavigationOrdersChannelWithEvent (RealTimeSystem self) (list query_idx query)))))

#|
(setf message '("/learnforTR" "'( (84 5486 623 47 8) (12 5530 108 100 16) (5 5530 101 101 16) (12 5786 108 100 16) (5 5849 101 101 16) (12 6094 108 100 16) (5 6168 101 101 16) (80 6096 295 53 8) (12 6435 108 100 16) (7 6487 101 102 16) (79 6406 335 62 8) (7 6806 101 102 16) )"))

(format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
(format *om-stream* "Type : ~a~%" (type-of message))
(format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
(format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
(format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))

(setf evts-to-learn (eval (read-from-string (nth 1 message))))
(type-of evts-to-learn)

(let (evts-to-learn (eval (read-from-string (nth 1 message))))
    (if (string= (car message) "/learnforTR")
        (progn
          (format *om-stream* "$*$*$*$*$* /Indeed learnforTR ===>~%$*$*$*$*$* Giving ~a as argument to L-Fillevents...~%" evts-to-learn)
          )))
|#


#|
;-----------------------------------------------------------
;-----------------------------------------------------------
;ANCIENNEMENT METHODE DE LA CLASSE "TUNE"
;ICI REFAIT POUR NOUVELLE CLASSE OSCenvforAEDPCS, MAIS IL FAUDRA BIEN VERIFIER SI TOUT EST OK QUAND ON AURA JUSTE UN """ OSCenvforAEDPCS """"... 
;...A INTEGRER DANS LA CLASSE TUNE. EN PARTICULIER VOIR QUOI FAIRE AVEC LA METHODE "end-realtime" de la classe tune !!!!!!
; En fait je crois que je l'ai déjà transféré à la classe OSCenvforAEDPCS
;-----------------------------------------------------------
;-----------------------------------------------------------
(defmethod launch-realtime ((self OSCenvforAEDPCS) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
  (end-realtime self)
  (setf (RealTimeSystem self) 
        (make-instance 'AEDPCS 
                       :sharedData (let ((improv (NewRealTimeImprovizer)))
                                     (setf (max-continuity improv) 1000)
                                     improv)
                       :LearnMethodOnSharedData #'learn-realtime 
                       :GenerateMethodOnSharedData #'(lambda (realTimeImpro NavigationOrder) 
                                                       (navigate-realtime 
                                                        realTimeImpro NavigationOrder 
                                                        beatduration host_send port_send_ante numAntescofo))))

  (setf (ThreadRcvLearning self) (bordeaux-threads:make-thread #'(lambda () 
                                                                   (setf (ServerRcvLearning self)
                                                                         (om-start-osc-server port_rcv_learn host_send 
                                                                                              #'(lambda (message host) 
                                                                                                  (handle-evts-to-learn-from-osc (RealTimeSystem self) (om-decode-msg-or-bundle message) "/learnforTR") nil)))) :name 'L-filler)
        (ThreadRcvNavigOrder self) (bordeaux-threads:make-thread #'(lambda () 
                                                                     (setf (ServerRcvGenQuery self)
                                                                           (om-start-osc-server port_rcv_navig_order host_send 
                                                                                                #'(lambda (message host) 
                                                                                                    (handle-navig-order-from-osc (RealTimeSystem self) (om-decode-msg-or-bundle message) "/antescofo/improvize_next_steps") nil)))) :name 'G-filler)
        (ThreadProcessLearning self) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer)
        (ThreadProcessNavig self) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))
|#


