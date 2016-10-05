

(in-package :om)

(defclass* realtimetune (tune)
  (
   
   (RealTimeSystem :initform nil :initarg :RealTimeSystem :accessor RealTimeSystem) 
   (ThreadRcvLearning :initform nil :initarg :ThreadRcvLearning :accessor ThreadRcvLearning) 
   (ThreadRcvNavigOrder :initform nil :initarg :ThreadRcvNavigOrder :accessor ThreadRcvNavigOrder)
   (ThreadProcessLearning :initform nil :initarg :ThreadProcessLearning :accessor ThreadProcessLearning)
   (ThreadProcessNavig :initform nil :initarg :ThreadProcessNavig  :accessor ThreadProcessNavig )
   (ServerRcvLearning :initform nil :initarg :ServerRcvLearning :accessor ServerRcvLearning) 
   (ServerRcvNavigOrder :initform nil :initarg :ServerRcvNavigOrder :accessor ServerRcvNavigOrder)
   
   (DataFromAnalysis :initform nil :initarg :DataFromAnalysis :accessor DataFromAnalysis)
   
   ))



;=============================================================================================================================================================================================================
;NOUVEAU
;=============================================================================================================================================================================================================

(defmethod end-realtime ((self realtimetune))
  ; TODO : KILL THREADS !!!!!!
  (if (ServerRcvLearning self)
      (progn
        (om-stop-osc-server (ServerRcvLearning self))
        (setf (ServerRcvLearning self) nil)))

  (if (ServerRcvNavigOrder self)
      (progn
        (om-stop-osc-server (ServerRcvNavigOrder self))
        (setf (ServerRcvNavigOrder self) nil))))

#|
(defmethod launch-realtime-midi ((self realtimetune) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
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
                                                                                                  (handle-evts-to-learn-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'L-filler)
        (ThreadRcvNavigOrder self) (bordeaux-threads:make-thread #'(lambda () 
                                                                     (setf (ServerRcvNavigOrder self)
                                                                           (om-start-osc-server port_rcv_navig_order host_send 
                                                                                                #'(lambda (message host) 
                                                                                                    (handle-navig-order-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'G-filler)
        (ThreadProcessLearning self) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer)
        (ThreadProcessNavig self) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))


(defmethod launch-realtime-audio ((self realtimetune) (beatduration integer) (host_send t) (port_send_ante integer) (numAntescofo integer) (port_rcv_learn integer) (port_rcv_navig_order integer))
  (end-realtime self)
  (setf (RealTimeSystem self) 
        (make-instance 'AEDPCS 
                       :sharedData (let ((improv (NewRealTimeImprovizer)))
                                     (setf (max-continuity improv) 1000)
                                     improv)
                       :LearnMethodOnSharedData #'learn-realtime-audio ;AUDIOBEAT.LISP 
                       :GenerateMethodOnSharedData #'(lambda (realTimeImpro NavigationOrder) 
                                                       (navigate-realtime 
                                                        realTimeImpro NavigationOrder 
                                                        beatduration host_send port_send_ante numAntescofo))))

  (setf (ThreadRcvLearning self) (bordeaux-threads:make-thread #'(lambda () 
                                                                   (setf (ServerRcvLearning self)
                                                                         (om-start-osc-server port_rcv_learn host_send 
                                                                                              #'(lambda (message host) 
                                                                                                  (handle-evts-to-learn-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'L-filler)
        (ThreadRcvNavigOrder self) (bordeaux-threads:make-thread #'(lambda () 
                                                                     (setf (ServerRcvNavigOrder self)
                                                                           (om-start-osc-server port_rcv_navig_order host_send 
                                                                                                #'(lambda (message host) 
                                                                                                    (handle-navig-order-from-max (RealTimeSystem self) (om-decode-msg-or-bundle message)) nil)))) :name 'G-filler)
        (ThreadProcessLearning self) (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer)
        (ThreadProcessNavig self) (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))
|#





  



#|
  (let* (
         (L-filler (bordeaux-threads:make-thread #'(lambda () 
                                                     (om-start-osc-server port_rcv_learn host_send 
                                                                          #'(lambda (mess host) 
                                                                              (handle-evts-to-learn-from-max (RealTimeSystem self) (om-decode-msg-or-bundle mess)) nil))) :name 'L-filler))
         (G-filler (bordeaux-threads:make-thread #'(lambda () 
                                                     (om-start-osc-server port_rcv_navig_order host_send 
                                                                          #'(lambda (mess host) 
                                                                              (handle-navig-order-from-max (RealTimeSystem self) (om-decode-msg-or-bundle mess)) nil))) :name 'G-filler))
         (L-processer (bordeaux-threads:make-thread #'(lambda () (L-processLearning (RealTimeSystem self))) :name 'L-processer))
         (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation (RealTimeSystem self))) :name 'G-processer)))))
|#



#|
(defmethod handle-evts-to-learn-from-max ((self AEDPCS) (message list))
  (if (not (eq (type-of (car message)) 'simple-base-string)) (progn (pop message) (setf message (car message))))
  (if (= *print_received_from_Max* 1) 
      (progn
        (format *om-stream* "~% RECEIVED_FROM_MAX : ~a~%" message)
        (format *om-stream* "Type : ~a~%" (type-of message))
        (format *om-stream* "Car : ~a, de type : ~a~%" (car message) (type-of (car message)))
        (format *om-stream* "Cdr : ~a, de type : ~a~%" (cdr message) (type-of (cdr message)))
        (format *om-stream* "nth 1 : ~a, de type : ~a~%" (nth 1 message) (type-of (nth 1 message)))))
  (let (evts-to-learn (eval (read-from-string (nth 1 message))))
    (if (string= (car message) "/learnforTR")
        (L-fillEventsToLearnChannelWithEvent (RealTimeSystem self) evts-to-learn))))


;VIEUX TEST DE MERDE
;--------------------
(defmethod write-in-list ((self list) (i integer))
  (nconc self (list i) ))
(defmethod read-in-list ((self list) (i integer)) 
  (format *om-stream* "---->Reading ~D : ~D ~%~%" (- i 1) (nth (- i 1) self)))
(setf PCS (make-instance 'AEDPCS :sharedData '(0 1 2 3 4 5 6 7 8 9) :LearnMethodOnSharedData 'write-in-list :GenerateMethodOnSharedData 'read-in-list ))


(setf thread 
      (bordeaux-threads:make-thread #'(lambda () 
                                        (om-start-osc-server 7410 "127.0.0.1" 
                                                             #'(lambda (message host) 
                                                                 (handle-evts-to-learn-from-max PCS (om-decode-msg-or-bundle message)) nil))) :name 'L-filler))
|#




;JEROME 26/10/13 POUR TR
(defmethod generate-grid-param ((self realtimetune) beatdur)
;;GENERICITE : A REPRENDRE
#|            
  ;from "generate-grid"
            (let* ((harmgrid (simplegrid self))
                   (gridlength (list-length harmgrid))
                   (beatlist (loop for x in harmgrid collect (make-instance 'midiharmbeat :Label (NewHarmlabel (nth 0 x) (nth 1 x)))))
                   (beatduration (beatduration self))
                   (absolute-path-filename (make-pathname :directory (append (pathname-directory (tunedir self)) (list (tunename self) "realtime"))
                                                          :name (format nil "~a-grid-param.txt" (tunename self))))
                   (suffixes "")
                   (midicodes1 "")
                   (midicodes2 "")
                   (suffix nil)
                   (midicode nil)
                   (j 0))
              
  ;from "save-for-antescofo2"
              (ensure-directories-exist absolute-path-filename)
              (with-open-file 
                  (ss absolute-path-filename 
                      :direction :output :if-exists :supersede)
                (setf beatlist (transfer-syncopated-event beatlist beatduration))
                (setf beatlist (add-grid-to-midiharmbeatlist beatlist beatduration))
                
                (loop with c-beatlist = (mapcar #'clone-object beatlist)
                      for beat in c-beatlist for nextbeat in (append (cdr c-beatlist) (list nil)) for i from 1
                      do (progn 
                           (setf (MidiSet beat) (sort (MidiSet beat) #'<= :key #'second)) 
                           (setf j 0)
                           (loop for previousevent in (cons '(60 0 1000 120 1) (MidiSet beat)) for event in (MidiSet beat)
                                 do 
                                 (progn 
                                   (incf j)
                                   (setf midicode (format nil "(~d, \"~a ~a 0.0.~a ~a\")" 
                                                         i 
                                                         (MEPitch event)     
                                                         (MEVel event) 
                                                         (round (* (/ (MEDur event) beatduration) 480))
                                                         (MEChannel event)))
                                   (if (= j 1)
                                       (setf midicodes1 (concatenate 'string midicodes1 midicode))
                                     (if (= j 2)
                                       (setf midicodes2 (concatenate 'string midicodes2 midicode))
                                     ))))
                                 (setf suffix (format nil "(~d,\"\'~a\")" i harmgrid))
                                 (setf suffixes (concatenate 'string suffixes suffix))
                                 ; JEROME 01/09/13
                                 ; POUR POUVOIR FAIRE IMPROS SUR PLUSIEURS GRILLES DE SUITES : CIRCULAIRE !
                                 ;(pop harmgrid)
                                 ;(if harmgrid (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                                 (setf harmgrid (rotate harmgrid))
                                 (if nextbeat (progn (setf suffixes (concatenate 'string suffixes ",") midicodes1 (concatenate 'string midicodes1 ",") midicodes2 (concatenate 'string midicodes2 ","))))))
                      ;(format ss "EVENT 1.0~%")
                      ;(format ss "group {~%")
                      (format ss "$suffixes := MAP{")
                      ;QUOTE AU DEBUT ??
                      (format ss "~a}~%~%" suffixes)
                      (format ss "$midicodes1 := MAP{")
                      (format ss "~a}~%~%" midicodes1)
                      (format ss "$midicodes2 := MAP{")
                      (format ss "~a}~%~%" midicodes2)
                      (format ss "$gridlength := ~d~%" gridlength)
                      ;(format ss "}~%")
                      ))
|#
)
            
#|
(setf tune Jaime_tune)
(generate-grid-param tune (beatduration tune))
|#







