 (in-package :om)
;-------------------------------------------------------------------------
;http://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;-------------------------------------------------------------------------

;===============================================
;MAKE-THREAD SANS ARGUMENT
(defun lion () 
  (format *om-stream* "Dans la jungle le lion est mort ce soir~%"))

(bordeaux-threads:make-thread 'lion :name '1seullion)

;===============================================
bordeaux-threads:*default-special-bindings*

;===============================================
; MAKE-THREAD AVEC ARGUMENTS
(defun lions (a) 
  (format *om-stream* "Dans la jungle ~D lions sont morts ce soir~%" a))

#|
ATTENTION, ERREUR SI APPELE DE CETTE MANIERE :
(bordeaux-threads:make-thread (lions 23) :name 'pleindlions)

BONNE SYNTAXE :
|#
(bordeaux-threads:make-thread #'(lambda () (lions 23)) :name 'pleindlions)

;===============================================
;CURRENT-THREAD 
(bordeaux-threads:current-thread) ;->Fil principal

(defun lions2 (a) 
  (format *om-stream* "Dans la jungle ~D lions sont morts ce soir~%" a)
  (bordeaux-threads:current-thread))

(bordeaux-threads:make-thread #'(lambda () (lions2 65)) :name 'saymyname);->Saymyname

;===============================================
;LOCKS
(defvar mylock (bordeaux-threads:make-lock "lock1"))

(setf data 0)

(defun data-plus (a N)
  (loop for i from 1 to N do
        (progn
          (bordeaux-threads:acquire-lock mylock t)
          (format *om-stream* "WRITING PLUS~%")
          (setf data (+ data a))
          (bordeaux-threads:release-lock mylock)
          (sleep 0.5)
          )))

(defun data-moins (a N)
  (loop for i from 1 to N do
        (progn
          (bordeaux-threads:acquire-lock mylock t)
          (format *om-stream* "WRITING MOINS~%")
          (setf data (- data a))
          (bordeaux-threads:release-lock mylock)
          (sleep 0.5)
          )))


(defun data-read (N)
  (loop for i from 1 to N do
        (progn
          (bordeaux-threads:acquire-lock mylock t)
          (format *om-stream* "DATA = ~D~%" data)
          (bordeaux-threads:release-lock mylock)
          (sleep 0.1)
          )))

(let ((read (bordeaux-threads:make-thread #'(lambda () (data-read 40)) :name 'reader))
      (plus (bordeaux-threads:make-thread #'(lambda () (data-plus 2 10)) :name 'writerplus))
      (moins (bordeaux-threads:make-thread #'(lambda () (data-moins 1 10)) :name 'writermoins))))

;===============================================
;CONDITION VARIABLES

#|
A condition variable provides a mechanism for threads to put themselves to sleep while waiting for the state of something to change, then to be subsequently woken by another thread which has changed the state.

A condition variable must be used in conjunction with a lock to protect access to the state of the object of interest. The procedure is as follows:

Suppose two threads A and B, and some kind of notional event channel C. A is consuming events in C, and B is producing them. CV is a condition-variable

- A acquires the lock that safeguards access to C
- A threads and removes all events that are available in C
- When C is empty, A calls CONDITION-WAIT, which atomically releases the lock and puts A to sleep on CV
- Wait to be notified; CONDITION-WAIT will acquire the lock again before returning
- Loop back to step 2, for as long as threading should continue


When B generates an event E, it

- acquires the lock guarding C
- adds E to the channel
- calls CONDITION-NOTIFY on CV to wake any sleeping thread
- releases the lock


/!\
To avoid the "lost wakeup" problem, the implementation must guarantee that CONDITION-WAIT in thread A atomically releases the lock and sleeps. If this is not guaranteed there is the possibility that thread B can add an event and call CONDITION-NOTIFY between the lock release and the sleep - in this case the notify call would not see A, which would be left sleeping despite there being an event available.
|#


(defparameter C-eventChannel '())

(defparameter mylock-on-C (bordeaux-threads:make-lock "lock-on-C"))
(defparameter mycondition-variable-on-C (bordeaux-threads:make-condition-variable))

(defun A_schedule-wait-and-consume ()
  (let ((OwnsLock nil))
    (loop for i from 1 to 50 do
          (progn
            (if (not OwnsLock) 
                (progn
                  (format *om-stream* "[A (~D) asks...]~%" i)
                  (setf OwnsLock (bordeaux-threads:acquire-lock mylock-on-C t))
                  (format *om-stream* "[A (~D) LOCK !!!]~%" i)))
            ;Traiter les évènements
            ;Vider le canal
            (if OwnsLock 
                (loop while (car C-eventChannel) do
                      (format *om-stream* "---------> Read : ~D~%" (pop C-eventChannel))))
            ;Si le canal est vide -> CV-wait
            ;(loop while (and OwnsLock (not (car C-eventChannel))) do ??????
            (loop while (not (car C-eventChannel)) do
                  ;"the caller must always test on waking that there is threading to be done, instead of assuming that it can go ahead."  
                  (progn 
                    (format *om-stream* "[A (~D) sleeps waiting...]~%" i)
                    (bordeaux-threads:condition-wait mycondition-variable-on-C mylock-on-C)))
            (setf OwnsLock t)
            (format *om-stream* "[A (~D) WOKE UP => LOCK !!!]~%" i)
            (sleep 0.3)))))


(defun B_schedule-wait-and-produce ()
  (loop for i from 1 to 30 do 
        (progn
          (format *om-stream* "[B (~D) asks..]~%" i)
          (bordeaux-threads:acquire-lock mylock-on-C t)
          (format *om-stream* "[B (~D) LOCK !!!]~%" i)
          ;Produire et remplir le canal
          (push i C-eventChannel)
          (format *om-stream* "[B (~D) will notify...]~%" i)
          (bordeaux-threads:condition-notify mycondition-variable-on-C)
          (format *om-stream* "[B (~D) NOTIFICATION DONE]~%" i)
          (bordeaux-threads:release-lock mylock-on-C)
          (format *om-stream* "[B (~D) UNLOCK !!!]~%" i)
          (sleep 0.2)
          )))


(let ((A-reader (bordeaux-threads:make-thread #'(lambda () (A_schedule-wait-and-consume)) :name 'A-reader))
      (B-producer (bordeaux-threads:make-thread #'(lambda () (B_schedule-wait-and-produce)) :name 'B-producer))))



#|
/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
condition-wait (condition-variable lock)
----------------------------------------
Atomically release LOCK and enqueue the calling thread waiting for CONDITION-VARIABLE. The thread will resume when another thread has notified it using CONDITION-NOTIFY; it may also resume if interrupted by some external event or in other implementation-dependent circumstances: the caller must always test on waking that there is threading to be done, instead of assuming that it can go ahead.
However and for whatever reason the thread is resumed, the system always reacquires LOCK before returning to the caller. It is an error to call this unless from the thread that holds LOCK.
In an implementation that does not support multiple threads, this function signals an error.

condition-notify (condition-variable)
----------------------------------------
Notify at least one of the threads waiting for CONDITION-VARIABLE. It is implementation-dependent whether one or more than one (and possibly all) threads are woken, but if the implementation is capable of waking only a single thread (not all are) this is probably preferable for efficiency reasons. The order of wakeup is unspecified and does not necessarily relate to the order that the threads went to sleep in.
CONDITION-NOTIFY has no useful return value. In an implementation that does not support multiple threads, it has no effect.

/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\/!\
|#



;===============================================
; ------> SIMULATION

(defparameter L-EventsToLearn '())
(defparameter lock-on-L (bordeaux-threads:make-lock "lock-on-L"))
(defparameter CV-on-L (bordeaux-threads:make-condition-variable))

(defparameter G-NavigationOrders '())
(defparameter lock-on-G (bordeaux-threads:make-lock "lock-on-G"))
(defparameter CV-on-G (bordeaux-threads:make-condition-variable))

(defparameter O-SharedMemory (make-hash-table :test #'eq))
(defparameter lock-on-O (bordeaux-threads:make-lock "lock-on-O"))
(defparameter CV-on-O (bordeaux-threads:make-condition-variable))

  
(defun L-fillEventsToLearnChannel ()
    (loop for i from 1 to 30 do 
        (progn
          (format *om-stream* "(L-fillEventsToLearnChannel ~D asks L... )~%" i)
          (bordeaux-threads:acquire-lock lock-on-L t)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D LOCK L )~%" i)
          ;Produire et remplir le canal
          (push (format nil "~D lapin(s)" i) L-EventsToLearn)
          (bordeaux-threads:condition-notify CV-on-L)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D NOTIFY CVL)~%" i)
          (bordeaux-threads:release-lock lock-on-L)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D UNLOCK L)~%" i)

          ;(sleep 0.3)
          (sleep 0.1)
          )))

(defun L-processLearning ()
  (let ((cpt 1)
        (OwnsLockL nil))
    (loop for i from 1 to 50 do
        (progn
          (if (not OwnsLockL)
              (progn
                (format *om-stream* "(L-processLearning ~D asks L...)~%" i)
                (setf OwnsLockL (bordeaux-threads:acquire-lock lock-on-L t))
                (format *om-stream* "(L-processLearning ~D LOCK L)~%" i)))
          
          ;Traiter les évènements
          ;Vider le canal
          (if OwnsLockL
              (progn
                (format *om-stream* "(L-processLearning ~D asks O...)~%" i)
                (bordeaux-threads:acquire-lock lock-on-O t)
                (format *om-stream* "(L-processLearning ~D LOCK O)~%" i)
                (loop while (car L-EventsToLearn) do
                      (progn 
                        (setf (gethash cpt O-SharedMemory) (pop L-EventsToLearn))
                        (incf cpt)))
                (bordeaux-threads:condition-notify CV-on-O) 
                (format *om-stream* "(L-processLearning ~D NOTIFY CVO)~%" i)
                (bordeaux-threads:release-lock lock-on-O)
                (format *om-stream* "(L-processLearning ~D UNLOCK O)~%~%" i)))

          ;Si le canal est vide -> CV-wait
          (loop while (not (car L-EventsToLearn)) do
                ;"the caller must always test on waking that there is threading to be done, instead of assuming that it can go ahead."
                (progn 
                  (format *om-stream* "(L-processLearning ~D sleeps waiting L CVL...)~%" i)
                  (bordeaux-threads:condition-wait CV-on-L lock-on-L)))
          (setf OwnsLockL t)
          (format *om-stream* "(L-processLearning ~D WOKE UP => LOCK L)~%" i)   
          ;(sleep 0.2)
          (sleep 0.05)
          ))))

(defun G-fillNavigationOrdersChannel ()
    (loop for i from 1 to 20 do 
        (progn
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D asks G...)~%" i)
          (bordeaux-threads:acquire-lock lock-on-G t)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D LOCK G)~%" i)
          ;Produire et remplir le canal
          ;(push i G-NavigationOrders)
          (setf G-NavigationOrders (append G-NavigationOrders (list i)))
          (bordeaux-threads:condition-notify CV-on-G)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D NOTIFY CVG)~%" i)
          (bordeaux-threads:release-lock lock-on-G)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D UNLOCK G)~%" i)

          (sleep 0.4)
          )))

(defun G-processNavigation ()
  (let ((OwnsLockO nil) (OwnsLockG nil))
    (loop for i from 1 to 40 do
          (progn
            (if (not OwnsLockG)
                (progn
                  (format *om-stream* "(G-processNavigation ~D asks G...)~%" i)
                  (bordeaux-threads:acquire-lock lock-on-G t)
                  (format *om-stream* "(G-processNavigation ~D LOCK G)~%" i)))
            ;Traiter les évènements
            ;Vider le canal
            (if (not OwnsLockO)
                (progn   
                  (format *om-stream* "(G-processNavigation ~D asks O...)~%" i)
                  (bordeaux-threads:acquire-lock lock-on-O t)
                  (format *om-stream* "(G-processNavigation ~D LOCK O)~%" i)))
              
            (loop while (car G-NavigationOrders) do
                  (let ((NavigOrder (pop G-NavigationOrders)))
                    (if (gethash NavigOrder O-SharedMemory)
                        (format *om-stream* "~%~%----->>>>Reading pos ~D : ~a ~%" NavigOrder (gethash NavigOrder O-SharedMemory))
                      (format *om-stream* "~%~%----->>>>Nothing to read at pos ~D~%" NavigOrder))))
            
            (format *om-stream* "(G-processNavigation ~D sleeps waiting O CVO...)~%" i)
            (bordeaux-threads:condition-wait CV-on-O lock-on-O)
            (setf OwnsLockO t)
            (format *om-stream* "(G-processNavigation ~D WOKE UP => LOCK O )~%" i)
            
            ;Si le canal est vide -> CV-wait
            (loop while (not (car G-NavigationOrders)) do
                (progn
                  (format *om-stream* "(G-processNavigation ~D sleeps waiting G CVG)~%" i)
                  (bordeaux-threads:condition-wait CV-on-G lock-on-G)))
            (setf OwnsLockG t)
            (format *om-stream* "(G-processNavigation ~D WOKE UP => LOCK G)~%" i)
            (sleep 0.2)))))

(let (
      (L-filler (bordeaux-threads:make-thread #'(lambda () (L-fillEventsToLearnChannel)) :name 'L-filler))
      (G-filler (bordeaux-threads:make-thread #'(lambda () (G-fillNavigationOrdersChannel)) :name 'G-filler))
      (L-processer (bordeaux-threads:make-thread #'(lambda () (L-processLearning)) :name 'L-processer))
      (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation)) :name 'G-processer))
      ))


