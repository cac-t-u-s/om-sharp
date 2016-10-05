(in-package :om)

(defparameter L-EventsToLearn '())
(defparameter lock-on-L (bordeaux-threads:make-recursive-lock "lock-on-L"))
(defparameter CV-on-L (bordeaux-threads:make-condition-variable))

(defparameter G-NavigationOrders '())
(defparameter lock-on-G (bordeaux-threads:make-recursive-lock "lock-on-G"))
(defparameter CV-on-G (bordeaux-threads:make-condition-variable))

(defparameter O-SharedMemory (make-hash-table :test #'eq))
(defparameter lock-on-O (bordeaux-threads:make-recursive-lock "lock-on-O"))
(defparameter CV-on-O (bordeaux-threads:make-condition-variable))

  
(defun L-fillEventsToLearnChannel ()
    (loop for i from 1 to 30 do 
        (progn
          (bordeaux-threads:acquire-recursive-lock lock-on-L t)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D LOCK L)~%" i)
          ;Produire et remplir le canal
          (push (format nil "~D lapin(s)" i) L-EventsToLearn)
          (bordeaux-threads:condition-notify CV-on-L)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D NOTIFY CVL)~%" i)
          (bordeaux-threads:release-recursive-lock lock-on-L)
          (format *om-stream* "(L-fillEventsToLearnChannel ~D UNLOCK L)~%" i)

          (sleep 0.3)
          )))

(defun L-processLearning ()
  (let ((cpt 1))
    (loop for i from 1 to 50 do
        (progn
          (bordeaux-threads:acquire-recursive-lock lock-on-L t)
          (format *om-stream* "(L-processLearning ~D LOCK L)~%" i)
          ;Traiter les évènements
          ;Vider le canal
          (bordeaux-threads:acquire-recursive-lock lock-on-O t)
          (format *om-stream* "(L-processLearning ~D LOCK O)~%" i)
          (loop while (car L-EventsToLearn) do
                (progn 
                  (setf (gethash cpt O-SharedMemory) (pop L-EventsToLearn))
                  (incf cpt)))
          (bordeaux-threads:condition-notify CV-on-O) 
          (format *om-stream* "(L-processLearning ~D NOTIFY CVO)~%" i)
          ;Si le canal est vide -> CV-wait
          (if (not (car L-EventsToLearn))
              (progn 
                (sys:setup-atomic-funcall 'bordeaux-threads:condition-wait 
                                          CV-on-L 
                                          lock-on-L)
                (format *om-stream* "(L-processLearning ~D COND WAIT L CVL)~%" i)
                ))
          
          (sleep 0.2)
          ))))

(defun G-fillNavigationOrdersChannel ()
    (loop for i from 1 to 20 do 
        (progn
          (bordeaux-threads:acquire-recursive-lock lock-on-G t)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D LOCK G)~%" i)
          ;Produire et remplir le canal
          (push i G-NavigationOrders)
          (bordeaux-threads:condition-notify CV-on-G)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D NOTIFY CVG)~%" i)
          (bordeaux-threads:release-recursive-lock lock-on-G)
          (format *om-stream* "(G-fillNavigationOrdersChannel ~D UNLOCK G)~%" i)

          (sleep 0.4)
          )))

(defun G-processNavigation ()
    (loop for i from 1 to 40 do
        (progn
          (bordeaux-threads:acquire-recursive-lock lock-on-G t)
          (format *om-stream* "(G-processNavigation ~D LOCK G)~%" i)
          ;Traiter les évènements
          ;Vider le canal
          (bordeaux-threads:acquire-recursive-lock lock-on-O t)
          (format *om-stream* "(G-processNavigation ~D LOCK O)~%" i)
          (loop while (car G-NavigationOrders) do
                (let ((NavigOrder (pop G-NavigationOrders)))
                  (if (gethash NavigOrder O-SharedMemory)
                      (format *om-stream* "----->>>>Reading pos ~D : ~a ~%" NavigOrder (gethash order O-SharedMemory))
                    (format *om-stream* "----->>>>Nothing to read at pos ~D~%" NavigOrder))))
          (sys:setup-atomic-funcall 'bordeaux-threads:condition-wait 
                                    CV-on-O 
                                    lock-on-O)
          (format *om-stream* "(G-processNavigation ~D COND WAIT O CVO)~%" i)
          ;Si le canal est vide -> CV-wait
          (if (not (car G-NavigationOrders))
              (progn
                (sys:setup-atomic-funcall 'bordeaux-threads:condition-wait 
                                          CV-on-G 
                                          lock-on-G)
                (format *om-stream* "(G-processNavigation ~D COND WAIT G CVG)~%" i)))
          
          (sleep 0.2)
          )))

(let (
      (L-filler (bordeaux-threads:make-thread #'(lambda () (L-fillEventsToLearnChannel)) :name 'L-filler))
      (G-filler (bordeaux-threads:make-thread #'(lambda () (G-fillNavigationOrdersChannel)) :name 'G-filler))
      (L-processer (bordeaux-threads:make-thread #'(lambda () (L-processLearning)) :name 'L-processer))
      (G-processer (bordeaux-threads:make-thread #'(lambda () (G-processNavigation)) :name 'G-processer))
      ))


