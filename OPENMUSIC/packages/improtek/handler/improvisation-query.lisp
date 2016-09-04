(in-package :om)

(defvar *print-queries* nil)
;(setq *print-queries* t)
;(setq *print-queries* nil)

;;;====================================================================================
;;;=================================== IMPRO QUERIES ==================================
;;;====================================================================================

(defstruct (improvisation-query
            (:print-object
             (lambda (q stream)
               (print-unreadable-object (q stream :type t :identity t)
                 (princ `(:query :=,(q-name q) :at ,(q-gen-start q) :. :for :=,(q-inputs q):=,(q-vals q) :. :output ,(length (q-output q))) stream))))
            (:conc-name q-))
  (name "Improvisation-Query" :type string)
  (handler nil :type (or null improvisation-handler))
  (inputs '() :type list)
  (vals '() :type list)
  (gen-start 0 :type integer)
  (process nil :type (or null mp:process))
  (output nil)
  (outputed nil)
  (waiting-process nil)
  (:documentation ""))

(defmethod q-curpos ((self improvisation-query))
  (1- (if (q-process self)
          (currentimproidx (rtimprovizer (q-handler self)))
        (q-gen-start self))))

(defmethod same-inputs ((q1 improvisation-query) (q2 improvisation-query))
  (let ((res t))
    (loop for input in (q-inputs q1) do
          (setq res (and res (find input (q-inputs q2)))))
    res))

(defun print-if (data)
  (if *print-queries*
      (print data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;Query pool;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun query-pool (size)
  (let ((list (make-list size)))
    (loop
     for n on list do
     (setf (car n) (make-improvisation-query)))
    list))

(let* ((cache-lock (mp:make-lock))
       (cache-size 64)
       (cache-list '()))
  (declare (type fixnum cache-size))

  (mp:with-lock (cache-lock)
    (setf cache-list (query-pool cache-size)))

  (defun query-alloc (&key (name "Improvisation-Query") (inputs '()) (vals '()) (gen-start 0) process handler)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (query-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((query (pop cache-list)))
	(setf (q-name query) name
              (q-handler query) handler
              (q-inputs query) inputs
              (q-vals query) vals
              (q-gen-start query) gen-start
              (q-process query) process
              (q-output query) nil
              (q-outputed query) nil
              (q-waiting-process query) nil
              ;(q-will-be-relayed query) nil
              )
        (if (eq (q-inputs query) '(scenario))
            (setf (scenario (q-handler query)) (car (q-vals query))))
        query)))

  (defmethod query-free ((self improvisation-query))
    (mp:with-lock (cache-lock)
      (setf (q-name self) "Improvisation-Query"
            (q-handler self) nil
            (q-inputs self) '()
            (q-vals) '()
            (q-gen-start self) 0
            (q-process self) nil
            (q-output self) nil
            (q-outputed self) nil
            (q-waiting-process self) nil
            ;(q-will-be-relayed self) nil
            )
      (push self cache-list)
      nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod pos-in-queue ((self improvisation-query))
  (position self (queries (q-handler self))))

(defmethod query-push ((self improvisation-query) &key (process t))
  (push self (queries (q-handler self)))
  (sort (queries (q-handler self)) '< :key 'q-gen-start)
  (if process (process-query self)))


(defmethod process-query ((self improvisation-query))
  (if (= (length (queries (q-handler self))) 1)
      (progn
        (print-if (list "RUN BY INIT PROCESS" self))
        (run self))
    (let (pos
          same-start-min
          same-start-max)
      (setq pos (position self (queries (q-handler self))))
      (setq same-start-min (position (q-gen-start self)
                                     (queries (q-handler self))
                                     :key 'q-gen-start))
      (setq same-start-max (position (q-gen-start self)
                                     (queries (q-handler self))
                                     :key 'q-gen-start
                                     :from-end t))
      (loop for i from same-start-min to same-start-max
            do
            (if (not (= i pos))
                (process-eqstart-query self (nth i (queries (q-handler self))))))
      (if (>= (1- same-start-min) 0)
          (process-supstart-query self (nth (1- same-start-min) (queries (q-handler self)))))
      (if (< (1+ same-start-max) (length (queries (q-handler self))))
          (process-infstart-query self (nth (1+ same-start-max) (queries (q-handler self))))))))

(defmethod process-infstart-query ((self improvisation-query) (qi improvisation-query)) 
  (print-if "INF")
  (reset-query qi)
  (wait-for-relay self qi)
  (if (eq (car (queries (q-handler self))) self)
      (progn
        (print-if (list "RUN BECAUSE NEW FIRST" self))
        (run self))))

(defmethod process-supstart-query ((self improvisation-query) (qi improvisation-query))
  (print-if "SUP")
  (let ((pos (+ (q-gen-start qi) (length (q-output qi))))
        new-query)
    (if (<= (q-gen-start self) pos)
        (relay qi self)
      (progn
        (wait-for-relay qi self)
        (when (and (next-query self)
                   (< pos (q-gen-start (next-query self))))
          (setq new-query (query-alloc :gen-start pos :handler (q-handler self)))
          ;(push new-query (queries (q-handler new-query)))
          ;(sort (queries (q-handler new-query)) '< :key 'q-gen-start)
          (print-if (list "RUN BY FORCE" new-query))
          (run new-query))))))

(defmethod process-eqstart-query ((self improvisation-query) (qi improvisation-query)) 
  (print-if "EQ")
  (if (same-inputs self qi)
      (progn
        (kill qi)
        (delete qi (queries (q-handler self)) :test 'eq)
        (process-query self))
    (merge-query self qi)))

(defmethod reset-query ((self improvisation-query)) 
  (print-if (list "RESET QUERY" self))
  (if (mp:process-p (q-process self))
      (mp:process-kill (q-process self)))
  (if (next-query self)
      (reset-query (next-query self)))
  (setf (q-output self) '()))

(defmethod run ((self improvisation-query))
  ;;;Affect new values to rtimprovizer parameters
  (loop for inp in (q-inputs self)
        for val in (q-vals self) do
        (if (not (eq inp 'scenario))
            (setf (slot-value (rtimprovizer (q-handler self)) inp) val)))
  ;;;Start a new generation process
  (setf (q-process self) (mp:process-run-function (q-name self) nil
                                                  #'(lambda (hnd gnstrt)
                                                      ;;;Bind the generation output to the query output
                                                      (setf (q-outputed self) nil
                                                            (q-output self) (proceed-improvisation-handler hnd gnstrt))
                                                      (print (list "OUTPUT" self))
                                                      ;;;Call the output method with:
                                                      ;;;the output chunk, the chunk start index, the chunk start time, the handler and the target objet
                                                      (funcall (output-fun (q-handler self))
                                                               (q-output self)
                                                               (q-gen-start self)
                                                               (reduce #'+ (nthcar (q-gen-start self) (slice-list (q-handler self))) :key #'duration)
                                                               (q-handler self)
                                                               (target-object (q-handler self)))
                                                      (setf (q-outputed self) t)
                                                      ;;;If there is a waiting query and this one didn't reach it, start a new one 
                                                      (let ((pos (+ (q-gen-start self) (length (q-output self))))
                                                            new-query)
                                                        ;(print (list "run next" pos (next-query self) (if (next-query self) (q-gen-start (next-query self)))))
                                                        (when (and (next-query self)
                                                                   (< pos (q-gen-start (next-query self))))
                                                          (setq new-query (query-alloc :gen-start (1+ pos) :handler (q-handler self)))
                                                          ;(push new-query (queries (q-handler new-query)))
                                                          ;(sort (queries (q-handler new-query)) '< :key 'q-gen-start)
                                                          (print-if (list "RUN BY FORCE" new-query))
                                                          (run new-query))
                                                        )
                                                      )
                                                  (q-handler self)
                                                  (q-gen-start self))))

(defmethod kill ((self improvisation-query)) 
  (print-if (list "KILL" self))
  (if (mp:process-p (q-waiting-process self))
      (mp:process-kill (q-waiting-process self)))
  (if (mp:process-p (q-process self))
      (mp:process-kill (q-process self))))

(defmethod %relay ((q1 improvisation-query) (q2 improvisation-query)) ;;;FAIS PASSER Q2
  (print-if (list "RELAY" q1 q2))
  (let* ((out (output (rtimprovizer (q-handler q1))))
         (max (max 0 (min (abs (- (q-gen-start q2) (q-gen-start q1))) (1- (length out))))))
    (when (not (q-outputed q1));(and (q-process q1) (eq (mp:process-state (q-process q1)) :active))
      (print-if "RELAY RUNNING")
      (funcall (output-fun (q-handler q1))
               (nthcar max out)
               (q-gen-start q1)
               (reduce #'+ (nthcar (q-gen-start q1) (slice-list (q-handler q1))) :key #'duration)
               (q-handler q1)
               (target-object (q-handler q1)))
      (setf (slice-list (q-handler q1)) (append (nthcar (q-gen-start q1) (slice-list (q-handler q1))) (nthcar max out))
            (empty-pos (q-handler q1)) (length (slice-list (q-handler q1)))))
    (kill q1)
    (print-if (list "RUN BY RELAY" q2))
    (run q2)))

(defmethod relay ((q1 improvisation-query) (q2 improvisation-query))
  (%relay q1 q2))

(defmethod %wait-for-relay ((q1 improvisation-query) (q2 improvisation-query)) ;;FAIT ATTENDRE Q2
  (print-if (list "WAIT-FOR-RELAY" q1 q2))
  (push (setf (q-waiting-process q2)
              (mp:process-run-function "Wait-For-Relay" nil
                                       #'(lambda (query)
                                           (mp:process-wait "Waiting..." 
                                                            #'(lambda () (if (previous-query query)
                                                                             (>= (q-curpos (previous-query query))
                                                                                 (q-gen-start query)))))
                                           (relay (previous-query query) query))
                                       q2))
        (waiting-processes (q-handler q1))))
               




(defmethod wait-for-relay ((q1 improvisation-query) (q2 improvisation-query))
  (if (not (q-waiting-process q2))
      (%wait-for-relay q1 q2)))

(defmethod merge-query ((q1 improvisation-query) (q2 improvisation-query)) (print-if (list "MERGE" q1 q2))
  (let ((inputs (append (q-inputs q1) (q-inputs q2)))
        (vals (append (q-vals q1) (q-vals q2))))
  (kill q1)
  (process-query (query-alloc :inputs inputs :vals vals :gen-start (q-gen-start q2)))))

(defmethod q-running-p ((self improvisation-query))
  (mp:process-p (q-process self)))

;(defmethod previous-query ((self improvisation-query))
;  (if (>= (1- (pos-in-queue self)) 0)
;      (nth (1- (pos-in-queue self)) (queries (q-handler self)))))

(defmethod previous-query ((self improvisation-query))
  (let ((pos-in-queue (position (q-gen-start self) (queries (q-handler self)) :test '<= :key 'q-gen-start)))
    (if (>= (1- pos-in-queue) 0)
        (nth (1- pos-in-queue) (queries (q-handler self))))))

;(defmethod next-query ((self improvisation-query))
;  (if (< (1+ (pos-in-queue self)) (length (queries (q-handler self))))
;      (nth (1+ (pos-in-queue self)) (queries (q-handler self)))))

(defmethod next-query ((self improvisation-query))
  (let ((pos-in-queue (position (q-gen-start self) (queries (q-handler self)) :test '< :key 'q-gen-start)))
    (if (and pos-in-queue (< pos-in-queue (length (queries (q-handler self)))))
        (nth pos-in-queue (queries (q-handler self))))))
