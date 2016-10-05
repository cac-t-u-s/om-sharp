(in-package :om)

(defclass improvisation-handler ()
  ((name :initform "Improvizer-Handler" :accessor name :initarg :name :type string)
   (num :initform 1 :accessor num :initarg :num)
   ;;;Improvizer
   (rtimprovizer :initform nil :accessor rtimprovizer :initarg :rtimprovizer)
   (scenario :initform nil :accessor scenario :initarg :scenario :type list)
   (expanded-scenario :initform nil :accessor expanded-scenario :type list)
   (label-class :initform 'label :accessor label-class :initarg label-class)
   ;(db-path :initform nil :accessor db-path :initarg :db-path :type (or null string))
   ;;;Slice Data
   (slice-list :initform nil :accessor slice-list :type list)
   ;(slice-index :initform 0.0 :accessor slice-index :type single-sloat)
   (perf-time :initform 0 :accessor perf-time :type integer)
   (empty-pos :initform 0 :accessor empty-pos :type integer)
   (max-pos :initform 0 :accessor max-pos :type integer)
   ;;;Handler
   (output-fun :initform 'output-sequence :accessor output-fun :type (or null function))
   (epsilon :initform 5 :accessor epsilon :initarg :epsilon :type integer)
   (play-pos :initform 0 :accessor play-pos :type integer)
   (queries :initform '() :accessor queries :type list)
   (is-running :initform nil :accessor is-running :type boolean)
   (waiting-processes :initform '() :accessor waiting-processes :type list)
   (target-object :initform nil :accessor target-object))
  (:documentation "
A handler for Improtek (Copyright 2013 (C) J.Nika).
This object can automate improvization generation based on the rtimprovizer class from Improtek."))
  
(defmethod set-target-object ((self improvisation-handler) object)
  (setf (target-object self) object)
  self)

(defmethod display-modes-for-object ((self improvisation-handler))
  '(:hidden :text :mini-view))

(defmethod draw-mini-view ((self improvisation-handler) (box t) x y w h &optional time) nil)
;  (when (name self)
;    (om-with-font (om-make-font "Helvetica" 24)
;                  (om-draw-string 10 30 (concatenate 'string "[Handler] w/ " (name self))))))

(defmethod initialize-instance :after ((self improvisation-handler) &rest initargs)
  (shared-initialize self t initargs)
  (if (rtimprovizer self)
      (setf (expanded-scenario self) (scenario self)
            (max-pos self) (length (expanded-scenario self))
            (gen-callback (rtimprovizer self))
            #'(lambda (val) 
                (loop for proc in (waiting-processes self)
                      do
                      (om-poke-process proc))))))

(defmethod (setf perf-time) (index (self improvisation-handler))
  (setf (slot-value self 'perf-time) index)
  (when (= index 0) 
    (setf (queries self) '())
    (resettabou (rtimprovizer self))
    (setf (waiting-processes self) (remove nil (waiting-processes self)))
    (loop while (waiting-processes self)
          do
          (om-kill-process (pop (waiting-processes self)))))
  ;(print (list index '/ (empty-pos self) '=> (>= (+ index (epsilon self)) (empty-pos self))))
  (if (and (>= (+ index (epsilon self)) (empty-pos self))
           (not (is-running self))
           (< (empty-pos self) (max-pos self)))
      (let (new-query)
        (incf (empty-pos self))
        (setq new-query (query-alloc :gen-start (1- (empty-pos self)) :handler self))
        ;(push new-query (queries self))
        ;(sort (queries self) '< :key 'q-gen-start)
        (print-if (list "RUN BY DEPLETION" new-query))
        (run new-query))))

(defmethod send-performance-tick ((self improvisation-handler) tick)
  (setf (perf-time self) tick))

(defmethod reset-handler ((self improvisation-handler))
  (setf (perf-time self) 0))

;;;Run one generation step
(defmethod proceed-improvisation-handler ((self improvisation-handler) gen-start)
  (let* ((scenario-suffix (nthcdr gen-start (expanded-scenario self)))
         result-slice-list)
    ;;;When improvization is not over
    (when (< gen-start (max-pos self))
      ;;;Run improvization as far as possible
      (setf (is-running self) t)
      (setq result-slice-list (improvize_onephase (rtimprovizer self)
                                                  (length scenario-suffix)
                                                  scenario-suffix
                                                  gen-start))
      (setf (is-running self) nil)
      ;;;Add the generated slice list to the handler slice-list, set the new empty position and the next generation index
      (setf (slice-list self) (append (nthcar gen-start (slice-list self)) result-slice-list))
      (if result-slice-list
          (setf (empty-pos self) (length (slice-list self)))))
    result-slice-list))

(defmethod set-scenario ((handler improvisation-handler) new-scenario)
  (let ((pos (perf-time handler)))
    (setf (slot-value handler 'scenario) new-scenario
          (slot-value handler 'max-pos) (length new-scenario))
    (if (< (+ (epsilon handler) pos) (max-pos handler))
        (query-push (query-alloc :handler handler
                                 :gen-start (if (zerop pos) pos
                                              (+ (epsilon handler) pos)))))))


(defmethod set-improvizer-param ((self improvisation-handler) (slots list) (new-vals list) &optional gen-start)
  (let ((q (query-alloc :inputs slots
                        :vals new-vals
                        :handler self
                        :gen-start (or gen-start (if (zerop (perf-time self))
                                                     (perf-time self)
                                                   (+ (epsilon self) (perf-time self)))))))
    (query-push q
                :process (>= (q-gen-start q) (perf-time self)))))

(defmethod set-improvizer-continuity ((self realtimeimprovizer) value)
  ;(setf (max-continuity self) value)
  self)

(defmethod output-sequence ((self list) index time handler target)
  (if self
      (output-sequence-of self (car self) index time handler target)))


;;;JNMR
(defvar *improvizer-1* nil)
(defvar *improvizer-2* nil)
(setq *improvizer-1* nil
      *improvizer-2* nil)

(defmethod generate-n-beats ((self improvisation-handler) gen-start n)
  (if (not *improvizer-1*)
      (setq *improvizer-1* (rtimprovizer self))
    (if (not *improvizer-2*)
        (setq *improvizer-2* (rtimprovizer self))))

  (let ((imp (if (eq (rtimprovizer self) *improvizer-1*) *improvizer-2* *improvizer-1*))
        (traceindx 0))
    (if imp
        (setf (CurrentStateIdx (rtimprovizer self)) (or (StateIdx-at-ImproIdx-in-traceimpro imp (max 0 (- gen-start 1))) 0)))
    
    (Improvize_OnePhase (rtimprovizer self) n (nthcdr gen-start (expanded-scenario self)) gen-start)))

(defmethod harmbeats->midi ((self list) &optional channel)
  (let* ((dur (reduce '+ self :key 'duration))
         (dates (dx->x 0 (loop for harmbeat in self collect (duration harmbeat))))
         (midisets (mapcar #'(lambda (beat)
                               (format-midi-list (midiset beat)))
                           (thread-midiharmbeats self))))

    (loop for noteset in midisets
          for offset in dates
          do
          (loop for note in noteset do
                (progn
                  (incf (nth 0 note) offset)
                  (setf (nth 4 note) channel))))
    (setq midisets (delete nil (flat midisets 1)))
    (setq midisets (loop for note in midisets
                         collect
                             (make-midinote :onset (nth 0 note)
                                            :pitch (nth 1 note)
                                            :vel (nth 2 note)
                                            :dur (nth 3 note)
                                            :channel (nth 4 note))))
    
    (setq midisets (append (list (make-midinote :onset 0
                                                :pitch 10000
                                                :vel 100
                                                :dur 1
                                                :channel 1))
                           midisets
                           (list (make-midinote :onset (1- dur)
                                                :pitch 10000
                                                :vel 100
                                                :dur 1
                                                :channel 1))))

    
    (make-instance 'piano-roll
                   :midi-notes midisets)))



;(setf (CurrentImproIdx newvoice) (StateIdx-at-ImproIdx-in-traceimpro oldvoice (1- genstartdenew)))





#|
(defmacro get-impro-slot-callback (slot)
  (case slot 
    ('perf-time
     `#'(lambda (self)
          (when (and (< (- (empty-pos self) new-val) (epsilon self))
                     (< (empty-pos self) (slice-max-pos self)))
            (setf (slice-index self) (empty-pos self))
            ;(proceed-improvisation-handler self)
            )))
    ('scenario
     `#'(lambda (self)
          (let* ((new-expanded-scenario (expand_grid new-val))
                 (pos (perf-time self))
                 (switch-pos (position nil (mapcar 'equal (nthcdr pos (expanded-scenario self)) (nthcdr pos new-expanded-scenario)))))
            (when switch-pos
              (incf switch-pos pos)
              (setf (slot-value handler 'expanded-scenario) new-expanded-scenario)
              (setf (slot-value handler 'slice-index) switch-pos)
              (setf (slot-value handler 'slice-max-pos) (length new-expanded-scenario)))
              ;(proceed-improvisation-handler self)
            )))
    (otherwise #'(lambda (self)))))

;;;a mettre dans l'output
 ;;;Stretch the result beat list to the desired tempo
      (loop for st in result-slice-list do
            (setf (MidiSet st) (timestretch (MidiSet st) (/ sdur (duration st)))
                  (duration st) sdur))
`
(defmacro enable-slot-reactivity (slot callback)
  `(defmethod (set-param ,slot) (new-val (self improvisation-handler))
     (setf (slot-value (rtimprovizer self) ',slot) new-val) ;;;MARCHE QUE POUR LE HANDLER
     (funcall ,callback self)
     new-val))

(defmacro disable-slot-reactivity (slot class)
  `(defmethod (setf ,slot) (new-val (self ,class))
     (setf (slot-value self ',slot) new-val)))

;;;scheduling midi
(when result-slice-list
        ;;;Turn the result beat list into a scheduling list
        (when (setq result-schedlist (midi->schedlist (beats->midi result-slice-list sdur 0 (* slice-index sdur)) self))
          ;;;Set the player-scheduler queue accrding to new values
          (mp:with-lock ((om-get-scheduler-lock (player-scheduler self)))
            ;;;If the player scheduler has a non-null queue, cut the non-processed queue at the right place, keeping only the note-off midi events, and mix with the new queue + sort.
            (if (setq queue (om-get-scheduler-queue (player-scheduler self)))
                (let* ((indx (or (position (caar result-schedlist) queue :test '<= :key 'car) (length queue)))
                       (queue-prefix (nthcar indx queue))
                       (queue-suffix (nthcdr indx queue))
                       evt notes-off)
                  (loop for elem in queue-suffix do
                        (setq evt (caar (last elem)))
                        (if (and (eq (type-of evt) 'om-midi::midi-evt) (eq (om-midi:midi-evt-type evt) :keyoff))
                            (push elem notes-off)))
                  (setq result-schedlist (sort (append result-schedlist notes-off) '< :key #'car))
                  (om-set-scheduler-queue (player-scheduler self) (append (nthcar indx queue) result-schedlist)))
              ;;;If the player scheduler has no queue, set it to the result queue
              (om-set-scheduler-queue (player-scheduler self) result-schedlist)))))


;;;Build an Improvizer Handler from a scenario and a database
;(defun build-improvisation-handler (&key name scenario rtimprovizer epsilon output-fun)
;  (let ((handler (make-instance 'improvisation-handler 
;                                :name (or name "Improvizer-Handler")
;                                :scenario scenario
;                                ;:expanded-scenario expanded-scenario;(expand_grid scenario)
;                                :rtimprovizer (or rtimprovizer (NewRealtimeImprovizer))
;                                :epsilon (or epsilon 3)
;                                :output-fun output-fun)))
;    (setf (expanded-scenario handler) (NewHarmLabelList (expand_grid scenario))
;          (gen-callback (rtimprovizer handler))
;          #'(lambda (val) 
;              (loop for proc in (waiting-processes handler) do
;                    (mp:process-poke proc)))) handler))


              ;(loop for slice in result-slice-list collect
              ;      (funcall (output-slice-fun self)
              ;               slice 
              ;               (+ gen-start (incf i)) 
              ;               (reduce #'+ (nthcar (+ gen-start i) (slice-list self)) :key #'duration)
              ;               ))
|#

