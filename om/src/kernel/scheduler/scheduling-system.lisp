;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: D. Bouche
;============================================================================


(in-package :om)

(defmethod make-player ((id (eql :reactive-player)) &key run-callback stop-callback (callback-tick 50) (time-window 5) single-threaded)
  (declare (ignore time-window))
  (let ((sched (or *scheduler* (setq *scheduler* (build-scheduler :run-callback run-callback
                                                                  :stop-callback stop-callback)))))
    (setq *dispatcher* (or *dispatcher* (build-dispatcher sched))
          *graphics* (or *graphics* (build-graphics-handler sched :precision callback-tick))
          *engine* (or *engine* (build-thread-pool 8));(or *engine* (build-engine))
          )
    *scheduler*))

(defmethod destroy-player ((self scheduler))
 ; (if (process *engine*)
    ;  (mp:process-kill (process *engine*)))
  (if (workers *engine*)
      (abort-thread-pool *engine*))
  (if (process *scheduler*)
      (mp:process-kill (process *scheduler*)))
  (if (process *dispatcher*)
      (mp:process-kill (process *dispatcher*)))
  (if (process *graphics*)
      (mp:process-kill (process *graphics*)))
  (setq *graphics* nil
        *scheduler* nil
        *engine* nil
        *dispatcher* nil))


(defun restart-scheduling-system ()
  (let ((runC (run-callback *scheduler*))
        (stopC (stop-callback *scheduler*))
        (precision (precision *graphics*)))
  ;  (if (process *engine*)
  ;      (mp:process-kill (process *engine*)))
  ;  (if (process *scheduler*)
  ;      (mp:process-kill (process *scheduler*)))
  ;  (if (process *dispatcher*)
  ;      (mp:process-kill (process *dispatcher*)))
  ;  (if (process *graphics*)
  ;      (mp:process-kill (process *graphics*)))
    (destroy-player *scheduler*) t
    (setq *graphics* nil
          *scheduler* nil
          *engine* nil
          *dispatcher* nil)
    (make-player :reactive-player 
                 :run-callback runC
                 :stop-callback stopC
                 :callback-tick precision)))

;(progn (abort-om-player) (init-om-player))


;;===========================================================================
;;;Links to previous architecture
;;===========================================================================
(defmethod player-play-object ((self scheduler) (object schedulable-object) caller &key parent interval)
  (play-schedulable-object object self :interval interval :caller caller :parent parent))

(defmethod player-stop-object ((self scheduler) (object schedulable-object))
  (stop-schedulable-object object self))

(defmethod player-pause-object ((self scheduler) (object schedulable-object))
  (pause-schedulable-object object self))

(defmethod player-continue-object ((self scheduler) (object schedulable-object))
  (continue-schedulable-object object self))

(defmethod player-get-object-state ((self scheduler) (object schedulable-object))
  (or (state object) :stop))

(defmethod player-get-object-time ((self scheduler) (object schedulable-object))
  (get-obj-time object))
;;===========================================================================
;;===========================================================================
