;;===========================================================================
;Copyright (C) 2016 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;
;Author: Dimitri Bouche
;;===========================================================================
(in-package :om)

(defvar *dispatcher* nil)

(declaim (optimize (speed 3) (safety 0) (debug 1)))

;================================================================Dispatcher structure
(defstruct (dispatcher)
  (state :stop :type symbol) ;Current state
  (process nil :type (or mp:process null)) ;Reference to the main process
  (precision 1 :type integer) ;Best possible time accuracy in dispatching actions (in ms)
  (scheduler nil :type scheduler) ;Reference to the linked scheduler (to access object register etc.)
  (:documentation "Component to dispatch actions from a scheduler register on due time."))
;====================================================================================



;===================================================================Getters & Setters
(defmethod state ((self dispatcher))
  (dispatcher-state self))
(defmethod (setf state) (new-state (self dispatcher))
  (setf (dispatcher-state self) new-state))

(defmethod process ((self dispatcher))
  (dispatcher-process self))
(defmethod (setf process) (process (self dispatcher))
  (setf (dispatcher-process self) process))

(defmethod precision ((self dispatcher))
  (dispatcher-precision self))
(defmethod (setf precision) (precision (self dispatcher))
  (setf (dispatcher-precision self) precision))

(defmethod scheduler ((self dispatcher))
  (dispatcher-scheduler self))
(defmethod (setf scheduler) (scheduler (self dispatcher))
  (setf (dispatcher-scheduler self) scheduler))
;====================================================================================



;=================================================================================API
(defun build-dispatcher (scheduler &key (precision 1))
  (let ((dispatcher (make-dispatcher :precision (max 1 (round precision))
                                     :scheduler scheduler)))
    (setf (process dispatcher) (mp:process-run-function "OM-Dispatcher"
                                                        nil
                                                        'dispatch-actions-cycle
                                                        dispatcher))
    dispatcher))

(defmethod abort-dispatcher ((self dispatcher))
  (if (mp:process-alive-p (process self))
      (mp:process-kill (process self))))

;====================================================================================
(declaim (inline play-object-action))
(defmethod play-object-action ((disp dispatcher) obj action)
  (let ((sched (scheduler disp)))
    (if (and (not (multi-thread sched)) (act-marker action))
        (progn
          (pause-schedulable-object obj sched)
          (%play-action action)
          (reschedule obj sched nil nil)
          (continue-schedulable-object obj sched))
      (%play-action action))))

(defmethod dispatch-actions-cycle ((self dispatcher))
  (let ((sched (scheduler self))
        (tick (/ (precision self) 1000.0)))
    (loop
     ;;;When idle, garbage collector + stop
     (when (scheduler-idle-p sched) 
       (%clean-action-garbage)
       (mp:process-stop (process self) "Idle"))
     ;;;Or look in the register who which objects need to play actions
     (loop for object+caller in (register sched)
           do
           (mp:with-lock ((plan-lock (car object+caller)))
             (loop while (and (plan (car object+caller))
                              (<= (act-timestamp (car (plan (car object+caller)))) (%get-obj-time (car object+caller))))
                   do
                   (om-ignore&print-error
                    (play-object-action self (car object+caller) (pop (plan (car object+caller))))))))
     ;;;And sleeps tick seconds (usually 0.001 s)
     (mp:process-wait-with-timeout "Sleeping" tick))))




(defmethod dispatch-actions-with-timer ((self dispatcher))
  (let ((sched (scheduler self))
        (anticipation 10)
        next-trigger)
    (loop
     (setq next-trigger (get-next-trigger sched))
     (mp:process-wait-local-with-timeout "Sleeping" 
                                         (max 0.001 (/ (- (cadr next-trigger) (om-get-internal-time) anticipation) 1000.0))
                                         'alarm sched)
     (om-ignore&print-error
      (if (car next-trigger)
          (if (alarm sched)
              (setf (alarm sched) nil)
            (progn
              (loop while (> (cadr next-trigger) (om-get-internal-time))
                    do
                    (mp:process-wait-with-timeout "Sleeping" 0.001))
              (mp:with-lock ((plan-lock (car next-trigger)))
                (%play-action (pop (plan (car next-trigger))))))))))))

;(progn (restart-scheduling-system) (init-om-player))
;(get-next-trigger *scheduler*) (plan (caar (register *scheduler*)))
;(register *scheduler*)

     ;(incf i) (refresh-next-date sched)
     ;(if (> (next-delay sched) 50)
     ;    (loop for object+caller in (register sched)
     ;          do
     ;          (when (and 
     ;                 (= 0 (mod i 50))
     ;                 (run-callback *graphics*) 
     ;                 (cadr object+caller)
     ;                 (eq (player-get-object-state sched (car object+caller)) :play))
     ;            (om-ignore&print-error
     ;             (funcall (run-callback *graphics*) (cadr object+caller) (player-get-object-time sched (car object+caller)))))))