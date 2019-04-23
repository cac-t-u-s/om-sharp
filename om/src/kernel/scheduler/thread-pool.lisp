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
; File author: S. Bell-Bell
;============================================================================

(in-package :om)

(defvar *threadpool* nil)

(defclass thread-pool ()
  ((taskqueue :initform (mp:make-mailbox) :accessor taskqueue)
   (pending-taskqueue :initform (list) :accessor pending-taskqueue)
   (workers :initform nil :initarg :workers :accessor workers)
   (state :initform :stop :accessor state)))

;============================================API
(defmethod build-thread-pool (nbthread)
  (setq *threadpool* (make-instance 'thread-pool))
  (setf (workers *threadpool*) 
        (loop for i from 1 to nbthread
              collect
              (mp:process-run-function (format nil "Thread-~A" i)
                                       nil
                                       'thread-function *threadpool*)))
  *threadpool*)

(defmethod abort-thread-pool ((self thread-pool))
  (loop for worker in (workers self)
        do
        (if (mp:process-alive-p worker)
            (mp:process-kill worker)))
  (setq *threadpool* nil))

(defmethod restart-thread-pool ((self thread-pool))
  (abort-thread-pool self)
  (build-thread-pool 8))

(defmethod task-number-left ((pool thread-pool))
  (mp:mailbox-count (taskqueue pool)))

(defmethod engine-tasks-left ()
  (task-number-left *threadpool*))

;=============================================
(defmethod thread-function ((self thread-pool))
  (mp:ensure-process-mailbox)
  (loop
   (let ((task (mp:mailbox-read (taskqueue self))))
     (if (not task)
         (mp:process-wait-local "Waiting for request asleep" 'mp:mailbox-not-empty-p (taskqueue self))
       (funcall task)))))

(defmethod poke-thread-pool ((pool thread-pool))
  (mapcar 'mp:process-poke (workers pool)))

(defmethod add-worker ((self thread-pool) thread)
  (push thread (workers self)))

(defmethod add-task ((self thread-pool) fun)
  (mp:mailbox-send (taskqueue self) fun))

(defmethod add-multiple-tasks ((self thread-pool) tasklist)
  (loop for task in tasklist
        do
        (add-task self task))
  (poke-thread-pool self))