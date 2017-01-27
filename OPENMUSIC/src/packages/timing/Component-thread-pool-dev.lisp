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
;Author: Samuel Bell-Bell
;;===========================================================================

(in-package :om)

(defvar *threadpool* nil)

(defclass thread-pool ()
  ((taskqueue :initform (mp:make-mailbox) :accessor taskqueue)
   (pending-taskqueue :initform (list nil) :accessor pending-taskqueue)
   (workers :initform nil :initarg :workers :accessor workers) 
   (state :initform :stop :accessor state)
   (monitor-time :initform (make-instance 'om-time-mesure) :accessor monitor-time) 
  (time-key :initform 'threadkey :accessor time-key)
  ))

;============================================API
(defmethod build-thread-pool (nbthread)
  (assign-threads-function (setq *threadpool* (make-instance 'thread-pool)) nbthread)
  *threadpool*)

(defmethod abort-thread-pool ((self thread-pool))
  (loop for worker in (workers self)
        do
        (if (mp:process-alive-p worker)
            (mp:process-kill worker))))

(defmethod task-number-left ((pool thread-pool))
  (mp:mailbox-count (taskqueue pool)))

(defmethod engine-tasks-left ()
  (task-number-left *threadpool*))

;=============================================
(defmethod thread-function ((self thread-pool))
  (mp:ensure-process-mailbox) ;create a mailbox to the process
  (loop
   (let ((occthread)
         (task (mp:mailbox-read (taskqueue self))))
                                  
     (if (not task)
         (mp:process-wait-local "Waiting for request asleep" 'mp:mailbox-not-empty-p (taskqueue self))
       
       (progn
         (incf (thread-actif-cpt (monitor-time self))) 
         (mesure-all-time-us (monitor-time self) (cpt (monitor-time *engine*)) mp:*current-process* 
           (funcall task))
         (decf (thread-actif-cpt (monitor-time self)))) 
       ))))

                                                         

(defmethod poke-thread-pool ((pool thread-pool))
  (loop for m in (workers pool) do
        (mp:process-poke m)))


(defmethod assign-threads-function ((pool thread-pool) nbthread)
  (setf (workers pool) 
        (loop for i from 1 to nbthread
              collect
              (mp:process-run-function (format nil "Thread-~A" i)
                                       nil
                                       'thread-function pool))))

(defmethod add-worker ((self thread-pool) thread)
  (push thread (workers self)))

(defmethod add-task ((self thread-pool) fun)
  )


(defmethod add-multiple-tasks ((self thread-pool) tasklist)
  (loop for task in tasklist
        do
        (add-task self task))
  (poke-thread-pool self))

(defmethod add-multiple-tasks-delay ((self thread-pool) tasklist delay)
  (loop for task in tasklist
        do
        (add-task self task)
             (sleep delay)))
 

;---------------------get-computation-list-test

(defmethod get-computation-list-for-play ((self OMMaquette) &optional interval)
  (loop for box in (get-all-boxes self :sorted t)
        append
        (let ((b box))
          (if (and (all-reactive-p box)
                   (or (not interval)
                       (and interval
                            (in-interval (- (get-box-onset box) (pre-delay box)) interval))))
              (progn
                (setf (ready b) nil)
                (print "in computation list")
                (list (list (- (get-box-onset box) (pre-delay box))
                            (get-box-onset box)
                            #'(lambda ()
                                (with-schedulable-object self
                                                         (eval-box b)
                                                         (setf (ready b) t))
                                (clear-ev-once b)
                                (reset-cache-display b)
                                (set-display b :value)
                                (contextual-update b self)))))))))


;;;-----getter *engine*

(defmethod get-engine(nb)
*engine*
)

