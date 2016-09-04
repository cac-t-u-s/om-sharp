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