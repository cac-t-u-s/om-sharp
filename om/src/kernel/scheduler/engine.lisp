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

(defvar *engine* nil)

(defstruct (engine)
  (state :idle :type symbol)
  (process nil :type (or mp:process null))
  (task-plan nil :type list)
  ;(alarm nil :type boolean)
  (behavior :EDF :type symbol)
  (:documentation "Component to perform non-zero time computations"))

;===================================================================Getters & Setters
(defmethod state ((self engine))
  (engine-state self))
(defmethod (setf state) (new-state (self engine))
  (setf (engine-state self) new-state))

(defmethod process ((self engine))
  (engine-process self))
(defmethod (setf process) (process (self engine))
  (setf (engine-process self) process))

(defmethod task-plan ((self engine))
  (engine-task-plan self))
(defmethod (setf task-plan) (plan (self engine))
  (setf (engine-task-plan self) plan))

(defmethod alarm ((self engine))
  (engine-alarm self))
(defmethod (setf alarm) (t-or-nil (self engine))
  (setf (engine-alarm self) t-or-nil))

(defmethod behavior ((self engine))
  (engine-behavior self))
(defmethod (setf behavior) (new (self engine))
  (setf (engine-behavior self) new))

(defun build-engine ()
  (let ((engine (make-engine)))
    (setf (process engine) (mp:process-run-function "OM-Engine"
                                                    nil
                                                    'engine-function
                                                    engine))
    engine))

(defmacro compute (&rest body)
  `(progn
     (mp:mailbox-send (taskqueue *engine*) (lambda () ,@body))
     (poke-thread-pool *engine*)))

;reflechir
(defmacro compute2 (obj clef time-type &body body)
`(let (res)
  (if (eq ,time-type 'ms)   
      (setq res (mesure-time-in-body-milli-sec ,obj ,clef ,@body)))    
;   (if (eq ,time-type 'us)
;       (setq res (mesure-time-in-body-micro-sec ,obj ,clef ,@body)))
;    (if (eq ,time-type 'ns)  
;       (setq res (mesure-time-in-body-nano-sec ,obj ,clef ,@body)))
    res))

(defmethod engine-function ((self engine))
  (mp:ensure-process-mailbox)
  (loop
   (setf (state self) :idle)
   ;(setf (alarm self) nil)
   (let ((task (mp:mailbox-read (mp:process-mailbox
                                 (process self))
                                "Waiting for computation request")))
     (when task
         (setf (state self) :busy)
         (om-ignore&print-error (funcall task))))))
   
;   (loop while (and (task-plan self)
;                    (<= (act-timestamp (first (task-plan self))) (om-get-internal-time)))
;         do
;         (om-ignore&print-error
;          (%play-action (pop (task-plan self)))))
;   (if (task-plan self)
;       (mp:process-wait-local-with-timeout "Sleeping" (max 0.001
;                                                           (/ (- (act-timestamp (first (task-plan self))) (om-get-internal-time))
;                                                              1000.0)) 'alarm self)
;     (mp:process-wait-with-timeout "Sleeping" 0.001))))