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

(defvar *scheduler* nil)
(defvar *Lmin* 10)
(defvar *Lmax* *positive-infinity*)
(defvar *Ldefault* 100)

(defstruct (scheduler
            (:print-object
             (lambda (s stream)
               (print-unreadable-object (s stream :type t :identity t)
                 (princ `(:currently ,(scheduler-state s) :with :next :action :at ,(scheduler-next-date s)) stream)
                 ))))
  (state :idle :type symbol)
  (process nil :type (or mp:process null))
  (multi-thread t :type boolean) ;Single or multi thread architecture
  (register nil :type list)
  (lock (mp:make-lock :name "Scheduler register lock") :type mp:lock)
  (alarm nil :type boolean)
  (stop-callback nil)
  (run-callback nil)
  (next-date *positive-infinity*)
  (timeout 1)
  (:documentation "Component to produce plans (schedule) on demand for objects in the register."))

;===================================================================Getters & Setters
(defmethod state ((self scheduler))
  (scheduler-state self))
(defmethod (setf state) (new-state (self scheduler))
  (setf (scheduler-state self) new-state))

(defmethod process ((self scheduler))
  (scheduler-process self))
(defmethod (setf process) (process (self scheduler))
  (setf (scheduler-process self) process))

(defmethod multi-thread ((self scheduler))
  (scheduler-multi-thread self))
(defmethod (setf multi-thread) (t-or-nil (self scheduler))
  (eval (architecture-switch t-or-nil))
  (setf (scheduler-multi-thread self) t-or-nil))

;;;list of object and graphic-caller pairs
(defmethod register ((self scheduler))
  (mp:with-lock ((lock self))
    (scheduler-register self)))
(defmethod (setf register) (register (self scheduler))
  (mp:with-lock ((lock self))
    (setf (scheduler-register self) register)))

;;;register lock
(defmethod lock ((self scheduler))
  (scheduler-lock self))
(defmethod (setf lock) (lock (self scheduler))
  (setf (scheduler-lock self) lock))

;;;wake-up alarm when running timer rendering mode
(defmethod alarm ((self scheduler))
  (scheduler-alarm self))
(defmethod (setf alarm) (t-or-nil (self scheduler))
  (setf (scheduler-alarm self) t-or-nil))

;;;stop-callback method to redefine for graphic-callers
;;;called when an object stops
(defmethod stop-callback ((self scheduler))
  (scheduler-stop-callback self))
(defmethod (setf stop-callback) (callback (self scheduler))
  (setf (scheduler-stop-callback self) callback))

;;;run-callback method to redefine for graphic-callers
;;;called periodically when playing
(defmethod run-callback ((self scheduler))
  (scheduler-run-callback self))
(defmethod (setf run-callback) (callback (self scheduler))
  (setf (scheduler-run-callback self) callback))

;;;next wake up date, when running timer rendering mode
(defmethod next-date ((self scheduler))
  (scheduler-next-date self))
(defmethod (setf next-date) (callback (self scheduler))
  (setf (scheduler-next-date self) callback))

;;;maximum duration for a scheduling operation to perform
(defmethod timeout ((self scheduler))
  (scheduler-timeout self))
(defmethod (setf timeout) (new-timeout (self scheduler))
  (setf (scheduler-timeout self) new-timeout))
;====================================================================================



;=================================================================================API
(defun build-scheduler (&key run-callback stop-callback)
  (let ((scheduler (make-scheduler :run-callback run-callback 
                                   :stop-callback stop-callback)))
    (setf (process scheduler) (mp:process-run-function "OM-Scheduler"
                                                       nil
                                                       'scheduler-function
                                                       scheduler))
    scheduler))

(defmethod abort-scheduler ((self scheduler))
  (if (mp:process-alive-p (process self))
      (mp:process-kill (process self))))

;=================================================================================
;;;reçoit des demandes de schedule dans sa mailbox et les traite
(defmethod scheduler-function ((self scheduler))
  (mp:ensure-process-mailbox)
  (loop
   (setf (state self) :idle)
   (let ((task (mp:mailbox-read (mp:process-mailbox
                                 (process self))
                                "Waiting for scheduling request")))
     (setf (state self) :busy)
     (om-ignore&print-error (funcall task)))))


(defmethod next-delay ((self scheduler))
  (- (next-date self) (om-get-internal-time)))

(defmethod refresh-next-date ((self scheduler))
  (setf (next-date self)
         (loop for obj in (register self)
               if (plan (car obj))
               minimize
               (act-timestamp (car (plan (car obj)))))))

(defmethod get-next-trigger ((self scheduler))
  (let ((date *positive-infinity*)
        (next (list nil *positive-infinity*)))
    (if (register self)
        (loop for object+caller in (register self)
              do
              (if (plan (car object+caller))
                  (setq date (act-timestamp (car (plan (car object+caller)))))
                (setq date 0))
              (if (< date (cadr next))
                  (setf (car next) (car object+caller)
                        (cadr next) date)))
      (setq next (list nil *positive-infinity*)))
    next))

(defmethod scheduler-idle-p ((self scheduler))
  (not (find :play (mapcar #'state (mapcar #'car (register self))))))

(defun poke-scheduling-system ()
  (setf (alarm *scheduler*) t)
  (if (and *dispatcher* (process *dispatcher*) (mp:process-stopped-p (process *dispatcher*)))
      (mp:process-unstop (process *dispatcher*)))
  (if (and *graphics* (process *graphics*) (mp:process-stopped-p (process *graphics*)))
      (mp:process-unstop (process *graphics*))))