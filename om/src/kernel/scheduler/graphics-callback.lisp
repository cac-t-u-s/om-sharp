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

(defvar *graphics* nil)

(defstruct (graphics-handler)
  (state :stop :type symbol)
  (process nil :type (or mp:process null))
  (scheduler nil :type scheduler)
  (precision 50 :type integer)
  (:documentation "Component to send back timing informations to playing objects regitered in the corresponding scheduler."))

;===================================================================Getters & Setters
(defmethod state ((self graphics-handler))
  (graphics-handler-state self))
(defmethod (setf state) (new-state (self graphics-handler))
  (setf (graphics-handler-state self) new-state))

(defmethod process ((self graphics-handler))
  (graphics-handler-process self))
(defmethod (setf process) (process (self graphics-handler))
  (setf (graphics-handler-process self) process))

(defmethod scheduler ((self graphics-handler))
  (graphics-handler-scheduler self))
(defmethod (setf scheduler) (scheduler (self graphics-handler))
  (setf (graphics-handler-scheduler self) scheduler))

(defmethod precision ((self graphics-handler))
  (graphics-handler-precision self))
(defmethod (setf precision) (precision (self graphics-handler))
  (setf (graphics-handler-precision self) precision))
;====================================================================================

;=================================================================================API
(defun build-graphics-handler (scheduler &key (precision 50))
  (let ((graphics (make-graphics-handler :scheduler scheduler
                                         :precision precision)))
    (setf (process graphics) (mp:process-run-function "OM-Graphics"
                                                      nil
                                                      'graphics-function
                                                      graphics))
    graphics))

;====================================================================================
(defmethod graphics-function ((self graphics-handler))
  (let ((sched (scheduler self)))
    (loop
     (when (scheduler-idle-p sched) 
       (mp:process-stop (process self) "Idle"))
     (loop for object+caller in (register sched)
           do
           (when (and (run-callback sched) 
                      (cadr object+caller)
                      (eq (player-get-object-state sched (car object+caller)) :play))
             (om-ignore&print-error
              (funcall (run-callback sched) (cadr object+caller) (%get-obj-time (car object+caller))))))
     (om-process-wait (precision self)))))
