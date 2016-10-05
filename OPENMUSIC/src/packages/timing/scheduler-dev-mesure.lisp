(in-package :om)
;---------------scheduler-mesure------------------------------------------------


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
   (monitor-time (make-instance 'om-time-mesure)) ;monitor
  (time-key 'schedkey) ;key 
  (timeout 1)
  (:documentation "Component to produce plans (schedule) on demand for objects in the register."))


;;operation nil ? cmt l'avoir.
(defmethod scheduler-function ((self scheduler))
  (mp:ensure-process-mailbox)
  (setf (time-type  (scheduler-monitor-time self)) 'us)
  (loop
   (setf (state self) :idle)
   (let ((task (mp:mailbox-read (mp:process-mailbox
                                 (process self))
                                "Waiting for scheduling request")))
     (setf (state self) :busy)
     (om-ignore&print-error ( mesure-all-time-sched (scheduler-monitor-time self) (scheduler-time-key self) 
                                                  (funcall task))))
))


(defun micro-to-millisec (lst)
  (let (( y))
    (loop for x in lst do
          for y = (round (/ x 1000))
          collect y)))
