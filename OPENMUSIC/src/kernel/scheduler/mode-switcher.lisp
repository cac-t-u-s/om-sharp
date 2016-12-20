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

;================================================================Architecture switch
;================================================================Multi/Single Thread
;;;This code has to be updated according to modifications in the following functions :
;;;   - compute (macro)
;;;   - cast-computation-list (function)
;;;   - schedule (method)
;;;Also, it has access to the slot process of the dispatcher.

(defun architecture-switch (multi-thread-p)
  (if multi-thread-p
      `(progn
         ;;;;Redefine compute macro
         (defmacro compute (&rest body)
           `(progn
              (mp:mailbox-send (taskqueue *engine*) (lambda () ,@body))
              (poke-thread-pool *engine*)))
         ;;;;Recompile fuction using the compute macro
         (defun cast-computation-list (plan)
           (loop for task in plan
                 collect
                 (let ((fun (nth 2 task))
                       (data (nth 3 task)))
                   (act-alloc :timestamp (nth 0 task)
                              :fun #'(lambda () (if data
                                                    (compute (apply fun data))
                                                  (compute (funcall fun))))
                              :marker t))))
         ;;;Recompile the schedule method
         (defmethod schedule ((sched scheduler) (obj schedulable-object))
           (mp:process-send (process sched)
                            #'(lambda ()
                                (let ((I (get-next-I obj))
                                      (start-t (or (car (interval obj)) 0))
                                      bundles actlist)
                                  ;;;Get actions as raw data
                                  (om-with-timeout (timeout sched)
                                                   #'(lambda () 
                                                       (print (format nil "Schedule operation timed out (> ~As) : ~A stopped" (timeout sched) obj))
                                                       (stop-schedulable-object obj sched))
                                                   #'(lambda () (setq bundles (get-action-list-for-play obj (subseq I 0 2)))))

                                  ;;;Wrap actions in the appropriate data structure
                                  (setq actlist (loop for bundle in bundles
                                                      collect
                                                      (act-alloc :timestamp (car bundle)
                                                                 :fun (cadr bundle)
                                                                 :data (caddr bundle))))
                         
                                  ;;;Update the scheduler next action date (used in timer execution mode only)
                                  (if (car actlist)
                                      (setf (next-date sched) (min (next-date sched) (act-timestamp (car actlist)))))
                                  ;;;Add new actions to the object plan 
                                  (mp:with-lock ((plan-lock obj))
                                    (setf (plan obj)
                                          (sort (append (plan obj)
                                                        (if (nth 2 I)
                                                            (if (eq (nth 2 I) :loop)
                                                                ;;;If the object has to loop, reset its time at the end
                                                                (append actlist
                                                                        (list (act-alloc :timestamp (1- (cadr I))
                                                                                         :fun #'(lambda () 
                                                                                                  (incf (loop-count obj))
                                                                                                  (setf (ref-time obj) (- (om-get-internal-time) 
                                                                                                                          start-t)
                                                                                                        (play-planned? obj) nil)
                                                                                                  (setf (current-local-time obj) start-t)
                                                                                                  (schedule sched obj)
                                                                                                  (interleave-tasks obj (list start-t
                                                                                                                              (+ start-t (time-window obj))))
                                                                                                  (funcall 'set-time-callback obj (car (interval obj)))))))
                                                              ;;;If the object has to stop, stop the object at the end of interval
                                                              (append actlist
                                                                      (list (act-alloc :timestamp (1- (cadr I))
                                                                                       :fun #'(lambda ()
                                                                                                (funcall (run-callback sched) 
                                                                                                         (get-caller obj sched)
                                                                                                         (1- (cadr I)))
                                                                                                (player-stop-object sched obj)
                                                                                                )))))
                                                          ;;;If the object continues, keep scheduling
                                                          (append (list (act-alloc :timestamp (car I)
                                                                                   :fun #'(lambda ()  
                                                                                            (schedule sched obj)
                                                                                            (interleave-tasks obj (list (cadr I)
                                                                                                                        (+ (cadr I) (time-window obj)))))))
                                                                  actlist)))
                                                '< :key 'act-timestamp))))))))
    `(progn
       (defmacro compute (&rest body)
         `(progn ,@body))

       (defun cast-computation-list (plan)
         (loop for task in plan
               collect
               (let ((fun (nth 2 task))
                     (data (nth 3 task)))
                 (act-alloc :timestamp (nth 0 task)
                            :fun #'(lambda () (if data
                                                  (compute (apply fun data))
                                                (compute (funcall fun))))
                            :marker t))))

       (defmethod schedule ((sched scheduler) (obj schedulable-object))
         (let ((I (get-next-I obj))
               (start-t (or (car (interval obj)) 0))
               bundles actlist)
           ;;;Get actions as raw data
           (om-with-timeout (timeout sched)
                            #'(lambda () 
                                (print (format nil "Schedule operation timed out (> ~As) : ~A stopped" (timeout sched) obj))
                                (stop-schedulable-object obj sched))
                            #'(lambda () (setq bundles (get-action-list-for-play obj (subseq I 0 2)))))

           ;;;Wrap actions in the appropriate data structure
           (setq actlist (loop for bundle in bundles
                               collect
                               (act-alloc :timestamp (car bundle)
                                          :fun (cadr bundle)
                                          :data (caddr bundle))))
                         
           ;;;Update the scheduler next action date (used in timer execution mode only)
           (if (car actlist)
               (setf (next-date sched) (min (next-date sched) (act-timestamp (car actlist)))))
           ;;;Add new actions to the object plan 
           (mp:with-lock ((plan-lock obj))
             (setf (plan obj)
                   (sort (append (plan obj)
                                 (if (nth 2 I)
                                     (if (eq (nth 2 I) :loop)
                                         ;;;If the object has to loop, reset its time at the end
                                         (append actlist
                                                 (list (act-alloc :timestamp (1- (cadr I))
                                                                  :fun #'(lambda () 
                                                                           (incf (loop-count obj))
                                                                           (setf (ref-time obj) (- (om-get-internal-time) 
                                                                                                   start-t)
                                                                                 (play-planned? obj) nil)
                                                                           (setf (current-local-time obj) start-t)
                                                                           (schedule sched obj)
                                                                           (interleave-tasks obj (list start-t
                                                                                                       (+ start-t (time-window obj))))
                                                                           (funcall 'set-time-callback obj (car (interval obj)))))))
                                       ;;;If the object has to stop, stop the object at the end of interval
                                       (append actlist
                                               (list (act-alloc :timestamp (1- (cadr I))
                                                                :fun #'(lambda ()
                                                                         (funcall (run-callback sched) 
                                                                                  (get-caller obj sched)
                                                                                  (1- (cadr I)))
                                                                         (player-stop-object sched obj)
                                                                         )))))
                                   ;;;If the object continues, keep scheduling
                                   (append (list (act-alloc :timestamp (car I)
                                                            :fun #'(lambda ()  
                                                                     (schedule sched obj)
                                                                     (interleave-tasks obj (list (cadr I)
                                                                                                 (+ (cadr I) (time-window obj)))))))
                                           actlist)))
                         '< :key 'act-timestamp))))))))