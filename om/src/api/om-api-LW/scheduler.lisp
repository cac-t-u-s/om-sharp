;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Authors: J. Bresson, C. Agon
;=========================================================================

;;===========================================================================
; SCHEDULER
;;===========================================================================

(in-package :om-api)

;;;===================
;;; export :
;;;===================
(export '(
          om-start-scheduler-old
          om-stop-scheduler-old
          om-delayed-funcall
          ) :om-api)


       
;------

(defvar *scheduler-timer* nil)

(defun scheduler-fun () t)

;(defun om-start-scheduler (fun)
;   (setq *scheduler-timer*  (mp:make-named-timer 'om-scheduler fun nil))
;   (mp:schedule-timer *scheduler-timer* 1 0.01))

(defun om-stop-scheduler-old ()
  (when *scheduler-timer*
    (mp:process-kill *scheduler-timer*)))

(defun om-start-scheduler-old (fun)
  (om-stop-scheduler-old)
  (setf *scheduler-timer*
        (mp:process-run-function  "OM SCHEDULER" '(:priority 10)
                                  #'(lambda ()
                                      (loop while t do
                                            (sleep 0.050)
                                            (funcall fun nil))))))

;(defun om-stop-scheduler ()
;  (when *scheduler-timer*
;    (mp:unschedule-timer *scheduler-timer*)
;    (setf *scheduler-timer* nil)))
  
(defun om-delayed-funcall (time func &rest args)
  (when *scheduler-timer* (mp:unschedule-timer *scheduler-timer*))
  (flet ((scheduler-fun () (apply func args)))
    (mp:schedule-timer *scheduler-timer* time)))

(define-action "When quitting image" "Stop scheduler" 'om-stop-scheduler-old)



