;=========================================================================
; LW Lisp Tools 
; Lisp programming tools for LispWorks delivered applications
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
; Author: J. Bresson
; Contributions from N. Ellis
;;========================================================================


(in-package :om-lisp)

(defvar *om-eval-process* nil)

(defun om-work-function ()
 
  #+cocoa(objc:make-autorelease-pool)
  (mp:ensure-process-mailbox)
 
 ;; This should really have an error handler.
 (loop (let ((event (mp:process-read-event (mp:process-event-queue
                                              (mp:get-current-process))
                                             "waiting for events")))
         (cond
          ((functionp event)
           (funcall event))
          ;((consp event)
          ; (apply (car event) (cdr event)))
          (t
           (eval event)
           )))))


(defparameter *eval-process-name* "OM EVAL PROCESS")

(defun init-om-eval-process ()
  (unless (and *om-eval-process*
               (mp:process-alive-p *om-eval-process*))
    (setq *om-eval-process*
          (mp:process-run-function *eval-process-name* () 'om-work-function))))

(defun om-eval-on-process (expression)
  (init-om-eval-process)
  (mp:ensure-process-mailbox *om-eval-process*)  
  (mp:process-send 
   *om-eval-process*
   expression
   ;(if (functionp expression) expression
   ;  #'(lambda () (eval expression)))
   ))

(defun om-kill-eval-process ()
  (when (and *om-eval-process*
             (mp:process-alive-p *om-eval-process*))
    (mp::process-kill *om-eval-process*)))

(define-action "When quitting image" "Abort EVAL process" 'om-kill-eval-process)


