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

;;===========================================================================
;  Actions
;;===========================================================================

(declaim (optimize (speed 3) (safety 0) (debug 1)))

(in-package :om)

;;;This list is used to accumulate already trigerred actions
;;;It is intended to avoid automatic garbage collection while rendering objects
;;;Its efficiency should be investigated  
(defvar *action-garbage* '())
  
;;===========================================================================
;;;Action
;;===========================================================================
;;;Structure
(defstruct (action
            (:print-object
             (lambda (a stream)
               (print-unreadable-object (a stream :type t :identity t)
                 (princ `(:at ,(act-timestamp a) :. :data :=,(act-data a)) stream))))
            (:conc-name act-))
  (timestamp 0 :type integer)
  (fun #'(lambda ()) :type function)
  (data nil)
  (marker nil :type boolean)
  (:documentation "A structure to wrap timed function calls from a SCHEDULABLE-OBJECT."))

;;;Allocate and Free action methods using cache pool
(defun new-action-pool (size)
  (let ((list (make-list size)))
    (loop
     for n on list do
     (setf (car n) (make-action)))
    list))

(let* ((cache-lock (mp:make-lock))
       (cache-size 1024)
       (cache-list '()))
  (declare (type fixnum cache-size))

  (mp:with-lock (cache-lock)
    (setf cache-list (new-action-pool cache-size)))

  (defun act-alloc (&key (timestamp 0) (fun #'(lambda ())) data marker)
    (mp:with-lock (cache-lock)
      (when (null cache-list)
	(setf cache-list (new-action-pool cache-size)
	      cache-size (* 2 cache-size)))
      (let ((act (pop cache-list)))
	(setf (act-timestamp act) timestamp
              (act-fun act) fun
              (act-data act) (if (listp data) data (list data))
              (act-marker act) marker)
	act)))

  (defmethod act-free ((self action))
    (mp:with-lock (cache-lock)
      (setf (act-timestamp self) 0
            (act-fun self) #'(lambda ())
            (act-data self) nil
            (act-marker self) nil)
      (push self cache-list)
      nil)))


;;;Function to trigger actions (inlined)
(declaim (inline %play-action))
(defun %play-action (self)
  (if (act-data self)
      (apply (act-fun self) (act-data self))
    (funcall (act-fun self)))
  (push self *action-garbage*))

;;;Function to clean the garbage list (inlined)
(declaim (inline %clean-action-garbage))
(defun %clean-action-garbage ()
  (loop while *action-garbage* do
        (act-free (pop *action-garbage*)))
  (gc-all))