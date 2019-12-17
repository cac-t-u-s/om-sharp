;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; File author: J. Bresson
;============================================================================


;;;==========================================================================
;;; BOXES WITH SPECIFIC BEHAVIOURS FOR REACTIVE PROCESSES
;;;==========================================================================


(in-package :om)

;;;=====================================
;;; SEND/RECEIVE INTERNAL
;;; WORKS WITH REACTIVE PROGRAMS
;;;=====================================

(defmethod* send ((self t) &optional (target :om))
  :initvals '(nil :om)
  :indoc '("something to send" "a target label")
  :doc "Sends <self> out for being received by RECEIVE boxes initialized with the <target> label."
  (let ((boxes (find-receive-boxes target)))
    (mapcar #'(lambda (b)
                (setf (value b) (list self))
                (self-notify b nil))
            boxes)
    (length boxes)))
           
      
(defmethod* receive (targetname) 
  :initvals '(:om)
  :indoc '("a target label")
  :doc "Receives data sent through SEND with the <targetname> label."
  t)

;;; for compat when loading old patches
(defmethod function-changed-name ((reference (eql 'om-receive))) 'receive)
(defmethod function-changed-name ((reference (eql 'om-send))) 'send)

(defclass ReactiveReceiveBox (OMGFBoxCall) ())

(defmethod boxclass-from-function-name ((self (eql 'receive))) 'ReactiveReceiveBox)

(defmethod boxcall-value ((self ReactiveReceiveBox))
  (let ((inval (omng-box-value (car (inputs self)))))
    (unless (equal inval (value (car (inputs self))))
      (print (format nil "RECEIVE ID SET TO: ~A" inval))
      (setf (value (car (inputs self))) inval)))
  (car (current-box-value self)))
 
(defun find-boxes (type)
  (loop for win in (remove-if-not 
                    #'(lambda (w) (equal 'patch-editor (type-of (editor w))))
                    (om-get-all-windows 'OMEditorWindow)) append
        (loop for b in (boxes (object (editor win))) 
              when (equal type (reference b))
              collect b)))

(defun find-receive-boxes (target)
  (let ((boxes (find-boxes 'receive)))
    (remove-if-not #'(lambda (b) (equal (value (nth 0 (inputs b))) target)) boxes)))


;;;=====================================
;;; ROUTE
;;; WORKS WITH REACTIVE PROGRAMS
;;;=====================================

;;; todo: check for undo/redo

(defun test-match (data test) 
  (if (functionp test) 
      (funcall test data)
    (equal test data)))

(defmethod* route (input &rest test)
  :indoc '("input data" "test-value or function")
  :doc "ROUTE sets its various outputs' values to <input> when <input> satisfies the test of the corresponding inputs, or to NIL otherwise.

The <test> can be a simple value (equality test) or a function (lambda) of 1 argument.

If the box output connections are reactive, reactive notifications will be send for non-NIL outputs.

The first output always outputs <input>.
"   
  (values-list (copy-list (cons input 
                                (mapcar 
                                 #'(lambda (route-item) 
                                      (when (test-match input route-item) input))
                                 test)))))

(defclass ReactiveRouteBox (RouteBox) 
  ((routed-o :initform nil :accessor routed-o)))

(defmethod boxclass-from-function-name ((self (eql 'route))) 'ReactiveRouteBox)

;;; (does nothing if there is no memory)
(defmethod boxcall-value ((self ReactiveRouteBox))
  (let ((new-values (multiple-value-list (call-next-method))))
    (setf (routed-o self) (loop for v in new-values 
                                for i = 0 then (+ i 1)
                                when v collect i))
    (values-list new-values)))
  

;;; NOTIFY ONLY THE ROUTED OUTPUT
;;; (can just check in values if there is no memory)
(defmethod OMR-Notify ((self ReactiveRouteBox) &optional input-name)
  (unless (push-tag self)
    (setf (push-tag self) t)
    (omNG-box-value self)
    (setf (gen-lock self) t)
    (let ((listeners (loop for o in (outputs self) 
                           for n = 0 then (1+ n) 
                           when (and (reactive o) (find n (routed-o self) :test '=))
                           append (loop for c in (connections o) 
                                        when (reactive (to c)) 
                                        collect (box (to c))))))
      (mapcar 'omr-notify listeners))
    (setf (gen-lock self) nil)
    ))



