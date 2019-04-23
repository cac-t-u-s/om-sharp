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
; File author: J. Bresson
;============================================================================

(in-package :om)

;;;=====================================
;;; ROUTE OSC ("adress" data)
;;;=====================================

(defun address-match (address route-path) 
    (string-equal address route-path))

(defmethod* route-osc ((message list) &rest osc-paths)
  :icon 'osc
  (values-list (copy-list (cons message 
                                (mapcar 
                                 #'(lambda (route-path) (if (listp (car message))
                                                            ;;; we have several messages here...
                                                            (let ((rep nil))
                                                              (loop for msg in message while (null rep) do
                                                                    (when (address-match (car msg) route-path) (setf rep (cdr msg))))
                                                              rep)
                                                         (when (address-match (car message) route-path) (cdr message))
                                                       ))
                                 osc-paths)))))

(defmethod boxclass-from-function-name ((self (eql 'route-osc))) 'ReactiveRouteBox)