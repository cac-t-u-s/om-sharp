;============================================================================
; om#: visual programming language for computer-assisted music composition
; J. Bresson et al. (2013-2020)
; Based on OpenMusic (c) IRCAM - Music Representations Team
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

(defmethod* osc-route ((message list) &rest osc-paths)
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

(defmethod boxclass-from-function-name ((self (eql 'osc-route))) 'ReactiveRouteBox)


;;; DEPRECATED / RENAMED. KEPT FOR COMPATIBILITY
(defmethod route-osc ((message list) &rest osc-paths)
  (apply 'osc-route (cons message osc-paths)))

(defmethod boxclass-from-function-name ((self (eql 'route-osc))) 'ReactiveRouteBox)
