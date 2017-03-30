(in-package :om)

;;;=====================================
;;; ROUTE OSC ("adress" data)
;;;=====================================

(defun address-match (address route-path) 
    (string-equal address route-path))

(defmethod* route-osc ((message list) &rest osc-paths)
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