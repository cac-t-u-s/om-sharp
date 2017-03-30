(in-package :om)

;THIS simple class combines methods needed for BPFs and Time-Sequences

(defclass object-with-action ()
  ((action :initform nil :accessor action :initarg :action)  ;;; user (interface) specification
   (action-fun :initform nil :accessor action-fun)  ;; actual (hidden lambda fun)
   ))

(defmethod set-action ((self object-with-action) action)
  (cond ((functionp action)
         (setf (action-fun self) action)
         (setf (action self) :internal-lambda))
        ((ompatch-p action)
         (compile-patch action)
         (setf (action-fun self) (intern-om (compiled-fun-name action)) 
               (action self) action))
        ((consp action)
         (setf (action-fun self) #'(lambda (x) (apply (car action) (cons x (cdr action))))
               (action self) action))
        ((and (symbolp action) (fboundp action))
         (setf (action-fun self) action
               (action self) action))
        ((equal action :internal-lambda) 
         (setf (action self) action)
         ;;; here we can do nothing more but hope there is a good lambda in action-fun
         (unless (functionp (action-fun self)) (om-beep-msg "Problem with internal lambda: Need to reload the action NEED TO RELOAD THE ACTION !!")))
        (action (om-beep-msg "Unrecognized action: ~A" action))))
  
(defmethod om-init-instance :around ((self object-with-action) &optional args)
  (let* ((object (call-next-method))
         (action (or (find-value-in-kv-list args :action)
                     (slot-value object 'action))))
    (set-action object action)
    object))

(defun test-action (p) 
  (print (list "test" p)))
