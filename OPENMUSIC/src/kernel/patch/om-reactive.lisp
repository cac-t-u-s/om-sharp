
(in-package :om)

;;;==================
;;; REACTIVE STATUS
;;;==================

;;; returns the active inputs connected to the active outputs of self
(defmethod get-listeners ((self OMBoxCall))
  (remove-duplicates 
   (loop for o in (outputs self) when (reactive o)
         append (loop for c in (connections o) 
                      when (reactive (to c)) 
                      collect (box (to c))))))
  
;;;==================
;;; NOTIFICATION
;;;==================

(defmethod OMR-Notify ((self OMBoxCall))
  ;(print (list "NOTIFIED BOX" (name self)))
  (unless (push-tag self)
    (setf (push-tag self) t)
    (let ((listeners (get-listeners self)))
      (if listeners
          (mapcar 'omr-notify listeners)
        (omNG-box-value self)))))

;;; SELF-NOTIFICATION (NOTIFIES AND REEVALUATES ON A NEW THREAD)
(defmethod self-notify ((box OMBoxCall) &optional (separate-thread t) (eval-box nil))
  (let ((panel (and (frame box) (om-view-container (frame box)))))
      (funcall 
       (if separate-thread 'om-eval-enqueue 'eval)
       ;;;(if panel    
           
       `(progn
          (setf *current-eval-panel* ,panel)
          (when ,eval-box (omng-box-value ,box)) ; => only when an input is modified
          (when (get-listeners ,box)
            (setf (state-lock ,box) t)
            (OMR-Notify ,box)
            (setf (state-lock ,box) nil))
          (if ,panel (clear-ev-once ,panel)))
         
         ;;;`(OMR-Notify ,box) )
         
         
         )))

(defmethod clear-ev-once :around ((self OMBoxcall))
  ;(print "clear")
  (call-next-method)
  (setf (state-lock self) nil)
  (setf (gen-flag self) nil)
  (setf (push-tag self) nil)
  (when (frame self) (om-invalidate-view (frame self))))


;;;=================
;;; DEBUG/VISUALIZE EVALUATION
;;;=================

(defparameter *box-color-time* nil)
(defparameter *eval-color* (om-def-color :dark-red))
(defparameter *notify-color* (om-make-color 0.5 0.6 0.7))

(defun temp-box-color (box color &optional wait)
  (when wait
    (setf (color box) color)
    (when (frame box)
      (om-invalidate-view (frame box)))
    (when (plusp wait) (sleep wait))))

(push :debug-mode *features*)

#+debug-mode
(defmethod OMR-Notify :around ((self OMBoxcall))
  (let ((bcolor (color self)) rep)
    (unwind-protect 
        (progn (temp-box-color self *notify-color* *box-color-time*)
          (call-next-method))
      (temp-box-color self bcolor *box-color-time*))))
  
;;;==================
;;; BOX-VALUE
;;;==================

(defmethod omNG-box-value :around ((self OMBoxcall) &optional (numout 0))
  #+debug-mode
  (let ((bcolor (color self)) rep)
    (unwind-protect 
        (progn 
          (temp-box-color self *eval-color* *box-color-time*)
          (if (state-lock self)
              (current-box-value self numout)
            (let ((val (call-next-method)))
              ;(when (or 
              ;       (not (gen-flag self))
              ;       (and (equal (lock-state self) :eval-once) (not (ev-once-flag self)))
              ;       (equal (lock-state self) nil))
              ;  (setf (state-lock self) t)
              ;  (self-notify self)
              ;  ;(print (list "REF" (reference self)))
              ;  (setf (state-lock self) nil)
              ;  )
              (setf (gen-flag self) t)
              val)))
      (temp-box-color self bcolor *box-color-time*)))
  #-debug-mode
  (if (state-lock self)
      (current-box-value self numout)
    (let ((val (call-next-method)))
      (setf (gen-flag self) t)
      val))
  )

;;;=========================
;;; EVENTS 
;;;=========================

;;; when a box is evaluated (on request)
(defmethod eval-box :around ((self OMBoxCall))
  (call-next-method)
  (setf (state-lock self) t)
  (self-notify self)
  (setf (state-lock self) nil))



 
      

;;; when an input is edited
(defmethod set-value ((self box-input) value)
  (call-next-method)
  (when (reactive self) 
    (self-notify (box self) t t)))

;;; when a value box is modified
(defmethod set-value ((self OMValueBox) value)
  (call-next-method)
  (when (reactive (car (outputs self)))
    (self-notify self)))

;;; when an editor reports to the box
(defmethod update-from-editor :around ((self OMBoxEditCall))
  (call-next-method)
  (self-notify self))


