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

;;;==================================
;;; BOXES WITH MEMORY
;;;==================================

(in-package :om)


(defclass OMPatchComponentWithMemory (OMPatchComponent) 
  ((mem-var :initform  (gentemp "MEM-") :accessor mem-var)
   (timer-var :initform  nil :accessor timer-var)))

(defmethod get-patch-component-box-def-color ((self OMPatchComponentWithMemory)) (om-make-color 0.82 0.7 0.7))

(defmethod initialize-instance :after ((self ompatchcomponentwithmemory) &rest initargs)
  (setf (timer-var self) (intern (string+ (symbol-name (mem-var self)) "-TIMER")))
  (eval `(defvar ,(mem-var self) nil))
  (eval `(defvar ,(timer-var self) nil)))


;;;------------------
;;; DELAY: 'mem'
;;; returns previous evaluation(s) on the right output
;;; the 'size' of memory can be expressed in number of cells (int) or in seconds (float)
;;;------------------

(defmethod special-box-p ((name (eql 'mem))) t)

(defclass OMMemory (OMPatchComponentWithMemory) 
  ((timetag :initform nil :accessor timetag)))

(defclass OMMemoryBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMMemory)) 'OMMemoryBox)
(defmethod box-symbol ((self OMMemory)) 'mem)


(defmethod get-icon-id ((self OMMemoryBox)) :m-mem)
(defmethod object-name-in-inspector ((self OMMemoryBox)) "memory/delay box")

(defmethod omNG-make-special-box ((reference (eql 'mem)) pos &optional init-args)
  (let* ((name (car (list! init-args)))
         (memory (make-instance 'OMMemory :name (if name (string name) "mem"))))
    (omNG-make-new-boxcall memory pos))
  )

(defmethod create-box-inputs ((self OMMemoryBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "data to record in memory")
   (make-instance 
    'box-input :box self :value NIL
    :name "size of memory")))

(defmethod create-box-outputs ((self OMMemoryBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "current value")
   (make-instance 
    'box-output :box self :value NIL
    :name "past value(s)")))

;;; ALWAYS IN "EV-ONCE" MODE

(defmethod omNG-box-value ((self OMMemoryBox) &optional (numout 0))
    
    (unless (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*))
      
       (let ((inval (omng-box-value (car (inputs self))))
             (size (omng-box-value (cadr (inputs self)))))
 
         (setf (ev-once-flag self) (get-ev-once-flag *ev-once-context*))
         
         (setf (value self)
               
               (cond 
                ((integerp size)
                 (list inval 
                       ;;; last-n values
                       (first-n (cons (car (value self)) 
                                      (list! (cadr (value self))))
                                size)))
                
                ((floatp size)
                 (list inval
                       ;;; values received since last time-window started
                       (if (or (null (timetag (reference self)))  ;;; fresh memory
                               (> (clock-time) (+ (* size 1000) (timetag (reference self))))) ;;; time out
                           (progn 
                             (setf (timetag (reference self)) (clock-time))
                             (list inval))
                         (cons inval 
                               (list! (cadr (value self)))))
                       ))
                
                (t (list inval (car (value self))))))
         )
       )
    (return-value self numout))



(defmethod gen-code  ((self OMMemoryBox) &optional (numout 0))
  
  (let* ((global-var (mem-var (reference self)))
         (global-timer (timer-var (reference self)))
         (local-name (intern (string+ (symbol-name global-var) "-LOCAL")))
         (first-call (not (check-let-statement local-name :global))))
    
    (when first-call
      (let ((new-val (gen-code (car (inputs self))))
            (mem-size (gen-code (cadr (inputs self)))))
                
        (push-let-statement 
         `(,local-name (setf ,global-var 
                             ,(cond ((integerp mem-size)
                                     `(list ,new-val  
                                            (first-n (cons (car ,global-var) 
                                                           (list! (cadr ,global-var)))
                                                     ,mem-size)))
                                         ((floatp mem-size)
                                          `(list ,new-val
                                                 ;;; values received since last time-window started
                                                 (if (or (null ,global-timer)  ;;; fresh memory
                                                         (> (clock-time) (+ ,(* mem-size 1000) ,global-timer))) ;;; time out
                                                     (progn 
                                                       (setf ,global-timer (clock-time))
                                                       (list ,new-val))
                                                   (cons ,new-val 
                                                         (list! (cadr ,global-var))))
                                                 ))
                                         (t `(list ,new-val (car ,global-var))))))
         :global)
        ))
    
    `(nth ,numout ,local-name)
    ))
  
  

;;;------------------
;;; COLLECT
;;;------------------



(defmethod special-box-p ((name (eql 'collect))) t)
(defmethod special-item-reference-class ((item (eql 'collect))) 'OMCollect)

(defclass OMCollect (OMPatchComponentWithMemory) ()
  (:documentation "General collector"))

(defclass OMCollectBox (OMPatchComponentBox) ())

(defmethod get-box-class ((self OMCollect)) 'OMCollectBox)
(defmethod box-symbol ((self OMCollect)) 'collect)

(defmethod get-icon-id ((self OMCollectBox)) :m-mem)
(defmethod object-name-in-inspector ((self OMCollectBox)) "collector box")



(defmethod omNG-make-special-box ((reference (eql 'collect)) pos &optional init-args)
  (let ((name (car (list! init-args))))
    (omNG-make-new-boxcall 
     (make-instance 'OMCollect :name (if name (string name) "collect"))
     pos)))

(defmethod create-box-inputs ((self OMCollectBox)) 
  (list 
   (make-instance 
    'box-input :box self :value NIL
    :name "data-in" :doc-string "(collected in memory)")
   (make-instance 
    'box-input :box self :value NIL
    :name "push" :doc-string "propagates reactive notification to the data-out outlet")
   (make-instance 
    'box-input :box self :value NIL
    :name "init" :doc-string "reinitializes memory")))

(defmethod create-box-outputs ((self OMCollectBox)) 
  (list 
   (make-instance 
    'box-output :box self :value NIL
    :name "collect" :doc-string "collects and outputs data-in")
   (make-instance 
    'box-output :box self :value NIL
    :name "data-out" :doc-string "get collected data")
   (make-instance 
    'box-output :box self :value NIL
    :name "init" :doc-string "reinitializes memory")))


;;; COLLECT DOESN'T RESPOND TO EV-ONCE AT ALL: CAN BE CALLED SEVERAL TIMES
(defmethod omNG-box-value ((self OMCollectBox) &optional (numout 0))
  
  (unless nil ;;; (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*))

    (unless (value self) (setf (value self) (list nil)))
    
    (case numout
      ;;; collect
      (0 (let ((inval (omng-box-value (nth 0 (inputs self)))))
           (push inval (car (value self)))))
      ;;; output ; does nothing to the memory
      (1 NIL)
      ;;; init with the value in
      (2 (let ((initval (omng-box-value (nth 2 (inputs self)))))
           (setf (car (value self)) 
                 (if (equal initval t) NIL
                   (list! (om-copy initval))))))
      )
    )
  
    (return-value self numout))

(defmethod return-value ((self OMCollectBox) &optional (numout 0))
  (case numout
    ;;; last pushed
    (0 (caar (value self)))
    ;;; output ; does nothing to the memory
    (1 (reverse (car (value self))))
    ;;; init
    (2 nil)
    ))

;;; called in reactive mode
(defmethod current-box-value ((self OMCollectBox) &optional (numout nil))
  (if numout (return-value self numout) (value self)))


;;; REACTIVE BEHAVIOUR
;;; NOTIFY ONLY IF PUSH COMES IN
;;; stops reactive notification,
;;; performs evaluations if needed
;;; continue or not...

(defmethod OMR-Notify ((self OMCollectBox) &optional input-name)
  
  (unless nil ; (push-tag self) ;; allow several push at different inputs 

    (setf (push-tag self) t)
    
    (cond 
     ((string-equal input-name "data-in")
      (omNG-box-value self 0))
     
     ((string-equal input-name "init")
      (omNG-box-value self 2))
          
     ((string-equal input-name "push")
      
      (let ((listeners (get-listeners self)))
        (when listeners
          (setf (gen-lock self) t)
          (loop for listener in listeners do (omr-notify (car listener) (cadr listener)))
          (setf (gen-lock self) nil))))
     )
    ))


;;; COMPILED FORM

(defmethod gen-code  ((self OMCollectBox) &optional (numout 0))

  ; (print (list "gen-code collect - stack = " *let-list-stack*))
  
  (let* ((global-var (mem-var (reference self)))
         (local-name (intern (string+ (symbol-name global-var) "-LOCAL")))
         (first-call (not (check-let-statement local-name :global))))
    
    (when first-call
      (let ((init-val (gen-code (nth 2 (inputs self)))))
        (push-let-statement `(,local-name ,(if (equal init-val t) nil init-val)) :global)))
    
    (case numout
      ;;; collect
      (0 `(let ((collect-val ,(gen-code (nth 0 (inputs self))))) 
            (pushr collect-val ,local-name)
            collect-val))
      ;;; output
      (1 local-name)
      ;;; init with the value in
      (2 `(let ((init-val ,(gen-code (nth 2 (inputs self)))))
            (setf ,local-name (if (equal init-val t) nil init-val))
            init-val))
      )
    ))



