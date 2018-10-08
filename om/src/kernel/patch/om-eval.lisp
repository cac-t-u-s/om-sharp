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

;;; set from the preferences
(add-preference :general :catch-errors "Handle Error Messages" :bool nil "Catch Lisp erros and display a simple message window")
;;; automatically decide wether a box should be evaluated in ev-once mode or not
(add-preference :general :auto-ev-once-mode "Auto ev-once" :bool t "Boxes automatically set in 'eval-once' mode")


(defvar *current-eval-panel* nil)

;;; ev-once-context can be a repeat-n or an om-loop box
;;; and return specific flags
(defparameter *ev-once-context* t)
(defmethod get-ev-once-flag ((self t)) t)

(defmethod clear-ev-once ((self t)) nil)

(defmethod clear-ev-once ((self patch-editor-view))
   "After one evaluation this methods set the ev-once flag of all boxes in ev-once mode to nil."
   (mapc #'(lambda (boxframe)
             (clear-ev-once (object boxframe))) (get-boxframes self))
   (setf *current-eval-panel* nil)
   (setf *ev-once-context* t))

(defmethod clear-ev-once ((self OMPatch))
   "After one evaluation this methods set the ev-once flag of all boxes in ev-once mode to nil."
   (mapc #'(lambda (box)
             (clear-ev-once box)) (boxes self))
   (setf *current-eval-panel* nil)
   (setf *ev-once-context* t))


(defmethod clear-ev-once ((self OMBox)) nil)

(defmethod clear-ev-once ((self OMBoxCall))
   "Reset the ev-once flag after each generation"
   (setf (ev-once-flag self) nil)
   (when (equal (lock-state self) :eval-once)
     (setf (value self) nil)))

(defmethod clear-after-error ((self OMBoxCall))
  (when (and (container self) (editor-view (container self)))
    (clear-ev-once (editor-view (container self))))
  )

(defun prompt-on-listeners (message)
  (om-lisp::om-listener-echo message)
  (prompt-on-main-window-listener message)
  (prompt-on-all-patch-listeners message))



;;;====================================================
;;; the 'flag' view is a utility view that tdisplays the box being evaluated
;;; and/or the box where an error occured, etc...
;;;====================================================

(defvar *flag-view-list* nil)

(defclass eval-flag-view (om-view) ())

(defmethod om-draw-contents ((self eval-flag-view))
  (om-draw-rounded-rect 0 0 (w self) (h self) :fill t :round 4))

(defun make-flag-view (box &optional color)
  (let ((v (om-make-view 'eval-flag-view :size (omp (+ (box-w box) 8) (box-h box))
                         :position (omp (- (box-x box) 4) (box-y box))
                         :fg-color (or color (om-make-color .3 .5 0.3 0.3)))))
    (push v *flag-view-list*)
    v))
       
(defun remove-flag-view (v)
  (when (om-view-container v)
    (om-remove-subviews (om-view-container v) v))
  (setf *flag-view-list* (remove v *flag-view-list*)))

(defun fade-out-flag-view (fv)
  (om-run-process 
   "fade-out-view"
   #'(lambda ()
       (loop for a = (om-color-a (om-get-fg-color fv))
             then (- a 0.1)
             while (>= a 0)
             do 
             (om-set-fg-color fv (om-make-color-alpha (om-get-fg-color fv) a))
             (om-invalidate-view fv)
             (sleep 0.1))
       (remove-flag-view fv)
       )))

(defun cleanup-flag-view-list ()
  (loop for v in *flag-view-list* do 
        (remove-flag-view v)))

;;;==========================
;;;==========================
;;;==========================

(defun eval-command (editor-view boxes)
  
  (prompt-on-listeners "Running...")
 
  (om-eval-enqueue  
   `(progn
      (setf *current-eval-panel* ,editor-view)
      (loop for b in ',boxes do
            (let ((fv (make-flag-view b)))
              (om-add-subviews ,editor-view fv)
              (eval-box b)
              (fade-out-flag-view fv)))
      (clear-ev-once ,editor-view)
      )
   :post-action #'(lambda ()
                    (prompt-on-listeners "Ready")))
  
  (om-invalidate-view editor-view))

(defun output-eval-command (out-area)
  (let* ((frame (frame out-area))
         (box (object frame))
         (n (position (object out-area) (outputs box)))
         (editor-view (om-view-container frame)))
    (prompt-on-listeners "Running...")
    (om-eval-enqueue 
     `(progn
        (setf *current-eval-panel* ,editor-view)
        (eval-box-output ,box ,n)
        (clear-ev-once ,editor-view)
        )
     :post-action #'(lambda () (prompt-on-listeners "Ready")))
    (om-invalidate-view editor-view)
    ))

(defun om-abort () 
  (when *current-eval-panel* (clear-ev-once *current-eval-panel*))
  (cleanup-flag-view-list)
  (prompt-on-listeners "Aborted")
  (om-lisp::om-kill-eval-process)
  (om-run-process "prompt_wait" 
                  #'(lambda () 
                      (om-process-wait 1000)
                      (prompt-on-listeners "Ready")))
  ;; (abort)
  )


(defmethod eval-box ((self ombox))
  (omng-box-value self)
  (let ((val (current-box-value self nil)))
    (if (<= (length val) 1)
        (om-print-format "=> ~s" (list (car val)) "OM")
      (om-print-format "=> [~{~s~^, ~}]" (list val) "OM"))
    ))

(defmethod eval-box ((self omboxeditcall))
  (om-ignore&print-error (player-stop-object *general-player* (car (value self))))
  (call-next-method))

(defmethod eval-box-output ((self ombox) n) 
  (let ((val (omng-box-value self n)))
    (if (om-shift-key-p)
        (output-value-as-new-box val *current-eval-panel* 
                                 (om-add-points (io-position-in-patch (area (nth n (outputs self)))) (om-make-point 0 20))
                                 (and (om-option-key-p) (nth n (outputs self))))
      (om-print-format "=> ~s" (list val) "OM"))))


;;;=================
;;; INPUT EVALUATION
;;;=================

(defmethod eval-box-inputs ((self OMBox))
  (append (loop for input in (remove-if
                              #'(lambda (item) (subtypep item 'box-keyword-input)) 
                              (inputs self) :key 'type-of)
                collect (omNG-box-value input))
          (eval-keywords self)))

(defmethod eval-keywords ((self OMBox))
  (loop for key-in in (get-keyword-inputs self)
        append (list (intern-k (name key-in)) (omNG-box-value key-in))))

(defmethod omNG-box-value ((self box-input) &optional numout)
   (declare (ignore numout))
   (if (connections self)
       ;;; in principle there is only 1 connection
       (let* ((output (from (car (connections self))))
              (box (box output))
              (numout (position output (outputs box)))
              (rep (omNG-box-value box numout)))   ;; ;; OM-COPY ??
         rep)
     (value self)))



(defmethod get-args-eval-curry ((self OMBoxCall) &optional (input-eval-fun 'value))
  (let* ((new-symbs nil)
         (args (loop for input in (inputs self) collect
                     (let ((val (if (connections input)
                                    (funcall input-eval-fun input)
                                  (let ((newsymbol (gensym)))
                                    (push newsymbol new-symbs)
                                    newsymbol))))
                        (if (subtypep (type-of input) 'box-keyword-input)
                            (list (intern-k (name input)) val)
                          (list val))))))
     (values (reverse new-symbs) args)))


;;;=================
;;; INPUT
;;; inputs are not evaluated when patch is compiled: they become function arguments...
;;;=================

(defmethod omNG-box-value ((self OMInBox) &optional (numout 0)) 
  (when t ; (inputs self)
    (set-value self (eval-box-inputs self)))
  (return-value self numout))


;;;=================
;;; BOX EVALUATION
;;;=================

;;; SETS VALUE AS A LIST FOR EVERY OUPUT 
;;; RETURNS THE REQUESTED (OR FIRST) INPUT
(defmethod omNG-box-value ((self OMBoxCall) &optional (numout 0)) 
  "Eval the output <numout> in <self>."
  
  (handler-bind ((error #'(lambda (c)
                            (when (get-pref-value :general :catch-errors)
                              (let ((fv (make-flag-view self (om-make-color 0.6 .3 .3 .5))))
                                
                                (when *current-eval-panel*
                                  (om-add-subviews *current-eval-panel* fv))
                                
                                (om-message-dialog (string+ "Error while evaluating the box " (string (name self)) " : " 
                                                            (format nil "~A" c))
                                                   :size (om-make-point 300 200))
                                (fade-out-flag-view fv)
                                (clear-after-error self)
                                (setf (eval-flag self) nil)
                                (sleep .5)
                                (om-abort))))))

    ;(print (list (lock-state self) (lambda-state self) (ev-once-flag self)))

    (cond
     
     ((equal (lambda-state self) :reference) (box-reference-value self))
     
     ((equal (lambda-state self) :box) 
      
      ;;; reevaluate normal then return the box
      (setf (lambda-state self) nil)
      (omng-box-value self numout)
      (setf (lambda-state self) :box)
      self)
     
     ((and (equal (lock-state self) :locked) (value self)) 
      (return-value self numout))
     
     ((and (or (equal (lock-state self) :eval-once)
               (get-pref-value :general :auto-ev-once-mode))
           
           (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*)))
      
      (return-value self numout))
     
     (t 
      (setf (eval-flag self) t)
      (om-invalidate-view (frame self))
      
      (let ((new-val 
             (cond ((equal (lambda-state self) :lambda) 
                    (multiple-value-list (box-lambda-value self)))
                   
                   ;;; general case here:
                   (t (multiple-value-list (boxcall-value self)))
                   )))
          (when (or (equal (lock-state self) :eval-once)
                    (get-pref-value :general :auto-ev-once-mode))
            ;;; first evaluation in this generation: set the value and flag
            (setf (ev-once-flag self) (get-ev-once-flag *ev-once-context*)))

          (set-value self new-val)
          (setf (eval-flag self) nil)
          (return-value self numout)))
     )))


(defmethod new-value-action ((self OMBoxCall)) nil)
 
(defmethod return-value ((self OMBoxCall) &optional (numout 0))
  (nth numout (value self)))

(defmethod boxcall-value ((self OMBoxCall))
  (apply (boxcall-function self) (eval-box-inputs self)))

(defmethod box-reference-value ((self OMBoxCall))
  (fdefinition (reference self)))

(defmethod box-lambda-value ((self OMBoxCall))
  (multiple-value-bind (lambda-args args) 
      (get-args-eval-curry self #'(lambda (input) `',(omNG-box-value input)))
    (let ((box-args (apply 'append args)))  ;;; flat the arg-list (for keywords etc.)
     ;(boxcall-lambda self new-symbs arglist)
     (values-list 
      (cons ;; MAIN OUTPUT 
            (eval `#'(lambda ,lambda-args (funcall ',(boxcall-function self) ,.box-args)))
            ;;; ADDITIONAL OUTPUTS
            (loop for n from 1 to (1- (length (outputs self))) collect 
                  (eval `#'(lambda ,lambda-args
                             (nth ,n (multiple-value-list (funcall ',(boxcall-function self) ,.box-args))))))
            )))))


;;;========================
;;; BOX-SPECIFIC
;;;========================

;;;----------------------
;;; FUNCION BOXES
;;;----------------------

(defmethod boxcall-function ((self OMFunBoxcall)) (reference self))

(defmethod boxcall-value ((self OMGFBoxcall))
  (let* ((arguments (eval-box-inputs self))
         (themethod (compute-applicable-methods (fdefinition (reference self)) arguments)))
    (if (null themethod)
        (progn (om-message-dialog (format nil "The method '~A' does not apply to arguments of types [~{~s~^ ~}]. " 
                                          (name self) (mapcar 'type-of arguments)))
          (om-abort))
      (apply (boxcall-function self) arguments))))

;;;--------------------------
;;; PATCH BOX
;;;--------------------------
        
(defmethod boxcall-function ((self OMBoxAbstraction))
  (compile-if-needed (reference self))
  (intern (string (compiled-fun-name (reference self))) :om))

(defmethod box-reference-value ((self OMBoxPatch))
  (reference self))

;;;----------------------
;;; VALUE BOX
;;;----------------------

(defmethod omNG-box-value ((self OMValueBox) &optional (numout 0))
  (declare (ignore num-out))
  (handler-bind ((error #'(lambda (c) 
                            (when (get-pref-value :general :catch-errors)
                              (let ((fv (make-flag-view self (om-make-color 0.6 .3 .3 .5))))
                                (when *current-eval-panel*
                                  (om-add-subviews *current-eval-panel* fv))
                                (om-message-dialog (string+ "Error while evaluating the box " (string (reference self)) " : " 
                                                            (format nil "~A" c))
                                                   :size (om-make-point 300 200))
                                (fade-out-flag-view fv)
                                (clear-after-error self)
                                (om-abort))))))
    (cond

     ((equal (lock-state self) :locked) (car (value self)))

     ((and (or (equal (lock-state self) :eval-once)
               (get-pref-value :general :auto-ev-once-mode)) 
           (equal (ev-once-flag self) (get-ev-once-flag *ev-once-context*)))
      
      (car (value self)))

     (t (when (inputs self) 
          (if (= 1 (length (inputs self))) 
              (set-value self (eval-box-inputs self))
            (set-value self (list (eval-box-inputs self)))))
        (when (equal (lock-state self) :eval-once)
          ;;; first evaluation in this generation: set the value and flag
          (setf (ev-once-flag self) (get-ev-once-flag *ev-once-context*)))
        (nth numout (value self))
        ))))

(defmethod clear-ev-once ((self OMValueBox))
  (when t ; (equal (lock-state self) :eval-once)
    (setf (ev-once-flag self) nil)))



;;;----------------------
;;; OBJECT BOX
;;;----------------------

;;; SETS the edit-params, no matter if the box is locked or not
(defmethod omNG-box-value ((self OMBoxEditCall) &optional (numout 0)) 
  
  ;(unless (equal (lock-state self) :locked)
  ;  (setf (value self) (list (make-instance (reference self)))) ;; test if no problem...
  ;  (om-invalidate-view (frame self)))

  (let ((box-attributes (loop for input in (cdr (inputs self))
                              when (and (find (intern-k (name input)) 
                                              (additional-box-attributes-names self))
                                        ;; (connections input)  ;; no need for it to be connected.. (or why ?)
                                        )
                              collect (list (intern-k (name input)) (omng-box-value input)))))
    
    (loop for attr in box-attributes do (set-edit-param self (car attr) (cadr attr)))
    
    (when (and (equal (lock-state self) :locked) ;; otherwise the editor will be updated later on anyway
               (editor self)
               box-attributes)
      (update-to-editor (editor self) self))
    )
  (call-next-method))



(defmethod current-box-value ((self OMBoxRelatedWClass) &optional (numout nil))
  (if numout (and (value self) (rep-editor self numout)) (value self)))

;;; when some things can be released.. (e.g. sound buffers)
(defmethod release-previous-value ((self t)) nil)

(defmethod return-value ((self OMBoxRelatedWClass) &optional (numout 0))
  (rep-editor self numout))

;;; Reset the ev-once flag after each generation but does not reinitialize the value
(defmethod clear-ev-once ((self OMBoxEditCall))
   (when t ; (equal (lock-state self) :eval-once)
     (setf (ev-once-flag self) nil)))

(defmethod set-value ((self OMBoxEditCall) value)
  (release-previous-value (car (value self)))
  (call-next-method)
  (update-after-eval self))


(defmethod boxcall-value ((self OMBoxEditCall))
  (let ((self-in (omNG-box-value (car (inputs self)))))
    (if self-in
        (make-value-from-model (reference self) self-in (get-connected-args self #'omng-box-value))
      (make-value (reference self) (get-all-args self #'omng-box-value))
      )))

(defmethod boxcall-value ((self OMSlotsBox)) 
  (let ((self-in (omNG-box-value (car (inputs self)))))
    (if self-in
        (let ((connected-args (get-connected-args self #'omng-box-value))) 
          (set-value-slots self-in (loop for arg in connected-args 
                                         collect (list (symbol-name (car arg)) (cadr arg))))
          self-in)
      (error "!! The 'SLOTS' box must be connected to an instance of ~D" (string-upcase (reference self))))))


(defmethod box-reference-value ((self OMBoxRelatedWClass))
  (find-class (reference self) nil))

;;; we just don't do that for the moment
;;; no very useful anyway..
(defmethod box-lambda-value ((self OMBoxEditCall))
  (multiple-value-bind (lambda-list args) 
      (get-args-eval-curry self #'(lambda (input) `',(omNG-box-value  input)))
    (let* ((names (loop for i in (inputs self) collect (if (keyword-input-p i) nil (intern-k (name i)))))
           (self-in (if (not (equal (caar args) (car lambda-list))) ;;; first input is not in the lambda list
                        (caar args) nil))
           (arglist (loop for arg in (cdr args) 
                          for name in (cdr names)
                          collect (if name `(list ,name ,(car arg)) `(list ,.arg)))))
      (if self-in
          (eval `#'(lambda ,lambda-list
                     (let ((obj (objFromObjs ,self-in (make-instance ',(reference self)))))
                       (set-value-slots obj ,(loop for arg in arglist collect (list (symbol-name (car arg)) (cadr arg))))
                       obj)))   
        (eval `#'(lambda ,(cdr lambda-list)
                     (make-value ',(reference self) (list ,.arglist))))
        )
      )))      
      
(defmethod box-lambda-value ((self OMSlotsBox))
   (multiple-value-bind (lambda-list args) 
       (get-args-eval-curry self #'(lambda (input) `',(omNG-box-value  input)))
     (let* ((names (loop for i in (inputs self) collect (if (keyword-input-p i) nil (intern-k (name i)))))
            (obj (caar args))
            (arglist (loop for arg in (cdr args)
                           for name in (cdr names)
                           collect (if name `(list ,name ,(car arg)) `(list ,.arg)))))
       (eval `#'(lambda ,lambda-list
                  (set-value-slots ,obj ,(loop for arg in arglist collect (list (symbol-name (car arg)) (cadr arg))))
                  ,obj))
       )))






