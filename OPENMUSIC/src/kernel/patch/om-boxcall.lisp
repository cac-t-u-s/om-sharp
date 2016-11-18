
(in-package :om)

;---------------------------------------
;Boxes in a Patch
;---------------------------------------

(defclass OMBoxCall (OMBox) 
  ((lock-state :initform nil :accessor lock-state :initarg :lock-state)    ;;; can be (nil :locked :eval-once)
   (lambda-state :initform nil :accessor lambda-state :initarg :lambda-state)  ;;; can be (nil :lambda :reference :box)
   (ev-once-flag :accessor ev-once-flag :initform nil)
   ;;; REACTIVE FLAGS
   (state-lock :accessor state-lock :initform nil) ;; this box is the event source and his evaluation is locked
   (gen-flag :accessor gen-flag :initform nil) ;; this box has already been valuated during this generation 
   (push-tag :accessor push-tag :initform nil) ;; this box is tagged as being is in the notification path for the current event
   ;;;MAQUETTE TAG
   (show-markers :accessor show-markers :initform nil)
   )
  (:documentation "This is the class for boxes in a Patch or in a maquette.
All boxes which their reference is a OM generic function are instances of this class.")
  (:metaclass omstandardclass))


;---------------Protocole--------------

(defmethod omNG-make-new-boxcall ((reference t) pos &optional init-args) nil)

(defmethod add-keyword-input ((self t) &key key (value nil val-supplied-p) doc reactive) nil)
(defmethod add-optional-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod allow-rename ((self OMBoxcall)) nil)

(defmethod get-input-doc ((self OMBoxcall) name) nil)
(defmethod get-input-menu ((self OMBoxcall) name) nil)
(defmethod get-input-def-value ((self OMBoxcall) name) nil)
(defmethod get-output-doc ((self OMBoxcall) i) (format nil "out~D" i))

(defmethod get-box-value ((self OMBoxCall)) (car (value self)))

(defmethod set-property ((object OMBoxCall) (prop-id (eql :lambda)) val)
  (set-lambda object val))

(defmethod set-lambda ((self OMBoxCall) value) 
  ;;; reinit value only if switch from lambda to normal.
  (when (or (and (equal (lambda-state self) :lambda) (equal value nil))
            (and (equal (lambda-state self) nil) (equal value :lambda)))
    (setf (value self) nil))
  (setf (lambda-state self) value))


(defmethod set-lock-mode ((self t)) nil)
(defmethod set-lambda-mode ((self t)) nil)
(defmethod set-evonce-mode ((self t)) nil)

(defmethod set-lock-mode ((box OMBoxCall)) 
  (when (valid-property-p box :lock)
    (setf (lock-state box) (if (lock-state box) nil :locked))
    (update-inspector-for-box box)
    (om-invalidate-view (frame box))
    (when (container box)
      (report-modifications (editor (container box))))))

(defmethod set-evonce-mode ((box OMBoxCall)) 
  (when (valid-property-p box :lock)
    (setf (lock-state box) (if (equal (lock-state box) :eval-once) nil :eval-once))
    (update-inspector-for-box box)
    (om-invalidate-view (frame box))
    (when (container box)
      (report-modifications (editor (container box))))))
  
(defmethod set-lambda-mode ((box OMBoxCall)) 
  (when (valid-property-p box :lambda)
    (set-lambda box (if (lambda-state box) nil :lambda))
    (update-inspector-for-box box)
    (om-invalidate-view (frame box))
    (when (container box)
      (report-modifications (editor (container box))))))


(defmethod set-reactive-mode ((box OMBox)) 
  (if (or (inputs box) (outputs box))
      (progn (set-reactive box (not (all-reactive-p box)))
        (update-inspector-for-box box)
        (om-invalidate-view (frame box))
        (update-frame-connections-display (frame box))
        (when (container box)
          (report-modifications (editor (container box)))))
    (om-print "Boxes must have inputs or outputs to be set reactive"))) 


;;; from inspector

;;; reactive is not a "real" property
(defmethod valid-property-p ((object OMBoxCall) (prop-id (eql :reactive))) nil)

(defmethod set-property ((object OMBoxCall) (prop-id (eql :reactive)) val)
  (set-reactive object val))
      
(defmethod get-property ((object OMBoxCall) (prop-id (eql :reactive)) &key (warn t))
  (all-reactive-p object))

(defmethod all-reactive-p ((self OMBoxCall))
  (and (or (inputs self) (outputs self))
       (not (find-if-not 'reactive (append (inputs self) (outputs self))))))
  
(defmethod set-reactive ((self OMBoxCall) val) 
  (mapc #'(lambda (io) 
            (setf (reactive io) val))
        (append (inputs self) (outputs self))))

(defmethod find-persistant-container ((self OMBox))
  (let ((container (container self)))
    (if container
        (or (is-persistant container)
            (find-persistant-container (car (references-to container))))
      (om-beep-msg "ERROR :: Could not find any parent document!"))
    ))

;-------------------------------------------
; PROPERTIES
;-------------------------------------------

(defmethod lock-modes-for-box ((self OMBoxCall)) '(nil :locked :eval-once))
(defmethod eval-modes-for-box ((self OMBoxCall)) '(nil :lambda :reference :box))

(defmethod get-properties-list ((self OMBoxCall))
  (append (call-next-method)
          `(("Execution" ;;; category
             (:lock "Lock state (b/1)" ,(lock-modes-for-box self) lock-state) ;;; id text type slot
             (:lambda "Eval mode" ,(eval-modes-for-box self) lambda-state) ;;; ex. :bool 
             (:reactive "Reactive (r)" :bool reactive)))))
                  
;--------------------------------------
; DEFAULT OUTPUTS
;-------------------------------------------

;;; VERIFY/DECIDE IF THE INPUTS/OUTPUTS NAMES ARE SYMBOLS OR STRINGS !!

(defmethod create-box-outputs ((self OMBoxCall)) 
  (loop for i from 0 to (1- (box-n-outs self)) collect
        (make-instance 'box-output :box self
                       :name (if (= 1 (box-n-outs self)) "out" (format nil "out~D" i))
                       :reference i
                       :doc-string (get-output-doc self i))))

;--------------------------------------
; BOXCALLs CAN HAVE SPECIAL OPTIONAL / KEYWORD INPUTS
;-------------------------------------------

(defclass box-optional-input (box-input) ())

(defmethod get-optional-inputs (box)
  (remove-if-not #'(lambda (item) (subtypep item 'box-optional-input)) (inputs box) :key 'type-of))

(defmethod next-optional-input ((self OMBox)) nil)
(defmethod more-optional-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod add-optional-input ((self OMBoxcall) &key name (value nil val-supplied-p) doc reactive)
  (set-box-inputs self (append (inputs self)
                               (list (make-instance 'box-optional-input
                                                    :name (string-downcase name)
                                                    :value value
                                                    :box self
                                                    :doc-string (or doc "optional input")
                                                    :reactive reactive)))))


(defmethod remove-one-optional-input ((self OMBoxCall))
   (let ((optionals (get-optional-inputs self)))
     (when optionals 
       (let ((last-in (car (last optionals))))
         (mapcar #'(lambda (c) (omng-remove-element (container self) c)) (connections last-in))
         (set-box-inputs self (remove last-in (inputs self) :test 'equal))
         (do-delete-one-input-extra self)
         t))))


;;; used for subclasses such as patch boxes, loop boxes, sequence...
(defmethod do-delete-one-input-extra ((self OMBoxcall)) nil)


(defclass box-keyword-input (box-input) ())

(defmethod keyword-input-p ((self t)) nil)
(defmethod keyword-input-p ((self box-keyword-input)) t)


(defun get-keyword-inputs (box)
  (remove-if-not #'(lambda (item) (subtypep item 'box-keyword-input)) (inputs box) :key 'type-of))

(defmethod next-keyword-input ((self OMBoxCall))
  (let ((keywordlist (apply 'append (get-all-keywords self)))
        (usedkeywords (mapcar #'(lambda (in) (intern-k (name in))) (get-keyword-inputs self))))
    (if keywordlist 
        (or (find-if-not #'(lambda (elt) (member elt usedkeywords)) keywordlist)
            (values nil "All keywords are already used.."))
      (values nil (string+ "No keyword for box '" (name self) "'.")))))

(defmethod io-prefix ((self box-keyword-input)) ":")

;(defmethod more-keyword-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod def-reactive ((self OMBoxcall) key) nil)

(defmethod more-keyword-input ((self OMBoxcall) &key key (value nil val-supplied-p) doc (reactive nil reactive-supplied-p))
  (multiple-value-bind (def-next err-message) 
      (next-keyword-input self)
    (if def-next ;;; a kyword exist/is available
      (let ((keyname 
             (if key ;;; a specific name is asked for
                 (let ((keywordlist (mapcar 'intern-k (apply 'append (get-all-keywords self))))
                       (usedkeywords (mapcar #'(lambda (in) (intern-k (name in))) (get-keyword-inputs self))))
                   (and (or (find (intern-k key) keywordlist) (om-beep-msg (string+ "Wrong keyword name: " (string key))))
                        (or (not (find (intern-k key) usedkeywords)) (om-beep-msg (string+ "Keyword name already used: " (string key))))
                        (intern-k key))) ;;; key is ok
               def-next)))
        (when keyname
          (add-keyword-input self :key keyname
                             :value (if val-supplied-p value (get-input-def-value self keyname))
                             :doc (get-input-doc self (string-downcase keyname))
                             :reactive (if reactive-supplied-p reactive (def-reactive self keyname))) 
          ))
      (om-beep-msg err-message)
      )))

(defmethod add-keyword-input ((self OMBoxcall) &key key (value nil val-supplied-p) doc reactive)
    (set-box-inputs self (append (inputs self)
                                (list (make-instance 'box-keyword-input
                                                     :name (string key) ;; string-downcase
                                                     :value value
                                                     :box self
                                                     :doc-string (or doc "keyword input")
                                                     :reactive reactive
                                                   )))))

(defmethod remove-one-keyword-input ((self OMBoxCall))
  (let ((keywords (get-keyword-inputs self)))
    (when keywords
      (let ((last-in (car (last keywords))))
        (mapcar #'(lambda (c) 
                    (omng-remove-element (container self) c)) 
                (connections last-in))
        (set-box-inputs self (remove last-in (inputs self) :test 'equal))
       t))))

(defmethod change-keyword ((input box-keyword-input) key)
  (let ((old-name (name input)))
    (setf (name input) (string-downcase key)
          (value input) (get-input-def-value (box input) key)
          (doc-string input) (get-input-doc (box input) (string-downcase key))
          (reactive input) (def-reactive (box input) key))
    (update-output-from-new-in (box input) old-name input)
    ))

(defmethod update-output-from-new-in (box name in) nil)

(defmethod smart-copy-additional-inputs ((self OMBoxCall) newbox)
  (mapcar 
     #'(lambda (in) 
         (more-optional-input newbox :name (name in) :value (value in) :reactive (reactive in)))
     (get-optional-inputs self))
    (mapcar 
     #'(lambda (in) 
         (more-keyword-input newbox :key (intern-k (name in)) :value (value in) :reactive (reactive in)))
     (get-keyword-inputs self)))

(defmethod om-copy ((self OMBoxCall)) 
  (let ((newbox (call-next-method)))
    ;;; add the optional/keywords
    (smart-copy-additional-inputs self newbox) 
    newbox))


;-------------------------------------------
; SPECIAL OUTPUTS CORRESPONDING TO KEYWORD/OPTIONAL INPUTS
;-------------------------------------------

(defclass box-keyword-output (box-output) ())
(defclass box-optional-output (box-output) ())

(defun get-keyword-outputs (box)
  (remove-if-not #'(lambda (item) (subtypep item 'box-keyword-output)) (outputs box) :key 'type-of))

(defmethod optional-input++ ((self OMBoxCall))
  (more-optional-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod optional-input-- ((self OMBoxCall))
  (remove-one-optional-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    (om-invalidate-view (om-view-container (frame self))) ;; in case there were connections...
    ))

(defmethod keyword-input++ ((self OMBoxCall))
  (more-keyword-input self)
  (when (frame self)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))))

(defmethod keyword-input-- ((self OMBoxCall))
  (remove-one-keyword-input self)
  (when (frame self) 
    (set-frame-areas (frame self))
    (om-invalidate-view (om-view-container (frame self))) ;; in case there were connections...
    ))



;-------------------------------------------
; EDITOR
;-------------------------------------------

(defmethod open-editor ((self OMBoxCall)) 
  (when (object-has-editor (reference self))
    (open-editor (reference self))))


(defmethod update-from-reference ((self OMBoxCall))

  (let ((new-inputs (append (loop for i in (create-box-inputs self) collect 
                                  (let ((exist (find (reference i) (inputs self) :test 'equal :key 'reference)))
                                    (if exist (copy-io exist) i)))
                            (get-keyword-inputs self)))
        (new-outputs (append (loop for o in (create-box-outputs self) collect 
                                   (let ((exist (find (reference o) (outputs self) :test 'equal :key 'reference)))
                                     (if exist (copy-io exist) o)))
                             (get-keyword-outputs self))))
    
    (set-box-inputs self new-inputs)
    (set-box-outputs self new-outputs)

    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    t))

;-------------------------------------------
; BOX FOR  FUNCTIONS
;-------------------------------------------

(defclass OMFunBoxcall (OMBoxcall) ()
  (:metaclass omstandardclass))

(defmethod omNG-make-new-boxcall ((reference function) pos &optional init-args)
  (let* ((box (make-instance (get-box-class reference)
                             :name (string-downcase (function-name reference))
                             :reference (function-name reference)))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    (add-args-to-box box init-args)
    box))

(defmethod box-n-outs ((self OMFunBoxcall)) 1)

(defmethod create-box-inputs ((self OMFunBoxcall)) 
  (mapcar #'(lambda (arg) 
              (make-instance 'box-input 
                             :name (string arg) :reference arg
                             :box self
                             :value (get-input-def-value self arg)
                             :doc-string (get-input-doc self arg)
                             ))
          (subseq (function-arglist (reference self)) 0 (function-n-args (reference self)))))

(defmethod add-args-to-box (box args)
  (let ((main-args (firstn args (length (inputs box))))
        (other-args (nthcdr (length (inputs box)) args)))
    (mapcar #'(lambda (input val) (setf (value input) val)) (inputs box) main-args)
    (loop while other-args do
      (let ((arg (pop other-args)))
        (or (more-optional-input box :value arg)
            (and (symbolp arg) (string-equal "KEYWORD" (package-name (symbol-package arg)))
                 (more-keyword-input box :key arg :value (pop other-args)))
            (om-beep)))
        )))

(defmethod next-optional-input ((self OMFunBoxcall))
  (let ((current-inp (length (inputs self)))
        (lambda-lis (function-arglist (reference self))))  
    (cond ((< current-inp (+ (function-n-args (reference self))
                             (length (function-optional-args (reference self)))))
           (nth current-inp (function-arg-list (reference self))))
          ((find '&rest lambda-lis)
           (nth (1+ (position '&rest lambda-lis)) lambda-lis))
          (t nil))))

(defmethod more-optional-input ((self OMFunBoxcall) &key name (value nil val-supplied-p) doc reactive)
  (let ((new-in (next-optional-input self)))
    (when (and name (not (string-equal name (string new-in))))
      (print (format nil "WARNING -- WRONG OPTIONAL INPUT NAME: ~A -- Correct optional in the list is now: ~A" name new-in)))
    (when new-in
      (add-optional-input self 
                          :name new-in
                          :value (if val-supplied-p value (get-input-def-value self new-in))
                          :doc (get-input-doc self new-in) :reactive reactive)
      )
    ))

(defmethod get-all-keywords ((self OMFunBoxcall))
  (list (function-keyword-args (reference self))))
 

;; is this class useful ?
(defclass OMFunBoxFrame (OMBoxFrame) ())
(defmethod get-box-frame-class ((self OMFunBoxcall)) 'OMFunBoxFrame)

(defmethod resize-areas ((self OMFunBoxFrame))
  (list 
   (make-instance 'h-resize-area :object self :frame self
                  :pos #'(lambda (f) (om-make-point (- (w f) 8) 16))
                  :pick #'(lambda (f) (list 0 0 12 (- (h f) 16))))
   ))

;;; IN MAQUETTE VIEW
(defmethod scale-in-x-? ((self OMFunBoxCall)) nil)
(defmethod scale-in-y-? ((self OMFunBoxCall)) nil)

(defmethod get-properties-list ((self OMFunBoxCall))
  (hide-properties 
   (call-next-method) 
   '(:group-id)))


;-------------------------------------------
; BOX FOR STANDARD LISP FUNCTIONS
;-------------------------------------------

(defclass OMLispFBoxcall (OMFunBoxcall) ()
  (:metaclass omstandardclass))

(defmethod get-box-class ((self function)) 'OMLispFBoxcall)

(defmethod get-object-type-name ((self OMLispFBoxcall)) "Standard Lisp Function")

(defmethod get-icon-id-from-reference ((self OMLispFBoxcall)) 'lisp)



;-------------------------------------------
; BOX FOR OMGENERICFUNCTION
;-------------------------------------------

;;; TODO: MAINTAIN A REFERENCES-TO LIST IN REFERENCE GENFUN

(defclass OMGFBoxcall (OMFunBoxcall) ()
  (:metaclass omstandardclass))

(defmethod boxclass-from-function-name ((self t)) 'OMGFBoxCall)
 
(defmethod get-box-class ((self OMGenericFunction)) 
  (boxclass-from-function-name (function-name self)))

(defmethod get-object-type-name ((self OMGFBoxcall)) "Generic Function")

(defmethod get-icon-id-from-reference ((self OMGFBoxcall)) 
  (let ((ic (icon (fdefinition (reference self)))))
    (if (symbolp ic) ic
      (intern (format nil "~A" ic)))))

(defmethod box-n-outs ((self OMGFBoxcall))  
  (numouts (fdefinition (reference self))))

(defmethod get-output-doc ((self OMGFBoxCall) i) 
  (let ((txt (nth i (outputs-doc (fdefinition (reference self))))))
      (if txt (format nil "out~D: ~A" i txt)
        (call-next-method))))

(defmethod get-input-doc ((self OMGFBoxCall) name)
  (let* ((fun (fdefinition (reference self)))
         (pos (position name (function-arg-list fun) :key 'string :test 'string-equal)))
    (when pos (nth pos (inputs-doc fun)))))

(defmethod get-input-def-value ((self OMGFBoxCall) name) 
  (let* ((fun (fdefinition (reference self)))
        (pos (position name (function-arg-list fun) :key 'string :test 'string-equal)))
    (when pos (nth pos (inputs-default fun)))))

(defmethod get-input-menu ((self OMGFBoxCall) name)
  (let* ((fun (fdefinition (reference self)))
         (pos (position name (function-arg-list fun) :key 'string :test 'string-equal)))
    (when pos (nth pos (inputs-menus fun)))))

;(inputs-menus (fdefinition 'sort-list))



