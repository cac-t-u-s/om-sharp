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

;---------------------------------------
;Boxes in a Patch
;---------------------------------------

(defclass OMBoxCall (OMBox) 
  ((lock-state :initform nil :accessor lock-state :initarg :lock-state)    ;;; can be (nil :locked :eval-once)
   (lambda-state :initform nil :accessor lambda-state :initarg :lambda-state)  ;;; can be (nil :lambda :reference :box)
   (ev-once-flag :accessor ev-once-flag :initform nil)
   (eval-flag :accessor eval-flag :initform nil))
  (:documentation "This is the class for boxes in a Patch or in a maquette.
All boxes which their reference is a OM generic function are instances of this class.")
  (:metaclass omstandardclass))


(defmethod omNG-make-new-boxcall ((reference t) pos &optional init-args) nil)

(defmethod add-keyword-input ((self t) &key key (value nil val-supplied-p) doc reactive) nil)
(defmethod add-optional-input ((self t) &key name (value nil val-supplied-p) doc reactive) nil)

(defmethod allow-rename ((self OMBoxcall)) nil)


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

(defmethod lock-modes-for-box ((self OMBoxCall)) 
  (append '(nil :locked) 
          (if (get-pref-value :general :auto-ev-once-mode) nil :eval-once)))

(defmethod eval-modes-for-box ((self OMBoxCall)) '(nil :lambda :reference :box))

(defmethod get-properties-list ((self OMBoxCall))
  (append (call-next-method)
          `(("Execution" ;;; category
             (:lock "Lock state (b/1)" ,(lock-modes-for-box self) lock-state) ;;; id text type slot
             (:lambda "Eval mode" ,(eval-modes-for-box self) lambda-state) ;;; ex. :bool 
             (:reactive "Reactive (r)" :bool reactive)))))
                  


(defmethod set-property ((object OMBoxCall) (prop-id (eql :lambda)) val)
  (set-lambda object val))

(defmethod set-lambda ((self OMBoxCall) value) 
  ;;; reinit value only if switch from lambda to normal
  (when (or (and (equal (lambda-state self) :lambda) (equal value nil))
            (and (equal (lambda-state self) nil) (equal value :lambda)))
    (setf (value self) nil))
  (setf (lambda-state self) value))

(defmethod update-after-change-mode ((box OMBox))
  (update-inspector-for-object box)
  (om-invalidate-view (frame box))
  (when (container box)
    (report-modifications (editor (container box)))))

(defmethod set-lock-state ((box OMBoxCall) mode) 
  (setf (lock-state box) mode)
  (update-after-change-mode box))

(defmethod switch-lock-mode ((self t)) nil)
(defmethod switch-lambda-mode ((self t)) nil)
(defmethod switch-evonce-mode ((self t)) nil)

(defmethod switch-lock-mode ((box OMBoxCall)) 
  (when (valid-property-p box :lock)
    (set-lock-state 
     box 
     (if (lock-state box) nil :locked))))

(defmethod switch-evonce-mode ((box OMBoxCall)) 
  (when (valid-property-p box :lock)
    (set-lock-state 
     box 
     (if (equal (lock-state box) :eval-once) nil :eval-once))
    ))
  
(defmethod switch-lambda-mode ((box OMBoxCall)) 
  (when (and (valid-property-p box :lambda)
             (member :lambda (eval-modes-for-box box)))
    (set-lambda box (if (lambda-state box) nil :lambda))
    (update-after-change-mode box)
    ))

;;; reactive is not a "real" property
(defmethod valid-property-p ((object OMBox) (prop-id (eql :reactive))) nil)

(defmethod set-property ((object OMBox) (prop-id (eql :reactive)) val)
  (set-reactive object val))
      
(defmethod get-property ((object OMBox) (prop-id (eql :reactive)) &key (warn t))
  (all-reactive-p object))

(defmethod all-reactive-p ((self OMBox))
  (and (or (inputs self) (outputs self))
       (not (find-if-not 'reactive (append (inputs self) (outputs self))))))
  
(defmethod set-reactive ((self OMBox) val) 
  (mapc #'(lambda (io) 
            (setf (reactive io) val))
        (append (inputs self) (outputs self))))

(defmethod set-reactive-mode ((box OMBox)) 
  (if (or (inputs box) (outputs box))
      (progn 
        (set-reactive box (not (all-reactive-p box)))
        (update-after-change-mode box)
        (update-frame-connections-display (frame box)))
    (om-beep-msg "Boxes must have inputs or outputs to be set reactive")))


;--------------------------------------
; DEFAULT OUTPUTS
;-------------------------------------------


(defmethod inputs-visible ((self OMBoxCall))
  (equal :reference (lambda-state self)))


;;; VERIFY/DECIDE IF THE INPUTS/OUTPUTS NAMES ARE SYMBOLS OR STRINGS !!

(defmethod create-box-outputs ((self OMBoxCall)) 
  (loop for i from 0 to (1- (box-n-outs self)) collect
        (make-instance 'box-output :box self
                       :name (if (= 1 (box-n-outs self)) "out" (format nil "out~D" i))
                       :reference i
                       :doc-string (get-output-doc self i))))

;-------------------------------------------
; EDITOR
;-------------------------------------------

(defmethod open-editor ((self OMBoxCall)) 
  (when (object-has-editor (reference self))
    (open-editor (reference self))))

(defmethod copy-if-exists ((io OMBoxIO) iolist)
  (let ((exists (find (reference io) iolist :test 'equal :key 'reference)))
    (if exists (copy-io exists) io)))

(defmethod update-from-reference ((self OMBoxCall))
  (let ((new-inputs (append (loop for i in (create-box-inputs self) collect 
                                  (copy-if-exists i (inputs self)))
                            (get-keyword-inputs self)))
        (new-outputs (append (loop for o in (create-box-outputs self) collect 
                                   (copy-if-exists o (outputs self)))
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

(defmethod omNG-make-new-boxcall ((reference function) pos &optional (init-args nil args-supplied-p))
  (let* ((box (make-instance (get-box-class reference)
                             :name (string-downcase (function-name reference))
                             :reference (function-name reference)))
         (size (minimum-size box)))
    (setf (box-x box) (om-point-x pos)
          (box-y box) (om-point-y pos)
          (box-w box) (om-point-x size)
          (box-h box) (om-point-y size))
    (when args-supplied-p (add-args-to-box box init-args))
    box))

(defmethod box-n-outs ((self OMFunBoxcall)) 1)

(defmethod h-resizable ((self OMFunBoxCall)) t)
(defmethod v-resizable ((self OMFunBoxCall)) nil)


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
        ;; (print arg)
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
      (om-print-format "WARNING -- WRONG OPTIONAL INPUT NAME: ~A -- Correct optional in the list is now: ~A" (list name new-in)))
    (when new-in
      (add-optional-input self 
                          :name new-in
                          :value (if val-supplied-p value (get-input-def-value self new-in))
                          :doc (get-input-doc self new-in) :reactive reactive)
      t)
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


(defmethod draw-name-as-icon ((box OMFunBoxCall) frame)
  (let* ((pack (symbol-package (reference box)))
         (pname (or (car (package-nicknames pack)) (package-name pack)))
         (font (om-make-font "Verdana" 7 :style '(:bold))))
    (om-draw-rounded-rect 2 6 20 (- (h frame) 12) :color (om-def-color :gray) :fill t :round 5)
    (om-draw-string (- 8 (* (length pname) 1.2)) 17 pname :font font :color (om-def-color :light-gray))
    t))


;-------------------------------------------
; BOX FOR STANDARD LISP FUNCTIONS
;-------------------------------------------

(defclass OMLispFBoxcall (OMFunBoxcall) ()
  (:metaclass omstandardclass))

(defmethod get-box-class ((self function)) 'OMLispFBoxcall)

(defmethod get-object-type-name ((self OMLispFBoxcall)) "Standard Lisp Function")

(defmethod get-icon-id ((self OMLispFBoxcall)) nil) ;; lisp

;; (defmethod box-draw ((self OMLispFBoxcall) (frame OMBoxFrame)) (call-next-method))


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

(defmethod get-icon-id ((self OMGFBoxcall)) 
  (let ((ic (icon (fdefinition (reference self)))))
    (and ic 
         (if (numberp ic) ic
           (intern-k (format nil "~A" ic)))
         )))
 
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



