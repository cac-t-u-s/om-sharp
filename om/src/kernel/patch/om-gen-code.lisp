;============================================================================
; o7: visual programming language for computer-aided music composition
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

;========================================
; LISP CODE GENERATION
; - most of thso part taken from OM6
;========================================



(in-package :om)


;;; Produces a (unique) name for a box
(defmethod gen-box-name ((self OMBox))
  (read-from-string 
   (string+ "box-" 
            (substitute #\- #\Space (name self)) 
            (format () "~3,'0d~3,'0d" (box-x self) (box-y self)))
   ))

;;;=================
;;; CONSTANT GEN-CODES
;;;=================

(defmethod gen-code ((self t) &optional numout) (declare (ignore numout)) self)
(defmethod gen-code ((self symbol) &optional numout) (declare (ignore numout)) `',self)
(defmethod gen-code ((self null) &optional numout) (declare (ignore numout)) nil)

;generate code for list
(defmethod gen-code ((self list) &optional numout) 
  (declare (ignore numout))
  (let ((sizelimit 2047))
    (if (> (length self) sizelimit)
        (let* ((tmp (copy-list self))
               (split (loop while tmp collect
                            (loop for n from 1 to sizelimit
                                  while tmp
                                  collect (pop tmp)))))
          `(append ,.(mapcar #'(lambda (x) (gen-code x)) split)))
      `(list ,.(mapcar #'(lambda (x) (gen-code x)) self))
      )))
  
;;;=================
;;; BOX INPUTS
;;;=================

(defun flat-quotes (list)
  (mapcar #'(lambda (val) (if (quoted-form-p val) (eval val) val)) list))

(defmethod gen-code-inputs ((self OMBoxCall))
   "Generation of Lisp code for inputs of <self>."
    (append (loop for input in 
                  (remove-if #'(lambda (item) (subtypep item 'box-keyword-input)) 
                             (inputs self) :key 'type-of)
                  collect (gen-code input))
            (gen-code-keywords self)))

(defmethod gen-code-keywords ((self OMBoxcall))
  (loop for key-in in (get-keyword-inputs self) 
        append (list (intern-k (name key-in)) (gen-code key-in))))

(defmethod gen-code ((self box-input) &optional numout)
   (declare (ignore numout))
   (if (connections self)
       ;;; in principle there is only 1 connection
       (let* ((output (from (car (connections self))))
              (box (box output))
              (numout (position output (outputs box))))
         (gen-code box numout))
     (gen-code (value self))))



;;;=================
;;; INPUT BOX
;;;=================

(defmethod gen-code ((self OMOutBox) &optional numout)
  (declare (ignore numout))
  (gen-code (car (inputs self))))

;;;=================
;;; INPUT BOX
;;;=================

(defmethod gen-code ((self OMInBox) &optional numout)
  (declare (ignore numout))
  (in-symbol (reference self)))

;;;=================
;;; GENERAL
;;;=================

(defmethod gen-code ((self OMBoxCall) &optional numout)
   "Generate Lisp code for the box <self> evaluated at <numout>."
   (cond
    
    ((equal (lock-state self) :locked)
     (gen-code-locked self numout))
    
    ((or (equal (lock-state self) :eval-once) 
         (and (get-pref-value :general :auto-ev-once-mode) 
              (> (length (get-out-connections self)) 1)))
     (gen-code-for-ev-once self numout))
    
    (t (gen-code-for-eval self numout))
    
    ))
 

(defmethod gen-code-locked ((self OMBoxCall) numout) 
  (gen-code (nth numout (value self))))

(defmethod gen-code-locked ((self OMBoxRelatedWClass) numout) 
  (if (or (null numout) (= 0 numout))
      (gen-code (car (value self)))
    `(get-slot-val ,(gen-code (car (value self))) ,(name (nth numout (outputs self))))))


;;; NOT LOCKED / FIRST EVAL-ONCE
(defmethod gen-code-for-eval ((self OMBoxCall) &optional numout)
  (case (lambda-state self)
    (:reference `',(reference self))
    (:box `,self)
    (:lambda ;(if (or (null numout)  ;;; we are iside a let / ev-once statement : return all as 'values'
             ;        (= (length (outputs self)) 1)) ;;  single output (to simplify the code)
             ;    `,(gen-code-lambda self)
             ;  `(nth ,numout (multiple-value-list ,(gen-code-lambda self))))
            `,(gen-code-lambda self numout))
    (otherwise (if (or (null numout)  ;;; we are inside a let / ev-once statement : return all as 'values'
                       (= (length (outputs self)) 1)) ;;  single output (to simplify the code)
                   `,(gen-code-for-call self)
                 ;;; normal call (not ev-once) and several outputs
                 `(nth ,numout (multiple-value-list ,(gen-code-for-call self)))
                 ))
    ))

(defmethod gen-code-for-eval ((self OMBoxRelatedWClass) &optional numout)
  (if (lambda-state self) 
      (call-next-method)
    (if (or (null numout)  ;;; we are inside a let / ev-once statement : return all as 'values'
            (= numout 0))  ;;  first output
        `,(gen-code-for-call self)
      `(get-slot-val ,(gen-code-for-call self) ,(name (nth numout (outputs self))))
      )))

;;;=================
;;; Eval Once
;;;=================
;;; EV-ONCE APPLIES AS A "LET" WHEN A BOX IS CONNECTED TO SEVERAL DESCENDANTS
;;; OR TO A REPEAT-N BOX
(defvar *let-list* nil)

;;; PUSHES IN THE LET-LIST: 
;;; - A LIST IF MULTIPLE OUTPUTS
;;; - A SIMPLE ELEMENT IF ONE OUTPUT
(defmethod gen-code-for-ev-once ((self OMBoxCall) numout)
   (let* ((varname (gen-box-name self))
          (newvar? (not (member varname *let-list* :test 'equal :key 'car))))
     (if (> (length (outputs self)) 1)
         (progn
           (when newvar?
             (push `(,varname (multiple-value-list ,(gen-code-for-eval self nil))) *let-list*))
           `(nth ,numout ,varname))
       (progn 
         (when newvar?
           (push `(,varname ,(gen-code-for-eval self 0)) *let-list*))
         `,varname)
       )))

(defmethod gen-code-for-ev-once ((self OMBoxEditCall) numout)
   (let* ((varname (gen-box-name self))
          (newvar? (not (member varname *let-list* :test 'equal :key 'car))))
     (when newvar?
       (push `(,varname ,(gen-code-for-eval self nil)) *let-list*))
     (if (= numout 0)
          `,varname
       `(get-slot-val ,varname ,(name (nth numout (outputs self))))
      )))


;;;=================
;;; Standard call code generation
;;;=================
(defmethod gen-code-for-call ((self OMFunBoxcall)  &optional args)
  (let ((arguments (or args (gen-code-inputs self))))
    `(,(reference self) ,.arguments)))

(defmethod gen-code-for-call ((self OMBoxAbstraction)  &optional args)
  (let ((fun `,(intern (string (compiled-fun-name (reference self))) :om))
        (arguments (or args (gen-code-inputs self))))
    (compile-if-needed (reference self))
    `(funcall ',fun ,.arguments)))


(defmethod gen-code-for-call ((self OMValueBox) &optional args)
  (declare (ignore args))
  (if (inputs self) 
      (if (= 1 (length (inputs self))) 
          `,(car (gen-code-inputs self))
        `(list ,.(gen-code-inputs self)))
    (gen-code (car (value self)))))

(defmethod gen-code-for-call ((self OMBoxEditCall) &optional args)
  (declare (ignore args))
  (let ((self-in (gen-code (car (inputs self)))))
    (if self-in
        (let ((c-args (get-connected-args self #'gen-code)))
          (if c-args 
              `(make-value-from-model ',(reference self) ,self-in 
                                      (list ,.(mapcar #'(lambda (arg) `(list ,(intern-k (car arg)) ,(cadr arg))) c-args)))
            `(make-value-from-model ',(reference self) ,self-in nil)))
      `(make-value ',(reference self) 
                   (list ,.(mapcar #'(lambda (arg) `(list ,(intern-k (car arg)) ,(cadr arg)))
                                   (get-all-args self #'gen-code)))))))
  
(defmethod gen-code-for-call ((self OMSlotsBox) &optional args)
  `(let ((obj ,(gen-code (car (inputs self)))))
     (when obj
       (set-value-slots obj (list ,.
                                  ;(mapcar #'(lambda (arg) `(list ,(intern-k (car arg)) ,(cadr arg)))
                                  (mapcar #'(lambda (arg) `(list ,(symbol-name (car arg)) ,(cadr arg)))
                                          (get-connected-args self #'gen-code))
                                  ))
       obj)))

;;;=================
;;; Lambda
;;;=================

;;; RETURN VALUES FOR THE DIFFERENT OUTPUTS
(defmethod gen-code-lambda ((self OMBoxcall) &optional numout)
  (multiple-value-bind (new-symbs args) 
      (get-args-eval-curry self #'gen-code)
    (let ((box-code (gen-code-for-call self (apply 'append args))))  ; `(funcall ',(reference self) ,.(apply 'append args))))
      (if (> (length (outputs self)) 1)
          (if numout ;; one specific output function is requested
              `#'(lambda ,new-symbs (nth ,numout (multiple-value-list ,box-code)))
            ;; all functions requested (LET/EV-ONCE)
            `(values .,(loop for n from 0 to (1- (length (outputs self))) collect
                             `#'(lambda ,new-symbs (nth ,n (multiple-value-list ,box-code))))))
        ;; single output
        `#'(lambda ,new-symbs ,box-code)))))
      
      
;;;=============================
;;; MAIN COMPILATION FUNCTION
;;;=============================

(defmethod gen-input-name ((in OMIn))
  (read-from-string 
   (string+ (format () "in~D_" (index in)) 
            (substitute #\_ #\Space (name in)))))

(defmethod gen-patch-input-names ((self OMPatch)) 
  (mapcar 
   #'(lambda (in) 
       (setf (in-symbol in) (gen-input-name in)))
   (sort (get-inputs self) '< :key 'index))
  )


(defmethod gen-patch-lisp-code ((self OMPatch)) 
  (let* ((boxes (boxes self))
         (input-names (gen-patch-input-names self))
         (*let-list* nil)
         
         (init-boxes (sort (get-boxes-of-type self 'OMPatchInitBox) '< :key 'index))
         (init-forms (loop for ib in init-boxes append (gen-code ib)))
         
         (loop-boxes (sort (get-boxes-of-type self 'OMPatchIteratorBox) '< :key 'index))
         (loop-forms (loop for lb in loop-boxes append (gen-code lb)))
         
         (out-boxes (sort (get-boxes-of-type self 'OMOutBox) '< :key 'index))
         (body 
          (if (> (length out-boxes) 1)
              `(values ,.(mapcar #'(lambda (out) (gen-code out)) out-boxes))
            (gen-code (car out-boxes)))))
    
    (values input-names
            (if *let-list*
                `(let* ,(reverse *let-list*) ,.init-forms ,.loop-forms ,body)
              (if (or init-forms loop-forms)
                  `(progn ,.init-forms ,.loop-forms ,body)
                body)
              ))
    ))


(defmethod get-patch-lambda-expression ((self OMPatch))
  (multiple-value-bind (input-names body)
      (gen-patch-lisp-code self)
    `(lambda (,.input-names) ,body)))


;;; compile : a fonction with N inputs and M outputs 
;;; (with tempin = 1st input / tempout = 1st output, if they exist)
(defmethod compile-patch ((self OMPatch)) 
  (let* ((oldletlist *let-list*))
    (setf *let-list* nil)
    (setf (compiled? self) t)
    
    (multiple-value-bind (input-names body)
        (gen-patch-lisp-code self)

      (let ((f-def `(defun ,(intern (string (compiled-fun-name self)) :om) 
                           (,.input-names) 
                      ,body)))
        
      ;(om-print-format "~%------------------------------------------------------~%PATCH COMPILATION:~%")
      ;(write function-def :stream om-lisp::*om-stream* :escape nil :pretty t)
      ;(om-print-format "~%------------------------------------------------------~%~%")
        
        (compile (eval f-def))))
         
    (setf *let-list* oldletlist)))


#|

(defmethod compile-patch ((self OMPatch)) 
  (let* ((boxes (boxes self))
         (out-boxes (sort (get-boxes-of-type self 'OMOutBox) '< :key 'index))
         (oldletlist *let-list*)
         (input-names 
          (mapcar #'(lambda (in) (setf (in-symbol in) (gen-input-name in))) 
                  (sort (get-inputs self) '< :key 'index))) 
         body function-def)
    (setf *let-list* nil)
    (setf (compiled? self) t)
    (setf body (if (> (length out-boxes) 1)
                   `(values ,.(mapcar #'(lambda (out) (gen-code out)) out-boxes))
                 (gen-code (car out-boxes))))
    (setf function-def
          `(defun ,(intern (string (compiled-fun-name self)) :om) (,.input-names) 
             (let* ,(reverse *let-list*) ,body)))
      ;(om-print-format "~%------------------------------------------------------~%PATCH COMPILATION:~%")
      ;(write function-def :stream om-lisp::*om-stream* :escape nil :pretty t)
      ;(om-print-format "~%------------------------------------------------------~%~%")
    (compile (eval function-def))
    (setf *let-list* oldletlist)
    ))



(defmethod curry-lambda-code ((self OMBoxEditCall) symbol)
  "Lisp code generetion for a factory in lambda mode."
  (let* ((nesymbs nil)
         (args  (mapcar #'(lambda (input)
                            (if (connected? input)
                              (gen-code input 0)
                              (let ((newsymbol (gensym)))
                                (push newsymbol nesymbs)
                                newsymbol))) (inputs self))))
    (cond
     ((connected? (first (inputs self)))
      `#'(lambda () 
           (objFromObjs ,(first args) (make-instance ',symbol))))
     ((= (length nesymbs) (length args))
      `#'(lambda ,(reverse nesymbs) 
           (cons-new-object (make-instance ',symbol) (list ,. args) nil)))
     (t
      `#'(lambda ,(cdr (reverse nesymbs)) 
           (cons-new-object (make-instance ',symbol) (list nil ,.(cdr args)) nil))))))

|#