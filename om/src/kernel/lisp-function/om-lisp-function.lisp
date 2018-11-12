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

(defclass OMLispFunction (OMProgrammingObject) 
  ((text :initarg :text :initform "" :accessor text)
   (error-flag :initform nil :accessor error-flag)
   ))

(defmethod object-doctype ((self OMLispFunction)) :textfun)
(defmethod default-compiled-gensym  ((self OMLispFunction)) (gensym "lispfun-"))



(defclass OMLispFunctionInternal (OMLispFunction) ()
  (:default-initargs :icon :lisp-f)
  (:metaclass omstandardclass))

(defmethod window-name-from-object ((self OMLispFunctionInternal))
  (format nil "~A  [~A]" (name self) "internal Lisp function"))

(defclass OMLispFunctionFile (OMPersistantObject OMLispFunction) ()
  (:default-initargs :icon :lisp-f-file) 
  (:metaclass omstandardclass))


;; For conversions
(defmethod internalized-type ((self OMLispFunctionFile)) 'OMLispFunctionInternal)
(defmethod externalized-type ((self OMLispFunction)) 'OMLispFunctionFile)
(defmethod externalized-icon ((self OMLispFunction)) :lisp-f-file)

(defparameter *default-lisp-function-text* 
  '(";;; Edit a valid LAMBDA EXPRESSION"
    ";;; e.g. (lambda (arg1 arg2 ...) ( ... ))"
    "(lambda () (om-beep))"))

(defmethod make-new-om-doc ((type (eql :lispfun)) name)
  (make-instance 'OMLispFunctionFile 
                 :name name
                 :text *default-lisp-function-text*))

(defmethod omNG-make-special-box ((reference (eql 'lisp)) pos &optional init-args)
  (omNG-make-new-boxcall 
   (make-instance 'OMLispFunctionInternal
                  :name (if init-args (format nil "~A" (car (list! init-args))) "my-function")
                  :text *default-lisp-function-text*)
   pos init-args))

(defmethod decapsulable ((self OMLispFunction)) nil)

#|
(defmethod get-inputs ((self OMLispFunction)) 
  (compile-if-needed self)
  (let ((fname (intern (string (compiled-fun-name self)) :om)))
    (when (fboundp fname) 
      (let ((args (function-arg-list fname)))
        (loop for a in args 
              for i from 0 collect 
          (let ((in (make-instance 'OMIn :name (string a))))
            (setf (index in) i)
            in)))
      )))

(defmethod get-outputs ((self OMLispFunction)) 
  ;(compile-if-needed self)
  (let ((o (make-instance 'OMOut :name "out")))
    (setf (index o) 0)
    o))
|#

(defmethod update-lisp-fun ((self OMLispFunction))
  (compile-patch self)
  (loop for item in (references-to self) do
        (update-from-reference item)))

(defmethod save-document ((self OMLispFunctionFile))
  (call-next-method)
  (update-lisp-fun self))

(defmethod copy-contents ((from OMLispFunction) (to OMLispFunction))  
  (setf (text to) (text from))
  to)

;; other causes of update-lisp-fun are below in the editor

(defmethod compile-patch ((self OMLispFunction)) 
  "Compilation of a lisp function"
  (handler-bind 
      ((error #'(lambda (err)
                  (om-beep-msg "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                  (setf (compiled? self) nil)
                  (setf (error-flag self) t)
                  (abort err)
                  )))
    (setf (error-flag self) nil)
    (let* ((lambda-expression (read-from-string 
                               (reduce #'(lambda (s1 s2) (concatenate 'string s1 (string #\Newline) s2))
                                       (text self))
                               nil))
           (function-def
            (if (and lambda-expression (lambda-expression-p lambda-expression))
                (progn (setf (compiled? self) t)
                  `(defun ,(intern (string (compiled-fun-name self)) :om) 
                          ,.(cdr lambda-expression)))
              (progn (om-beep-msg "ERROR IN LAMBDA EXPRESSION!!")
                (setf (error-flag self) t)
                `(defun ,(intern (string (compiled-fun-name self)) :om) () nil)))))
      (compile (eval function-def))
      ;(setf (compiled? self) t)
      )
    ))

;;;===================
;;; BOX
;;;===================

(defmethod special-box-p ((name (eql 'lisp))) t)

(defclass OMBoxLisp (OMBoxAbstraction) ())
(defmethod get-box-class ((self OMLispFunction)) 'OMBoxLisp)

(defmethod draw-patch-icon :after ((self OMBoxLisp) &optional (offset-x 0) (offset-y 0))
  (when (error-flag (reference self))
    (om-with-fg-color (om-def-color :dark-red)
      (om-with-font (om-make-font "Arial" 16 :style '(:bold))
        (om-draw-string (+ offset-x 2) (+ offset-y (- (box-h self) 8)) "Error !!")))))

;;; OMLispFunction doesn't have OMIn boxes to buils the box-inputs from
(defmethod create-box-inputs ((self OMBoxLisp)) 
  (compile-if-needed (reference self))
  (let ((fname (intern (string (compiled-fun-name (reference self))) :om)))
    (when (fboundp fname) 
      (let ((args (function-arg-list fname)))
        (loop for a in args collect
              (make-instance 'box-input :name (string a)
                             :box self :reference nil)))
      )))

;;; OMLispFunction doesn't have OMOut boxes to buils the box-inputs from
(defmethod create-box-outputs ((self OMBoxLisp)) 
  (list 
   (make-instance 'box-output :reference nil 
                  :name "out"
                  :box self)))


(defmethod update-from-reference ((self OMBoxLisp))
  
  
  (let ((new-inputs (loop for i in (create-box-inputs self) 
                          for ni from 0 collect 
                          (if (nth ni (inputs self)) 
                              (let ((ci (copy-io (nth ni (inputs self))))) ;;keep connections, reactivity etc.
                                (setf (name ci) (name i)) ;; just get the new name
                                ci)
                            i)))
        (new-outputs (loop for o in (create-box-outputs self) 
                           for no from 0 collect 
                           (if (nth no (outputs self))
                               (copy-io (nth no (outputs self)))
                             o))))
    
    ;;; remove orphan connections 
    (loop for in in (nthcdr (length new-inputs) (inputs self)) do
          (mapc #'(lambda (c) (omng-remove-element (container self) c)) (connections in)))
 
    (set-box-inputs self new-inputs)
    (set-box-outputs self new-outputs)
    (set-frame-areas (frame self))
    (om-invalidate-view (frame self))
    t))




;;;===================
;;; EDITOR
;;;===================

(defclass lisp-function-editor (OMDocumentEditor) ())

(defmethod object-has-editor ((self OMLispFunction)) t)
(defmethod get-editor-class ((self OMLispFunction)) 'lisp-function-editor)

;;; nothing, e.g. to close when the editor is closed
(defmethod close-internal-elements ((self OMLispFunction)) nil)

;;; maybe interesting to make this inherit from OMEditorWindow..

(defclass lisp-function-editor-window (om-lisp::om-text-editor-window) 
  ((editor :initarg :editor :initform nil :accessor editor)))

;;; this will disable the default save/persistent behaviours of the text editor
;;; these will be handled by OM following the model of OMPatch
(defmethod om-lisp::save-operation-enabled ((self lisp-function-editor-window)) nil)

(defmethod open-editor-window ((self lisp-function-editor))
  (if (and (window self) (om-window-open-p (window self)))
      (om-select-window (window self))
    (let* ((lispf (object self))
           (edwin (om-lisp::om-open-text-editor 
                   :contents (text lispf)
                   :lisp t
                   :class 'lisp-function-editor-window
                   :title (window-name-from-object lispf)
                   :x (and (window-pos lispf) (om-point-x (window-pos lispf)))
                   :y (and (window-pos lispf) (om-point-y (window-pos lispf)))
                   :w (and (window-size lispf) (om-point-x (window-size lispf)))
                   :h (and (window-size lispf) (om-point-y (window-size lispf)))
                   )))
      (setf (editor edwin) self)
      (setf (window self) edwin)
      (om-lisp::text-edit-window-activate-callback edwin t) ;; will (re)set the menus with the editor in place
      edwin)))

(defmethod om-lisp::text-editor-window-menus ((self lisp-function-editor-window)) 
  (om-menu-items (editor self)))

(defmethod om-lisp::om-text-editor-modified ((self lisp-function-editor-window))
  (touch (object (editor self)))
  (setf (text (object (editor self)))
        (om-lisp::om-get-text-editor-text self))
  (report-modifications (editor self))
  (call-next-method))

(defmethod om-lisp::update-window-title ((self lisp-function-editor-window) &optional (modified nil modified-supplied-p))
  (om-lisp::om-text-editor-window-set-title self (window-name-from-object (object (editor self)))))

(defmethod om-lisp::om-text-editor-check-before-close ((self lisp-function-editor-window))
  (ask-save-before-close (object (editor self))))

(defmethod om-lisp::om-text-editor-resized ((win lisp-function-editor-window) w h) 
  (when (editor win)
    (setf (window-size (object (editor win))) (omp w h))))

(defmethod om-lisp::om-text-editor-moved ((win lisp-function-editor-window) x y)
  (when (editor win)
    (setf (window-pos (object (editor win))) (omp x y))))

;;; update-lisp-fun at closing the window
(defmethod om-lisp::om-text-editor-destroy-callback ((win lisp-function-editor-window))
  (let ((ed (editor win)))
    (editor-close ed)
    (update-lisp-fun (object ed))
    (setf (window ed) nil)
    (setf (g-components ed) nil)
    (unless (references-to (object ed))
      (unregister-document (object ed)))
    (call-next-method)))

;;; update-lisp-fun at loosing focus
(defmethod om-lisp::om-text-editor-activate-callback ((win lisp-function-editor-window) activate)
  (when (editor win) 
    (when (equal activate nil)
      (update-lisp-fun (object (editor win))))
    ))

(defmethod om-lisp::type-filter-for-text-editor ((self lisp-function-editor-window)) 
  '("OM Lisp function" "*.olsp"))

;;; called from menu
(defmethod copy-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::text-edit-copy (window self))))

(defmethod cut-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::text-edit-cut (window self))))

(defmethod paste-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::text-edit-paste (window self))))

(defmethod select-all-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::text-select-all (window self))))

(defmethod undo-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::text-edit-undo (window self))))

;(defmethod redo-command ((self lisp-function-editor))
;  #'(lambda () (om-lisp::text-edit-redo (window self))))

(defmethod font-command ((self lisp-function-editor))
  #'(lambda () (om-lisp::change-text-edit-font (window self))))
