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

;=========================================================================
; OM Generic Function / Method definition
; most of this part is taken from OM6
;=========================================================================

(in-package :om)

(defvar *def-metaclass-genfun* 'omgenericfunction "the meta-class for om generic functions")
(defvar *def-metaclass-method* 'ommethod "the meta-class for om methods")

(defvar *genfun-att-list* '(name icon documentation numouts inputs-default inputs-doc inputs-menus))

(defmethod get-menu-list ((self t)) self)
(defmethod get-menu-list ((self symbol)) (eval self))

(defun get-menu-input (indice list)
   (let (rep i)
     (loop for item in list
           while (not rep) do
           (setf i (if (integerp (car item)) (car item)
                     (read-from-string (format nil "~d" (car (eval item))))))
           (when (= i indice)
             (setf rep item)))
     rep))


(defun parse-defgeneric* (args)
   (let* (icon numouts initvals doc menuins indoc)
     (loop for theargs in args do
           (cond
            ((equal (car theargs) :numouts)  (setf numouts  (second theargs)))
            ((equal (car theargs) :initvals)  (setf initvals (second theargs)))
            ((equal (car theargs) :indoc)     (setf indoc (second theargs)))
            ((equal (car theargs) :doc)       (setf doc (second theargs)))
            ((equal (car theargs) :documentation) (setf doc (second theargs)))
            ((equal (car theargs) :icon)      (setf icon (second theargs)))
            ((equal (car theargs) :menuins)   (setf menuins (second theargs)))))
     (unless numouts (setf numouts 1))
     (unless doc (setf doc "no documentation for this function"))
     (unless icon    (setf icon 150))   ;an icon by default
     (values numouts initvals icon indoc doc menuins)))

(defun remove-om-options (list)
  (loop for item in list
        when (not (or (member (string (car item)) *genfun-att-list* 
                              :test 'string-equal :key 'string) 
                      (equal (car item) :doc)
                      (equal (car item) :generic-function-class)
                      (equal (car item) :method-class))) collect item))


(defmacro defgeneric* (function-name lambda-list &rest options-and-methods &environment env)
   (multiple-value-bind (numouts initvals icon indoc doc menuins)
                        (parse-defgeneric* options-and-methods)
     (unless initvals
         (setf initvals `',(make-list (length lambda-list) :initial-element nil)))
       (unless indoc
         (setf indoc `',(make-list (length lambda-list) :initial-element "no documentation")))
     `(let* ((gen-fun (defgeneric ,function-name ,lambda-list  
                               (:documentation ,doc) 
                               (:generic-function-class ,*def-metaclass-genfun*)
                               (:method-class ,*def-metaclass-method*)
                               ,.(remove-om-options options-and-methods))))
               (setf (numouts gen-fun) ,numouts)
               (setf (inputs-default gen-fun) ,initvals)
               (setf (inputs-doc gen-fun) ,indoc)
               (setf (inputs-menus gen-fun) ',menuins)
               (setf (icon gen-fun) ,icon)
               (setf (name gen-fun) ,(string function-name))
        gen-fun)))


;-----------------
;Method Definition
;-----------------

;---------------------------------------------------
;select the optional keys in the function definition

(defun parse-defmethod* (name args)
  (let* ((theargs args)
          (body? nil)
          qualy lambda-list icon numouts initvals doc menuins body indoc outdoc)
     (when (or (equal (car theargs) :after) (equal (car theargs) :before) (equal (car theargs) :around))
       (setf qualy (list (pop theargs))))
     (setf lambda-list (pop theargs))
     (loop while (and theargs (not body?))
           do
           (cond
            ((equal (car theargs) :numouts)  (pop theargs) (setf numouts  (pop theargs)))
            ((equal (car theargs) :initvals) (pop theargs) (setf initvals (pop theargs)))
            ((equal (car theargs) :indoc)    (pop theargs) (setf indoc (pop theargs)))
            ((equal (car theargs) :outdoc)    (pop theargs) (setf outdoc (pop theargs)))
            ((equal (car theargs) :doc)      (pop theargs) (setf doc (pop theargs)))
            ((equal (car theargs) :icon)     (pop theargs) (setf icon     (pop theargs)))
            ((equal (car theargs) :menuins)  (pop theargs) (setf menuins   (pop theargs)))
            ((stringp (car theargs))         (setf doc (pop theargs)))
            (t                               (setf body theargs) (setf body? t))))
     (values qualy lambda-list numouts initvals icon indoc outdoc doc menuins body)))

;---------------------------------------------
;Take only the name parameter without the type

(defun get-lambda-var (list)
  (mapcar #'(lambda (nom) (first? nom)) list))


;-------------------------------------------------------------------------
;Define an OM method and an OM Generic function if this last doesn't exist

;(defun def-method-icon () 'default)
(defun def-method-icon () nil)

(defun decode-menuins (list)
  (loop for in from 0 to (apply 'max (mapcar 'car list)) collect
        (cadr (find in list :test '= :key 'car))))

(defmacro defmethod* (name &rest args)
   (multiple-value-bind (qualy lambda-list numouts initvals icon indoc outdoc doc menuins body)
                        (parse-defmethod* name args)
     (let ((lambda-var (get-lambda-var lambda-list))
           iv id od)
       (setf iv (or initvals `',(make-list (length lambda-var) :initial-element nil)))
       (setf id (or indoc `',(make-list (length lambda-var) :initial-element "")))
       (setf od (or outdoc `',(make-list (length lambda-var) :initial-element nil)))
       `(let (genfun method)
          (unless (fboundp ',name)
            (progn
              (setf genfun (defgeneric ,name ,lambda-var
                              (:documentation ,doc)
                              (:generic-function-class ,*def-metaclass-genfun*)
                              (:method-class ,*def-metaclass-method*)))
              
              (setf (numouts genfun) (or ,numouts 1))
              (setf (inputs-default genfun) ,iv)
              (setf (inputs-doc genfun) ,id)
              (setf (outputs-doc genfun) ,od)
              (setf (icon genfun) (or ,icon (def-method-icon)))
              (setf (name genfun) ,(string name))))
          
          (unless genfun 
            (setf genfun (fdefinition ',name))
            (when ,doc (setf (documentation genfun 'function) ,doc)))
          
          (when ,icon (setf (icon genfun) ,icon))
          (when ,numouts (setf (numouts genfun) ,numouts))
          (when ,initvals (setf (inputs-default genfun) ,initvals))
          (when ,indoc (setf (inputs-doc genfun) ,indoc))
          (when ,outdoc (setf (outputs-doc genfun) ,outdoc))
          (when ,menuins (setf (inputs-menus genfun) (decode-menuins ,menuins)))
          
          (setf method (defmethod ,name ,.qualy ,lambda-list ,.body))
          (setf (name method) ,(string name))
          
          method))))



