(in-package #:metabang.bind)

#|

use

(defmethod documentation (object doc-type)
  body...)

instead

(documentation :plist 'binding-form)

|#

(defmethod documentation (what (doc-type (eql 'metabang.bind:binding-form)))
  (binding-form-docstring what))

(defun binding-form-docstring (name)
  (let* ((docstrings (get 'bind :docstrings))
	 (forms (get 'bind :binding-forms))
	 (canonical-name (first (assoc name forms)))
	 )
    (and canonical-name
	 (assoc canonical-name docstrings))))

(defun (setf binding-form-docstring) (docstring name/s)
  (when (atom name/s)
    (setf name/s (list name/s)))
  (let* ((docstrings (get 'bind :docstrings))
	 (forms (get 'bind :binding-forms))
	 (canonical-name (first name/s))
	 (current-docstring-pair (assoc canonical-name docstrings)))
    (loop for name in name/s do
	 (let ((names-pair (assoc name forms)))
	   (if names-pair
	       (setf (cdr names-pair) name/s)
	       (push (cons name name/s) forms))))
    (if current-docstring-pair
	(setf (cdr current-docstring-pair) docstring)
	(push (cons canonical-name docstring) docstrings))
    (setf (get 'bind :docstrings) docstrings)
    (setf (get 'bind :binding-forms) forms)
    docstring))

(defmacro defbinding-form ((name/s &key docstring remove-nils-p
				   description (use-values-p t)
				   (accept-multiple-forms-p nil)) &body body)
  (declare (ignorable remove-nils-p description))
  (let* ((multiple-names? (consp name/s))
	 (main-method-name nil)
	 (force-keyword? (or multiple-names?
			     (eq (symbol-package name/s) 
				 (load-time-value (find-package :keyword)))))
	 #+(or)
	 (gignores (gensym "ignores")))    
    (cond (multiple-names?
	   (setf main-method-name (gensym (symbol-name '#:binding-generator)))
	   )
	  (t
	   (setf main-method-name 'bind-generate-bindings)
	   ))
    (flet ((form-keyword (name)
	     (intern (symbol-name name)
		     (load-time-value (find-package :keyword)))))
      (when force-keyword?
	(setf name/s (if multiple-names? 
			 (mapcar #'form-keyword name/s)
			 (form-keyword name/s))))
      `(progn
	 (setf (binding-form-docstring ',name/s) ,docstring)
	 ,@(loop for name in (if multiple-names? name/s (list name/s)) 
	       when (keywordp name) collect
		`(defmethod binding-form-accepts-multiple-forms-p 
		      ((binding-form (eql ,name)))
		    ,accept-multiple-forms-p))
	 (defmethod ,main-method-name 
	     (,@(unless multiple-names?
			(if force-keyword?
			    `((kind (eql ,name/s)))
			    `((kind ,name/s))))
	      variable-form value-form body declarations remaining-bindings)
	   ,(if use-values-p
		;; surely this could be simpler!
		`(let ((gvalues (next-value "values-")))
		   `((let ((,gvalues ,,(if accept-multiple-forms-p 
					   `value-form
					   `(first value-form))))
		       (,@,(if (symbolp (first body))
			       `(,(first body) variable-form gvalues)
			       `(funcall (lambda (variables values) ,@body)
					 variable-form gvalues))
					;		 ,@(when ,gignores `((declare (ignore ,@gignores))))
			   ,@(bind-filter-declarations 
			      declarations variable-form)
			   ,@(bind-macro-helper 
			      remaining-bindings declarations body)))))
		``((,@,(if (symbolp (first body))
			   `(,(first body) variable-form ,(if accept-multiple-forms-p
							      `value-form
							      `(first value-form)))
			   `(funcall (lambda (variables values) ,@body)
				     variable-form ,(if accept-multiple-forms-p
							`value-form
							`(first value-form))))
		       ,@(bind-filter-declarations declarations variable-form)
		       ,@(bind-macro-helper 
			  remaining-bindings declarations body)))))
	 ,@(when multiple-names?
		 (loop for name in name/s collect
		      `(defmethod bind-generate-bindings 
			   ((kind (eql ,name))
			    variable-form value-form body declarations 
			    remaining-bindings)
			 (,main-method-name 
			  variable-form value-form body declarations 
			  remaining-bindings))))
	 #+(or)
	 ,@(when multiple-names?
		 (loop for name in name/s collect
		      `(defmethod bind-generate-bindings 
			   ((kind (eql ,name))
			    variable-form value-form body declarations 
			    remaining-bindings)
			 (,main-method-name 
			  variable-form
			  ,(if accept-multiple-forms-p `value-form `(first value-form))
			  body declarations 
			  remaining-bindings))))
	 ))))

(defun next-value (x)
  (gensym x))

(defmacro lambda-bind ((&rest instrs) &rest body)
  "Use `bind' to allow restructuring of argument to lambda expressions.

This lets you funcall and destructure simultaneously. For example

    (let ((fn (lambda-bind ((a b) c) (cons a c))))
      (funcall fn '(1 2) 3))
    ;; => (1 . 3)

Via eschulte (see git://gist.github.com/902174.git).
"
  #+(or)
  (declare (indent 1))
  (let* ((evald-instrs instrs)
         (syms (mapcar (lambda (_)
			 (declare (ignore _))
			 (gensym))
		       evald-instrs)))
    `(lambda ,syms (bind ,(mapcar #'list evald-instrs syms) ,@body))))

