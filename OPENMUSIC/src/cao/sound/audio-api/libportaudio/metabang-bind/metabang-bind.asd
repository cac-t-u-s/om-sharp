(defpackage #:metabang.bind-system (:use #:cl #:asdf))
(in-package #:metabang.bind-system)

(defsystem metabang-bind
  :version "0.8.0"
  :author "Gary Warren King <gwking@metabang.com>"
  :licence "MIT License"    
  :description "Bind is a macro that generalizes multiple-value-bind, let, let*, destructuring-bind, structure and slot accessors, and a whole lot more."
  :components ((:module
		"dev"
		:serial t
		:components
		((:file "packages")
		 (:file "macros")
		 (:file "bind")
		 (:file "binding-forms")
		 #+allegro
		 (:file "bind-re-allegro" :depends-on ("bind")))))
  :in-order-to ((test-op (load-op metabang-bind-test)))
  :perform (test-op :after (op c)
		    (funcall
		      (intern (symbol-name '#:run-tests) :lift)
		      :config :generic))
  :depends-on ()) 

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'metabang-bind))))
  (values nil))






