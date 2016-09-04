(defpackage #:ffa-asd
  (:use :cl :asdf))

(in-package :ffa-asd)

(defsystem ffa
  :description "Foreign friendly arrays"
  :author "Tamas K Papp"
  :license "LLGPL"
  :serial t
  :components ((:file "package")
	       (:file "types")
	       (:file "utils")
	       (:file "pointer"))
  :depends-on (:cffi :cl-utilities :metabang-bind :iterate))

