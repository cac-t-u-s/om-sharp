(in-package #:ffa-asd)

(defpackage :ffa
  (:use :common-lisp :cl-utilities :bind :iterate)
  (:shadowing-import-from :iterate :collecting :collect)
  (:export 

   ;; types

   elt-type 

   ;; pointer

   with-pointer-to-array with-pointers-to-arrays))
