;;;=================
;;; SOUND PROJECT
;;;=================


(in-package :om)

(require-om-package "sound")

(defpackage :IAE 
  (:use :common-lisp :cl-user))

(load (merge-pathnames "src/load-iae-lib" *load-pathname*))
(compile&load (merge-pathnames "src/iae-bindings" *load-pathname*))
(compile&load (merge-pathnames "src/iae-om-objects" *load-pathname*))
