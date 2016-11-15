

(in-package :om)


(require-om-package "basic")

(compile&load (decode-local-path "osc-om/osc-struct"))
(compile&load (decode-local-path "osc-om/osc-send-receive"))
(compile&load (decode-local-path "osc-om/osc-route"))

(load (decode-local-path "libo/load-libo.lisp"))

(omNG-make-package 
 "OSC"
 :container-pack *om-package-tree*
  :doc "Tools for manipulating/communicating with data using the Open Sound Protocol."
   :classes '(osc-bundle)
   :functions '(osc-send osc-receive route-osc))
