

(in-package :om)

(compile&load (decode-local-path "data/data-stream"))
(compile&load (decode-local-path "osc/osc-struct"))
(compile&load (decode-local-path "osc/osc-send-receive"))
(compile&load (decode-local-path "osc/osc-route"))

(load (decode-local-path "sdif/sdif-lib/load-sdif.lisp"))

#+sdif(compile&load (decode-local-path "sdif/sdif-struct"))
#+sdif(compile&load (decode-local-path "sdif/sdif-file"))
#+sdif(compile&load (decode-local-path "sdif/sdif-tools"))
#+sdif(compile&load (decode-local-path "sdif/sdif-write"))


(load (decode-local-path "osc/libo/load-libo.lisp"))


(omNG-make-package 
 "Control"
 :container-pack *om-package-tree*
 :doc "Objects and tools for external communication and control."
 :classes '(data-stream)
 :functions ()
 :subpackages 
 (list 
  #+sdif(omNG-make-package 
         "SDIF"
         :doc "Tools for manipulating data in the Standard Description Interchange Format."
         :classes '(sdiffile sdifframe sdifmatrix sdiftype sdifnvt)
         :functions '( 
                      GetSDIFData GetSDIFTimes GetSDIFFrames 
                      GetSDIFPartials GetSDIFChrods
                      SDIFTypeDescription GetNVTList SDIFInfo
                      SDIF->text SDIF->chord-seq)
         )
  (omNG-make-package 
   "OSC"
   :doc "Tools for manipulating/communicating with data using the Open Sound Protocol."
   :classes '(osc-bundle)
   :functions '(osc-send osc-receive route-osc)
   )))
 