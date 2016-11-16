

(in-package :om)


(require-om-package "basic")
(require-om-package "score")

(load (decode-local-path "sdif-lib/load-sdif.lisp"))

(compile&load (decode-local-path "sdif-om/sdif-struct"))
(compile&load (decode-local-path "sdif-om/sdif-file"))
(compile&load (decode-local-path "sdif-om/sdif-tools"))
(compile&load (decode-local-path "sdif-om/sdif-write"))

(omNG-make-package 
 "SDIF"
 :container-pack *om-package-tree*
 :doc "Tools for manipulating data in the Standard Description Interchange Format."
 :classes '(sdiffile sdifframe sdifmatrix sdiftype sdifnvt)
 :functions '( 
              GetSDIFData GetSDIFTimes GetSDIFFrames 
                          GetSDIFPartials GetSDIFChords
                          SDIFTypeDescription GetNVTList SDIFInfo
                          SDIF->text SDIF->chord-seq)
 )
 