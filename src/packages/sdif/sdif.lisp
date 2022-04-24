;============================================================================
; om#: visual programming language for computer-assisted music composition
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


(require-om-package "basic")
(require-om-package "score")

(load (decode-local-path "sdif-lib/load-sdif.lisp"))

(compile&load (decode-local-path "sdif-om/sdif-struct"))
(compile&load (decode-local-path "sdif-om/sdif-file"))
(compile&load (decode-local-path "sdif-om/sdif-partials"))
(compile&load (decode-local-path "sdif-om/sdif-write"))
(compile&load (decode-local-path "sdif-om/sdif-tools"))
(compile&load (decode-local-path "sdif-om/sdif-editor"))
(compile&load (decode-local-path "compatibility"))

(omNG-make-package
 "SDIF"
 :container-pack *om-package-tree*
 :doc "Tools for manipulating data in the Standard Description Interchange Format (SDIF)"
 :classes '(sdiffile)
 :subpackages
 (list (omNG-make-package
        "SDIF Structures"
        :classes '(sdifframe sdifmatrix sdiftype sdifnvt)
        :functions '(find-in-nvt find-in-nvtlist))
       (omNG-make-package
        "Read and Convert"
        :functions '(SDIFInfo
                     SDIFTypeDescription GetNVTList
                     GetSDIFData GetSDIFTimes GetSDIFFrames
                     GetSDIFPartials GetSDIFChords
                     SDIF->chord-seq SDIF->bpf SDIF->markers
                     SDIF->text))
       (omNG-make-package
        "Write"
        :functions '(write-sdif-file
                     bpf->sdif markers->sdif chord-seq->sdif
                     open-sdif-stream sdif-write-frame sdif-write-header))
       ))

