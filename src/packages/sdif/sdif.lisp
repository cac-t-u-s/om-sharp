;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
 :doc "Tools for manipulating data in the Standard Description Interchange Format"
 :classes '(sdiffile sdifframe sdifmatrix sdiftype sdifnvt)
 :functions '( 
              GetSDIFData GetSDIFTimes GetSDIFFrames 
                          GetSDIFPartials GetSDIFChords
                          SDIFTypeDescription GetNVTList SDIFInfo
                          SDIF->text SDIF->chord-seq SDIF->bpf SDIF->markers
                          bpf->sdif markers->sdif
                          open-sdif-stream sdif-write-frame sdif-write-header)
 )
 