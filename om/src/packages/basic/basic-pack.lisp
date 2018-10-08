;============================================================================
; om7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
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


(let ((basicpack (omNG-make-package "Basic Tools"
                   :container-pack *om-package-tree*
                   :doc "Objects and tools for data representation and processing"
                   :subpackages 
                   (list (omNG-make-package "List Processing" 
                                            :doc ""
                                            :functions '(last-elem last-n first-n x-append flat create-list expand-lst 
                                                                   mat-trans group-list remove-dup subs-posn interlock list-modulo
                                                                   list-explode list-filter table-filter band-filter range-filter posn-match))
                         (omNG-make-package "Arithmetic" 
                                            :doc ""
                                            :functions '(om+ om- om* om/ om// om^ om-e om-abs om-min om-max
                                                             list-min list-max all-equal om-mean om-log om-round om-scale om-scale/sum reduce-tree
                                                             interpolation factorize om-random perturbation
                                                             om< om> om<= om>= om= om/=))
                         (omNG-make-package "Combinatorial" 
                                            :doc ""
                                            :functions '(sort-list rotate nth-random permut-random posn-order permutations))
                         (omNG-make-package "Series" 
                                            :doc ""
                                            :functions '(arithm-ser geometric-ser fibo-ser inharm-ser prime-ser prime? x->dx dx->x))
                         (omNG-make-package "Sets" 
                                            :doc ""
                                            :functions '(x-union x-intersect x-Xor x-diff included?))
                         (omNG-make-package "Interpolation" 
                                            :doc ""
                                            :functions '(x-transfer y-transfer om-sample linear-fun reduce-points reduce-n-points))
                         (omNG-make-package "Curves & Functions" 
                                            :doc ""
                                            :functions '(point-pairs bpf-interpol bpf-scale set-color)
                                            :classes '(bpf bpc)) 
                         (omNG-make-package "Text" 
                                            :doc ""
                                            :functions '(textbuffer-eval textbuffer-read)
                                            :classes '(textbuffer))
                         (omNG-make-package "Containers" 
                                            :doc ""
                                            :functions nil
                                            :classes '(data-stream))
                         (omNG-make-package "Arrays" 
                                            :doc ""
                                            :classes '(2D-array class-array)
                                            :subpackages 
                                            (list (omNG-make-package 
                                                   "Components parsing" 
                                                   :doc ""
                                                   :functions '(new-comp get-comp add-comp remove-comp comp-list comp-field) 
                                                   )))
                         ))))
  )


;;; SPLINE
;(AddGenFun2Pack '(om-spline) *function-package*)


;;; PICT
;(defvar *graphics-package*  (omNG-protect-object (omNG-make-new-package "Picture")))
;(AddClass2Pack '(picture) *graphics-package*)
;(AddGenFun2Pack '(get-RGB picture-size save-picture) *graphics-package*)



