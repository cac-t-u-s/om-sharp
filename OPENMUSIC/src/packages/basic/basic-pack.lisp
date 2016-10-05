(in-package :om)


(let ((basicpack (omNG-make-package "Basic Tools"
                   :container-pack *om-package-tree*
                   :doc "Objects and tools for data representation and processing."
                   :subpackages 
                   (list (omNG-make-package "List Processing" 
                                            :doc ""
                                            :functions '(last-elem last-n first-n x-append flat create-list expand-lst 
                                                                   mat-trans group-list remove-dup subs-posn interlock list-modulo
                                                                   list-explode list-filter table-filter band-filter range-filter posn-match))
                         (omNG-make-package "Arithmetic" 
                                            :doc ""
                                            :functions '(om+ om- om* om/ om// om^ om-e om-abs om-min om-max
                                                             list-min list-max om-mean om-log om-round om-scale om-scale/sum reduce-tree
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
                                            :functions '(point-pairs bpf-interpol)
                                            :classes '(bpf bpc))
                         (omNG-make-package "Text" 
                                            :doc ""
                                            :functions '(textbuffer-eval textbuffer-read)
                                            :classes '(textbuffer))
                         ))))
  ;(add-ref-section (gen-ref-entries kernelpack))
  )



;
;(AddGenFun2Pack '(            
;                              
;                              om-spline
;                               
;                              
;                              ) *function-package*)

;(AddClass2Pack '(  bpc bpc-lib) *function-package*     
;               :position (list (om-make-point 75 115) (om-make-point 75 225) (om-make-point 150 115) (om-make-point 150 225) (om-make-point 150 25)))
;(AddPackage2Pack *function-package* *basic-package*)


;;; DATA STRUCTURES
;(defvar *basic-data-package* (omNG-protect-object (omNG-make-new-package "Array")))
;(AddGenFun2Pack '(new-comp get-comp comp-list comp-field add-comp remove-comp) *basic-data-package*)
;(AddClass2Pack '(class-array) *basic-data-package* :position (list (om-make-point 175 50)))
;(AddPackage2Pack *basic-data-package* *basic-package*)

;;; FILE BOX / TEXTFILE
;(defvar *file-package* (omNG-protect-object (omNG-make-new-package "Text")))
;(addpackage2pack *file-package* *basic-package*)
;(AddClass2Pack '(TextFile) *file-package*)
;(addgenfun2pack '(eval-textfile save-data) *file-package*)


;;; PICT
;(defvar *graphics-package*  (omNG-protect-object (omNG-make-new-package "Picture")))

;(AddClass2Pack '(picture) *graphics-package*)
;(AddGenFun2Pack '(get-RGB picture-size save-picture) *graphics-package*)

;(addPackage2Pack *graphics-package* *basic-package*)



