;============================================================================
; o7: visual programming language for computer-aided music composition
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

;=========================================================================
; OM kernel class/function package setup
;=========================================================================

(in-package :om)

(let ((kernelpack (omNG-make-package "Kernel"
                   :container-pack *om-package-tree*
                   :doc "The OM kernel package contains the building blocks and basic tools for creating visual programs in OM"
                   :subpackages 
                   (list (omNG-make-package "Lisp" 
                                            :doc "Basic functions defined in the Lisp programming language"
                                            :functions '(first second third nth rest nthcdr butlast reverse length
                                                               list remove  cons  append  apply  funcall mapcar mapcan))
                         (omNG-make-package "Control" 
                                            :doc "Special boxes implementing control operators"
                                            :functions '(repeat-n seq hub list-elements omif omand omor)
                                            )
                         (omNG-make-package "Data" 
                                            :doc "Objects and data management in visual programs"
                                            :functions '(clone get-slot set-slot test-type save-as-text)
                                            :classes '(store collection))
                         (omNG-make-package "Files" 
                                            :doc "File I/O management and utilities"
                                            :functions '(file-chooser infile outfile tmpfile)
                                            )
                         (omNG-make-package "Reactive" 
                                            :doc "Special boxes for reactive patches"
                                            :functions '(send receive route coll timed-coll group-in mem)
                                            )))))
  ;(add-ref-section (gen-ref-entries kernelpack))
  )



;(defvar *filebox-package* (omNG-protect-object (omNG-make-new-package "File Box")))
;(AddGenFun2Pack '(file-box file-write-line file-write file-read-line file-eof-p) *filebox-package*)
;(AddPackage2Pack *filebox-package* *fileutils-package*)

;(defvar *system-package* (omNG-protect-object (omNG-make-new-package "System")))
;(AddGenFun2Pack '(om-shell) *system-package*)

;;; DI BOXES
;(defvar *di-package*  (omNG-protect-object (omNG-make-new-package "Interface Boxes")))
;(AddClass2Pack '(text-box text-view button check-box 
;                          slider single-item-list multi-item-list pop-up-menu) *di-package*)




