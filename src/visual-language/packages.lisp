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

;=========================================================================
; kernel class/function package setup
;=========================================================================

(in-package :om)

(omNG-make-package
 "Visual Language"
 :container-pack *om-package-tree*
 :doc "Building blocks and basic tools for creating visual programs"
 :subpackages
 (list (omNG-make-package
        "Lisp"
        :doc "Basic functions defined in the Lisp programming language"
        :functions '(first second third nth rest nthcdr butlast reverse length
                           list remove cons append apply funcall mapcar mapcan))
       (omNG-make-package
        "Control"
        :doc "Special boxes implementing control operators, argument passing and memory"
        :functions '(seq hub split)
        :special-symbols '(in out if and or repeat-n mem global)
        )
       (omNG-make-package
        "Loop"
        :doc "Special boxes for visual loop implementation"
        :special-symbols '(iterate init-do loop-for loop-while loop-list loop-tail accum collect tcollect)
        )
       (omNG-make-package
        "Data"
        :doc "Objects and data management"
        :functions '(clone test-type save-as-text)
        :classes '(store collection))
       (omNG-make-package
        "Files"
        :doc "File I/O management and utilities"
        :functions '(file-chooser infile outfile tmpfile open-file-stream close-file-stream
                                  file-write file-write-line file-read-line)
        )
       (omNG-make-package
        "Reactive"
        :doc "Special boxes for reactive patches"
        :functions '(send receive route)
        )
       (omNG-make-package
        "Interactive boxes"
        :doc "Interface boxes and widgets (sliders, buttons, etc.)"
        :special-symbols '(button slider check-box switch list-selection list-menu)
        )
       (omNG-make-package
        "Meta"
        :doc "Visual program manipulation"
        :functions '(get-boxes get-box-by-name file-path)
        :special-symbols '(thisbox thispatch)
        )
       ))




