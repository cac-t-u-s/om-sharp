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
 
(defvar *init-func-list* nil)

(defun add-om-init-fun (func-name)
   (unless (member func-name *init-func-list* :test 'equal)
      (pushnew func-name *init-func-list*)))

(defun om-init-funcall ()
  ;(om-print-dbg "START INIT CALLS")
  (mapc #'(lambda (x) 
            (om-print-dbg (string x)) 
            (funcall x))
        (reverse *init-func-list*))
  ;(om-print-dbg "END INIT CALLS")
  )




(mapc #'(lambda (filename) 
          (cl-user::compile&load (cl-user::decode-local-path filename)))      
      '(
        "tools/om-lisptools"
        "tools/om-pathnames"
        "tools/om-strings"
        "tools/om-traduction"
        "tools/om-documentation"
        "tools/om-help"

        "language/om-objects" 
        "language/om-metaclasses"   
        "language/om-defmethod"
        "language/om-defclass"
        
        "graphics/om-graphic-tools"
        "graphics/om-icon-picts"
        "graphics/om-graphic-components"
        "graphics/om-frames"
        "graphics/om-rulers"
        
        "environment/om-doc-manager"
        "environment/om-session"
        "environment/om-properties"        
        "environment/om-preferences" 
        "environment/om-package"
        "environment/om-library"
        "environment/om-workspace" 
        "environment/om-function-reference"
	"environment/om-swank"
	"environment/om-tty-listener"
        
        "windows/om-windows"
        "windows/om-main-window"
        "windows/om-preferences-window"
        "windows/om-editor"
        "windows/om-multi-editor"
        
        "patch/om-box"
        "patch/om-box-io"
        "patch/om-boxcall"
        "patch/om-boxsimple"
        "patch/om-boxobject"   
        "patch/om-connection"
        "patch/om-patch" 
        "patch/om-patch-component-boxes"  
        "patch/om-in-out"  
        "patch/om-patch-editor"
        "patch/om-boxframe"
        "patch/om-box-abstraction"  
        "patch/om-boxpatch"  
        "patch/om-eval"
        "patch/om-gen-code"
        "patch/om-reactive"
        "patch/om-comments"
        "patch/om-encapsulation"
        "patch/om-interfacebox"
        "patch/om-lost-reference"
        "patch/om-loop"
        "patch/om-lisp-function"
        
        "boxes/special-boxes"
        "boxes/control-boxes"
        "boxes/system-boxes"
        "boxes/repeat-n"
        "boxes/mem-collect"  
        "boxes/global"  
        "boxes/init-do"  
        "boxes/send-receive-route"  
        
        "basic/om-data-structures"
        "basic/om-file-utils"
        "basic/om-write-to-disk"
        "basic/om-file-stream"
        "basic/om-collections"
        "basic/om-networking"

        "scheduler/load-scheduling-system"
        "scheduler/clock"
  
        "play/timed-object"
        "play/general-player"
        "play/box-player"
        "play/editor-player"
        
        "utils/om-copy"
        "utils/om-save"
        "utils/om-undo"
        "utils/om-compatibility" 

        "kernel-pack"
             
        ))
  


