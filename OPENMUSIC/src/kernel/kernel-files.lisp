;=========================================================================
;  OpenMusic: Visual Programming Language for Music Composition
;
;  Copyright (C) 1997-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Gerard Assayag, Augusto Agon, Jean Bresson
;=========================================================================

;DocFile
;This file define the *kernel-files* global variable, which contains a list
;with the basic file's pathnames.
;Last Modifications :
;18/10/97 first date.
;DocFile


(in-package :om)
 
(defvar *init-func-list* nil)

(defun om-add-init-fun (func-name)
   (unless (member func-name *init-func-list* :test 'equal)
      (push func-name *init-func-list*)))

(defun om-init-funcall ()
  (print "== START OM INIT CALLS ==")
  (mapc #'(lambda (x) (print x) (funcall x)) (reverse *init-func-list*))
  (print "== END OM INIT CALLS =="))




(mapc #'(lambda (filename) 
          (compile&load (decode-local-path filename)))      
      '(
        "tools/om-lisptools"
        "tools/om-pathnames"
        "tools/om-strings"
        "tools/om-traduction"

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
        "environment/om-preferences" 
        "environment/om-package"
        "environment/om-library"
        "environment/om-workspace" 

        "windows/om-windows"
        "windows/om-main-window"
        "windows/om-preferences-window"
        "windows/om-editor"
        "windows/om-multi-editor"
        "windows/om-inspector"

        "patch/om-box"
        "patch/om-boxcall"
        "patch/om-boxsimple"
        "patch/om-boxobject"
        "patch/om-special-boxes"
        "patch/om-system-boxes"
        "patch/om-reactive-boxes"
        "patch/om-connection"
        "patch/om-patch" 
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
        "patch/om-lost-reference"
 

        "basic/om-data-structures"
        "basic/om-file-utils"
        "basic/om-write-to-disk"
        "basic/om-properties"        
        "basic/om-collections"
        "basic/om-networking"

        "scheduler/LOAD - Scheduling system"
        "scheduler/clock"
  
        "play/timed-object"
        "play/general-player"
        "play/box-player"
        "play/editor-player"
        "play/players"
        
        "maquette/om-maquette"  
        "maquette/om-maquette-editor"
        "maquette/om-metric-ruler"
        "maquette/om-maquette-api"
        "maquette/om-maquette-meta"

        "lisp-function/om-lisp-function"
                
        "utils/om-copy"
        "utils/om-save"
        
        "kernel-pack"
             
        ))
  


