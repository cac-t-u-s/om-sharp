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

;(require-om-package "sound")

;;; OpenGL interface (from LispWorks)
(load (merge-pathnames "OpenGL/lw-opengl/load" *load-pathname*))

;;; Mid-level OM-OpenGL interface:
(compile&load (om-relative-path '("OpenGL") "gl-user"))
(compile&load (om-relative-path '("OpenGL") "om-opengl-view"))
(compile&load (om-relative-path '("OpenGL") "om-3d-object"))

;;; Require no OpenGL
(compile&load (om-relative-path '("3D") "3dc"))
(compile&load (om-relative-path '("3D") "3d-tools"))
(compile&load (om-relative-path '("3D") "3d-functions"))
(compile&load (om-relative-path '("utils") "3d-elements"))
(compile&load (om-relative-path '("utils") "osc-manager"))

;;; Require OpenGL interface
(compile&load (om-relative-path '("3D") "3d-viewer"))
(compile&load (om-relative-path '("3D") "3dc-editor"))


(omNG-make-package 
 "3D"
 :container-pack (get-subpackage *om-package-tree* "Basic Tools")
 :doc "Spatialization tools"
 :classes '(3DC 3D-viewer)
 :functions '())
