
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

;;; Require OpenGL interface
(compile&load (om-relative-path '("3D") "3d-viewer"))
(compile&load (om-relative-path '("3D") "3dc-editor"))


(omNG-make-package 
 "Spat"
 :container-pack *om-package-tree*
 :doc "Spatialization tools and connection to the Spat framework."
 :classes '(3DC 3D-viewer)
 :functions '())
