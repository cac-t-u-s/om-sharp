
(in-package :om)

(require-om-package "sound")

;maybe we should move these into the om-api folder and package...
#+opengl(compile&load (om-relative-path '("3D") "gl-view"))
#+opengl(compile&load (om-relative-path '("3D") "3d-object"))

(compile&load (om-relative-path '("3D") "3dc"))
(compile&load (om-relative-path '("3D") "3dc-editor"))
(compile&load (om-relative-path '("3D") "3d-tools"))
(compile&load (om-relative-path '("3D") "3d-functions"))

(omNG-make-package 
 "Spat"
 :container-pack *om-package-tree*
 :doc "Spatialization tools and connection to the Spat framework."
 :classes '(3DC)
 :functions '())
