
(in-package :om)

(require-om-package "sound")

;maybe we should move these into the om-api folder and package...
#+opengl(compile&load (om-relative-path '("3D") "gl-view"))
#+opengl(compile&load (om-relative-path '("3D") "3d-object"))

(compile&load (om-relative-path '("3D") "3dc"))
(compile&load (om-relative-path '("3D") "3dc-editor"))
(compile&load (om-relative-path '("3D") "3d-tools"))
(compile&load (om-relative-path '("3D") "3d-functions"))

(load (om-relative-path '("spat" "spat-lib") "load-omspat"))
(compile&load (om-relative-path '("spat") "spat-scene"))
(compile&load (om-relative-path '("spat") "spat-editor"))
(compile&load (om-relative-path '("spat") "spatialize"))
#+sdif(compile&load (om-relative-path '("spat") "spat-sdif"))

(compile&load (om-relative-path nil "spat-pack"))

