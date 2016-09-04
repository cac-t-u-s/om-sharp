
(in-package :om)

;maybe we should move these into the om-api folder and package...
#+opengl(compile&load (om-relative-path '("3D") "gl-view"))
#+opengl(compile&load (om-relative-path '("3D") "3d-object"))

(compile&load (om-relative-path '("3D") "3dc"))
(compile&load (om-relative-path '("3D") "3dc-editor"))
(compile&load (om-relative-path '("3D") "3d-tools"))
(compile&load (om-relative-path '("3D") "3d-functions"))

(load (om-relative-path '("omspat" "spat-lib") "load-omspat"))
(compile&load (om-relative-path '("omspat") "spat-scene"))
(compile&load (om-relative-path '("omspat") "spat-editor"))
(compile&load (om-relative-path '("omspat") "spatialize"))
#+sdif(compile&load (om-relative-path '("omspat") "spat-sdif"))

(compile&load (om-relative-path nil "spat-pack"))

