
(in-package :om)

(defpackage :om-midi)

(compile&load (merge-pathnames "midi-api/midi-api" *load-pathname*))
(compile&load (merge-pathnames "midi-setup" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-event" *load-pathname*))
(compile&load (merge-pathnames "objects/midi-controllers" *load-pathname*))
(compile&load (merge-pathnames "objects/piano-roll" *load-pathname*))

(omNG-make-package 
 "MIDI"
 :container-pack *om-package-tree*
 :doc "MIDI tools and objects."
 :classes '(piano-roll)
 :functions nil
 :subpackages nil)
 