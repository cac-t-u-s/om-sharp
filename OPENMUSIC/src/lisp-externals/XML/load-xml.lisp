
(in-package :cl-user)

(compile&load (merge-pathnames "s-xml/package" *load-pathname*))
(compile&load (merge-pathnames "s-xml/dom" *load-pathname*))
(compile&load (merge-pathnames "s-xml/lxml-dom" *load-pathname*))
(compile&load (merge-pathnames "s-xml/xml" *load-pathname*))
 
(push :xml *features*)



