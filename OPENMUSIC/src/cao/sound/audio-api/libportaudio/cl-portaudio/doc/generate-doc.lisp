(defpackage portaudio-doc
  (:use :cl :portaudio :atdoc))

(in-package :portaudio-doc)
(defun generate-html ()
  "Generate html"
  (atdoc:generate-html-documentation
   '(:portaudio :portaudio-tests)
   (merge-pathnames "doc/" (asdf:component-pathname (asdf:find-system :cl-portaudio)))
   :index-title "CL-PortAudio references"
   :heading "CL-PortAudio"
   :single-page-p t      ;optional
   :include-internal-symbols-p nil))
(export 'generate-html)

(defun generate-pdf ()
  "Generate pdf"
  (atdoc:generate-latex-documentation '(:portaudio :portaudio-tests) (merge-pathnames "doc/" (asdf:component-pathname (asdf:find-system :cl-portaudio)))
                                            :title "CL-PortAudio" :include-slot-definitions-p t))
(export 'generate-pdf)

(defun generate-all ()
  (generate-pdf)
  (generate-html))
(export 'generate-all)