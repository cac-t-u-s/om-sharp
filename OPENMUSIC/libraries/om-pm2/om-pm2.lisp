;;;===================================================
;;;
;;; OM-pm2
;;; pm2 additive analysis/synthesis in OpenMusic
;;;
;;; Requires perliminary installation of pm2 (also included in AudioSculpt)
;;; Set pm2 path in OM preferences (externals) once the library is loaded.
;;;
;;;
;;; LIBRARY MAIN FILE
;;; Author: Jean Bresson (IRCAM - 2010)
;;;
;;;===================================================

(in-package :om)

(defpackage "OM-PM2-LIB"
  (:use "COMMON-LISP" "CL-USER")
  (:nicknames "pm2"))

(om::compile&load (namestring (om::om-relative-path '("sources") "pm2-additive")))
(om::compile&load (namestring (om::om-relative-path '("sources") "pm2-preferences")))

(om::set-library-packages 
 '(("Analysis" (partial-tracking chord-seq-analysis pm2-f0) nil nil)
   ("Synthesis" (pm2-add-synth) nil nil)
   ))

(om::doc-library 
 1.4
 (print "
;;;============================================
;;; OM-pm2:
;;; pm2 additive analysis/synthesis in OpenMusic
;;;
;;; (c) IRCAM 2006-2010
;;;============================================
"))


