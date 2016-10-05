;;; OpenMusic build file
;;; load this file then evaluate the following form :

;;; (clos::set-clos-initarg-checking nil)
;;; a faire au debut...
;;; (objc:make-autorelease-pool)

;;;=======================
;;; RUN:
;;;=======================
;;; (om::start-openmusic)
;;;=======================

(in-package "CL-USER")

(setf *print-circle* t)

(load (current-pathname "./load-tools.lisp"))

;;;=======================================
;;; APP/VERSION DATA
;;;=======================================
(defparameter *app-name* "OM")
(defparameter *om-version* 7.000000)
(defparameter *beta-release* t)
(defparameter *version-string* "")
(defparameter *release-language* :en)
(defparameter *release-date* (subseq (sys::date-string nil nil) 0 10))
(defparameter *release-author* "jean bresson")

(setf *version-string* (version-to-string *om-version* nil *beta-release*))
(export '(*app-name* *om-version* *beta-release* *version-string* *release-language* *release-date* *release-author*) :cl-user)


(defparameter *om-root-directory*  (make-pathname :directory (butlast (pathname-directory *load-pathname*))))

;;;=======================================
;;;; LOAD OM-API
;;;=======================================

(load (merge-pathnames "src/api/om-api-LW/om-api.lisp" *om-root-directory*))

;;;=======================================
;;;; LOAD EXTERNAL LISP TOOLS
;;;=======================================

(defparameter *externals-directory* (merge-pathnames "src/lisp-externals/" *om-root-directory*))

;;; required to load some of the other libs
;(load (merge-pathnames "asdf/asdf" *externals-directory*))
(require :asdf)

(load (merge-pathnames "ieee-floats/ieee-floats" *externals-directory*))

(progn (load (merge-pathnames "lispworks-udp/lispworks-udp.asd" *externals-directory*))
  (asdf:operate 'asdf:load-op 'lispworks-udp)
  (push :udp *features*))

;(setf *stdout* #.*standard-output*)
(progn
  (load (merge-pathnames "cl-osc/osc.asd" *externals-directory*))
  (asdf:operate 'asdf:load-op 'osc)
  (push :osc *features*))

(load (merge-pathnames "XML/load-xml" *externals-directory*))
(load (merge-pathnames "OpenGL/load-opengl" *externals-directory*))

(load (merge-pathnames "ffi/foreign-interface" *externals-directory*))

(load (merge-pathnames "mach-lib/mach-lib" *externals-directory*))
#+macosx(om-fi::add-foreign-loader 'mach::load-mach-lib)

(progn 
  (load (merge-pathnames "Yason/package" *externals-directory*))
  (load (merge-pathnames "Yason/parse" *externals-directory*)))

(progn
  (load (merge-pathnames "cl-svg/cl-svg.asd" *externals-directory*))
  (asdf:load-system :cl-svg))


;;;=======================================
;;;; OPENMUSIC
;;;=======================================

(defpackage "OpenMusic"
    (:use "COMMON-LISP" "CL-USER" "OM-API" "OM-FI" "LISPWORKS" "HCL" "OM-LISP")
    (:import-from "CL-USER")
    (:nicknames "OM"))


(load (merge-pathnames "src/kernel/kernel-files.lisp" cl-user::*om-root-directory*))

(defun load-om-packages (folder &optional names)
  (let ((package-list (or names 
                          (mapcar 
                           #'(lambda (path) (car (last (pathname-directory path))))
                           (oa::om-directory folder :files nil :directories t)))))
    (mapc #'(lambda (packname)
              (load (make-pathname :directory (append (pathname-directory folder) (list packname)) 
                                   :name packname :type "lisp")) ; :if-does-not-exist nil
              )
          package-list)))


(load-om-packages 
 (merge-pathnames "src/packages/" cl-user::*om-root-directory*)
 '("basic" "control" "midi" "score" "sound"
   "metronome" 
   "spatialisation" 
   "interface" 
   "tempo"
   ;"timing"
   ))

; (cl-user::clean-sources)

(defun cl-user::start-openmusic () (om::start-openmusic))


;; (gen-lib-reference (find-library "OM-Chant"))
;;; avant de faire un package :
;; (om::load-all-om-libs)

;;; (start-openmusic)
;;; (om::show-workspace-win)
;;; (om::gen-om-reference)

;;; WINDOWS :
;;; INSTALL OM FONTS IN C:/WINDOWS/Fonts/
;;; LIBS in C:/WINDOWS/   :
;;   mshare32.dll player32.dll msmmsystem.dll msmmsystem.ini midishare.ini
;;   libaudiostream.dll libsndfile.dll
;;   sdif.dll

;;; MAC :
;;; INSTALL OM FONTS IN /Library/Fonts/
;;; Run MidiShare installer
;;; LIBS in /Library/Frameworks   :
;;; LibAudioStream.frameworks SDIF.framework


