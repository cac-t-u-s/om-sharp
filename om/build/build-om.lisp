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

;(setf *print-circle* NIL)

(load (current-pathname "./load-tools.lisp"))

;;;=======================================
;;; APP/VERSION DATA
;;;=======================================

(defparameter *app-name* "om7-beta")
(defparameter *om-version* 0.010500)
(defparameter *beta-release* nil)
(defparameter *version-string* "")
(defparameter *release-language* :en)
(defparameter *release-date* (sys::date-string nil nil))
(defparameter *release-author* "jean bresson")

(setf *version-string* (version-to-string *om-version* t *beta-release*))
(export '(*app-name* *om-version* *beta-release* *version-string* *release-language* *release-date* *release-author*) :cl-user)

(defparameter *om-root-directory* (make-pathname :directory (butlast (pathname-directory *load-pathname*))))

;;;=======================================
;;;; LOAD OM-API
;;;=======================================

(load (merge-pathnames "src/api/om-api-LW/om-api.lisp" *om-root-directory*))

(load (merge-pathnames "src/api/foreign-interface/foreign-interface" *om-root-directory*))

;;;=======================================
;;;; LOAD EXTERNAL LISP TOOLS
;;;=======================================

(defparameter *externals-directory* (merge-pathnames "src/lisp-externals/" *om-root-directory*))

;;; required to load some of the other libs
;(load (merge-pathnames "asdf/asdf" *externals-directory*))
(require :asdf)

(load (merge-pathnames "ieee-floats/ieee-floats" *externals-directory*))

(load (merge-pathnames "mach-lib/mach-lib" *externals-directory*))
#+macosx(om-fi::add-foreign-loader 'mach::load-mach-lib)

(progn (load (merge-pathnames "lispworks-udp/lispworks-udp.asd" *externals-directory*))
  (asdf:operate 'asdf:load-op 'lispworks-udp)
  (push :udp *features*))

;(setf *stdout* #.*standard-output*)
(progn
  (load (merge-pathnames "cl-osc/osc.asd" *externals-directory*))
  (asdf:operate 'asdf:load-op 'osc)
  (push :osc *features*))

(load (merge-pathnames "XML/load-xml" *externals-directory*))

(progn 
  (compile&load (merge-pathnames "Yason/package" *externals-directory*))
  (compile&load (merge-pathnames "Yason/parse" *externals-directory*)))

(progn
  (load (merge-pathnames "cl-svg/cl-svg.asd" *externals-directory*))
  (asdf:load-system :cl-svg))


;;;=======================================
;;;; OPENMUSIC
;;;=======================================

(defpackage :openmusic
  (:use "OM-API" "OM-FI" "OM-LISP" "COMMON-LISP" "CL-USER" "HCL")
  (:nicknames "OM" "OpenMusic"))


(in-package :om)

(import '(cl-user:compile&load 
          cl-user::decode-local-path) 
        :om)

(load (merge-pathnames "src/kernel/kernel-files.lisp" cl-user::*om-root-directory*))

(push :om7 *features*)

;;; used for source tracking
;;; updated in delivered init call
(om-lisp::om-set-source-tree-root-folder (merge-pathnames "src/" cl-user::*om-root-directory*))


(defparameter *om-packages* nil)
(defparameter *packages-folder* (merge-pathnames "src/packages/" cl-user::*om-root-directory*))

(defun load-om-package (name)
  (let ((packager-loader (make-pathname :directory (append (pathname-directory *packages-folder*) (list name)) 
                                        :name name :type "lisp")))
    (if (probe-file packager-loader)
      (progn 
        (print (format nil "LOADING PACKAGE: ~A" packager-loader))
        (load packager-loader)
        (push name *om-packages*)
        name)
      (progn 
        (print (format nil "PACKAGE LOADER NOT FOUND !!"))
        nil))
    ))

(defun find-om-package (name)
  (find name *om-packages* :test 'string-equal))

;; can be called from a package...
(defun require-om-package (name)
  (or (find-om-package name)
      (load-om-package name)
      (progn
        (capi:beep-pane)
        (print (format nil "Required package ~S not found !" name))
        NIL)))

(load-om-package "basic")
(load-om-package "midi")
(load-om-package "metronome")
(load-om-package "score")
(load-om-package "sdif")
(load-om-package "osc")
(load-om-package "sound")
(load-om-package "synth")
(load-om-package "space")
(load-om-package "interface")

;(cl-user::clean-sources)


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


