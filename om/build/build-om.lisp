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

(defparameter *app-name* "om-sharp")

(defparameter *version-major* 1)
(defparameter *version-minor* 0) 
(defparameter *version-patch* 0)
(defparameter *version-update* 0)

(defparameter *version* 
  (+ *version-major* 
     (/ *version-minor* 100.0) 
     (/ *version-patch* 10000.0) 
     (/ *version-update* 1000000.0)))

(defparameter *version-string* 
  (concatenate 'string (format nil "~d.~d" *version-major* *version-minor*)
               (if (and *version-patch* (plusp *version-patch*)) (format nil ".~d" *version-patch*) "")
               (if (and *version-update* (plusp *version-update*)) (format nil "-u~d" *version-update*) "")
               ))

(defparameter *release-language* :en)
(defparameter *release-date* (sys::date-string nil nil))
(defparameter *release-author* "jean bresson")


(export '(*app-name* *version* *version-string* *release-language* *release-date* *release-author*) :cl-user)

(defparameter *om-root-directory* (make-pathname :directory (butlast (pathname-directory *load-pathname*))))

;;;========================================
;;; FILE I/O ENCODING, USE UTF-8 AS DEFAULT
;;;========================================

(defun utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

(setq system:*file-encoding-detection-algorithm*
      (substitute 'utf-8-file-encoding
                  'system:locale-file-encoding
                  system:*file-encoding-detection-algorithm*))

(setq system:*file-encoding-detection-algorithm*
      '(utf-8-file-encoding))

(lw::set-default-character-element-type 'cl:character)

;;;=======================================
;;;; LOAD OM-API
;;;=======================================


(load (merge-pathnames "src/api/om-lisp-LW/load-om-lisp.lisp" *om-root-directory*))

(load (merge-pathnames "src/api/om-api-LW/om-api.lisp" *om-root-directory*))

(load (merge-pathnames "src/api/foreign-interface/foreign-interface" *om-root-directory*))


;;;=======================================
;;;; LOAD EXTERNAL LISP TOOLS
;;;=======================================

(defparameter *externals-directory* (merge-pathnames "src/lisp-externals/" *om-root-directory*))

(require :asdf)

(load (merge-pathnames "slime/swank-loader.lisp" *externals-directory*))
(setq swank-loader::*fasl-directory* (merge-pathnames "slime/fasl/" *externals-directory*))
(swank-loader:init :setup nil :load-contribs t)


(load (merge-pathnames "ieee-floats/ieee-floats" *externals-directory*))

(load (merge-pathnames "mach-lib/mach-lib" *externals-directory*))
#+macosx(om-fi::add-foreign-loader 'mach::load-mach-lib)

(progn (load (merge-pathnames "lispworks-udp/lispworks-udp.asd" *externals-directory*))
  (asdf:operate 'asdf:load-op 'lispworks-udp)
  (push :udp *features*))

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

(defpackage :om-sharp
  (:use "OM-API" "OM-FI" "OM-LISP" "COMMON-LISP" "CL-USER" "HCL")
  (:nicknames "OM" "OPENMUSIC"))


(in-package :om)

(import '(cl-user:compile&load 
          cl-user::decode-local-path) 
        :om)

(load (merge-pathnames "src/kernel/kernel-files.lisp" cl-user::*om-root-directory*))

(editor:setup-indent "defclass*" 2 2 4)
(editor:setup-indent "defclass!" 2 2 4)
(editor:setup-indent "defmethod*" 0 2 4)
(editor:setup-indent "defmethod!" 0 2 4)

(push :om-sharp *features*)

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
(load-om-package "osc")
(load-om-package "maquette")
(load-om-package "score")
(load-om-package "math")
(load-om-package "sound")
(load-om-package "sdif")
(load-om-package "space")


(defun cl-user::start-openmusic () (om::start-openmusic))




