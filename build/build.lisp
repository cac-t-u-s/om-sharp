;;; OpenMusic build file
;;; load this file then evaluate the following form :

;;; (clos::set-clos-initarg-checking nil)
;;; a faire au debut...
;;; (objc:make-autorelease-pool)

;;;=======================
;;; RUN:
;;;=======================
;;; (om::start-omsharp)
;;;=======================

(in-package "CL-USER")

;(setf *print-circle* NIL)

(load (current-pathname "./load-tools.lisp"))

;;;=======================================
;;; APP/VERSION DATA
;;;=======================================

(defparameter *app-name* "om-sharp")

(defparameter *version-major* 1)
(defparameter *version-minor* 2)
(defparameter *version-patch* 1)
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
(defparameter *release-date* (subseq (sys::date-string nil nil) 0 10))
(defparameter *release-author* "jean bresson")


(export '(*app-name* *version* *version-string* *release-language* *release-date* *release-author*) :cl-user)

(defparameter *om-root-directory* (make-pathname :directory (butlast (pathname-directory *load-pathname*))))

;;;========================================
;;; FILE I/O ENCODING, USE UTF-8 AS DEFAULT
;;;========================================

#|
(defun utf-8-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :utf-8))

(defun unicode-file-encoding (pathname ef-spec buffer length)
  (declare (ignore pathname buffer length))
  (system:merge-ef-specs ef-spec :unicode))

(setq system:*file-encoding-detection-algorithm*
      (substitute 'utf-8-file-encoding
                  'system:locale-file-encoding
                  system:*file-encoding-detection-algorithm*))
|#

;(pushnew :utf-8 system:*specific-valid-file-encodings*)
;(pushnew :latin-1 system:*specific-valid-file-encodings*)

(lw::set-default-character-element-type 'character)

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

(let ((slime/swank-loader (merge-pathnames "slime/swank-loader.lisp" *externals-directory*)))
  (if (probe-file slime/swank-loader)
      (load slime/swank-loader)
    (error "~S not found.  You probably forgot to 'git submodule init' && 'git submodule update'" slime/swank-loader)))
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
;;;; LOAD THE SOURCES
;;;=======================================

(defpackage :om-sharp
  (:use "OM-API" "OM-FI" "OM-LISP" "COMMON-LISP" "CL-USER" "HCL")
  (:nicknames "OM" "OPENMUSIC"))


(in-package :om)

(import '(cl-user:compile&load
          cl-user::decode-local-path)
        :om)

(load (merge-pathnames "src/visual-language/load.lisp" cl-user::*om-root-directory*))
(load (merge-pathnames "src/player/load.lisp" cl-user::*om-root-directory*))


(editor:setup-indent "defclass*" 2 2 4)
(editor:setup-indent "defclass!" 2 2 4)
(editor:setup-indent "defmethod*" 0 2 4)
(editor:setup-indent "defmethod!" 0 2 4)
(editor:setup-indent "defgeneric*" 0 2 4)


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
(load-om-package "sequencer")
(load-om-package "score")
(load-om-package "sound")
(load-om-package "sdif")
(load-om-package "space")


;;;=================================
;;; Lisp formatting utils
;;;=================================

(defun lisp-format-folder (dir &key exclude-folders)

  (loop for item in (oa::om-directory dir :directories t)
        unless (equal item dir)
        do (if (system::directory-pathname-p item)

               (unless (member (car (last (pathname-directory item))) exclude-folders :test 'string-equal)
                 (lisp-format-folder item :exclude-folders exclude-folders))

             (when (and (pathname-type item)
                        (string= (pathname-type item) "lisp"))
               (om-lisp::om-lisp-format-file item))
             )
        ))

(defun format-sources ()
  (lisp-format-folder 
   (merge-pathnames "src/" cl-user::*om-root-directory*)
   :exclude-folders '("_BUILD" "lisp-externals" "lw-opengl" "foreign-interface" "libsndfile")))

;=> Call this before comitting to the repository !
;
; (format-sources)
;

;;;=================================
;;; Start
;;;=================================

(defun cl-user::start-omsharp () (om::start-omsharp))
