

(in-package :om)

(defvar *om-libs-tree* (make-instance 'OMPackage :name "External Libraries Package") "The package containing External libraries")

(defvar *current-lib* nil "containts the library that is being loaded")

;;;===================================
;;; EXTERNAL LIBRARIES
;;; ... are just special kinds of dynamic-loaded packages
;;;===================================

(defclass OMLib (OMPersistantPackage) 
  ((loaded? :initform nil  :accessor loaded?)))

;;; Conventional folders for library icons and ressources
(defmethod lib-icons-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-pathname self)) (list "resources" "icon"))))
(defmethod lib-resources-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-pathname self)) (list "resources"))))

(defun find-library (name)
  (finf name (elements *om-libs-tree*) :key 'name :test 'string-equal))

(defun duplicate-lib-message (libs name)
  (om-message-dialog
   (format nil (reduce 'string+ (append 
                                 (list "Several versions of library " name " are installed and currently visible in OM.~%Please choose one of the following and remove the other one(s) from the OM registered search paths: ~%~%")
                                 (loop for lib in libs collect 
                                       (string+ (namestring (make-pathname :directory (pathname-directory (lib-pathname lib))))
                                                " ~%~%")))))))