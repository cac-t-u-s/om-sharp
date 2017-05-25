;;;===================================
;;; EXTERNAL LIBRARIES
;;; ... are just special kinds of dynamic-loaded packages
;;;===================================

(in-package :om)

;;; ideally third-party libraries are coded in a separate Lisp package
;;; these are the basic :OM functions which might be needed in a library
(export 
 '(doc-library               ;;; used to set a documentation 
   set-library-packages      ;;; used to organize the package
   require-library           ;;; requies another lib
   gen-lib-reference         ;;; auto-generates the lib reference
   defclass!         ;;; defines a class in the library
   defmethod!        ;;; defines a method in the library
   ) :om)

(defclass OMLib (OMPersistantPackage) 
  ((loaded? :initform nil :accessor loaded?)
   (version? :initform nil :accessor version :initarg :version)))

;;; conventional folders for library
(defmethod lib-resources-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (mypathname self)) '("resources"))))

(defmethod lib-icons-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-resources-folder self)) '("icons"))))

(defmethod lib-loader-file ((self OMLib))
   (om-make-pathname :directory (mypathname self) :name (name self) :type "lisp"))


(add-preference-module :libraries "Libraries")
(add-preference :libraries :libs-folder1 "Libraries folder 1" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder2 "Libraries folder 2" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder3 "Libraries folder 3" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder4 "Libraries folder 4" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :auto-load "Auto load" :bool nil "(Will silently load any required libraries)")

;;;=================================
;;; registered libraries package
;;;=================================

(defvar *om-libs-root-package* (make-instance 'OMPackage :name "External Libraries Package") "The package containing External libraries")

; (all-om-libraries t)
(defun all-om-libraries (&optional loaded-only)
  (mapcar 'name
          (if loaded-only (remove-if-not 'loaded? (elements *om-libs-root-package*))
            (elements *om-libs-root-package*))))

(defun find-om-library (name)
  (find name (elements *om-libs-root-package*) :test 'string-equal :key 'name))

(defun make-new-om-library (path)
  (let* ((lib-name (string-until-space (car (last (pathname-directory path)))))
         (lib (make-instance 'OMLib :mypathname path :name lib-name)))
    (if (probe-file (lib-loader-file lib))
        lib
      (om-beep-msg "Library doesn't have a loader file: ~A.lisp not found.." lib-name))
    ))
  
;;; adds the library to the OM package tree
;;; does not load the lib
(defun register-om-library (path &optional (warn-if-exists t))
  (let ((new-library (make-new-om-library path)))
    (if new-library
        (if (find-om-library (name new-library))
            (when warn-if-exists (om-beep-msg "A library named ~A already exists. Could not register library." (name new-library)))
          (setf (elements *om-libs-root-package*)
                (append (elements *om-libs-root-package*) (list new-library))))
      (om-beep-msg "Could not register library: ~A" path))))

(defparameter *libs-folders* nil)
(defparameter *default-libs-folder* nil)

(defun init-default-libs-folder ()
  (setq *default-libs-folder* (merge-pathnames "libraries/" (om-root-folder))))

(defun register-all-libraries (&optional (warn-if-exists t))
  (unless *default-libs-folder* (init-default-libs-folder))
  (loop for folder in (remove nil 
                              (list *default-libs-folder* 
                                    (get-pref-value :libraries :libs-folder1)
                                    (get-pref-value :libraries :libs-folder2)
                                    (get-pref-value :libraries :libs-folder3)
                                    (get-pref-value :libraries :libs-folder4)))
        do (mapc #'(lambda (path) (register-om-library path warn-if-exists))
                 (om-directory folder :directories t :files nil))))

;;; called when the folder(s) change
(defun update-registered-libraries ()
  (setf (elements *om-libs-root-package*)
        (remove-if-not 'loaded? (elements *om-libs-root-package*)))
  (register-all-libraries nil))

;; called from the preferences
(defun update-libraries-folder ()
  (update-registered-libraries)
  (when *om-main-window*
    (update-libraries-tab *om-main-window*)))

; (register-all-libraries)

#|
(defun duplicate-lib-message (libs name)
  (om-message-dialog
   (format 
    nil 
    (reduce 
     'string+ 
     (append 
      (list 
       "Several versions of library " 
       name 
       " are installed and currently visible in OM.~%Please choose one of the following and remove the other one(s) from the OM registered search paths: ~%~%")
      (loop for lib in libs collect 
            (string+ (namestring (make-pathname :directory (pathname-directory (lib-pathname lib))))
                     " ~%~%")))))))
|#

;;;===========
;;; LOADING
;;;===========


(defvar *current-lib* nil "containts the library that is being loaded")

;;; loads a registered library
(defmethod load-om-library ((lib string))
  (let ((the-lib (find-om-library lib)))
    (if the-lib (load-om-library the-lib)
      (om-beep-msg "Library: ~S not registered !" lib))))

(defmethod load-om-library ((lib OMLib))    
  (let ((packager-loader (lib-loader-file lib)))                                      
    (if (probe-file packager-loader)
      (handler-bind ((error #'(lambda (c)
                                (om-message-dialog (format nil "Error while loading the library ~A:~%~s" 
                                                           (name lib) (om-report-condition c))
                                                   :size (om-make-point 300 200))
                                (om-abort))))
        (let ((*current-lib* lib))
          (om-print-format "Loading library: ~A..." (list packager-loader) "OM")
          (load packager-loader)
          (register-images (lib-icons-folder lib))
          (setf (loaded? lib) t)
          (update-preference-window-module :libraries) ;;; update the window is opened       
          packager-loader))
      (om-beep-msg "Library doesn't have a loader file: ~A NOT FOUND.." packager-loader))
    ))

;;; can be called from another library, etc.
(defun require-library (name)
  (let ((required-lib (find-om-library name)))
    (if required-lib
        (unless (loaded? required-lib)
          (load-om-library required-lib))
      (om-beep-msg "Required library: ~S not found !" name))
    ))

;;;===========
;;; DOC ETC.
;;;===========

(defun doc-library (version text &optional lib)
  (let ((thelib (if (stringp lib) (find-om-library lib)
                  (or lib *current-lib*))))
    (when thelib
      (setf (doc thelib) text
            (version thelib) version))
    ))


; COMPAT WITH OM6
; init the subpackages of the current library from a list of the form :
; ("sub-pack-name" subpack-list class-list function-list class-alias-list)
(defun fill-library (desc-list &optional lib)
  (flet ((reorder-pack (pack)
           (list (nth 0 pack)
                 (nth 3 pack)
                 (nth 2 pack)
                 (and (nth 1 pack) (reorder-pack (nth 1 pack))))))
    (set-library-packages 
     (loop for item in desc-list collect
           (reorder-pack item))
     lib)))

; NEW VERSION: SIMPLER PACKAGE LIST
; desc-list is a list of (name-or-nil functions classes sub-packages)
(defun set-library-packages (desc-list &optional lib)
  (let ((thelib (if (stringp lib) (find-om-library lib)
                  (or lib *current-lib*))))
    (when thelib
      (CleanupPackage thelib)
      (loop for item in desc-list do
            (omNG-make-package (car item) 
                               :container-pack thelib
                               :functions (nth 1 item) 
                               :classes (nth 2 item) 
                               :subpackages (nth 3 item)))
      (set-om-pack-symbols)
      t)))

;;;=================================
;;; LOAD LIBRARY-DEPENDENT BOXES
;;;=================================


(defvar *required-libs-in-current-patch* nil)

(defmethod om-load-from-id :before ((id (eql :box)) data)
  (let ((library-name (find-value-in-kv-list data :library)))
    (when (and library-name ;;; the box comes from a library
               (not (find library-name *required-libs-in-current-patch* :test 'string-equal))) ;;; situation already handled (for this patch): do not repeat 
      (push library-name *required-libs-in-current-patch*)
      (let ((the-library (find-om-library library-name)))
        (if the-library
            (unless (loaded? the-library)
              (when (or (get-pref-value :libraries :auto-load)
                        (let ((reply (om-y-n-cancel-dialog (format nil "Some element(s) require the library '~A'.~%~%Do you want to load it ?" library-name))))
                          (if (equal reply :cancel) (abort) reply)))
                (load-om-library the-library)))
          (om-message-dialog (format nil "Some element(s) require the unknow library: '~A'.~%~%These boxes will be temporarily disabled." library-name))
          )))
    ))


;;;=================================
;;; METHOD / CLASS DEFINITION
;;;=================================

; LIBRARY METHOD definition
; Similar to defmethod* + set the flag library of the generic function
(defmacro defmethod! (name &rest args)
  `(let* ((function-already-exists (fboundp ',name))
          (themethod (defmethod* ,name ,.args))
          (thefunction (fdefinition ',name))
          (function-lib (or *current-lib*  ;; the lib being loaded
                            (and function-already-exists (library thefunction)
                                 (find-om-library (library thefunction))) ;; the lib specified in the existing function
                            )))
     ;;; function is in a user library and did not exist before
     ;;; we could maybe do this better by tracking in the file system which library we're in
     (when (and function-lib (not function-already-exists))
       (setf (library thefunction) (string-until-space (name *current-lib*))))
     
     ;;; compat with OM6: now we just store the icon id   
     (when (and (find :icon ',args) (listp (icon thefunction)))
       (setf (icon thefunction) (car (icon thefunction))))
     
     themethod))

; LIBRARY CLASS definition
; Similar to defclass* + set the flag library of the class
(defmacro defclass! (name superclass slots &rest class-options)
   `(let ((newclass (defclass* ,name ,superclass ,slots ,.class-options)))
      (when *current-lib*
        (setf (library (find-class ',name)) (string-until-space (name *current-lib*)))
        
        ;;; compat with OM6: now we just store the icon id   
        (if (listp (icon (find-class ',name)))
          (setf (icon (find-class ',name)) (car (icon (find-class ',name)))))
        )
      newclass))

