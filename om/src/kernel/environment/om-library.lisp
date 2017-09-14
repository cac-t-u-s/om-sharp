;;;===================================
;;; EXTERNAL LIBRARIES
;;; ... are just special kinds of dynamic-loaded packages
;;;===================================

(in-package :om)

;;; ideally third-party libraries are coded in a separate Lisp package
;;; these are the basic :OM functions which might be needed in a library
(export 
 '(require-library           ;;; requies another lib
   gen-lib-reference         ;;; auto-generates the lib reference
   defclass!         ;;; defines a class in the library
   defmethod!        ;;; defines a method in the library
   ) :om)

(defclass OMLib (OMPersistantPackage) 
  ((version :initform nil :accessor version :initarg :version :documentation "version number (\"a.b.c\")")
   (author :initform nil :accessor author :initarg :author :documentation "a string documenting authorship/copyright")
   (source-files :initform nil :accessor source-files :initarg :source-files :documentation "a list of Lisp source files pathnames")
   (loaded? :initform nil :accessor loaded? :documentation "true if the sources are loaded")
   ))

;;; conventional folders for library
(defmethod lib-resources-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (mypathname self)) '("resources"))))

(defmethod lib-icons-folder ((self OMLib))
   (om-make-pathname :directory (append (pathname-directory (lib-resources-folder self)) '("icons"))))

(add-preference-module :libraries "Libraries")
(add-preference :libraries :libs-folder1 "Libraries folder 1" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder2 "Libraries folder 2" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder3 "Libraries folder 3" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :libs-folder4 "Libraries folder 4" :folder :no-default nil 'update-libraries-folder)
(add-preference :libraries :auto-load "Auto load" :bool nil "(Will silently load any required libraries)")

;; typically used from libraries, this Preference tab will allow to deal with externalk utils' preferences (e.g. Csound, etc.)
(add-preference-module :externals "Externals")


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

;;=============

(defparameter *libs-folders* nil)
(defparameter *default-libs-folder* nil)

(defun init-default-libs-folder ()
  (setq *default-libs-folder* (merge-pathnames "libraries/" (om-root-folder))))

(defmethod lib-loader-file ((self OMLib))
   (om-make-pathname :directory (mypathname self) :name (name self) :type "omlib"))

  
;;; adds the library to the OM package tree
;;; does not load the lib yet
(defun register-new-library (name path &optional (warn-if-exists t))
  (if (find-om-library name)
      (when warn-if-exists (om-beep-msg "A library named ~A already exists. Can not register it as new library." name))
    (let ((new-library (make-instance 'OMLib :mypathname path :name name)))
      (if new-library
          (setf (elements *om-libs-root-package*)
                (append (elements *om-libs-root-package*) (list new-library)))
        (om-beep-msg "Could not register library: ~A" path)))))


(defun register-all-libraries (&optional (warn-if-exists t))
  (unless *default-libs-folder* (init-default-libs-folder))
  (loop for folder in (remove nil 
                              (list *default-libs-folder* 
                                    (get-pref-value :libraries :libs-folder1)
                                    (get-pref-value :libraries :libs-folder2)
                                    (get-pref-value :libraries :libs-folder3)
                                    (get-pref-value :libraries :libs-folder4)))
        do (mapc #'(lambda (path) 
                     (let ((lib-name? (string-until-space (car (last (pathname-directory path))))))
                       (if (probe-file (om-make-pathname :directory path :name lib-name? :type "omlib"))
                         ;;; this is an OM library!
                         (register-new-library lib-name? path warn-if-exists)
                         (om-beep-msg "Library doesn't have a loader file: ~A.omlib not found.." lib-name?))))
                 (om-directory folder :directories t :files nil))))

;;; called when the folder(s) change
;;; can not unload loaded libraries: won"t be updated (but silently)
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


;;;===========
;;; LOADING
;;;===========

;;; used by defmethod! and defclass!
(defvar *current-lib* nil "containts the library that is being loaded")

;;; loads a registered library
(defmethod load-om-library ((lib string))
  (let ((the-lib (find-om-library lib)))
    (if the-lib (load-om-library the-lib)
      (om-beep-msg "Library: ~S not registered !" lib))))


(defmethod load-om-library ((lib OMLib))    
  (let ((lib-file (om-make-pathname :directory (mypathname lib) :name (name lib) :type "omlib")))                                      
    (if (probe-file lib-file)
      (handler-bind ((error #'(lambda (c)
                                (om-message-dialog (format nil "Error while loading the library ~A:~%~s" 
                                                           (name lib) (om-report-condition c))
                                                   :size (om-make-point 300 200))
                                (om-abort))))
        (om-print-format "~%~%Loading library: ~A..." (list lib-file) "OM")
        (let* ((*current-lib* lib)
               (file-contents (list-from-file lib-file))  ;; "/Users/bresson/SRC/OM7/OM7/LIBRARIES/om-supervp/om-supervp.omlib"
               (lib-data (find-values-in-prop-list file-contents :om-lib))
               (version (find-value-in-kv-list lib-data :version))
               (author (find-value-in-kv-list lib-data :author))
               (doc (find-value-in-kv-list lib-data :doc))
               (files (find-values-in-prop-list lib-data :source-files))
               (symbols (find-values-in-prop-list lib-data :symbols)))
        
          (setf (version lib) version
                (doc lib) doc
                (author lib) author)
          
          (om-print-format 
           "=====================~%~A ~A ~%~A~%~A~%=====================~%~%"
           (list (name lib) (or (version lib) "") (or (doc lib) "") (or (author lib) "")))
          
          ;;; load sources
          (mapc #'(lambda (f)
                    (let ((path (merge-pathnames (omng-load f) (mypathname lib))))   
                      (compile&load (namestring path))))
                files)

          ;;; set packages
          (CleanupPackage lib)
          (mapc #'(lambda (class) (addclass2pack class lib)) (find-values-in-prop-list symbols :classes))
          (mapc #'(lambda (fun) (addFun2Pack fun lib)) (find-values-in-prop-list symbols :functions))
          (mapc #'(lambda (pk) 
                    (let ((new-pack (omng-load pk)))
                      (addpackage2pack new-pack lib)))
                (find-values-in-prop-list symbols :packages))
    
          (set-om-pack-symbols) ;; brutal...
          
          (register-images (lib-icons-folder lib))
          (setf (loaded? lib) t)
          (update-preference-window-module :libraries) ;;; update the window is opened       
          lib-file))
      (om-beep-msg "Library doesn't have a loader file: ~A NOT FOUND.." lib-file))
    ))

;;; can be called from another library, etc.
(defun require-library (name)
  (let ((required-lib (find-om-library name)))
    (if required-lib
        (unless (loaded? required-lib)
          (load-om-library required-lib))
      (om-beep-msg "Required library: ~S not found !" name))
    ))




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

