;;;======================
;;; EXTERNAL PACKAGES:
;;; MANAGEMENT OF USER CLASSES AND METHODS
;;;======================

(in-package :om)

(defclass OMExtPackage (OMPackage OMPersistantFolder)
  ()
  ;((icon-resources  :initform nil :accessor icon-resources :documentation "package specific resource list"))
  (:documentation "External or user-defined package corresponding to 'real' folders and containing their own (self contained) resources"))

(defclass OMLib (OMExtPackage) 
   ((lib-pathname :initform nil :initarg :lib-pathname :accessor lib-pathname :documentation "main pathname to the library load file")
    (loaded? :initform nil :accessor loaded? :documentation "is the library loaded?")
    (version :initform nil :initarg :version :accessor version :documentation "library version"))
   (:documentation "The class for the OM Libraries. OM libraries are collections of classes and generic functions loaded dinamically in OM.

Libraries are registered in OM when they are detected in some of the predified or user-defined library search paths. They are loaded on demand or when needed by a patch using some of their contents."))


(defclass OMLispFile (OMBasicObject OMPersistantObject) ())

;;; already inherits:
;;; NAME from OMObject
;;; DOC, VERSION, CREATE-INFO from OMProgrammingObject
;;; ICON from OMBasicObject
(defclass OMUndefClass (OMProgrammingObject)
  ((superclasses :initform nil :initarg :sperclasses :accessor superclasses :documentation "the class superclasses (list of symbols)")
   (slot-list :initform nil :initarg :slot-list :accessor slot-list :documentation "the list of slot specification")
   (metaclass :initform nil :initarg :metaclass :accessor metaclass :documentation "the class metaclass"))
  (:documentation "Temporary object containing class definition info used during package and system loading process"))

;;; already inherits:
;;; NAME from OMObject
;;; DOC, VERSION, CREATE-INFO from OMProgrammingObject
;;; ICON from OMBasicObject
(defclass OMUndefMethod (OMProgrammingObject)
  ()
  (:documentation "Temporary object containing method definition info used during package and system loading process"))


(defmethod load-elements ((self OMExtPackage))
  (setf *loading-classes* nil
        *loading-methods* nil
        *loading-initmet* nil)
  (when (probe-file (mypathname self))
    (load-package-from-path self)
    (let* ((classes *loading-classes*)
           (methods *loading-methods*)
           (init-class-met *loading-initmet*)
           semi-methods final-methods badclasses)
      (setf badclasses (eval-initial-classes classes)) 
      (setf semi-methods (mapcar #'(lambda (elem) 
                                     (let ((new-semimethod (eval (second elem))))
                                       (when (ommethod-p new-semimethod) 
                                         (setf (mypathname new-semimethod) (first elem))
                                         new-semimethod))) methods))
      (setf final-methods (loop for elem in semi-methods
                                when elem collect (define-really-method elem)))
      (initial-methods-for-classes init-class-met badclasses) 
      (mapc #'(lambda (path) (put-in-packpackages path self badclasses))
            (om-directory (mypathname self) :files t :directories t))
      )))


(defmethod load-package-from-path ((self OMPackage))
  (catch 'om-read-error
    (handler-bind ((error #'(lambda (err) (om-message-dialog (format nil "~s could not be loaded because of the following error: ~s" 
                                                          (namestring path) (om-report-condition err)))
                            (throw 'om-read-error nil))))
      (setf (elements self)
            (remove nil 
                    (mapcar #'(lambda (path) 
                                (if (directoryp path)
                                    (let ((new-pack (make-instance 'OMExtPackage :name (name-of-directory path) 
                                                                   :mypathname path 
                                                                   :father self)))
                                      (load-package-from-path new-pack)
                                      newpack)
                                  (cond ((find (pathname-type path) '("lisp" "lsp") :test 'string-equal)
                                         (make-instance OMLispFile :name (pathname-name path) :mypathname path))
                                        ((find (pathname-type path) '("omc") :test 'string-equal)
                                         (when (om-check-persistant-file path :class)
                                           (load-om-class path)))
                                        ((find (pathname-type path) '("omm") :test 'string-equal)
                                         (when (om-check-persistant-file path :method)
                                           (load-om-method path)))
                                        (t nil))))
                            (om-directory (mypathname self) :files t :directories t))
                    ))
      )))


;;; RETURNS VALUES: CLASS-CODE INIT-METHOD-CODE
(defun load-om-class (path)
  (values-list (cdr (load-class-or-method path))))

(defun load-om-method (path)
  (load-class-or-method path))

;;; RETURNS LAST ELEMENT
;;; CODE IS NOT LOADED YET
(defun load-class-or-method (path) 
  (let ((*package* (find-package :om))
        rep eofp)
    (WITH-OPEN-FILE (in path :direction :input :if-does-not-exist nil)
      (loop while (not (eq eofp :eof)) do
            (setf rep eofp)
            (setf eofp (read in nil :eof))
            (when (and (listp eofp) (equal (car eofp) 'load-lib-for-first))
              (eval eofp))))
    rep))


;========================================
;Loading classes from disk

(defun test-class (x y)
  (string-equal y (car x)))

(defun test-class1 (x y)
  (string-equal x (car y)))

(defun sort-class-name-list (list visited)
   "Sort a list of classes, where a class precedes another one if it is a superclass"
   (if (null list) nil
       (let (rep)
         (loop for item in list do
               (let* ((superclasses (mapcar #'(lambda (x) (string x)) (second item)))
                      (allowed t))
                 (loop for super in superclasses
                       while allowed do
                       (unless (or (member super visited :test 'string-equal)
                                   (not (member super list :test 'test-class1)))
                         (setf allowed nil)))
                 (when allowed
                   (push (first item) rep)
                   (push (first item) visited))))
         (setf rep (reverse rep))
         (if (null rep)
           (error "Error at organizing loaded CLASSES !")
           (concatenate 'list rep (sort-class-name-list (set-difference list rep :test 'test-class) visited))))))

;;; EVAL CLASSES IN CORRECT ORDER ACCORDING TO SUPERCLASS DEPENDENCIES
;;; RETURNS CLASSES NOT LOADED
(defun eval-initial-classes (list)
   "This function is called in order to load the user-package's classes or when you import a package."
   (let* ((list-to-sort (loop for item in list
                              collect (list (second item) (eval (fourth item)))))
          (ordered-list (sort-class-name-list list-to-sort nil))
          badclasses)
     (loop for item in ordered-list do
           (let ((classdef (catch 'om-class-def-error 
                             (handler-bind ((error #'(lambda (err) 
                                                       (om-message-dialog (format nil "The class ~A could not be defined because of the following error: ~s" 
                                                                                  item (om-report-condition err)))
                                                       (throw 'om-class-def-error nil))))
                            (eval (find-if #'(lambda (x) (string-equal (second x) item)) list))))))
             (unless classdef (push item badclasses))))
     badclasses))


;(OM-LOAD-CLASS-WS  "ADD-11" 601 '(CS-EVT) '((SOURCE-CODE :INITFORM '(vgy) :TYPE T :ALLOCATION :CLASS :DOCUMENTATION "no documentation" :ACCESSOR SOURCE-CODE) (GLOBALS-LIST :INITFORM (LIST (LIST "gimaxamp" " init 32767 			; 16 bits")) :TYPE T :ALLOCATION :CLASS :DOCUMENTATION "no documentation" :ACCESSOR GLOBALS-LIST) (NUMCHAN :INITFORM 1 :TYPE T :ALLOCATION :CLASS :DOCUMENTATION "no documentation" :ACCESSOR NUMCHAN) (P4 :INITFORM 0 :INITARG :P4 :TYPE T :ALLOCATION :INSTANCE :DOCUMENTATION "no documentation" :ACCESSOR P4) (P5 :INITFORM 0 :INITARG :P5 :TYPE T :ALLOCATION :INSTANCE :DOCUMENTATION "no documentation" :ACCESSOR P5) (P6 :INITFORM 0 :INITARG :P6 :TYPE T :ALLOCATION :INSTANCE :DOCUMENTATION "no documentation" :ACCESSOR P6)) (OM-MAKE-POINT 50 50) "no doc" 'OMSTANDARDCLASS 'NIL NIL)


(defun detect-class-redefinition (list)
   "This function is used to detect if a class exists already when you import a package."
   (let (Rep)
     (loop for item in list do
           (when (exist-class-p (second item))
             (push (second item) rep)))
     rep))


(defun put-in-packpackages (path package badclasses)       
  (catch 'om-read-error
    (handler-bind ((error #'(lambda (err) (print (format nil "The user item ~s has not be loaded because of the following error: ~s" 
                                                                     (namestring path) (om-report-condition err)))
                              (throw 'om-read-error nil))))
      (when (om-persistant-p path)
        (if (directoryp path)
            (let ((new-pack (find-if #'(lambda (x) (string-equal (name x) (name-of-directory path)))
                                     (subpackages package))))
              (when new-pack
                (mapc #'(lambda (newpath) 
                          (put-in-packpackages newpath new-pack badclasses))
                      (om-directory  path :files t :directories t))))
          (let ((pathtype (file-type path)))
            (cond
             ((equal pathtype :CLAS)
              (let* ((loadsedlist (load-a-class/method path))
                     (name (interne (first loadsedlist))))
                (when (and (find-class name nil) (not (member (string name) badclasses :test 'string-equal)))
                  (addClass2Pack name package :protect nil :position (get-icon-pos (find-class name nil)))
                  )))
             ((equal pathtype :METH)
              (let* ((loadsedlist (load-a-class/method path))
                     (name (eval (second loadsedlist)))
                     (exists? (find-if #'(lambda (x) (string-equal (name x) (string name)))
                                       (functions package))))
                (when (and (fboundp name) (not exists?) (not (kernel-p (fdefinition name))))
                  (if (OMGenfun-p (fdefinition name))
                      (addGenFun2PAck name package :protect nil)
                    (addLispFun2Pack name package :protect nil)
                    ))))
             (t nil))))))))



;Used to prevent to put a folder in self.


(defmethod depending-p ((self OMPackage))
   "Test if there are classes or function used in other package the 'self' and its sub-package."
  (let ((classes (get-classes self))
        (rep nil))
    (loop for item in classes
          while (not rep) do
          (let ((superclass-item (remove-if #'(lambda (class) 
                                                (not (omclass-p  (class-of class))))
                                            (class-direct-superclasses item))))
            (loop for super in superclass-item
                  while (not rep) do
                  (when (and (not (kernel-p super))
                             (not (member super classes :test 'equal)))
                    (setf rep (string+ "class " (string (name item)) " inherits from class "
                                       (string (name super))))))))
    (when (not rep)
      (let ((genfuns (get-functions self)))
        (loop for item in genfuns
              while (not rep) do
              (let ((methods (get-elements item)))
                (loop for met in methods
                      while (not rep) do
                      (let ((quali (method-specializers met)))
                        (loop for qua in quali
                              while (not rep) do
                              (when (and (omclass-p  (class-of qua))
                                         (not (kernel-p qua))
                                         (not (member qua classes :test 'equal)))
                                (setf rep (string+ "A method of the generic function " (string (name item))
                                                   " is specialized by the class "
                                                   (string (name qua))))))))))))
    rep))



;This function is called when you drag the user package to the library package
;This function is not enough tested, but it works....
;;; ajouter pour les resources !!!
(defmethod package2userLib ((self OMPackage))
   "Save a package as a Librarie."
  (let ((continue-p  (om-create-directory (make-pathname :directory (append (pathname-directory *om-lib-dir*)
                                                                         (list (name self))))
                                       :if-exists nil)))
    (if (not continue-p) 
        (om-beep-msg "This lib name already exists, change the package name")
      (om-with-cursor *om-wait-cursor*
        (let* ((elements (mapcar #'(lambda (elem) (omNG-save-ws elem)) (subpackages self)))
               (classes (get-classes self))
               (code (omNG-save-packlist self))
               (method-list (get-methods self))
               (list-to-sort (loop for item in classes
                                   collect (list (name item) (make-super-class-list item))))
               (ordered-list (sort-class-name-list  list-to-sort nil)))
          (setf elements (remove-if 'null elements))
          (om-create-directory (make-pathname :directory (append (pathname-directory *om-lib-dir*)
                                                                         (list (name self) "patches"))))
          (om-create-directory (make-pathname :directory (append (pathname-directory *om-lib-dir*)
                                                                         (list (name self) "sources"))))
          (WITH-OPEN-FILE (out (make-pathname :directory (append (pathname-directory *om-lib-dir*)
                                                                 (list (name self)))
                                              
                                              :name (string+ (name self) ".lib"))
                               :direction :output ) 
            (prin1 '(in-package :om) out)
            (prin1 `(load-om ,(string+ (car (last (pathname-directory *om-lib-dir*))) ";" (name self) ";sources;" (name self)) out)))
          (WITH-OPEN-FILE (out (make-pathname :directory (pathname-directory
                                                          (OMRoot (string+ (car (last (pathname-directory *om-lib-dir*))) ";" (name self) ";sources;")))
                                              :name (string+ (name self) ".lisp"))
                               :direction :output ) 
            (prin1 '(in-package :om) out)
            (prin1 `(let* ((classes ',(mapcar #'(lambda (elem) (om-save-class (find-class (interne elem)))) ordered-list))
                           (methods ',(loop for met in method-list  collect (om-save-methods met)))
                           (subpack ',code)
                           (init-class-met ',(save-initial-class-methods classes)) badclasses smethods)
                      (setf badclasses (eval-initial-classes classes))
                      (setf smethods (mapcar #'(lambda (elem) (eval elem)) methods)) 
                      (mapc #'(lambda (elem) (when elem (define-really-method elem))) smethods)
                      (initial-methods-for-classes init-class-met badclasses)
                      (add-new-packages (eval subpack) *current-lib* badclasses 21)) out))
          t)))))