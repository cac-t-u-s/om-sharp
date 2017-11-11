;============================================================================
; o7: visual programming language for computer-aided music composition
; Copyright (c) 2013-2017 J. Bresson et al., IRCAM.
; - based on OpenMusic (c) IRCAM 1997-2017 by G. Assayag, C. Agon, J. Bresson
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================
; File author: J. Bresson
;============================================================================

;;;===========================
;;; OMPACKAGE IS A SPECIAL KIND OF FOLDER 
;;; WITH ABILITY TO MANAGE CLASS/METHOD DEFINITION AND ORGANIZATION
;;;===========================

(in-package :om)

;;; OMPackage <elements> (inherited from OMFolder) are the subpackages
(defclass OMAbstractPackage () 
  ((doc :initform "" :accessor doc :documentation "documentation")
   (classes  :initform nil :accessor classes :documentation "a list of OMClasses")
   (functions  :initform nil :accessor functions :documentation "a list of OMGenericFunctions or standard Lisp functions")
   (special-items  :initform nil :accessor special-items :documentation "a list ofspecial items :)")
   (aliasclasses  :initform nil :accessor aliasclasses :documentation "a list of OMClasses aliases referring classes in other package (used for inheritance)"))
  (:documentation "This is the class of the OMPackage metaobject. A package is a collection of classes and generic functions. Packages can also contain subpackage and initiate hierearchical package trees.

For easier browsing it is recommended that a package do not contain at the same time subpackage and classes or functions.

<elements> is a lst of subpackages"))



(defclass OMPackage (OMAbstractPackage OMFolder) ())

;;; used when the package corresponds to a 'real' folder, e.g. for external libraries
(defclass OMPersistantPackage (OMAbstractPackage OMPersistantFolder) ())

(defvar *om-package-tree* (make-instance 'OMPackage :name "OM Root Package") "The package of all OM predefined functions and classes")


(defmethod subpackages ((self OMAbstractPackage)) (elements self))

(defmethod omNG-add-element ((self OMAbstractPackage) (element OMPackage))
  (setf (elements self) (append (elements self) (list element))))

(defmethod omNG-add-element ((self OMAbstractPackage) (element OMClass))
  (setf (classes self) (append (classes self) (list element))))

(defmethod omNG-add-element ((self OMAbstractPackage) (element function))
  (setf (functions self) (append (functions self) (list element))))

(defmethod omNG-add-element ((self OMAbstractPackage) (element symbol))
  (setf (special-items self) (append (special-items self) (list element))))


;;; Empty package
(defmethod CleanupPackage ((self OMAbstractPackage))
  (setf (elements self) nil
        (functions self) nil
        (classes self) nil
        (aliasclasses self) nil))

;;; Determines in <container> is a ancestor (container) of <self>
(defmethod ancestor-p ((self OMAbstractPackage) (container OMAbstractPackage))
  (or (eq self container) 
      (let ((ancestor nil))
        (loop for pack in (subpackages self)
              while (not ancestor) do
              (setf ancestor (ancestor-p pack container)))
        ancestor)))
    

;;; No function in this package and subpackages
(defun empty-fun-p (pack)
  (and (null (functions pack))
       (or (null (subpackages pack))
           (let ((empty t))
             (loop for p in (subpackages pack) while empty do
                   (unless (empty-fun-p p) (setf empty nil)))
             empty))))

;;; No class in this package and subpackages
(defun empty-class-p (pack)
  (and (null (classes pack))
       (or (null (subpackages pack))
           (let ((empty t))
             (loop for p in (subpackages pack) while empty do
                   (unless (empty-class-p p) (setf empty nil)))
             empty))))

;;; Gets all symbol names in package

(defmethod get-name ((self function)) (string-downcase (function-name self)))

; (get-all-symbol-names *om-package-tree*)
(defmethod get-all-symbol-names ((self OMAbstractPackage))
  (append (mapcar 'get-name (functions self))
          (mapcar 'get-name (classes self))
          (special-items self)
          (loop for item in (elements self) append (get-all-symbol-names item))))




;;; Fill package tools : Subpackages
(defmethod AddPackage2Pack ((new-Package OMAbstractPackage) inPackage)
    (unless (member (name new-Package) (subpackages inPackage) :test 'string-equal :key 'name)
      (omNG-add-element inPackage new-package)
      new-package))

(defmethod AddPackage2Pack ((name string) inPackage)
  (AddPackage2Pack (make-instance 'OMPackage :name package-name) in-package))

;;; Fill package tools : Classes
(defmethod AddClass2Pack ((classname symbol) inPackage)
    (if (find-class classname nil) 
        (unless (find classname (classes inPackage) :test 'equal :key 'class-name)
          (export-symbol-from-om classname)
          (omNG-add-element inPackage (find-class classname)))
      (om-beep-msg (format nil "Undefined class: ~A" classname))))

(defmethod AddClass2Pack ((classname string) inPackage)
  (AddClass2Pack (read-from-string classname) inPackage))

(defmethod AddClass2Pack ((classname list) inPackage)
  (mapc #'(lambda (class) (AddClass2Pack class inPackage)) classname))

;;; Fill package tools : Functions
(defmethod AddFun2Pack ((funname symbol) inPackage)
    (if (fboundp funname)
        (unless (find funname (functions inPackage) :test 'equal :test 'function-name)
          (if (subtypep (type-of (fdefinition funname)) 'omgenericfunction)
              (progn
                (export-symbol-from-om funname)
                (omNG-add-element inPackage (fdefinition funname)))
            ;;;(omNG-add-element inPackage (omNG-make-new-lispfun funName)) ; je crois que c'est pas la peine
            (omNG-add-element inPackage (fdefinition funname))))
      (om-beep-msg (format nil "Undefined function: ~A" funname))
      ))

(defmethod AddFun2Pack ((funname string) inPackage)
  (AddFun2Pack (read-from-string funname) inPackage))


(defmethod AddSpecialItem2Pack ((item symbol) inPackage)
  (unless (find item (special-items inPackage) :test 'equal)
    (omNG-add-element inPackage item)))
 

(defmethod AddFun2Pack ((funname list) inPackage)
   (mapcar #'(lambda (fun) (AddGenFun2Pack fun inPackage)) funname))

; Creates a package tree form a list of strings (names) and symbols, with pack as root
(defun omNG-make-package  (package-name &key doc container-pack subpackages functions classes)
  (let* ((new-pack (cond ((and package-name container-pack)
                          (or (find package-name (subpackages container-pack) :key 'name :test 'string-equal)
                              (let ((pack (make-instance 'OMPackage :name package-name)))
                                (addpackage2pack pack container-pack)
                                pack)))
                         (package-name
                          (make-instance 'OMPackage :name package-name))
                         (container-pack container-pack)
                         (t (make-instance 'OMPackage :name "Untitled Package")))))
    (when doc (setf (doc new-pack) doc))
    (mapcar #'(lambda (class) (addclass2pack class new-pack)) classes)
    (mapcar #'(lambda (fun) (addFun2Pack fun new-pack)) functions)
    (mapcar #'(lambda (pk) (addpackage2pack pk new-pack)) subpackages)
    new-pack))


#|
;;; Creates a menu with package functions
(defun package-fun2menu (package &optional name action)
  (let ((subpack (copy-list (subpackages package))))
    (om-make-menu (or name "Functions")
                  (list  
                   (mapcar #'(lambda (f) (om-make-menu-item (name f) #'(lambda () (funcall action f))))
                           (functions package))
                   (remove nil 
                           (loop for item in subpack collect
                                 (when (and (not (and (subtypep (type-of item) 'OMLib) (not (loaded? item))))
                                            (or (not (empty-fun-p item)) (equal item *package-user*)))
                                   (package-fun2menu item (name item) action)))
                           )))))

;;; Creates a menu with package classes
(defun package-classes2menu (package &optional name action)
  (let ((subpack (copy-list (subpackages package))))
    (om-make-menu (or name "Classes")
                  (list  
                   (mapcar #'(lambda (c) (om-make-menu-item (name c) #'(lambda () (funcall action c))))
                           (classes package))
                   (remove nil 
                           (loop for item in subpack collect
                                 (when (and (not (and (subtypep (type-of item) 'OMLib) (not (loaded? item))))
                                            (or (not (empty-class-p item)) (equal item *package-user*)))
                                   (package-classes2menu item (name item) action)))
                           )))))
|#


