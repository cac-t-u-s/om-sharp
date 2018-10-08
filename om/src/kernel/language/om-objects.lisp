;============================================================================
; om7: visual programming language for computer-aided music composition
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

(in-package :om)

;;; for the OM Objects with name
(defclass named-object ()
  ((name :initform nil :accessor name :type string))
  (:documentation "Superclass for all OpenMusic Objects"))

(defmethod set-name ((self named-object) text)  (setf (name self) text))
(defmethod get-name ((self named-object)) (name self))

(defmethod set-name ((self t) newname) nil)
(defmethod get-name ((self t)) nil)

;;; Allows to use :name
(defmacro om-make-instance (class &rest initargs)
  "Special make-instance - class must be an OMObject."
  (if (getf initargs :name) 
      (let* ((pos (position :name initargs))
             (name (nth (1+ pos) initargs))
             (other-args (append (subseq initargs 0 pos) (subseq initargs (+ pos 2)))))
        `(let ((obj (make-instance ,class ,.other-args)))
           (set-name obj ,name)
           obj))
    `(make-instance ,class ,.initargs)))


;;; THE SUPERCLASS FOR EVERYTHING IN THE VPL
;;; name is with initarg
(defclass OMObject (named-object)
  ((name :initform nil :initarg :name :accessor name))
  (:documentation "Superclass for all OpenMusic Objects"))

;; SPECIALIZED FOR FUNCALLABLE OBJECTS
;; WHICH DO NOT ALLOW OMOBJECT INHERITANCE
(defclass OMFuncallableObject (standard-generic-function) 
  ((name :initform nil :initarg :name :accessor name))
  (:metaclass clos::funcallable-standard-class))

;;; careful: some objects shall better not be renamed (functions, classes...)
(defmethod set-name ((self OMFuncallableObject) text)  (setf (name self) text))
(defmethod get-name ((self OMFuncallableObject)) (name self))

;;; SUPECLASS FOR BOX, CONNECTIONS ETC.
(defclass OMVPObject (OMObject) ())

;;; AN OBJECT WITH AN EDITOR
(defclass ObjectWithEditor () 
  ((editor :initform nil :accessor editor)
   (window-pos :initform nil :accessor window-pos)
   (window-size :initform nil :accessor window-size)))

;===========================
; OMBASICOBJECTS = OBJECTS FROM THE VISUAL LANGUAGE
;===========================
(defclass OMBasicObject (OMObject ObjectWithEditor) 
  ((name :initform nil :accessor name :initarg :name :type string)  ;; name is redefined with :initarg
   (protected-p :initform nil :initarg :protected-p :accessor protected-p :documentation "determines if the object is protected (i.e. modifyable by the user) or not") 
   (icon :initform nil :initarg :icon :accessor icon)
   (references-to :initform nil :accessor references-to :documentation "mutable list containing the existing objects containing or referring to this object")
   ;(infowin :initform nil :accessor infowin :documentation "reference to the info window currently open for the object (if any)")
   )
  (:documentation "Superclass for OM metaobjects, like patches, classes, generic functions..."))

(defmethod release-reference ((self t) from) nil)

(defmethod release-reference ((self OMBasicObject) from)  
  (setf (references-to self) (remove from (references-to self))))

;===========================
; FUNCTIONS, METHODS ARE FUNCALLABLE BASIC OBJECTS
;===========================
(defclass OMFuncallableBasicObject (OMFuncallableObject)
  ((name :initform nil :accessor name :initarg :name :type string) ;; name is redefined with :initarg
   (protected-p :initform nil :initarg :protected-p :accessor protected-p)
   (icon :initform nil :initarg :icon :accessor icon)
   (frames :initform nil :accessor frames)
   (references-to :initform nil :accessor references-to))
  (:metaclass clos::funcallable-standard-class))


;(defmethod omNG-rename ((self OMfuncallableObject) name)
;  "This method changes the name of the object self with the new name NAME"
;  (setf (name self) name))


;===========================
; BASIC OM PROTOCOL FUNCTIONS
;===========================
;;; add something somewhere
(defgeneric omng-add-element (container element))

;;; remove something from somewhere
;;; element might still exist after
(defgeneric omng-remove-element (container element))

;;; called when an object is deleted (cleanup, etc.)
(defgeneric omng-delete (obj))

(defmethod omng-delete ((obj t)) nil)
(defmethod omng-delete ((obj OMBasicObject)) (close-editor obj))
(defmethod omng-delete ((obj OMFuncallableBasicObject)) (close-editor obj))

;------------------------------------------------------------------------------
; SPECIFIC SUBCLASSES
;------------------------------------------------------------------------------

(defclass OMProgrammingObject (OMBasicObject)
  ((compiled-fun-name :initform nil :accessor compiled-fun-name)
   (compiled? :initform nil  :accessor compiled?)
   (doc :initform "" :accessor doc :documentation "documentation")
   (omversion :initform *om-version* :accessor omversion :documentation "version of OM (last saved)")
   (create-info :initform '(nil nil) :accessor create-info :documentation "information about creation and last modification of the document (text)")
   (loaded? :initform t :accessor loaded? :documentation "is this document loaded?")
   (dependencies :initform nil :accessor dependencies :documentation "a list of subpatches"))
  (:documentation "Superclass for programming object and workspace elements (patches, maquette, etc.)"))

(defmethod initialize-instance :after ((self OMProgrammingObject) &rest initargs)
  (setf (compiled-fun-name self) (default-compiled-gensym self)))

(defmethod default-compiled-gensym  ((self OMProgrammingObject)) (gensym "om-"))

(defmethod box-symbol ((self OMProgrammingObject)) nil)

(defmethod set-name ((self OMProgrammingObject) name)
  (call-next-method)
  (when (editor self) (update-window-name (editor self)))
  (loop for box in (references-to self) ;; in principel there is only one at this stage
        do (set-name box (name self))
        do (om-invalidate-view (frame box))))

(defmethod compile-if-needed ((self OMProgrammingObject))
  ; (print (list "COMPILE" (name self) (compiled? self)))
  (unless (compiled? self) 
    (compile-patch self)))

(defmethod touch ((self t)) nil)

(defmethod touch ((self OMProgrammingObject))
  (setf (compiled? self) nil)
  (call-next-method))

(defclass OMPersistantObject () 
  ((mypathname :initform nil :initarg :mypathname :accessor mypathname :documentation "associated file pathname")
   (saved? :initform nil :accessor saved? :documentation "as the object been modified without saving?"))
  (:documentation "Mixin class for perstistant objects. Persistants object are the subset of the metaobjects which are stored as files or folders."))

(defmethod touch ((self OMPersistantObject))
  (setf (saved? self) nil)
  (call-next-method))

(defmethod update-from-editor ((self OMProgrammingObject)  &key (value-changed t) (reactive t))
  (loop for ref in (references-to self)
        do (update-from-editor ref :value-changed value-changed :reactive reactive))
  (touch self)
  (call-next-method))

;(defmethod update-from-editor ((self OMPersistantObject))
;  (touch self)
;  (call-next-method))

(defmethod is-persistant ((self OMPersistantObject)) self)
(defmethod is-persistant ((self t)) nil)

(defmethod find-persistant-container ((self OMPersistantObject)) self)

;;; go check with the container of the reference box
;;; in principle all references have the same container
(defmethod find-persistant-container ((self OMProgrammingObject))
  (when (references-to self)
    (find-persistant-container (car (references-to self)))))

(defmethod release-reference :around ((self OMPersistantObject) from)  
  (call-next-method)
  (unless (or (references-to self) (editor-window self))
    (unregister-document self)))

(defmethod window-name-from-object ((self OMPersistantObject))
  (if (mypathname self) 
      (if (probe-file (mypathname self))
          ;;; normal case
          (format nil "~A~A  [~A]" 
                  (if (saved? self) "" "*") (name self)
                  (namestring (om-make-pathname :directory (mypathname self))))
        ;;; problem : the patch is open but the file is missing
        (format nil "MISSING FILE ! [Will be saved as ~A]" (namestring (mypathname self)))
        )
    ;;; no pathname yet : newly created window
    (format nil "~A~A  [...]" (if (saved? self) "" "*") (name self))))

;;; DOES NOT COPY !! (e.G. if I copy a box refereing to a persistant object, the copy will refer to the same object)
(defmethod om-copy ((self OMPersistantObject)) self)

;;;=======================================
;;; FOLDERS (not used for the moment...)
;;;=======================================

(defclass OMFolder (OMBasicObject) 
   ((elements :initform nil :accessor elements :documentation "folders, patches and maquettes contained in the workspace"))
   ;(presentation :initform 1 :initarg :presentation :accessor presentation :documentation "presentation mode: 1=list, 0=icons"))
   (:documentation "The class of the OM folders"))

(defclass OMPersistantFolder (OMFolder OMPersistantObject) ()
  (:documentation "Superclass of persistant objects that are saved as folders."))








