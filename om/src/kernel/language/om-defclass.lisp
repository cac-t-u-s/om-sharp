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

;=========================================================================
; OM Class definition
;=========================================================================

(in-package :om)

(defvar *def-metaclass-class*  'omstandardclass "the meta-class for om classes")
(defvar *def-metaclass-slot*   'omslot "the meta-class for om slots")


;;--------------------------------------------------
;; OM Class definition : DEFCLASS* 
;;--------------------------------------------------
; defclass* uses standard defclass, but set automaticly the metaclass to the OM meta-class.
; this macro fill the omslot of the class object i.e. icon, name.
; if the option :update is set to T the macro update all objects (classes, frames, etc.) attached to the class."
(defun parse-defclass-options (args)
   (let* (icon metaclass doc newargs update?)
     (loop while args do
           (cond
            ((equal (caar args) :icon)            (setf icon (second (pop args))))
            ((equal (caar args) :documentation)   (setf doc (second (pop args))))
            ((equal (caar args) :metaclass)       (setf metaclass (second (pop args))))
            ((equal (caar args) :update)          (setf update? (second (pop args))))
            (t (push (pop args) newargs ))))
     (values newargs icon metaclass doc update?)))
 
(defmethod update-from-reference  ((self t)) nil)

(defmethod update-from-reference  ((self OMClass) &optional)
  "This method is called when you redifine <self> to update the attached objects."
  (mapc #'update-from-reference (class-direct-subclasses self))
  (mapc #'update-from-reference (references-to self)))

;;; supported options are
;;; :icon :documentation :metaclass :update 
(defmacro defclass* (name superclasses slots &rest class-options)
  (let ((slot-doc (loop for slot in slots
                        collect (list (car slot) 
                                      (let ((p (position :documentation slot :test 'equal)))
                                        (if p (nth (1+ p) slot) ""))))))

    (multiple-value-bind (new-options icon metaclass doc up?)
       (parse-defclass-options class-options)
      (unless metaclass (setf metaclass *def-metaclass-class*))
      (unless doc (setf doc ""))
     
     `(let ((new-class (defclass ,name ,superclasses ,slots 
                         (:metaclass ,metaclass)
                         (:documentation ,doc)
                         ,.new-options)))
        
        
        ;(setf (slot-docs new-class) ',slot-doc)
        (setf (name new-class) (string ',name))
        (setf (icon new-class) (or ,icon 'icon-class))
        (update-from-reference new-class)
        new-class)
     )))
