;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
; Class definition
;=========================================================================

(in-package :om)

(defvar *def-metaclass-class*  'omstandardclass "the meta-class for om classes")
(defvar *def-metaclass-slot*   'omslot "the meta-class for om slots")


;;--------------------------------------------------
;; Class definition : DEFCLASS* 
;;--------------------------------------------------
; defclass* uses standard defclass, but set automatically the metaclass to the OMClass meta-class.
; this macro fills the omslot of the class object i.e. icon, name.
; if the option :update is set to T the macro update all objects (classes, frames, etc.) attached to the class."
;;--------------------------------------------------

(defun parse-defclass-options (options)
   (let* (icon metaclass doc other-options)
     (loop while options do
           (cond
            ((equal (caar options) :icon)            (setf icon (second (pop options))))
            ((equal (caar options) :documentation)   (setf doc (second (pop options))))
            ((equal (caar options) :metaclass)       (setf metaclass (second (pop options))))
            (t (push (pop options) other-options))))
     (values other-options icon metaclass doc)))
 
(defmethod update-from-reference  ((self t)) nil)

(defmethod update-from-reference  ((self OMClass))
  "This method is called when you redifine <self> to update the attached objects."
  (mapc #'update-from-reference (class-direct-subclasses self))
  (mapc #'update-from-reference (references-to self)))

;;; supported options are
;;; :icon :documentation :metaclass :update 
(defmacro defclass* (name superclasses slots &rest class-options)
 
  (multiple-value-bind (other-options icon metaclass doc update?)
      (parse-defclass-options class-options)
        
    `(let ((new-class (defclass ,name ,superclasses ,slots 
                        (:metaclass ,(or metaclass *def-metaclass-class*))
                        (:documentation ,(or doc ""))
                        ,.other-options)))
       
       (setf (name new-class) (string ',name))
       (setf (icon new-class) (or ,icon 'icon-class))
       (update-from-reference new-class)
       new-class)
     ))
