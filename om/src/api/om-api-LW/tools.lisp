;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;=========================================================================
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed; in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;
;=========================================================================
; Authors: J. Bresson, C. Agon
;=========================================================================

;;===========================================================================
; Misc. TOOLS
;;===========================================================================

(in-package :om-api)


(export '(function-name
          method-name
          function-arglist
          function-documentation
          class-documentation
          class-instance-slots
          class-direct-instance-slots
          slot-name
          slot-initform
          slot-initargs
          slot-type
          slot-doc
          ) :om-api)


;========================
; FUNCTION NAMES AND DOC
;========================

;;; cases and examples:
;;; generic function, e.g. 'om::om+
;;; functio,, e.g. 'append
;;; special "macro-function", e.g. 'setf
;;; anonymmous function
;;; subfunction
;;; struct accessors


; (function-name (fdefinition 'pathname-name))

(defun function-arglist (fun)
  (let ((fsym (if (symbolp fun) fun (function-name fun)))
        (fdef (if (symbolp fun) (fdefinition fun) fun))) 
    (cond ((clos::generic-function-p fdef)
           (hcl::method-lambda-list (car (hcl::generic-function-methods fdef))))
          ((macro-function fsym)
           (function-lambda-list fsym))
          (fsym (clos::extract-lambda-list-names-and-keys (function-lambda-list fsym)))
          (t (function-lambda-list fun)))
    ))
 
(defmethod function-name ((fun symbol)) fun)

(defmethod function-name (fun)
  (if (clos::generic-function-p fun)
      (hcl::generic-function-name fun)
    (let ((lambda-exp-name (nth 2 (multiple-value-list (FUNCTION-LAMBDA-EXPRESSION fun)))))
      (cond ((equal lambda-exp-name 'system::fast-list) 'list)
            ((symbolp lambda-exp-name) lambda-exp-name)  ;; does not work, e.g. for CONS...
            ;; ((consp lambda-exp-name) nil) ;; the function is (?) a subfunction named by the compiler // removed because struct-accessors fall here...
            (t (system::function-name fun))   ;;; does not work with non-inbuilt functions...
            ))))

;; does not work with structure accessors
;;(function-name (fdefinition (read-from-string "las::las-sound-ptr")))

(defun method-name (method)
  (clos::generic-function-name (clos::method-generic-function method)))

(defun function-documentation (function)
  (or (system::function-documentation function)
      (documentation function 'function)))


;=================
; CLOS/MOP
;=================

(defun get-class-precedence-list (class) (hcl::class-precedence-list class))
(defun get-class-default-initargs (class) (hcl::class-default-initargs class))
(defun class-slots (class) (hcl::class-slots class))
(defun class-direct-slots (class) (hcl::class-direct-slots class))

(defun slot-name (slot) (clos::slot-definition-name slot))
(defun slot-allocation (slot) (clos::slot-definition-allocation slot))
(defun slot-initargs (slot) (clos::slot-definition-initargs slot))
(defun slot-initform (slot) (clos::slot-definition-initform slot))
(defun slot-type (slot) (clos::slot-definition-type slot))
(defun slot-doc (slot) (slot-value slot 'clos::documentation-slot))

(defun class-documentation (class)
  (let ((realclass (if (symbolp class) (find-class class nil) class)))
    (if realclass
        (system::class-documentation realclass)
      (concatenate 'string "Class " (string class) " not found"))))

(defun class-instance-slots (class)
  (remove :class (class-slots class) :test 'equal :key 'slot-allocation))

(defun class-direct-instance-slots (class)
  (remove :class (class-direct-slots class) :test 'equal :key 'slot-allocation))

(defun class-class-slots (class)
  (remove :instance (class-slots class) :test 'equal :key 'slot-allocation))

;=================
; Special LW compatibility 

(in-package :cl-user)

(defun set-nthcdr (index list new-value)
  "If INDEX is 0, just return NEW-VALUE."
  (if (not (zerop index))
    (rplacd (nthcdr (1- index) list)
            new-value))
  new-value)

(let ((lispworks::*HANDLE-WARN-ON-REDEFINITION* nil)) 
  (defsetf nthcdr set-nthcdr))

