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
; EXTERNAL POINTERS
;;===========================================================================

(in-package :oa)

; (setq fli::*default-dlflags* 6)
; (setq fli::*can-use-dlopen* nil)

;;;====================================
;;; POINTERS
;;;====================================
(export '(om-alloc-memory
          om-free-memory
          om-free-pointer
          om-read-ptr
          om-write-ptr
          om-make-null-pointer
          om-null-pointer-p
          om-pointer-equal
          om-cleanup-mixin
          om-cleanup
          om-create-with-gc
          )
        :om-api)

(defun om-make-null-pointer ()
  (fli:make-pointer :address 0 :type :void))

(defun om-alloc-memory (size &key (type :byte) (clear nil))
  (if clear
      (fli:allocate-foreign-object :type type :nelems size :fill 0)
    (fli:allocate-foreign-object :type type :nelems size)))

(defun om-free-memory (ptr)
  (fli::free-foreign-object ptr))

;;; !!! does not work very well: crashes a lot on Mac, e.g. with SDIF files
;(defun om-write-ptr (ptr pos type value)
;  (setf (fli:dereference ptr :type type :index pos) value))

; => inspired from CFFI
(defun om-write-ptr (ptr pos type value)
  (locally (declare (optimize (speed 3) (safety 0)))
    (setf (fli:foreign-typed-aref type ptr (the fixnum pos)) value)))

(defun om-read-ptr (ptr pos type)
  (fli:dereference ptr :type type :index pos))

(defun om-null-pointer-p (ptr) 
  ;(check-type ptr fli::pointer)
  (fli:null-pointer-p ptr))

(defun om-pointer-equal (ptr1 ptr2) 
  ;(check-type ptr fli::pointer)
  (fli:pointer-eq ptr1 ptr2))

#|

;; redefinitions using CFFI
;; previously cffi::%mem-set
(defun om-write-ptr (ptr pos type value) 
  (cffi::mem-set value ptr type pos))

;; previously cffi::%mem-ref
;; previously cffi::mem-ref
(defun om-read-ptr (ptr pos type) 
  (cffi::mem-aref ptr type pos))
 
;(setf aaa (om-make-pointer 5))
;(om-write-ptr aaa 1 :float 8.0)
;(om-read-ptr aaa 0 :float)

|#

;;;========================
;;; A POINTER STRUCT WITH COUNTER
;;;========================

(defstruct om-pointer 
  (ptr) 
  (size) 
  (count 0 :type integer))

(defun om-pointer-get-ptr (om-pointer) (om-pointer-ptr om-pointer))

;;; can be redefined for subtypes of om-pointer
(defmethod om-pointer-release-function ((self om-pointer)) 'om-free-pointer)

(defmethod om-free-pointer ((self om-pointer))
  (om-free-memory (om-pointer-ptr self)))

(defmethod om-retain ((omptr om-pointer))
  (setf (om-pointer-count omptr) (1+ (om-pointer-count omptr))))

(defmethod om-release ((omptr om-pointer))
  (setf (om-pointer-count omptr) (1- (om-pointer-count omptr)))
  (when (<= (om-pointer-count omptr) 0)
    (funcall (om-pointer-release-function omptr) omptr)))

;;;========================
;;; CLEANUP/DEALLOC UTILS
;;;========================

(hcl::add-special-free-action 'om-cleanup)

;;; FOR CLASSES
;;; to be subclassed by special-cleanup objects
(defclass om-cleanup-mixin () ())

;;; FOR STRUCTS
(defmacro om-create-with-gc (&body body)
   `(let ((object ,@body))
      ;(print (list "FLAG CLEANUP FOR" object))
      (hcl::flag-special-free-action object)
      object))

(defmethod initialize-instance :before ((self om-cleanup-mixin) &rest args) 
  ;(print (list "FLAG CLEANUP FOR" self))
  (hcl::flag-special-free-action self))

;;; to be redefined for specific om-cleanup-mixin subclasses
(defmethod om-cleanup ((self t)) nil)


