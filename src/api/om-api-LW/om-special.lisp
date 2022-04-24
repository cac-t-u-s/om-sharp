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

;===========================================================================
; SPECIAL OPENMUSIC FUNCTIONS
;===========================================================================


(in-package :om-api)


(export '(
          om-special-lisp-form-p
          om-root-folder
          om-set-root-folder
          om-resources-folder
          om-inspect
          ) :om-api)



;;; types of functions that are not accepted as OM boxes
(defun om-special-lisp-form-p (symbol)
  (or
   (lispworks::special-form-p symbol)
   #+macosx(and (fboundp symbol) ;;; what is this for... ?
                (system:closurep (fdefinition symbol)))
   ))

;=======================
; SOURE MANAGEMENT
;=======================

(defparameter *om-root* cl-user::*om-root-directory*)

(defun om-root-folder () *om-root*)

(defun om-set-root-folder (path)
  (setf *om-root* path))


(defun om-resources-folder ()
  #+macosx
  (if (oa::om-standalone-p)
      (make-pathname
       :host (pathname-host (oa::om-lisp-image))
       :device (pathname-device (oa::om-lisp-image))
       :directory (append (butlast (pathname-directory (oa::om-lisp-image))) '("Resources")))
    (make-pathname
     :host (pathname-host (oa::om-root-folder))
     :device (pathname-device (oa::om-root-folder))
     :directory (append (pathname-directory (oa::om-root-folder)) '("resources"))))
  #+linux
  (if (oa::om-standalone-p)
      (make-pathname
       :host (pathname-host (oa::om-lisp-image))
       :device (pathname-device (oa::om-lisp-image))
       :directory (append (butlast (pathname-directory (oa::om-lisp-image))) '("share" "om-sharp" "resources")))
    (make-pathname
     :host (pathname-host (oa::om-root-folder))
     :device (pathname-device (oa::om-root-folder))
     :directory (append (pathname-directory (oa::om-root-folder)) '("resources"))))
  #+win32
  (make-pathname
   :host (pathname-host (oa::om-root-folder))
   :device (pathname-device (oa::om-root-folder))
   :directory (append (pathname-directory (oa::om-root-folder)) '("resources")))
  )


;=======================
; INSPECTOR
;=======================

(defclass inspect-dialog (om-dialog)
  ((object :initarg :object :initform nil :accessor object)
   (parents :initform nil :accessor parents)
   (list1 :initform nil :accessor list1)
   (list2 :initform nil :accessor list2)
   (selected-item :initform nil :accessor selected-item)
   (parent-button :initform nil :accessor parent-button)
   (parent-label  :initform nil :accessor parent-label)))


(defun decompose-object (obj)
  (cond ((consp obj)
         (if (listp (cdr obj))
             (loop for item in obj
                   for i = 0 then (+ i 1)
                   collect (list i item))
           (list (list "car" (car obj)) (list "cdr" (cdr obj)))))
        ((arrayp obj)
         (loop for i = 0 then (+ i 1) while (< i (length obj))
               collect (list i (aref obj i))))
        ((structurep obj)
         (loop for sl in (structure::structure-class-slot-names (find-class (type-of obj) nil))
               collect (list sl (slot-value obj sl))))
        ((and (find-class (type-of obj) nil) (hcl::class-slots (find-class (type-of obj))))
         (loop for slot in (hcl::class-slots (find-class (type-of obj)))
               collect (list (hcl::slot-definition-name slot)
                             (if (hcl::slot-boundp obj (hcl::slot-definition-name slot))
                                 (slot-value obj (hcl::slot-definition-name slot))
                               :unbound))))
        ((atom obj) (list obj))
        (t nil)))


(defun set-inspector-panel (item-list object)
  (setf (capi::collection-items item-list)
        (loop for elt in (decompose-object object)
              collect (if (consp elt)
                          (concatenate 'string
                                       (string-upcase (format nil "~A" (car elt)))
                                       (format nil ":   ~A"  (cadr elt)))
                        (format nil "~A"  elt)))))


(defmethod set-selected-item ((win inspect-dialog) selected-value)
  (setf (selected-item win) selected-value)
  (set-inspector-panel (list2 win) (selected-item win)))


(defmethod set-inspected-object ((win inspect-dialog) name value)

  (om-set-window-title win (format nil "Inspecting ~A" (if (numberp name) value name)))

  (setf (object win) value)

  (set-inspector-panel (list1 win) value)

  (set-selected-item win (cadr (car (decompose-object value))))

  (om-set-dialog-item-text (parent-label win) (format nil "Parent: ~A" (car (parents win))))
  (om-enable-dialog-item (parent-button win) (parents win)))


(defmethod initialize ((self inspect-dialog))

  (setf (list1 self)
        (om-make-di 'om-single-item-list
                    :size (om-make-point nil 260)
                    :focus nil
                    :scrollbars :v
                    :callback-type '(:collection)
                    :test-function 'string-equal
                    :range nil
                    :action-callback #'(lambda (list)
                                         (let* ((selection (nth (capi::choice-selection list)
                                                                (decompose-object (object self))))
                                                (selection-name (car selection))
                                                (selection-value (cadr selection)))

                                           (when (consp (car (decompose-object selection-value)))
                                             (push (object self) (parents self))
                                             (set-inspected-object self selection-name selection-value)
                                             )))
                    :selection-callback #'(lambda (list)
                                            (let ((selection (nth (capi::choice-selection list)
                                                                  (decompose-object (object self)))))
                                              (set-selected-item self (cadr selection))))
                    ))

  (setf (list2 self)
        (om-make-di 'om-single-item-list
                    :size (om-make-point nil 260)
                    :focus nil
                    :scrollbars :v
                    :callback-type '(:collection)
                    :test-function 'string-equal
                    :range nil
                    :action-callback #'(lambda (list)
                                         (let ((selection (nth (capi::choice-selection list)
                                                               (decompose-object (selected-item self)))))
                                           (when (consp selection)
                                             (let ((selection-name (car selection))
                                                   (selection-value (cadr selection)))
                                               (when (consp (car (decompose-object selection-value)))
                                                 (push (object self) (parents self))
                                                 (push (selected-item self) (parents self))
                                                 (set-inspected-object self selection-name selection-value)
                                                 )))
                                           ))
                    ))

  (setf (parent-button self)
        (om-make-di 'om-button
                    :size (omp 40 24)
                    :text "<"
                    :di-action #'(lambda (button)
                                   (declare (ignore button))
                                   (set-inspected-object self 0 (pop (parents self))))
                    ))

  (setf (parent-label self)
        (om-make-di 'om-simple-text :size (omp nil 16)))

  (om-add-subviews
   self
   (om-make-layout
    'om-column-layout
    :delta 6
    :subviews (list
               (om-make-layout
                'om-row-layout
                :align :center
                :subviews (list (parent-button self) (parent-label self)))
               (om-make-layout
                'om-row-layout
                :subviews (list (list1 self) (list2 self))))))
  )



(defun om-inspect (object &optional position)

  (let* ((win (om-make-window 'inspect-dialog
                              :object object
                              :size (omp 400 nil)
                              :position (or position (om-make-point 200 200))
                              :resizable t
                              :win-layout 'om-simple-layout
                              :menu-items (list (om-make-menu
                                                 "File"
                                                 (list (om-make-menu-item
                                                        "Close all inspector windows"
                                                        #'(lambda () (mapc 'om-close-window (om-get-all-windows 'inspect-dialog)))
                                                        :key "w"))))
                              )))

    (initialize win)
    (set-inspected-object win 0 object)

    (om-select-window win)))

