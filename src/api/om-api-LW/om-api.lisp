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


(in-package :cl-user)


(defpackage "OM-API"
  (:nicknames "OA")
  ;(:use "COMMON-LISP" "CL-USER" "OM-LISP" "CAPI" "LISPWORKS" "GP")
  (:use "COMMON-LISP" "LISPWORKS")
  )

(in-package :oa)

(defparameter *api-directory* (pathname-directory (truename *load-pathname*)))

(export '(om-api-init) :om-api)

;;; API INIT
(defparameter *api-init-list* nil)

(defun om-api-add-init-fun (fun-name)
  (unless (member fun-name *api-init-list* :test 'equal)
    (push fun-name *api-init-list*)))

(defun om-api-init ()
  (print "Initializing API")
  (mapc #'(lambda (fun) fun (funcall fun)) (reverse *api-init-list*)))


(let ((api-files '("files"
                   "processes"
                   "scheduler"
                   "tools"
                   "system"
                   "graphics"
                   "graphic-object"
                   "actions"
                   "window"
                   "layout"
                   "view"
                   "dialog-items"
                   "draw"
                   "transient-drawing"
                   "icons-picts"
                   "menu"
                   "item-view"
                   "item-tree"
                   "cursor"
                   "user-interface"
                   "draganddrop"
                   "tooltips"
                   "libraries"
                   "om-special"
                   )))

  (mapc #'(lambda (filename)
            (cl-user::compile&load (make-pathname :directory *api-directory* :name filename))
            (terpri))
        api-files))
