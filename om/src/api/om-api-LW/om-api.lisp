;=========================================================================
; OM API 
; Multiplatform API for OpenMusic
; LispWorks Implementation
;
;  Copyright (C) 2007-2009 IRCAM-Centre Georges Pompidou, Paris, France.
; 
;    This file is part of the OpenMusic environment sources
;
;    OpenMusic is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    OpenMusic is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU General Public License for more details.
;
;    You should have received a copy of the GNU General Public License
;    along with OpenMusic.  If not, see <http://www.gnu.org/licenses/>.
;
; Authors: Carlos Agon, Jean Bresson
;=========================================================================

(in-package :cl-user)

(load (make-pathname :directory (append (pathname-directory (truename *load-pathname*)) 
                                        '("lw-lisp-tools"))
                     :name "load-lw-lisp-tools" :type "lisp"))

(defpackage "OM-API"
  (:nicknames "OA")
  (:use "COMMON-LISP" "CL-USER" "OM-LISP" "CAPI" "LISPWORKS" "GP"))

(in-package :oa)

(defparameter *api-directory* (pathname-directory (truename *load-pathname*)))

(export '(om-api-init om-api-exit) :om-api)

;;; API INIT 
(defparameter *api-init-list* nil)

(defun om-api-add-init-fun (fun-name)
   (unless (member fun-name *api-init-list* :test 'equal)
        (push fun-name *api-init-list*)))

(defun om-api-init ()
  (print "== START OM-API INIT CALLS ==")
  (mapc #'(lambda (fun) (print fun) (funcall fun)) (reverse *api-init-list*))
  (print "== END OM-API INIT CALLS =="))

;;; API CLEANUP 
(defparameter *api-cleanup-list* nil)

(defun om-add-cleanup-fun (fun-name &optional last?)  
(unless (member fun-name *api-cleanup-list* :test 'equal)
  (if last?
     (push fun-name  *api-cleanup-list*)
     (setf  *api-cleanup-list* (append  *api-cleanup-list* (list fun-name)))
     )))

(defun om-api-exit ()
  (mapc #'(lambda (x) (funcall x)) (reverse *api-cleanup-list*))
  t)


(let ((api-files '(
                   "system"
                   "files"
                   "processes"
                   "scheduler"
                   "tools"
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
                   "print"
                   "libraries"                 
                   "om-special"  
                   )
                 ))
  (mapc #'(lambda (filename) 
            (compile&load (make-pathname :directory *api-directory* :name filename))) 
        api-files)
  )






