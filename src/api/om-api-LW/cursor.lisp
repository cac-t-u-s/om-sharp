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

;====================================================================
; CURSORS
;====================================================================

(export '(om-make-cursor
          om-add-cursor
          om-get-cursor
          om-set-cursor-location
          om-view-cursor
          om-set-view-cursor
          ) :om-api)

(in-package :oa)

(defparameter *om-cursor-folder* nil)
(defparameter *om-cursor-type* #-win32 "tif" #+win32 "cur")
(defparameter *om-cursors* nil)

;;; default cursor location to be set by main application
(defun om-set-cursor-location (path)
  (setf *om-cursor-folder* path))

;;; registers a new cursor
;;; if cursor is a path or file name the cursor is not created yet 
(defun om-add-cursor (id cursor &optional (click-pos (om-make-point 0 0)))
  (let ((p (position id *om-cursors* :key 'car))
        (curs (if (or (stringp cursor) (pathnamep cursor)) (list cursor click-pos) cursor)))
    (if p (setf (cadr (nth p *om-cursors*)) curs)
      (push (list id curs) *om-cursors*))))

;;; returns the cursor indexed by "id"
(defun om-get-cursor (id)
  (let ((curs (find id *om-cursors* :key 'car)))
    (when (and curs (listp (cadr curs)))
      (setf (cadr curs)
            (om-make-cursor (car (cadr curs)) (cadr (cadr curs)))))
    (cadr curs)))
               
;;; Creates a system cursor from name
;;; if name is a pathname, it must point to a valid cursor file 
;;; (type can be omitted and will be replaced by the adequate type) 
;;; if name is a string it must the name of a cursor file in *om-cursor-folder*
(defun om-make-cursor (name &optional (click-pos (om-make-point 0 0)))
  (let ((cursorpath (if (pathnamep name) 
                        (probe-file (if (pathname-type name) name
                                      (om-make-pathname :directory name :name (pathname-name name) :type *om-cursor-type*)))
                      (if (and (stringp name) *om-cursor-folder* (probe-file *om-cursor-folder*))
                          (om-make-pathname :name name :directory *om-cursor-folder* :type *om-cursor-type*))))
        (cursor nil))
    (if (and cursorpath (probe-file cursorpath))
        (setf cursor (capi::load-cursor (list (list :cocoa cursorpath :x-hot (om-point-x click-pos) :y-hot (om-point-y click-pos))
                                              (list :win32 cursorpath))))
      (print (format nil "Cursor file: ~S not found!" name)))
    cursor)
  )


#+win32(setf win32::*change-cursor-on-gc* nil)

;;;================
;;; VIEW CURSORS
;;;================


;;; version SETF   (LW style)
(defmethod om-set-view-cursor ((self om-graphic-object) cursor) 
  (setf (capi::simple-pane-cursor (om-get-view self)) cursor))

;;; version CALLBACK   (MCL style)
(defmethod om-view-cursor ((self om-graphic-object)) nil)

(defmethod update-view-cursor ((self om-view) pos)
  (setf (capi::simple-pane-cursor self) (om-view-cursor self)))

(defmethod update-view-cursor ((self om-item-view) pos)
  (when (item-container self)
    (setf (capi::simple-pane-cursor (item-container self)) (om-view-cursor self))))





