;;;====================================================================
;;;CURSORS
;;;====================================================================

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

;;; init built-in cursors
;;; the names specified as string are supposed to be installed in the cursor-folder when they will be needed
(defun om-init-cursors ()
  (om-add-cursor :wait 
                 #+cocoa "wait-cursor" #-cocoa :busy
                 (om-make-point 8 8))
  (om-add-cursor :arrow nil) 
  (om-add-cursor :h-size #+cocoa "h-resize-cursor" #-cocoa  :h-double-arrow
                 (om-make-point 8 8))  
  (om-add-cursor :v-size #+cocoa "v-resize-cursor" #-cocoa  :v-double-arrow
                 (om-make-point 8 8))
  (om-add-cursor :resize 
                 #+cocoa "resize-cursor" #-cocoa :bottom-right-corner
                 (om-make-point 8 8))
  (om-add-cursor :i-beam :i-beam)
  (om-add-cursor :cross 
                 #+cocoa "croix" #-cocoa :fleur
                 (om-make-point 8 8))
  (om-add-cursor :hand 
                 #+cocoa :open-hand #-cocoa "hand-cursor"
                 ))

#+win32(setf win32::*change-cursor-on-gc* nil)

(om-init-cursors)


;;;================
;;; VIEW CURSORS
;;;================

;;; version CALLBACK   (MCL style)
(defmethod om-view-cursor ((self om-graphic-object)) nil)

;;; version SETF   (LW style)
(defmethod om-set-view-cursor ((self om-graphic-object) cursor) 
  (setf (capi::simple-pane-cursor (om-get-view self)) cursor))


(defmethod update-view-cursor ((self om-view) pos)
  (setf (capi::simple-pane-cursor self) (om-view-cursor self)))

(defmethod update-view-cursor ((self om-item-view) pos)
  (when (item-container self)
    (setf (capi::simple-pane-cursor (item-container self)) (om-view-cursor self))))





