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
; FILE UTILITIES
;===========================================================================

(in-package :om-api)

(export '(
          om-compiled-type
          om-make-pathname
          om-pathname-location
          om-directory-pathname-p
          om-directory
          
          om-lisp-image
          om-user-home
          om-user-pref-folder
          om-resources-folder

          om-create-file
          om-create-directory
          om-copy-file
          om-copy-directory
          om-delete-file
          om-delete-directory
          
          om-read-line
          om-clean-line
          om-stream-eof-p
          
          ) :om-api)

;;;==================
;;; PATHNAME MANAGEMENT (create, copy, delete...)
;;;==================

(defparameter *compiled-type* (pathname-type *load-pathname*))
(setq *compiled-type* (pathname-type (cl-user::compile-file-pathname "")))

;;; NOT WORK IN STANDALONE !
(defun om-compiled-type () 
  (if (om-standalone-p) 
      *compiled-type*
    (pathname-type (cl-user::compile-file-pathname ""))))

;; handles device, host, etc.
;; dircetory is a pathname
(defun om-make-pathname (&key directory name type host device)
  (make-pathname #+win32 :host #+win32 (or host (and directory (pathnamep directory) (pathname-host directory)))
                 :device (or device (and directory (pathnamep directory) (pathname-device directory)))
                 :directory (if (or (stringp directory) (pathnamep directory)) (pathname-directory directory) directory)
                 :name name :type type))

(defun om-pathname-location (path)
  (lw::pathname-location path))

(defun om-directory-pathname-p (p)
  (system::directory-pathname-p p))

(defun om-directory (path &key (type nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil) (recursive nil))
  (when path
    (let ((rep (directory (namestring path) :link-transparency resolve-aliases)))
      (when (not files)
        (setf rep (remove-if-not 'om-directory-pathname-p rep)))
      (when (not directories)
        (setf rep (remove-if 'om-directory-pathname-p rep)))
      (when (not hidden-files)
        (setf rep (remove-if #'(lambda (item) (or (and (om-directory-pathname-p item) 
                                                       (string-equal (subseq (car (last (pathname-directory item))) 0 1) "."))
                                                  (and (stringp (pathname-name item)) (> (length (pathname-name item)) 0)
                                                       (string-equal (subseq (pathname-name item) 0 1) "."))))
                             rep)))
      (when type
        (cond ((stringp type)
               (setf rep (loop for item in rep when (or (om-directory-pathname-p item) (string-equal (pathname-type item) type)) collect item)))
              ((consp type)
               (setf rep (loop for item in rep when (or (om-directory-pathname-p item) 
                                                        (member (pathname-type item) type :test 'string-equal)) collect item)))
              (t nil)))
      (if recursive
          (append rep 
                  (loop for dir in (om-directory path :directories t :files nil :hidden-files hidden-files :recursive nil)
                        append (om-directory dir :type type :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files 
                                             :recursive t))
                  )
        rep))))

;;;==================
;;; SPECIAL LOCATIONS
;;;==================

(defun om-lisp-image ()
  (lw::lisp-image-name)) 

(defun om-user-home ()
 (USER-HOMEDIR-PATHNAME))

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
  #-macosx 
  (make-pathname
   :host (pathname-host (oa::om-root-folder))
   :device (pathname-device (oa::om-root-folder)) 
   :directory (append (pathname-directory (oa::om-root-folder)) '("resources")))
  )
   

(defun om-user-pref-folder ()
  (let* ((userhome (om-user-home)))
    (make-pathname
     :host (pathname-host userhome)
     :device (pathname-device userhome)
     :directory 
     #+cocoa(append (pathname-directory userhome) (list "Library" "Preferences"))
     #+win32(append (pathname-directory userhome) (list "Application Data"))
     #+linux(append (pathname-directory userhome) (list ".local" "share"))
     )))

;;;==================
;;; FILE/FOLDER MANAGEMENT (create, copy, delete...)
;;;==================
(defun om-create-file (pathname)
  (with-open-file (file pathname :direction :output :if-does-not-exist :create)))

(defun om-create-directory (pathname &key (if-exists nil))
   (lw::ENSURE-DIRECTORIES-EXIST pathname)
   pathname)

(defun om-copy-file (sourcepath targetpath &key (if-exists :supersede))
  (handler-bind 
      ((error #'(lambda (err)
                  (capi::display-message "An error of type ~a occurred: ~a" (type-of err) (format nil "~A" err))
                  (abort err))))
    (when (probe-file targetpath)
      (delete-file targetpath))
    (system::copy-file sourcepath targetpath) ;;:if-exists if-exists
    targetpath
    ))

(defun om-copy-directory (sourcedir targetpath)
  (system::call-system (concatenate 'string "cp -R \"" (namestring sourcedir) "\" \"" 
                                    (namestring targetpath) "\"")))

(defun om-delete-file (name)
  (when (and name (probe-file name))
    (delete-file name :no-error)))

(defun om-delete-directory (path)
  (if (system::directory-pathname-p path)
      (let ((directories (om-directory  path :directories t :files t)))     
        (loop for item in directories do
              (om-delete-directory item))
        (delete-directory path :no-error)
        t)
    (delete-file path :no-error)))


;;;==================
;;; I/O STREAMS
;;;==================

(defun om-read-line (file)
  (read-line file nil 'eof))

; removes wrong characters at the end 
(defun om-clean-line (line) 
  (if (stringp line)
      (if (> (length line) 0)
          (let ((lastchar (elt line (- (length line) 1))))
            (if (or (equal lastchar #\Space)
                    (equal lastchar #\LineFeed))
                (om-clean-line (subseq line 0 (- (length line) 1)))
              line))
        line)
    line))
     
(defun om-stream-eof-p (s)
  ;(stream::stream-check-eof-no-hang s)
  (let ((c (read-char s nil 'eof)))
    (unless (equal c 'eof) (unread-char c s))
    (equal c 'eof)))



