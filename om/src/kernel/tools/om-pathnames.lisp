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

;;;=======================
;;; Pathname utils for OM
;;;=======================

(in-package :om)

;;; check file exist + path = non-nil
(defun file-exist-p (path)
  (and path (probe-file path)))

(defun valid-pathname-p (path)
  (or (pathnamep path) (stringp path)))

;;; test if pathname is has extension <type>
(defun file-type-p (path type)
  (and (file-exist-p path)
       (pathname-type path) (stringp (pathname-type path))
       (string-equal (pathname-type path) type)))

;;; returns the name of the directory
(defun name-of-directory (path)
  (car (last (pathname-directory path))))

;;; creates a pathname relative to the current file
(defun om-relative-path (dirs file &optional (reference-path :current))
  (let ((ref (cond ((pathnamep reference-path) reference-path)
                   ((symbolp reference-path) 
                    (case reference-path 
                      (:workspace (if *current-workspace* (mypathname *current-workspace*)))
                      (:om (om-root-folder))
                      (:current *load-pathname*)
                      (otherwise (make-pathname :directory '(:absolute)))))
                   (t *load-pathname*)
                   )))
    (if ref
        (make-pathname
     :host (pathname-host ref) :device (pathname-device ref) 
     :directory (append (pathname-directory ref) dirs)
     :name file))))


;;; Check if a folder exist and create it if it does not
(defun check-folder (path)
  (unless (probe-file path) (om-create-directory path :if-exists nil))
  path)

(defun pathname-dir (pathname)
  (make-pathname :directory (pathname-directory pathname)
                 :host (pathname-host pathname) :device (pathname-device pathname)))


; (find-file-in-folder "Untitled" "/Users/bresson/Desktop/" :recursive t :return-all t)

(defun find-file-in-folder (name folder &key type recursive return-all)
  
  (let ((result nil)
        (search-folder (if (equal folder :local) 
                           (om-make-pathname :directory *load-pathname*)
                         folder)))
    
    (if return-all 
      
        (loop for elt in (om-directory search-folder :files t :directories recursive) append 
             
              (if (om-directory-pathname-p elt)

                  (find-file-in-folder name elt :type type :recursive recursive :return-all t)
                
                (when (and (pathname-name elt) 
                           (string-equal name (pathname-name elt))
                           (or (null type)
                               (and (pathname-type elt) 
                                    (string-equal type (pathname-type elt)))))
                  (list elt))))
      
      
      (let ((result nil))
     
        ;;; we search files first (lower-level)
        (loop for elt in (om-directory search-folder :files t :directories nil) 
              while (not result) do
              (when (and (pathname-name elt) 
                         (string-equal name (pathname-name elt))
                         (or (null type)
                             (and (pathname-type elt) 
                                  (string-equal type (pathname-type elt)))))
                (setf result elt)))
     
        ;;; then we search subfolders (if recursive)
        (when (and (not result) recursive)
          (loop for subfolder in (om-directory search-folder :files nil :directories t) 
                while (not result) do
                (setf result (find-file-in-folder name subfolder :type type :recursive t :return-all nil))))
     
        result)
   
      )))

;;;=========================
;;; RELATIVE PATHNAMES
;;;=========================
;(setf p1 #P"/Users/bresson/WORKSPACES/aaaa/elements/mk-examples.omp")
;(setf p2 #P"/Users/bresson/WORKSPACES/aaaa/elements/NewFolder/bouches/piece1.omp")
;(setf p3 #P"/Users/bresson/WORKSPACES/infiles/test.aif")
;(relative-pathname p3 p1)

(defun relative-pathname (path refpath)
  (let ((dirlist '(:relative))
        (refrest (cdr (pathname-directory refpath)))
        (dirrest (cdr (pathname-directory path))))
    (loop for path-dir in (cdr (pathname-directory path))
          for ref-dir in (cdr (pathname-directory refpath)) 
          while (string-equal path-dir ref-dir) do
          (setf refrest (cdr refrest)
                dirrest (cdr dirrest)))
    (loop for item in refrest do
          (setf dirlist (append dirlist (list ".."))))
    (loop for item in dirrest do
          (setf dirlist (append dirlist (list item))))
    (make-pathname :device (pathname-device refpath) :host (pathname-host refpath)
     :directory dirlist :name (pathname-name path) :type (pathname-type path))
    ))


(defvar *relative-path-reference* nil)

(defmacro with-relative-ref-path (path &body body)
  `(let ((current-relative-path *relative-path-reference*))
     (setq *relative-path-reference* ,path)
     (let ((rep (progn ,@body)))
       (setq *relative-path-reference* current-relative-path)
       rep)))

;(let ((*relative-path-reference* "/Users/bresson/WORKSPACES/mk-examples.omp"))
;  (restore-path "../../../../../../test/ist/ooo.omp"))


;;; seems like 'merge-pathnames' does pretty much the same job
(defmethod restore-path ((self pathname) refpath)
  (let ((dir (pathname-directory self)))
    (if refpath
        (cond ((and (equal :relative (car dir)) (cdr dir))
               (let ((updirs (or (position-if-not #'(lambda (item) (equal item :up)) (cdr dir)) 0)))
                 (make-pathname 
                  :device (pathname-device refpath) :host (pathname-host refpath)
                  :directory (append (list (car (pathname-directory refpath)))
                                     (butlast (cdr (pathname-directory refpath)) updirs) 
                                     (nthcdr updirs (cdr dir)))
                  :name (pathname-name self) :type (pathname-type self))))
              ((equal :absolute (car dir)) self)
              ((or (null dir) (null (cdr dir)))
               ;; could not restore pathname
               (make-pathname 
                :device (pathname-device refpath) :host (pathname-host refpath)
                :directory (pathname-directory refpath)
                :name (pathname-name self) :type (pathname-type self))))
      self)
     ))

(defmethod restore-path ((self string) refpath)
  (restore-path (pathname self) refpath))

(defmethod restore-path ((self t) refpath) nil)

;;;=========================
;;; EXTERNAL EXECs
;;;=========================

;;; a utility function to get the executable path from a .app on Mac
(defun real-exec-pathname (path)
  (let ((name (car (last (pathname-directory path)))))
    (if (and (om-directory-pathname-p path)
             (string-equal "app" (subseq name (- (length name) 3))))
        ;;; path is an application bundle
        (make-pathname :directory (append (pathname-directory path) (list "Contents" "MacOS"))
                       :name (subseq name 0 (- (length name) 4)))
      ;;; otherwise...
      path)))






