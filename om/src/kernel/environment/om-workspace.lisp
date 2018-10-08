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
; WORKSPACE
;=========================================================================

(in-package :om)

(defvar *current-workspace* nil "The WorkSpace used in the current session.")

(defclass OMWorkSpace (OMPersistantFolder) 
  ((om-package :initform nil :accessor om-packages :documentation "OM in-built package containing functions and classes")
   (user-package :initform nil :accessor user-packages :documentation "User package containing user-defined functions and classes")
   (lib-package :initform nil :accessor lib-package :documentation "Package containing all registered libraries")
   (globals-package :initform nil :accessor globals-package :documentation "Special package containing all global variables")
   (ws-window :initform nil :accessor ws-window :documentation "The main workspace window")
   (preferences :initform nil :accessor preferences :documentation "All preferences associated to this workspace"))
  (:documentation "The class of the OM workspace.
A workspace contains all the elements of an OM session (documents, pointer to libraries, preferences, etc.)"))

;;; NOT USED
; At the difference of OMPersistantFolder the WS elements are not in the foler pathname, but in "pathname/elements/"
(defmethod elements-pathname ((self OMWorkspace)) 
   (make-pathname 
    :device (pathname-device (mypathname self)) :device (pathname-host (mypathname self))
    :directory  (append (pathname-directory (mypathname self)) (list "elements"))))

(defclass OMGlobalsFolder (OMPersistantFolder) ()
  (:documentation "Special folder containing globals variables."))

(defmethod save-patch-list ((self OMWorkspace))
  (mapcar #'(lambda (elt)
              (or (and (mypathname elt)
                       (namestring (mypathname elt)))
                  (name elt)))
          (elements self)))


;;; temp
(defvar *skip-libs* nil)
(defvar *error-files* nil)

(defmethod load-ws-contents ((self OMWorkspace) list)
  (let ((elements nil)
        (skip-libs *skip-libs*))
    (print (string+ "Loading workspace: " (name self) " ..."))
    ;(setf (elements self) (remove nil (mapcar #'(lambda (x) (ws-load-element x (incf j))) elements) :test 'equal))
    (setf elements
        (loop for file in (find-values-in-prop-list list :patches) collect
              (make-instance 'ompatch :mypathname file
                             :name (pathname-name file))))  
    (setf *skip-libs* skip-libs)
     (when *error-files* 
       (print "==============================================")
       (print "Some files could not be loaded in the workspace (see documentation window).")
       (om-lisp::om-show-output-lines (append (list "THE FOLLOWING FILES COULD NOT BE LOADED IN THE WORKSPACE:") *error-files*)))
     (print (string+ "Workspace " (name self) " loaded."))
     elements))

;============================
; HANDLES DISPLAY etc.
;============================

(defclass workspace-editor (OMEditor)  
  ((elements-view-filter :accessor elements-view-filter :initform :all)
   (elements-view-sort :accessor elements-view-sort :initform :name)
   (elements-view-mode :accessor elements-view-mode :initform :name) 
   ))

;(add-preference-module :workspace "Workspace")
;(add-preference :workspace :show-types "Show Types" :bool t)
;(add-preference :workspace :show-dates "Show modification date" :bool t)
;(add-preference :workspace :auto-import nil :bool t)
;(add-preference :workspace :ask-import nil :bool t)

;============================
; WS INIT / LOADING
;============================

; (start-workspace)

;;; CALLED AT OM STARTUP: 
;;; SELECT AND LOAD A WORKSPACE SESSION
(defun start-workspace ()
  (let ((initpath (catch :cancel (choose-user-ws-folder))))
    (cond ((equal 'no initpath)
           (start-without-ws-file))

          ((not (pathnamep initpath))
           ;;; Try again
           (start-workspace))

          ((probe-file initpath)
           ;;; Existing Workspace         
           (if (file-type-p initpath "omws")
               ;;; Start WS
               (start-from-ws-file initpath)
             ;;; An error occured somewhere: Restart
             (progn 
               (om-message-dialog (str-check "Please start again from an OM workspace file (.omws)"))
               (start-workspace))
             ))

          (t ;;; file do not exist (yet) 
             ;;; New Workspace
             (let ((name (car (last (pathname-directory initpath)))))
               (if name
                   (progn
                     (om-message-dialog 
                      (str-check 
                       (format nil 
                               "A new folder will be created containing your workspace preferences and data.~%~%Use the main workspace file \"~A.omws\" to reload the workspace at the next session." 
                               name)))
                     (let ((wsfile (create-new-workspace initpath)))
                       (start-from-ws-file wsfile)))
                 (progn (om-message-dialog (str-check "Error in creating new workspace folder. Please try again..."))
                   (start-workspace))
                 )))
          )))

;;; Check if <path> is a valid workspace folder
(defun check-ws-folder (path)
  (let ((ws-contents (om-directory (om-make-pathname :directory path))) 
        dirs files)
    (loop for item in ws-contents do
          (if (om-directory-pathname-p item)
              (push (name-of-directory item) dirs)
            (push item files)))
    (and (find "files" dirs :test 'string-equal) 
         (find "elements" dirs :test 'string-equal)
         (find "omws" files :test 'string-equal :key 'pathname-type))))
  
(defun choose-user-ws-folder ()
  (let* ((prev (get-om-pref :prev-ws))
         (choix (ws-dialog prev))
         (search-folder (if prev 
                            (om-make-pathname :device (pathname-device prev) :directory (butlast (pathname-directory prev))) 
                          (om-user-home))))
    (cond
     ((equal 'new choix)
      (let ((ws (om-choose-new-directory-dialog :prompt "Choose a location and name for the new workspace" :directory search-folder)))
        (if (pathnamep ws) ws (choose-user-ws-folder))
        ))
     ((equal 'existing choix)
      (let ((ws (om-choose-file-dialog :prompt "Please select an existing workspace project" :directory search-folder :types '("OM Workspace" "*.omws"))))
        (if (and (pathnamep ws) 
                 (or (check-ws-folder ws) 
                     (om-y-or-n-dialog (format nil "Are you sure this is a valid OM workspace folder?~%~% The selected workspace file and location do not look like a standard workspace.~%~% Click 'Yes' to start anyway, or 'No' to choose another workspace."))))
                 ws (choose-user-ws-folder))
        ))
     ((equal 'previous choix) prev)
     ((equal 'no choix) choix)
     ((equal 'quit choix) (om-quit))
     (t nil))
    ))


(defun create-ws-main-ws-file (dirpath)
  (WITH-OPEN-FILE (out (om-make-pathname :directory dirpath :name  (name-of-directory dirpath) :type "omws")
                       :direction :output 
                       :if-does-not-exist :create :if-exists :supersede) 
    (print `(:info (:om-version ,*om-version* :saved ,(om-get-date))) out)))


(defun create-new-workspace (path)
  (let ((wsfile (om-make-pathname :directory path :name (car (last (pathname-directory path))) :type "omws")))
    (om-create-directory (om-make-pathname :directory path) :if-exists nil)
    (create-ws-main-ws-file wsfile)
    (om-create-directory (om-make-pathname :directory (append (pathname-directory path) (list "elements"))) :if-exists nil)
    (om-create-directory (om-make-pathname :directory (append (pathname-directory path) (list "files"))) :if-exists nil)
    wsfile))
  


; (start-workspace)

(defun start-from-ws-file (pathname)
  (remember-previous-ws pathname)
  (setf *current-workspace* (make-instance 'OMWorkspace
                                           :name (pathname-name pathname)
                                           :mypathname pathname))

  
  (setf (editor *current-workspace*) (make-instance 'workspace-editor))

  ;;; load ws
  (catch :load-ws 
    (handler-bind 
        ((error #'(lambda (err)
                    (om-message-dialog (format nil "Warning: An error occurred while loading the workspace.~%=> ~A" err))
                    (throw :load-ws :err))))
      (load-ws-from-file *current-workspace* pathname)
      ))
  
  ;(libs-autoload)

  (show-main-om-window)
  (save-workspace-file *current-workspace*)
  )


(defvar *libs-auto-load* nil)

(defun libs-autoload ()
  (when *libs-auto-load* 
    (mapc #'(lambda (lib) 
              (let* ((libname (lib-true-name (car lib)))
                     (omlib (exist-lib-p libname)))
                (unless omlib
                  (if (and (pathnamep (cadr lib)) (probe-file (cadr lib)))
                      (add-one-lib (cadr lib) nil)
                    (om-message-dialog (string+ "Library " (car lib) " not found."))))
                (when (or omlib (setf omlib (exist-lib-p (car lib))))
                  (load-om-library omlib))))
          *libs-auto-load*)
    ))


; (start-workspace)

(defmethod load-ws-from-file ((ws OMWorkspace) pathname)
  (when (probe-file pathname)
    (let* ((loaded-list (list-from-file pathname))
           (saved-prefs (find-values-in-prop-list loaded-list :user-preferences))
           (ws-contents (find-values-in-prop-list loaded-list :contents)))
      ;;; default prefs may have changed given the new context (with workspace)
      (restore-default-preferences)
      ;;; load and apply new prefs from workspace
      (load-saved-prefs saved-prefs)
      ;;; load ws elements
      (setf (elements ws) (load-ws-contents ws ws-contents))
      )))

; (find-values-in-prop-list (list-from-file (mypathname *current-workSpace*)) :contents)

(defmethod save-workspace-file ((ws OMWorkspace))
  (let ((pathtemp (make-pathname :directory (pathname-directory (mypathname ws)) :name (name ws) :type "omws.tmp"))
        (path (make-pathname :directory (pathname-directory (mypathname ws)) :name (name ws) :type "omws")))
    (catch :save-ws 
      (handler-bind 
          ((error #'(lambda (err)
                      (om-message-dialog (format nil "Warning: An error occurred while saving the workspace.~%=> ~A" err))
                      (throw :save-ws :err))))
        (with-open-file (out pathtemp :direction :output 
                             :if-does-not-exist :create :if-exists :supersede)
          (let ((*print-pretty* t))
            (print `(:info (:om-version ,*om-version*) (:saved ,(om-get-date))) out)
            (print `(:user-preferences 
                     .,(mapcar #'(lambda (item) 
                                   (save-pref-module (car item)))
                               *user-preferences*)) out)
            (print `(:contents (:patches .,(save-patch-list *current-workspace*))) out)
            ))
        (rename-file pathtemp path)
        ))
    path))

; (save-workspace-file *current-workspace*)



        




