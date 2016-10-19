; General windows management


(in-package :om)

;;;===============================
;;; GENERAL MENU AND COMMANDS
;;;===============================

;;; SELF = editor (in general...)
(defun default-file-menu-items (self)
  (list (om-make-menu "New..." 
                      (list (om-make-menu-comp 
                             (list
                              (om-make-menu-item "New Patch" #'(lambda () (open-new-document :patch)) :key "n")
                              (om-make-menu-item "New Maquette" #'(lambda () (open-new-document :maquette)) :key "m")
                              (om-make-menu-item "New Lisp function" #'(lambda () (open-new-document :lispfun)))))
                            (om-make-menu-item "New Text/Lisp Buffer" #'(lambda () (om-lisp::om-open-text-editor)) :key "N")
                            ))
        (om-make-menu-item "Open..." #'(lambda () (funcall (open-command self))) :key "o" :enabled (and (open-command self) t))
        (om-make-menu-comp 
         (list (om-make-menu-item "Save" #'(lambda () (funcall (save-command self))) :key "s" :enabled (and (save-command self) t))
               (om-make-menu-item "Externalize..." #'(lambda () (funcall (save-as-command self))) :key "S" :enabled (and (save-as-command self) t))
               (om-make-menu-item "Revert (Last Saved)" #'(lambda () (funcall (revert-command self))) :enabled #'(lambda () (and (revert-command self) t)))
               (om-make-menu-item "Close" #'(lambda () (funcall (close-command self))) :key "w" :enabled (and (close-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Print" #'(lambda () (funcall (print-command self))) :key "p" :enabled (and (print-command self) t))))))

(defun default-edit-menu-items (self)
  (list (om-make-menu-comp 
         (list (om-make-menu-item "Undo" #'(lambda () (funcall (undo-command self))) :key "z" :enabled (and (undo-command self) t))
               (om-make-menu-item "Redo" #'(lambda () (funcall (redo-command self))) :key "Z" :enabled (and (redo-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Copy" #'(lambda () (funcall (copy-command self))) :key "c" :enabled (and (copy-command self) t))
               (om-make-menu-item "Cut" #'(lambda () (funcall (cut-command self))) :key "x" :enabled (and (cut-command self) t))
               (om-make-menu-item "Paste"#'(lambda () (funcall (paste-command self))) :key "v" :enabled (and (paste-command self) t))
               (om-make-menu-item "Delete" #'(lambda () (funcall (clear-command self))) :enabled (and (clear-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Select All" #'(lambda () (funcall (select-all-command self))) :key "a" :enabled (and (select-all-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu "Font" (list
                                     (om-make-menu-comp 
                                      (list (om-make-menu-item "Bold" #'(lambda () (funcall (font-bold-command self))) :key "B" :enabled (and (font-bold-command self) t))
                                            (om-make-menu-item "Italics" #'(lambda () (funcall (font-italics-command self))) :key "I" :enabled (and (font-italics-command self) t))))
                                     (om-make-menu-item "Open Fonts..." #'(lambda () (funcall (font-command self))) :key "T" :enabled (and (font-command self) t)))
                             :enabled (and (or (font-bold-command self) (font-italics-command self) (font-command self)) t))))
        ))

(defun default-windows-menu-items (self)
  (append 
   (unless (om-standalone-p) 
     (list (om-make-menu-item "Preferences" 'show-preferences-win)
           (om-make-menu-item "MIDI Setup" 'show-midi-setup)))
   (list 
    (om-make-menu-comp 
     (list (om-make-menu-item "Inspector" #'(lambda () (funcall (get-info-command self))) :key "i" 
                              :enabled (and (get-info-command self) t))))
    (om-make-menu-comp  
     (list 
      (om-make-menu-item "Workspace/Library Window" 'show-main-om-window :key "W")
      (om-make-menu-item "Lisp Listener" 'show-listener-win :key "L")
      (om-make-menu-item "Shell" 'show-shell)
      ))
    (om-make-menu-comp 
     #'(lambda (w) 
         (mapcar #'(lambda (w) 
                     (om-make-menu-item (om-window-title w)
                                        #'(lambda () (om-select-window w))
                                        :enabled #'(lambda () (not (equal w (om-front-window))))))
                 (remove 'workspace-window (append (om-get-all-windows 'om-window)
                                                   (om-get-all-windows 'om-lisp::om-text-editor-window))
                         :key 'type-of))
         ))))
  )

(defun default-help-menu-items (self)
  (list
   (om-make-menu-item "Editor Commands..." #'(lambda () (funcall (help-command self))) :key "H" :enabled (and (help-command self) t))
   (om-make-menu-comp  
    (list 
     (om-make-menu-item "Online User Manual" nil :enabled nil)
     (om-make-menu-item "OM Function Reference" nil :enabled nil)
     ))
   ))

(defun main-app-menu-item ()
  #-cocoa(when (om-standalone-p) 
           (om-make-menu (string+ "OM " *version-string*)
                         (list (om-make-menu-comp 
                                (list (om-make-menu-item "About OM..." 'show-about-win)
                                      (om-make-menu-item "Preferences" 'show-preferences-win)))
                               (om-make-menu-item "Quit" 'om-confirmed-quit)))))

;;; self = the OMEditor or anything (e.g. om-listener, etc.)
(defmethod om-menu-items ((self t))
  (remove nil
          (list 
           (main-app-menu-item)
           (om-make-menu "File" (default-file-menu-items self))
           (om-make-menu "Edit" (default-edit-menu-items self))
           (om-make-menu "Windows" (default-windows-menu-items self))
           (om-make-menu "Help" (default-help-menu-items self))
           )))

;;; General commands (to be implemented by the different editors)
;;; must return a function to call 
;;; I they are not implemented (or return NIL) the menu items are disabled 
(defmethod help-command (self) nil)
(defmethod font-command (self) nil)
(defmethod font-bold-command (self) nil)
(defmethod font-italics-command (self) nil)


(defmethod select-all-command (self) nil)
(defmethod undo-command (self) nil)
(defmethod redo-command (self) nil)
(defmethod print-command (self) nil)
(defmethod get-info-command (self) nil)
(defmethod save-command (self) nil)
(defmethod save-as-command (self) nil)
(defmethod revert-command (self) nil)

(defmethod copy-command (self) nil)
(defmethod cut-command (self) nil)
(defmethod paste-command (self) nil)
(defmethod clear-command (self) nil)

(defmethod close-command (self) 
  #'(lambda () (om-close-window (om-front-window))))

(defmethod open-command (self) 
  #'(lambda () (open-om-document)))


;;;===============================
;;; TEXT WINDOWS
;;; Exist for 
;;; - Lisp functions
;;; - TEXTBUFFER objects
;;; - Free Text / Lisp editing 
;;; uses om-text-editor-window from the OM-LISP package
;;;===============================

;;; will also respond to the 'standard' OM-API window title method calls
(defmethod om-set-window-title ((self om-lisp::om-text-editor-window) (title string))
  (om-lisp::om-text-editor-window-set-title self title))
(defmethod om-window-title ((self om-lisp::om-text-editor-window))
  (om-lisp::om-text-editor-window-title self))

;;;===============================
;;; LISTENER
;;;===============================

; (show-listener-win)

;;; redefine the menu-bar of the listener
(defmethod om-lisp::om-listener-window-menus ((self om-listener))
  (om::om-menu-items nil))

(defun show-listener-win ()
  (let ((listenerwin (car (om-get-all-windows 'om-listener))))
    (if listenerwin (om-select-window listenerwin)
      (om-make-listener :title "OM Listener" 
                       ;:initial-lambda #'(lambda () (in-package :om-user))
                        :initial-prompt *om-startup-string*
                        :height 200))))

(defmethod font-command ((window om-listener))
  #'(lambda () (om-lisp::change-listener-font window)))

(defmethod copy-command ((window om-listener))
  #'(lambda () (om-lisp::listener-copy window)))

(defmethod cut-command ((window om-listener))
  #'(lambda () (om-lisp::listener-cut window)))

(defmethod paste-command ((window om-listener))
  #'(lambda () (om-lisp::listener-paste window)))

(defmethod select-all-command ((window om-listener))
  #'(lambda () (om-lisp::listener-select-all window)))

(defmethod close-command ((window om-listener))
  #'(lambda () (om-lisp::listener-close window)))
  

(defun show-shell () (om-open-shell)) 


;;;===============================
;;; WORKSPACE
;;;===============================
                               
;;; redef in main-window.lisp 
(defun show-main-om-window () nil)

;;;===============================
;;; ABOUT
;;;===============================
                                
(defun show-about-win () nil)

;;;===============================
;;; PREFERENCES
;;;===============================
                                
(defvar *pref-window* nil)

(defun show-preferences-win ()
   (if (and *pref-window* (om-window-open-p *pref-window*))
     (om-select-window *pref-window*)
     (prog1 
         (setf *pref-window* (make-preferences-window))
       (om-open-window *pref-window*))))






