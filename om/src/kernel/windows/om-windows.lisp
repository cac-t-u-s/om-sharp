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

; General windows management

(in-package :om)

;;;===============================
;;; GENERAL MENU AND COMMANDS
;;;===============================

(defmethod save-as-menu-name ((self t)) "Save as...")

;;; SELF = editor (in general...)
(defun default-file-menu-items (self)
  (list (om-make-menu "New..." 
                      (list (om-make-menu-comp 
                             (list
                              (om-make-menu-item "New Patch" #'(lambda () (open-new-document :patch)) :key "n")
                              (om-make-menu-item "New Maquette" #'(lambda () (open-new-document :maquette)))
                              (om-make-menu-item "New Lisp function" #'(lambda () (open-new-document :lispfun)))))
                            (om-make-menu-item "New Text/Lisp Buffer" #'(lambda () (om-lisp::om-open-text-editor :lisp t)) :key "N")
                            ))
        (om-make-menu-item "Open..." #'(lambda () (funcall (open-command self))) :key "o" :enabled (and (open-command self) t))
        (om-make-menu "Open Recent..."
                      #'(lambda (item)
                          (mapcar #'(lambda (file) 
                                      (om-make-menu-item (namestring file) #'(lambda () (open-om-document file))))
                                  *om-recent-files*)))
        (om-make-menu-comp 
         (list (om-make-menu-item "Save" #'(lambda () (funcall (save-command self))) :key "s" :enabled (and (save-command self) t))
               (om-make-menu-item (save-as-menu-name self) 
                                  #'(lambda () (funcall (save-as-command self))) :key "S" 
                                  :enabled (and (save-as-command self) t))
               (om-make-menu-item "Revert (Last Saved)" #'(lambda () (funcall (revert-command self))) :enabled #'(lambda () (and (revert-command self) t)))
               (om-make-menu-item "Close" #'(lambda () (funcall (close-command self))) :key "w" :enabled (and (close-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Print" #'(lambda () (funcall (print-command self))) :key "p" :enabled (and (print-command self) t))))))

(defun default-edit-menu-items (self)
  (list (om-make-menu-comp 
         (list (om-make-menu-item "Undo" #'(lambda () (funcall (undo-command self))) :key "z" :enabled #'(lambda () (and (undo-command self) t)))
               (om-make-menu-item "Redo" #'(lambda () (funcall (redo-command self))) :key "Z" :enabled #'(lambda () (and (redo-command self) t)))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Copy" #'(lambda () (funcall (copy-command self))) :key "c" :enabled (and (copy-command self) t))
               (om-make-menu-item "Cut" #'(lambda () (funcall (cut-command self))) :key "x" :enabled (and (cut-command self) t))
               (om-make-menu-item "Paste"#'(lambda () (funcall (paste-command self))) :key "v" :enabled (and (paste-command self) t))
               (om-make-menu-item "Delete" #'(lambda () (funcall (clear-command self))) :enabled (and (clear-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Select All" #'(lambda () (funcall (select-all-command self))) :key "a" :enabled (and (select-all-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu 
                "Font" 
                (list
                 (om-make-menu-comp 
                  (list (om-make-menu-item "Bold" #'(lambda () (funcall (font-bold-command self))) :key "B" :enabled (and (font-bold-command self) t))
                        (om-make-menu-item "Italics" #'(lambda () (funcall (font-italics-command self))) :key "I" :enabled (and (font-italics-command self) t))))
                 (om-make-menu-item "Open Fonts..." #'(lambda () (funcall (font-command self))) :key "T" :enabled (and (font-command self) t)))
                :enabled (and (or (font-bold-command self) (font-italics-command self) (font-command self)) t))))
        ))

(defun default-windows-menu-items (self)
  (append 
   (unless (om-standalone-p) 
     (list (om-make-menu-item "Preferences" 'show-preferences-win :key ",")
           ;(om-make-menu-item "MIDI Setup" 'show-midi-setup)
           ))
   (list 
    (om-make-menu-comp  
     (list 
      (om-make-menu-item "Session Window" 'show-main-om-window :key "W")
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

(defmethod get-selection-for-menu ((self t)) nil)

(defun default-help-menu-items (self)
  (list
   
   (om-make-menu-comp  
    (list 
     
     (om-make-menu-item  
      "Find source..." 
      #'(lambda () 
          (let ((symbols (get-selection-for-menu self)))
            (if symbols

                (loop for ref in symbols do (om-lisp::om-edit-definition ref))
              
              (let ((str (om-get-user-string "Enter a Lisp function or class name to search.")))
                (when str 
                  (let ((symbol (read-from-string str)))
                    (om-lisp::om-edit-definition symbol))))
              )
            ))
      :key "E")
     
     (om-make-menu-item  
      "Function/Class Reference" 
      #'(lambda () 
          (let ((symbols (get-selection-for-menu self)))
            (if symbols 
                (loop for ref in symbols do (show-reference-page ref))
              (om-open-in-browser (namestring (get-om-reference-pages-index))))))
      :key "d")
     ))
   
    (om-make-menu-comp  
     (list 
      (om-make-menu-item 
       "Editor Help..." 
       #'(lambda () (funcall (help-command self))) 
       :key "H" :enabled (and (help-command self) t))
        
      (om-make-menu-item 
       "Online Resources" 
       #'(lambda() (om-open-in-browser "https://openmusic-project.github.io/")) 
       :enabled t)
      ))
    
    ))


(defun main-app-menu-item ()
  #-cocoa(when (om-standalone-p) 
           (om-make-menu (string+ "OM " *version-string*)
                         (list (om-make-menu-comp 
                                (list (om-make-menu-item "About OM..." 'show-about-win)
                                      (om-make-menu-item "Preferences" 'show-preferences-win)))
                               (om-make-menu-item "Quit" 'om-quit)))))

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
(defmethod save-command (self) nil)
(defmethod save-as-command (self) nil)
(defmethod revert-command (self) nil)

(defmethod copy-command (self) nil)
(defmethod cut-command (self) nil)
(defmethod paste-command (self) nil)
(defmethod clear-command (self) nil)

;;; always work
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
(defmethod om-lisp::om-listener-window-menus ((self om-lisp::om-listener))
  (om-menu-items self))

(add-preference :general :listener-on-top "Keep Listener in Front" :bool nil "Does not apply to the current Listener window" 'restart-listener)
(add-preference :general :listener-input "Enable Listener Input" :bool nil "Allows you to type Lisp commands in the Listener window" 'restart-listener)

(defun restart-listener ()
  (let ((listenerwin (get-listener)))
    (when listenerwin 
      (let ((ig (capi::interface-geometry listenerwin)))
        (om-close-window listenerwin)
        (om-lisp::om-make-listener :x (car ig) :y (cadr ig) :width (caddr ig) :height (cadddr ig)
                                   :input (get-pref-value :general :listener-input)
                                   :on-top (get-pref-value :general :listener-on-top)
                                   :initial-lambda #'(lambda () (in-package :om))
                                   )))))
    

(defun get-listener () (car (om-get-all-windows 'om-lisp::om-listener)))

(defun show-listener-win ()
  
  (let ((listenerwin (get-listener)))
    
    (om-lisp::om-set-listener-font (get-pref-value :general :listener-font))
    (om-lisp::om-set-text-editor-font (get-pref-value :general :textedit-font))
    
    (if listenerwin
        (om-select-window listenerwin)
      (om-lisp::om-make-listener 
       :initial-lambda #'(lambda () (in-package :om))
       :initial-prompt *om-startup-string*
       :height 200 
       :input (get-pref-value :general :listener-input)
       :on-top (get-pref-value :general :listener-on-top)
       ))))

      

;(add-preference-section :appearance "Lisp")
(add-preference :general :listener-font "Listener font" :font om-lisp::*listener-font* nil 'set-listener-font)
(add-preference :general :textedit-font "Text editors font" :font om-lisp::*text-editor-font* nil 'set-text-editor-font)

(defun set-listener-font ()
  (om-lisp::om-set-listener-font (get-pref-value :general :listener-font)))

(defun set-text-editor-font ()
  (om-lisp::om-set-text-editor-font (get-pref-value :general :textedit-font)))



(defmethod copy-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-copy window)))

(defmethod cut-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-cut window)))

(defmethod paste-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-paste window)))

(defmethod select-all-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-select-all window)))


#|
(defmethod font-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::change-listener-font window)))

(defmethod close-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-close window)))
|#

(defun show-shell () (om-lisp::om-open-shell)) 



;=======================
; Print OM messages
;=======================
 
(defun om-beep-msg (format-string &rest args)
   (om-beep)
   (om-print (apply 'format (append (list nil format-string) args)) "[!!]")
   NIL)


(add-preference :general :debug "Debug mode" :bool nil)

(defun om-print-dbg (str &optional args prompt)  
  (when (get-pref-value :general :debug)
    (om-print-format str args (or prompt "DEBUG"))))


;;;===============================
;;; WORKSPACE
;;;===============================                            
;;; defined in om-main-window.lisp 
;;; (defun show-main-om-window () nil)

;;;===============================
;;; ABOUT
;;;===============================
                                
(defun show-about-win () nil)

;;;===============================
;;; PREFERENCES
;;;===============================
;;; defined in om-preferences-window.lisp 
;;; (defun show-preferences-win () nil)                          






