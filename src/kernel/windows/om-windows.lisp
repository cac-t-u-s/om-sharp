;============================================================================
; om#: visual programming language for computer-aided music composition
; J. Bresson et al., IRCAM (2013-2019)
; Based on OpenMusic (c) IRCAM / G. Assayag, C. Agon, J. Bresson
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
                          (declare (ignore item))
                          (mapcar #'(lambda (file) 
                                      (om-make-menu-item (namestring file) #'(lambda () (open-om-document file))))
                                  *om-recent-files*)))
        (om-make-menu-item "Open Folder..." #'(lambda () (funcall (open-folder-command self))) 
                           :key "O"
                           :enabled (and (open-command self) t))
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
         (list (om-make-menu-item "Undo" #'(lambda () (when (undo-command self) (funcall (undo-command self))))
                                  :key "z" :enabled #'(lambda () (and (undo-command self) t)))
               (om-make-menu-item "Redo" #'(lambda () (when (redo-command self) (funcall (redo-command self))))
                                  :key "Z" :enabled #'(lambda () (and (redo-command self) t)))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Copy" #'(lambda () (funcall (copy-command self))) :key "c" :enabled #'(lambda () (and (copy-command self) t)))
               (om-make-menu-item "Cut" #'(lambda () (funcall (cut-command self))) :key "x" :enabled #'(lambda () (and (cut-command self) t)))
               (om-make-menu-item "Paste"#'(lambda () (funcall (paste-command self))) :key "v" :enabled #'(lambda () (and (paste-command self) t)))
               (om-make-menu-item "Delete" #'(lambda () (funcall (clear-command self))) :enabled (and (clear-command self) t))))
        (om-make-menu-comp 
         (list (om-make-menu-item "Select All" #'(lambda () (funcall (select-all-command self))) :key "a" :enabled #'(lambda () (and (select-all-command self) t)))))
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
  
  (declare (ignore self))

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
     #'(lambda (win) 
         (declare (ignore win))
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
   
   (om-make-menu-item 
    "Online Documentation" 
    #'(lambda () (om-open-in-browser "https://cac-t-u-s.github.io/om-sharp/pages/index")) 
    :enabled t)
    
   
   (om-make-menu-item 
    "Print Editor Help [H]" 
    #'(lambda () (funcall (help-command self))) 
    :enabled (and (help-command self) t))
   
   (om-make-menu-item 
    "Print Documentation [D]" 
    #'(lambda () (funcall (help-command self))) 
    :enabled #'(lambda () (get-selection-for-menu self))
    )

   (om-make-menu-item  
    "Function / Class Reference" 
    #'(lambda () 
        (let ((symbols (get-selection-for-menu self)))
          (if symbols 
              (loop for ref in symbols do (show-reference-page ref))
            (om-open-in-browser (namestring (get-om-reference-pages-index))))))
    :key "d")
   
   (om-make-menu-item  
    "Find Source..." 
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
    "Box Help Patch..." 
    #'(lambda () 
        (let ((help-patches (remove nil (mapcar #'get-symbol-help-patch (get-selection-for-menu self)))))
          (if help-patches 
              (loop for patch in help-patches do 
                    (open-help-patch patch))
            (om-beep))
          ))
    :enabled #'(lambda () (get-selection-for-menu self))
    :key "H")
    
   (om-make-menu-comp  
    #'(lambda (win) 
        (declare (ignore win))
        (list 
         (om-make-menu
          "Help Patches..." 
          (append 
           (make-base-help-menu-items)  
           (list 
            (om-make-menu-comp 
             #'(lambda (win) 
                 (declare (ignore win))
                 (cons 
                  (om-make-menu-item "Libraries:" nil :enabled nil)
                  (loop for lib in (all-om-libraries) 
                        when (get-lib-help-patches-foler lib)
                        collect (make-lib-help-menu lib))
                  ))
             ))
           )))))
   ))




(defun main-app-menu-item ()
  #-cocoa(when (om-standalone-p) 
           (om-make-menu (string+ *app-name* " " *version-string*)
                         (list (om-make-menu-comp 
                                (list (om-make-menu-item (string+ "About " *app-name* "...") 'show-about-win)
                                      (om-make-menu-item "Preferences" 'show-preferences-win :key ",")))
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

(defmethod open-folder-command (self) 
  #'(lambda ()
      (let ((folder (om-choose-directory-dialog :directory (or *last-open-dir* (om-user-home)))))
        (when folder
          (setf *last-open-dir* folder)
          (let ((files (om-directory folder :type (append 
                                                   (mapcar #'doctype-to-extension *om-doctypes*)
                                                   (doctype-to-ext-list :old))
                                     :recursive t)))
            (loop for file in files 
                  do (catch :load-error
                       (handler-bind ((error #'(lambda (e)
                                                 (om-message-dialog (format nil "An error occured at loading ~S: ~%~%~A" file e))
                                                 (throw :load-error nil)
                                                 )))
                       
                         (open-om-document file nil)))
                  ))))))



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
                                   :font (get-pref-value :general :listener-font)
                                   :initial-lambda #'(lambda () (in-package :om))
                                   )))))
    

(defun get-listener () (car (om-get-all-windows 'om-lisp::om-listener)))

(defun show-listener-win ()
  
  (let ((listenerwin (get-listener)))

    (om-lisp::om-set-text-editor-font (get-pref-value :general :textedit-font))
    
    (if listenerwin
        (om-select-window listenerwin)
      (om-lisp::om-make-listener 
       :initial-lambda #'(lambda () (in-package :om))
       ; :initial-prompt *om-startup-string*
       :height 200  
       :font (get-pref-value :general :listener-font)
       :input (get-pref-value :general :listener-input)
       :on-top (get-pref-value :general :listener-on-top)
       ))))

      

;(add-preference-section :appearance "Lisp")
(add-preference :general :listener-font "Listener font" :font om-lisp::*default-listener-font* nil 'set-listener-font)
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

(defmethod font-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::change-listener-font window)))

#|
(defmethod close-command ((window om-lisp::om-listener))
  #'(lambda () (om-lisp::listener-close window)))
|#

(defun show-shell () (om-lisp::om-open-shell)) 



;=======================
; Print messages
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
              
(defclass om-about-window (om-windoid) ())

(defun show-about-win () 
  (let ((about-win
         (car (om-get-all-windows 'om-about-window))))
    (if about-win
        (om-show-window about-win)
     (om-make-window 
      'om-about-window
      :subviews 
      (list (om-make-di 'om-multi-text 
                        :text (format nil 
                                      "~A v~D~%r. ~A"
                                      *app-name* *version-string* *release-date*)
                        :size (omp 200 40)
                        ))
      :close-callback #'(lambda (win) (setf *about-window* nil))
      ))
    ))

; (show-about-win)

;;;===============================
;;; PREFERENCES
;;;===============================
;;; defined in om-preferences-window.lisp 
;;; (defun show-preferences-win () nil)                          






